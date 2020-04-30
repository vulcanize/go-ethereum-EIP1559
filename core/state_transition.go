// Copyright 2014 The go-ethereum Authors
// This file is part of the go-ethereum library.
//
// The go-ethereum library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The go-ethereum library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the go-ethereum library. If not, see <http://www.gnu.org/licenses/>.

package core

import (
	"errors"
	"math"
	"math/big"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/vm"
	"github.com/ethereum/go-ethereum/log"
	"github.com/ethereum/go-ethereum/params"
)

var (
	errInsufficientBalanceForGas      = errors.New("insufficient balance to pay for gas")
	ErrEIP1559GasPriceLessThanBaseFee = errors.New("EIP11559 GasPrice is less than the current BaseFee")
)

/*
The State Transitioning Model

A state transition is a change made when a transaction is applied to the current world state
The state transitioning model does all the necessary work to work out a valid new state root.

1) Nonce handling
2) Pre pay gas
3) Create a new state object if the recipient is \0*32
4) Value transfer
== If contract creation ==
  4a) Attempt to run transaction data
  4b) If valid, use result as code for the new state object
== end ==
5) Run Script section
6) Derive new state root
*/
type StateTransition struct {
	gp              *GasPool
	gp1559          *GasPool
	msg             Message
	gas             uint64
	gasPrice        *big.Int
	initialGas      uint64
	value           *big.Int
	data            []byte
	state           vm.StateDB
	evm             *vm.EVM
	isEIP1559       bool
	eip1559GasPrice *big.Int
}

// Message represents a message sent to a contract.
type Message interface {
	From() common.Address
	//FromFrontier() (common.Address, error)
	To() *common.Address

	GasPrice() *big.Int
	Gas() uint64
	Value() *big.Int

	Nonce() uint64
	CheckNonce() bool
	Data() []byte

	GasPremium() *big.Int
	FeeCap() *big.Int
}

// IntrinsicGas computes the 'intrinsic gas' for a message with the given data.
func IntrinsicGas(data []byte, contractCreation, isHomestead bool, isEIP2028 bool) (uint64, error) {
	// Set the starting gas for the raw transaction
	var gas uint64
	if contractCreation && isHomestead {
		gas = params.TxGasContractCreation
	} else {
		gas = params.TxGas
	}
	// Bump the required gas by the amount of transactional data
	if len(data) > 0 {
		// Zero and non-zero bytes are priced differently
		var nz uint64
		for _, byt := range data {
			if byt != 0 {
				nz++
			}
		}
		// Make sure we don't exceed uint64 for all data combinations
		nonZeroGas := params.TxDataNonZeroGasFrontier
		if isEIP2028 {
			nonZeroGas = params.TxDataNonZeroGasEIP2028
		}
		if (math.MaxUint64-gas)/nonZeroGas < nz {
			return 0, vm.ErrOutOfGas
		}
		gas += nz * nonZeroGas

		z := uint64(len(data)) - nz
		if (math.MaxUint64-gas)/params.TxDataZeroGas < z {
			return 0, vm.ErrOutOfGas
		}
		gas += z * params.TxDataZeroGas
	}
	return gas, nil
}

// NewStateTransition initialises and returns a new state transition object.
func NewStateTransition(evm *vm.EVM, msg Message, gp, gp1559 *GasPool) *StateTransition {
	isEIP1559 := evm.ChainConfig().IsEIP1559(evm.BlockNumber) && msg.GasPrice() == nil && msg.GasPremium() != nil && msg.FeeCap() != nil && evm.BaseFee != nil && gp1559 != nil
	st := &StateTransition{
		gp:        gp,
		gp1559:    gp1559,
		evm:       evm,
		msg:       msg,
		gasPrice:  msg.GasPrice(),
		value:     msg.Value(),
		data:      msg.Data(),
		state:     evm.StateDB,
		isEIP1559: isEIP1559,
	}
	if isEIP1559 {
		// EP1559 gasPrice = min(BASEFEE + tx.fee_premium, tx.fee_cap)
		st.eip1559GasPrice = new(big.Int).Add(evm.BaseFee, msg.GasPremium())
		if st.eip1559GasPrice.Cmp(msg.FeeCap()) > 0 {
			st.eip1559GasPrice.Set(msg.FeeCap())
		}
	}
	return st
}

// ApplyMessage computes the new state by applying the given message
// against the old state within the environment.
//
// ApplyMessage returns the bytes returned by any EVM execution (if it took place),
// the gas used (which includes gas refunds) and an error if it failed. An error always
// indicates a core error meaning that the message would always fail for that particular
// state and would never be accepted within a block.
func ApplyMessage(evm *vm.EVM, msg Message, gp, gp1559 *GasPool) ([]byte, uint64, bool, error) {
	return NewStateTransition(evm, msg, gp, gp1559).TransitionDb()
}

// to returns the recipient of the message.
func (st *StateTransition) to() common.Address {
	if st.msg == nil || st.msg.To() == nil /* contract creation */ {
		return common.Address{}
	}
	return *st.msg.To()
}

func (st *StateTransition) useGas(amount uint64) error {
	if st.gas < amount {
		return vm.ErrOutOfGas
	}
	st.gas -= amount

	return nil
}

func (st *StateTransition) buyGas() error {
	if st.isEIP1559 {
		return st.buyGasEIP1559()
	}
	return st.buyGasLegacy()
}

func (st *StateTransition) buyGasEIP1559() error {
	// tx.origin pays gasPrice * tx.gas
	mgval := new(big.Int).Mul(new(big.Int).SetUint64(st.msg.Gas()), st.eip1559GasPrice)
	if st.state.GetBalance(st.msg.From()).Cmp(mgval) < 0 {
		return errInsufficientBalanceForGas
	}
	if err := st.gp1559.SubGas(st.msg.Gas()); err != nil {
		return err
	}
	st.gas += st.msg.Gas()

	st.initialGas = st.msg.Gas()
	st.state.SubBalance(st.msg.From(), mgval)
	return nil
}

func (st *StateTransition) buyGasLegacy() error {
	mgval := new(big.Int).Mul(new(big.Int).SetUint64(st.msg.Gas()), st.gasPrice)
	if st.state.GetBalance(st.msg.From()).Cmp(mgval) < 0 {
		return errInsufficientBalanceForGas
	}
	if err := st.gp.SubGas(st.msg.Gas()); err != nil {
		return err
	}
	st.gas += st.msg.Gas()

	st.initialGas = st.msg.Gas()
	st.state.SubBalance(st.msg.From(), mgval)
	return nil
}

func (st *StateTransition) preCheck() error {
	// Make sure this transaction's nonce is correct.
	if st.msg.CheckNonce() {
		nonce := st.state.GetNonce(st.msg.From())
		if nonce < st.msg.Nonce() {
			return ErrNonceTooHigh
		} else if nonce > st.msg.Nonce() {
			return ErrNonceTooLow
		}
	}
	// If we have reached the EIP1559 finalization block and we do not conform with EIP1559, throw an error
	if st.evm.ChainConfig().IsEIP1559Finalized(st.evm.BlockNumber) && !st.isEIP1559 {
		return ErrTxNotEIP1559
	}
	// If we are before the EIP1559 activation block, throw an error if we have EIP1559 fields or do not have a GasPrice
	if !st.evm.ChainConfig().IsEIP1559(st.evm.BlockNumber) && (st.msg.GasPremium() != nil || st.msg.FeeCap() != nil || st.gp1559 != nil || st.evm.BaseFee != nil || st.msg.GasPrice() == nil) {
		return ErrTxIsEIP1559
	}
	// If transaction has both legacy and EIP1559 fields, throw an error
	if (st.msg.GasPremium() != nil || st.msg.FeeCap() != nil) && st.msg.GasPrice() != nil {
		return ErrTxSetsLegacyAndEIP1559Fields
	}
	// We need a BaseFee if we are past EIP1559 activation
	if st.evm.ChainConfig().IsEIP1559(st.evm.BlockNumber) && st.evm.BaseFee == nil {
		return ErrNoBaseFee
	}
	// We need either a GasPrice or a FeeCap and GasPremium to be set
	if st.msg.GasPrice() == nil && (st.msg.GasPremium() == nil || st.msg.FeeCap() == nil) {
		return ErrMissingGasFields
	}
	// If it is an EIp1559 transaction, make sure the derived gasPrice is >= baseFee
	if st.isEIP1559 {
		if st.eip1559GasPrice.Cmp(st.evm.BaseFee) < 0 {
			return ErrEIP1559GasPriceLessThanBaseFee
		}
	}
	return st.buyGas()
}

// TransitionDb will transition the state by applying the current message and
// returning the result including the used gas. It returns an error if failed.
// An error indicates a consensus issue.
func (st *StateTransition) TransitionDb() (ret []byte, usedGas uint64, failed bool, err error) {
	if err = st.preCheck(); err != nil {
		return
	}
	msg := st.msg
	sender := vm.AccountRef(msg.From())
	homestead := st.evm.ChainConfig().IsHomestead(st.evm.BlockNumber)
	istanbul := st.evm.ChainConfig().IsIstanbul(st.evm.BlockNumber)
	contractCreation := msg.To() == nil

	// Pay intrinsic gas
	gas, err := IntrinsicGas(st.data, contractCreation, homestead, istanbul)
	if err != nil {
		return nil, 0, false, err
	}
	if err = st.useGas(gas); err != nil {
		return nil, 0, false, err
	}

	var (
		evm = st.evm
		// vm errors do not effect consensus and are therefor
		// not assigned to err, except for insufficient balance
		// error.
		vmerr error
	)
	if contractCreation {
		ret, _, st.gas, vmerr = evm.Create(sender, st.data, st.gas, st.value)
	} else {
		// Increment the nonce for the next transaction
		st.state.SetNonce(msg.From(), st.state.GetNonce(sender.Address())+1)
		ret, st.gas, vmerr = evm.Call(sender, st.to(), st.data, st.gas, st.value)
	}
	if vmerr != nil {
		log.Debug("VM returned with error", "err", vmerr)
		// The only possible consensus-error would be if there wasn't
		// sufficient balance to make the transfer happen. The first
		// balance transfer may never fail.
		if vmerr == vm.ErrInsufficientBalance {
			return nil, 0, false, vmerr
		}
	}
	st.refundGas()
	if st.isEIP1559 {
		// block.coinbase gains (gasprice - BASEFEE) * gasused
		coinBaseCredit := new(big.Int).Mul(new(big.Int).Sub(st.eip1559GasPrice, st.evm.BaseFee), new(big.Int).SetUint64(st.gasUsed()))
		// coinbaseCredit cannot be negative since we precheck that eip1559GasPrice >= st.evm.BaseFee
		st.state.AddBalance(st.evm.Coinbase, coinBaseCredit)

		return ret, st.gasUsed(), vmerr != nil, err
	}
	st.state.AddBalance(st.evm.Coinbase, new(big.Int).Mul(new(big.Int).SetUint64(st.gasUsed()), st.gasPrice))

	return ret, st.gasUsed(), vmerr != nil, err
}

func (st *StateTransition) refundGas() {
	if st.isEIP1559 {
		st.refundGasEIP1559()
		return
	}
	st.refundGasLegacy()
}

func (st *StateTransition) refundGasLegacy() {
	// Apply refund counter, capped to half of the used gas.
	refund := st.gasUsed() / 2
	if refund > st.state.GetRefund() {
		refund = st.state.GetRefund()
	}
	st.gas += refund

	// Return ETH for remaining gas, exchanged at the original rate.
	remaining := new(big.Int).Mul(new(big.Int).SetUint64(st.gas), st.gasPrice)
	st.state.AddBalance(st.msg.From(), remaining)

	// Also return remaining gas to the block gas counter so it is
	// available for the next transaction.
	st.gp.AddGas(st.gas)
}

func (st *StateTransition) refundGasEIP1559() {
	// Apply refund counter, capped to half of the used gas.
	refund := st.gasUsed() / 2
	if refund > st.state.GetRefund() {
		refund = st.state.GetRefund()
	}
	st.gas += refund

	// tx.origin gets refunded gasprice * (tx.gas - gasused)
	txGasSubUsed := new(big.Int).Sub(new(big.Int).SetUint64(st.msg.Gas()), new(big.Int).SetUint64(st.gasUsed()))
	remaining := new(big.Int).Mul(st.eip1559GasPrice, txGasSubUsed)
	st.state.AddBalance(st.msg.From(), remaining)

	// Also return remaining gas to the block gas counter so it is
	// available for the next transaction.
	st.gp1559.AddGas(st.gas)
}

// gasUsed returns the amount of gas used up by the state transition.
func (st *StateTransition) gasUsed() uint64 {
	return st.initialGas - st.gas
}
