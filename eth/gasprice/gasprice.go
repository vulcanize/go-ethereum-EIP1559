// Copyright 2015 The go-ethereum Authors
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

package gasprice

import (
	"context"
	"math/big"
	"sort"
	"sync"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/internal/ethapi"
	"github.com/ethereum/go-ethereum/params"
	"github.com/ethereum/go-ethereum/rpc"
)

var (
	maxPrice   = big.NewInt(500 * params.GWei)
	maxPremium = big.NewInt(500 * params.GWei)
	maxFeeCap  = big.NewInt(1000 * params.GWei)
)

type Config struct {
	Blocks            int
	Percentile        int
	DefaultGasPrice   *big.Int `toml:",omitempty"`
	DefaultGasPremium *big.Int `toml:",omitempty"`
	DefaultFeeCap     *big.Int `toml:",omitempty"`
}

// Oracle recommends gas prices based on the content of recent
// blocks. Suitable for both light and full clients.
type Oracle struct {
	backend     ethapi.Backend
	lastHead    common.Hash
	lastPrice   *big.Int
	lastPremium *big.Int
	lastCap     *big.Int
	lastBaseFee *big.Int
	cacheLock   sync.RWMutex
	fetchLock   sync.Mutex

	checkBlocks, maxEmpty, maxBlocks int
	percentile                       int
}

// NewOracle returns a new oracle.
func NewOracle(backend ethapi.Backend, params Config) *Oracle {
	blocks := params.Blocks
	if blocks < 1 {
		blocks = 1
	}
	percent := params.Percentile
	if percent < 0 {
		percent = 0
	}
	if percent > 100 {
		percent = 100
	}
	return &Oracle{
		backend:     backend,
		lastPrice:   params.DefaultGasPrice,
		lastPremium: params.DefaultGasPremium,
		lastCap:     params.DefaultFeeCap,
		checkBlocks: blocks,
		maxEmpty:    blocks / 2,
		maxBlocks:   blocks * 5,
		percentile:  percent,
	}
}

// SuggestPrice returns the recommended gas price.
func (gpo *Oracle) SuggestPrice(ctx context.Context) (*big.Int, error) {
	gpo.cacheLock.RLock()
	lastHead := gpo.lastHead
	lastPrice := gpo.lastPrice
	gpo.cacheLock.RUnlock()

	head, _ := gpo.backend.HeaderByNumber(ctx, rpc.LatestBlockNumber)
	headHash := head.Hash()
	if headHash == lastHead {
		return lastPrice, nil
	}

	gpo.fetchLock.Lock()
	defer gpo.fetchLock.Unlock()

	// try checking the cache again, maybe the last fetch fetched what we need
	gpo.cacheLock.RLock()
	lastHead = gpo.lastHead
	lastPrice = gpo.lastPrice
	gpo.cacheLock.RUnlock()
	if headHash == lastHead {
		return lastPrice, nil
	}

	blockNum := head.Number.Uint64()
	ch := make(chan getBlockPricesResult, gpo.checkBlocks)
	sent := 0
	exp := 0
	var blockPrices []*big.Int
	for sent < gpo.checkBlocks && blockNum > 0 {
		go gpo.getBlockPrices(ctx, types.MakeSigner(gpo.backend.ChainConfig(), big.NewInt(int64(blockNum))), blockNum, ch)
		sent++
		exp++
		blockNum--
	}
	maxEmpty := gpo.maxEmpty
	for exp > 0 {
		res := <-ch
		if res.err != nil {
			return lastPrice, res.err
		}
		exp--
		if res.price != nil {
			blockPrices = append(blockPrices, res.price)
			continue
		}
		if maxEmpty > 0 {
			maxEmpty--
			continue
		}
		if blockNum > 0 && sent < gpo.maxBlocks {
			go gpo.getBlockPrices(ctx, types.MakeSigner(gpo.backend.ChainConfig(), big.NewInt(int64(blockNum))), blockNum, ch)
			sent++
			exp++
			blockNum--
		}
	}
	price := lastPrice
	if len(blockPrices) > 0 {
		sort.Sort(bigIntArray(blockPrices))
		price = blockPrices[(len(blockPrices)-1)*gpo.percentile/100]
	}
	if price.Cmp(maxPrice) > 0 {
		price = new(big.Int).Set(maxPrice)
	}

	gpo.cacheLock.Lock()
	gpo.lastHead = headHash
	gpo.lastPrice = price
	gpo.cacheLock.Unlock()
	return price, nil
}

// SuggestPremium returns the recommended gas premium.
func (gpo *Oracle) SuggestPremium(ctx context.Context) (*big.Int, error) {
	gpo.cacheLock.RLock()
	lastHead := gpo.lastHead
	lastPremium := gpo.lastPremium
	gpo.cacheLock.RUnlock()

	head, _ := gpo.backend.HeaderByNumber(ctx, rpc.LatestBlockNumber)
	headHash := head.Hash()
	if headHash == lastHead {
		return lastPremium, nil
	}

	gpo.fetchLock.Lock()
	defer gpo.fetchLock.Unlock()

	// try checking the cache again, maybe the last fetch fetched what we need
	gpo.cacheLock.RLock()
	lastHead = gpo.lastHead
	lastPremium = gpo.lastPremium
	gpo.cacheLock.RUnlock()
	if headHash == lastHead {
		return lastPremium, nil
	}

	blockNum := head.Number.Uint64()
	ch := make(chan getBlockPremiumsResult, gpo.checkBlocks)
	sent := 0
	exp := 0
	var blockPremiums []*big.Int
	for sent < gpo.checkBlocks && blockNum > 0 {
		go gpo.getBlockPremiums(ctx, types.MakeSigner(gpo.backend.ChainConfig(), big.NewInt(int64(blockNum))), blockNum, ch)
		sent++
		exp++
		blockNum--
	}
	maxEmpty := gpo.maxEmpty
	for exp > 0 {
		res := <-ch
		if res.err != nil {
			return lastPremium, res.err
		}
		exp--
		if res.premium != nil {
			blockPremiums = append(blockPremiums, res.premium)
			continue
		}
		if maxEmpty > 0 {
			maxEmpty--
			continue
		}
		if blockNum > 0 && sent < gpo.maxBlocks {
			go gpo.getBlockPremiums(ctx, types.MakeSigner(gpo.backend.ChainConfig(), big.NewInt(int64(blockNum))), blockNum, ch)
			sent++
			exp++
			blockNum--
		}
	}
	premium := lastPremium
	if len(blockPremiums) > 0 {
		sort.Sort(bigIntArray(blockPremiums))
		premium = blockPremiums[(len(blockPremiums)-1)*gpo.percentile/100]
	}
	if premium.Cmp(maxPremium) > 0 {
		premium = new(big.Int).Set(maxPremium)
	}

	gpo.cacheLock.Lock()
	gpo.lastHead = headHash
	gpo.lastPremium = premium
	gpo.cacheLock.Unlock()
	return premium, nil
}

// SuggestCap returns the recommended fee cap.
func (gpo *Oracle) SuggestCap(ctx context.Context) (*big.Int, error) {
	gpo.cacheLock.RLock()
	lastHead := gpo.lastHead
	lastCap := gpo.lastCap
	gpo.cacheLock.RUnlock()

	head, _ := gpo.backend.HeaderByNumber(ctx, rpc.LatestBlockNumber)
	headHash := head.Hash()
	if headHash == lastHead {
		return lastCap, nil
	}

	gpo.fetchLock.Lock()
	defer gpo.fetchLock.Unlock()

	// try checking the cache again, maybe the last fetch fetched what we need
	gpo.cacheLock.RLock()
	lastHead = gpo.lastHead
	lastCap = gpo.lastCap
	gpo.cacheLock.RUnlock()
	if headHash == lastHead {
		return lastCap, nil
	}

	blockNum := head.Number.Uint64()
	ch := make(chan getBlockCapsResult, gpo.checkBlocks)
	sent := 0
	exp := 0
	var blockCaps []*big.Int
	for sent < gpo.checkBlocks && blockNum > 0 {
		go gpo.getBlockCaps(ctx, types.MakeSigner(gpo.backend.ChainConfig(), big.NewInt(int64(blockNum))), blockNum, ch)
		sent++
		exp++
		blockNum--
	}
	maxEmpty := gpo.maxEmpty
	for exp > 0 {
		res := <-ch
		if res.err != nil {
			return lastCap, res.err
		}
		exp--
		if res.cap != nil {
			blockCaps = append(blockCaps, res.cap)
			continue
		}
		if maxEmpty > 0 {
			maxEmpty--
			continue
		}
		if blockNum > 0 && sent < gpo.maxBlocks {
			go gpo.getBlockCaps(ctx, types.MakeSigner(gpo.backend.ChainConfig(), big.NewInt(int64(blockNum))), blockNum, ch)
			sent++
			exp++
			blockNum--
		}
	}
	cap := lastCap
	if len(blockCaps) > 0 {
		sort.Sort(bigIntArray(blockCaps))
		cap = blockCaps[(len(blockCaps)-1)*gpo.percentile/100]
	}
	if cap.Cmp(maxFeeCap) > 0 {
		cap = new(big.Int).Set(maxFeeCap)
	}

	gpo.cacheLock.Lock()
	gpo.lastHead = headHash
	gpo.lastCap = cap
	gpo.cacheLock.Unlock()
	return cap, nil
}

type getBlockPricesResult struct {
	price *big.Int
	err   error
}

type getBlockPremiumsResult struct {
	premium *big.Int
	err     error
}

type getBlockCapsResult struct {
	cap *big.Int
	err error
}

type transactionsByGasPrice []*types.Transaction

func (t transactionsByGasPrice) Len() int      { return len(t) }
func (t transactionsByGasPrice) Swap(i, j int) { t[i], t[j] = t[j], t[i] }
func (t transactionsByGasPrice) Less(i, j int) bool {
	iPrice := t[i].GasPrice()
	jPrice := t[j].GasPrice()
	if iPrice == nil {
		iPrice = big.NewInt(0)
	}
	if jPrice == nil {
		jPrice = big.NewInt(0)
	}
	return iPrice.Cmp(jPrice) < 0
}

type transactionsByGasPremium []*types.Transaction

func (t transactionsByGasPremium) Len() int      { return len(t) }
func (t transactionsByGasPremium) Swap(i, j int) { t[i], t[j] = t[j], t[i] }
func (t transactionsByGasPremium) Less(i, j int) bool {
	iPremium := t[i].GasPremium()
	jPremium := t[j].GasPremium()
	if iPremium == nil {
		iPremium = big.NewInt(0)
	}
	if jPremium == nil {
		jPremium = big.NewInt(0)
	}
	return iPremium.Cmp(jPremium) < 0
}

type transactionsByFeeCap []*types.Transaction

func (t transactionsByFeeCap) Len() int      { return len(t) }
func (t transactionsByFeeCap) Swap(i, j int) { t[i], t[j] = t[j], t[i] }
func (t transactionsByFeeCap) Less(i, j int) bool {
	iCap := t[i].FeeCap()
	jCap := t[j].FeeCap()
	if iCap == nil {
		iCap = big.NewInt(0)
	}
	if jCap == nil {
		jCap = big.NewInt(0)
	}
	return iCap.Cmp(jCap) < 0
}

// getBlockPrices calculates the lowest transaction gas price in a given block
// and sends it to the result channel. If the block is empty, price is nil.
func (gpo *Oracle) getBlockPrices(ctx context.Context, signer types.Signer, blockNum uint64, ch chan getBlockPricesResult) {
	block, err := gpo.backend.BlockByNumber(ctx, rpc.BlockNumber(blockNum))
	if block == nil {
		ch <- getBlockPricesResult{nil, err}
		return
	}

	blockTxs := block.Transactions()
	txs := make([]*types.Transaction, len(blockTxs))
	copy(txs, blockTxs)
	sort.Sort(transactionsByGasPrice(txs))

	for _, tx := range txs {
		sender, err := types.Sender(signer, tx)
		if err == nil && sender != block.Coinbase() {
			ch <- getBlockPricesResult{tx.GasPrice(), nil}
			return
		}
	}
	ch <- getBlockPricesResult{nil, nil}
}

// getBlockPremiums calculates the lowest transaction gas premium in a given block
// and sends it to the result channel. If the block is empty, price is nil.
func (gpo *Oracle) getBlockPremiums(ctx context.Context, signer types.Signer, blockNum uint64, ch chan getBlockPremiumsResult) {
	block, err := gpo.backend.BlockByNumber(ctx, rpc.BlockNumber(blockNum))
	if block == nil {
		ch <- getBlockPremiumsResult{nil, err}
		return
	}

	blockTxs := block.Transactions()
	txs := make([]*types.Transaction, len(blockTxs))
	copy(txs, blockTxs)
	sort.Sort(transactionsByGasPremium(txs))

	for _, tx := range txs {
		sender, err := types.Sender(signer, tx)
		if err == nil && sender != block.Coinbase() {
			ch <- getBlockPremiumsResult{tx.GasPremium(), nil}
			return
		}
	}
	ch <- getBlockPremiumsResult{nil, nil}
}

// getBlockCaps calculates the lowest transaction fee cap in a given block
// and sends it to the result channel. If the block is empty, price is nil.
func (gpo *Oracle) getBlockCaps(ctx context.Context, signer types.Signer, blockNum uint64, ch chan getBlockCapsResult) {
	block, err := gpo.backend.BlockByNumber(ctx, rpc.BlockNumber(blockNum))
	if block == nil {
		ch <- getBlockCapsResult{nil, err}
		return
	}

	blockTxs := block.Transactions()
	txs := make([]*types.Transaction, len(blockTxs))
	copy(txs, blockTxs)
	sort.Sort(transactionsByFeeCap(txs))

	for _, tx := range txs {
		sender, err := types.Sender(signer, tx)
		if err == nil && sender != block.Coinbase() {
			ch <- getBlockCapsResult{tx.FeeCap(), nil}
			return
		}
	}
	ch <- getBlockCapsResult{nil, nil}
}

type bigIntArray []*big.Int

func (s bigIntArray) Len() int           { return len(s) }
func (s bigIntArray) Less(i, j int) bool { return s[i].Cmp(s[j]) < 0 }
func (s bigIntArray) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
