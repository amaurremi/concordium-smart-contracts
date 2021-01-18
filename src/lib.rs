use concordium_std::*;
use std::collections::btree_map::BTreeMap;
use core::fmt::Debug;

#[contract_state(contract = "auction")]
#[derive(Debug, Serialize, SchemaType, Eq, PartialEq)]
pub struct AuctionState {
    // Is the auction active?
    active: bool,
    highest_bid: Amount,
    // Keeping track of which account bid how much money
    #[concordium(map_size_length = 2)]
    bids: BTreeMap<AccountAddress, Amount>,
}

fn fresh_state() -> AuctionState {
    AuctionState {
        active: true,
        highest_bid: Amount::zero(), // todo: take parameter for highest bid and expiry time
        bids: BTreeMap::new()
    }
}
// todo implement proxy bid in which the winner pays the second-highest bid

#[derive(Debug, PartialEq, Eq)]
enum ReceiveError {
    ContractSender,
    ZeroTransfer,
    BidTooLow { bidded: Amount, highest_bid: Amount },
    AuctionFinalized,
}

#[derive(Debug, PartialEq, Eq)]
enum FinalizeError {
    SenderMustBeOwner,
    AuctionFinalized,
    BidMapError
}

// todo: send notification to participants if higher bid received?

#[init(contract = "auction")]
fn auction_init(_ctx: &impl HasInitContext) -> InitResult<AuctionState> {

    Ok(fresh_state())
}
// todo: store the thing being auctioned

#[receive(contract = "auction", name = "bid", payable)]
fn auction_bid<A: HasActions>(
    ctx: &impl HasReceiveContext,
    amount: Amount,
    state: &mut AuctionState,
) -> Result<A, ReceiveError> {

    ensure!(state.active, ReceiveError::AuctionFinalized);
    ensure!(amount.micro_gtu > 0, ReceiveError::ZeroTransfer);

    let sender_address = match ctx.sender() {
        Address::Contract(_) => bail!(ReceiveError::ContractSender),
        Address::Account(account_address) => account_address,
    };
    let existing_bid = *state.bids.entry(sender_address).or_insert_with(|| Amount::zero()); // todo store u64, not Amount

    let new_bid = existing_bid + amount;
    // Ensure that the new bid exceeds the highest bid so far
    ensure!(new_bid > state.highest_bid, ReceiveError::BidTooLow { bidded: amount, highest_bid: state.highest_bid });
    state.highest_bid = new_bid;
    state.bids.insert(sender_address, existing_bid + amount);

    Ok(A::accept())
}

#[receive(contract = "auction", name = "finalize")]
fn auction_finalize<A: HasActions>(
    ctx: &impl HasReceiveContext,
    state: &mut AuctionState,
) -> Result<A, FinalizeError> {
    let owner = ctx.owner();
    let sender = ctx.sender();

    ensure!(sender.matches_account(&owner), FinalizeError::SenderMustBeOwner);
    ensure!(state.active, FinalizeError::AuctionFinalized);
    state.active = false;

    let balance = ctx.self_balance();
    if balance == Amount::zero() {
        Ok(A::accept())
    } else {
        let mut return_action = A::simple_transfer(&owner, state.highest_bid);
        let mut remaining_bids = BTreeMap::new();
        // Return bids that are smaller than highest
        for (addr, amnt) in state.bids.iter() {
            if *amnt < state.highest_bid {
                return_action = return_action.and_then(A::simple_transfer(addr, *amnt));
            } else {
                remaining_bids.insert(*addr, *amnt);
            }
        }
        // Ensure that the only bidder left in the map is the one with the highest bid
        ensure!(remaining_bids.len() == 1, FinalizeError::BidMapError);
        match remaining_bids.iter().next() {
            Some((_, amount)) => {
                ensure!(*amount == state.highest_bid, FinalizeError::BidMapError);
                Ok(return_action)
            },
            None =>
                bail!(FinalizeError::BidMapError)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_infrastructure::*;

    fn expect_error<E, T>(expr: Result<T, E>, err: E, msg: &str)
        where E: Eq + Debug, T: Debug {
        let actual = expr.expect_err(msg);
        assert_eq!(actual, err);
    }

    #[test]
    fn test_init() {
        let ctx = InitContextTest::empty();
        let state_result = auction_init(&ctx);
        let state = state_result.expect("Contract initialization results in error");
        assert_eq!(state,
                   fresh_state(),
                   "Auction state should be new after initialization"
        );
    }

    #[test]
    fn test_auction_bid_and_finalize() {
        let owner = AccountAddress([0u8; 32]);
        let account1 = AccountAddress([1u8; 32]);
        let account2 = AccountAddress([2u8; 32]);

        let ctx0 = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        let mut ctx2 = ReceiveContextTest::empty();
        let mut ctx3 = ReceiveContextTest::empty();
        let mut ctx4 = ReceiveContextTest::empty();

        ctx1.set_sender(Address::Account(account1));
        ctx2.set_sender(Address::Account(account2));
        ctx3.set_owner(owner);
        ctx3.set_sender(Address::Account(account2));
        ctx4.set_owner(owner);
        ctx4.set_sender(Address::Account(owner));

        let amount1 = Amount::from_micro_gtu(100);
        let amount2 = amount1 + amount1 + amount1;
        let amount3 = amount2 + amount2;

        let mut bid_map = BTreeMap::new();

        // initializing auction
        let mut state = auction_init(&ctx0).expect("Initialization should pass");
        let mut highest_bid = Amount::zero();

        // 1st bid: account1 bids amount1
        let res1: Result<ActionsTree, _> = auction_bid(&ctx1, amount1, &mut state);
        res1.expect("Bidding 1 should pass");
        // now highest bid should be 100
        bid_map.insert(account1, amount1);
        highest_bid += amount1;
        assert_eq!(state, AuctionState {
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // 2nd bid: account1 bids amount1 again
        // should work even though it's the same amount because account1 simply increases their bid
        let res2: Result<ActionsTree, _> = auction_bid(&ctx1, amount1, &mut state);
        res2.expect("Bidding 2 should pass");
        highest_bid += amount1;
        bid_map.insert(account1, highest_bid);
        assert_eq!(state, AuctionState{
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // 3rd bid: second account
        let res3: Result<ActionsTree, _> = auction_bid(&ctx2, amount2, &mut state);
        res3.expect("Bidding 3 should pass");
        highest_bid = amount2;
        bid_map.insert(account2, highest_bid);
        assert_eq!(state, AuctionState{
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // finalizing auction with wrong owner
        let finres: Result<ActionsTree, _> = auction_finalize(&ctx3, &mut state);
        expect_error(finres, FinalizeError::SenderMustBeOwner, "Finalizing auction should fail with the wrong sender");

        // finalizing auction
        ctx4.set_self_balance(highest_bid);
        let finres2: Result<ActionsTree, _> = auction_finalize(&ctx4,  &mut state);
        let actions = finres2.expect("Finalizing auction should work");
        assert_eq!(actions, ActionsTree::simple_transfer(&owner, amount2)
                            .and_then(ActionsTree::simple_transfer(&account1, amount1 + amount1)));
        assert_eq!(state, AuctionState{
            active: false,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // attempting to finalize auction again should fail
        let finres3: Result<ActionsTree, _> = auction_finalize(&ctx4, &mut state);
        expect_error(finres3, FinalizeError::AuctionFinalized, "Finalizing auction a second time should fail");

        // attempting to bid again should fail
        let res4: Result<ActionsTree, _> = auction_bid(&ctx2, amount3, &mut state);
        expect_error(res4, ReceiveError::AuctionFinalized, "Bidding should fail because the auction is finalized");
    }

    #[test]
    fn test_auction_bid_repeated_bid() {
        let account1 = AccountAddress([1u8; 32]);
        let account2 = AccountAddress([2u8; 32]);
        let amount = Amount::from_micro_gtu(100);
        let ctx0 = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        let mut ctx2 = ReceiveContextTest::empty();
        let mut bid_map = BTreeMap::new();

        ctx1.set_sender(Address::Account(account1));
        ctx2.set_sender(Address::Account(account2));

        // initializing auction
        let mut state = auction_init(&ctx0).expect("Init results in error");
        let mut highest_bid = Amount::zero();

        // 1st bid: account1 bids amount1
        let res1: Result<ActionsTree, _> = auction_bid(&ctx1, amount, &mut state);
        res1.expect("Bidding 1 should pass");
        // now highest bid should be amount1
        bid_map.insert(account1, amount);
        highest_bid += amount;
        assert_eq!(state, AuctionState {
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // 2nd bid: account2 bids amount1
        // should fail because amount is equal to highest bid
        let res2: Result<ActionsTree, _> = auction_bid(&ctx2, amount, &mut state);
        expect_error(res2, ReceiveError::BidTooLow { bidded: amount, highest_bid: highest_bid }, "Bidding 2 should fail because bidded amount must be higher than highest bid");
    }

    #[test]
    fn test_auction_bid_zero() {
        let account = AccountAddress([1u8; 32]);
        let ctx = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        ctx1.set_sender(Address::Account(account));

        let mut state = auction_init(&ctx).expect("Init results in error");

        let res: Result<ActionsTree, _> = auction_bid(&ctx1, Amount::zero(), &mut state);
        expect_error(res, ReceiveError::ZeroTransfer, "Bidding zero should fail");
    }
}
// todo factor out common code in tests