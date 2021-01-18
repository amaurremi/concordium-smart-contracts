use concordium_std::*;
use std::collections::btree_map::BTreeMap;
use core::fmt::Debug;

#[contract_state(contract = "auction")]
#[derive(Debug, Serialize, SchemaType, Eq, PartialEq)]
pub struct State {
    // Is the auction active?
    auction_state: AuctionState,
    // The highest bid so far
    highest_bid: Amount,
    // The sold item
    item: String,
    // Expiration time of the auction at which bids will be closed, the winner
    // pays the highest price, and everyone else gets their money back
    expiry: Timestamp,
    // Keeping track of which account bid how much money
    #[concordium(map_size_length = 2)]
    bids: BTreeMap<AccountAddress, Amount>,
}

#[derive(Debug, Serialize, SchemaType, Eq, PartialEq)]
pub enum AuctionState {
    ACTIVE,
    SOLD,
    EXPIRED,
}

fn fresh_state(itm: String, exp: Timestamp) -> State {
    State {
        auction_state: AuctionState::ACTIVE,
        highest_bid: Amount::zero(),
        item: itm,
        expiry: exp,
        bids: BTreeMap::new(),
    }
}
// todo implement proxy bid in which the winner pays the second-highest bid

#[derive(Serialize)]
struct InitParameter {
    item: String,
    expiry: Timestamp,
}

// todo custom serialize for initparameter

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
    BidMapError,
    AuctionExpired,
}

// todo: show better error messages?
// todo: log when new bids arrive

#[init(contract = "auction")]
fn auction_init(ctx: &impl HasInitContext) -> InitResult<State> {
    let parameter: InitParameter = ctx.parameter_cursor().get()?;
    Ok(fresh_state(parameter.item, parameter.expiry))
}

#[receive(contract = "auction", name = "bid", payable)]
fn auction_bid<A: HasActions>(
    ctx: &impl HasReceiveContext,
    amount: Amount,
    state: &mut State,
) -> Result<A, ReceiveError> {

    ensure!(state.auction_state == AuctionState::ACTIVE, ReceiveError::AuctionFinalized);
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
    state: &mut State,
) -> Result<A, FinalizeError> {

    let slot_time = ctx.metadata().slot_time();
    if slot_time > state.expiry {
        bail!(FinalizeError::AuctionExpired);
    }

    let owner = ctx.owner();
    let sender = ctx.sender();

    ensure!(sender.matches_account(&owner), FinalizeError::SenderMustBeOwner);
    ensure!(state.auction_state == AuctionState::ACTIVE, FinalizeError::AuctionFinalized);
    state.auction_state = AuctionState::SOLD;

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

    fn dummy_fresh_state() -> State {
        dummy_state(Amount::zero(), BTreeMap::new())
    }

    fn dummy_state(highest: Amount, bids: BTreeMap<AccountAddress, Amount>) -> State {
        State {
            auction_state: AuctionState::ACTIVE,
            highest_bid: highest,
            item: String::from("test"),
            expiry: Timestamp::from_timestamp_millis(1),
            bids: bids
        }
    }

    fn expect_error<E, T>(expr: Result<T, E>, err: E, msg: &str)
        where E: Eq + Debug, T: Debug {
        let actual = expr.expect_err(msg);
        assert_eq!(actual, err);
    }

    #[test]
    fn test_init() {
        let mut ctx = InitContextTest::empty();
        let parameter = InitParameter {
            item: "test".to_string(),
            expiry: Timestamp::from_timestamp_millis(1)
        };
        let parameter_bytes = to_bytes(&parameter);
        ctx.set_parameter(&parameter_bytes);
        let state_result = auction_init(&ctx);
        let state = state_result.expect("Contract initialization results in error");
        assert_eq!(state,
                   dummy_fresh_state(),
                   "Auction state should be new after initialization"
        );
    }

    #[test]
    fn test_auction_bid_and_finalize() {
        let owner = AccountAddress([0u8; 32]);
        let account1 = AccountAddress([1u8; 32]);
        let account2 = AccountAddress([2u8; 32]);

        let mut ctx0 = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        let mut ctx2 = ReceiveContextTest::empty();
        let mut ctx3 = ReceiveContextTest::empty();
        let mut ctx4 = ReceiveContextTest::empty();
        let mut ctx_expired = ReceiveContextTest::empty();

        let parameter = InitParameter {
            item: "test".to_string(),
            expiry: Timestamp::from_timestamp_millis(1)
        };
        let parameter_bytes = to_bytes(&parameter);
        ctx0.set_parameter(&parameter_bytes);

        ctx1.set_sender(Address::Account(account1));
        ctx2.set_sender(Address::Account(account2));
        ctx3.set_owner(owner);
        ctx3.set_sender(Address::Account(account2));
        ctx3.set_metadata_slot_time(Timestamp::from_timestamp_millis(0));
        ctx4.set_owner(owner);
        ctx4.set_sender(Address::Account(owner));
        ctx4.set_metadata_slot_time(Timestamp::from_timestamp_millis(0));
        ctx_expired.set_metadata_slot_time(Timestamp::from_timestamp_millis(2));

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
        assert_eq!(state, dummy_state(highest_bid, bid_map.clone())); // todo bad?

        // 2nd bid: account1 bids amount1 again
        // should work even though it's the same amount because account1 simply increases their bid
        let res2: Result<ActionsTree, _> = auction_bid(&ctx1, amount1, &mut state);
        res2.expect("Bidding 2 should pass");
        highest_bid += amount1;
        bid_map.insert(account1, highest_bid);
        assert_eq!(state, dummy_state(highest_bid, bid_map.clone()));

        // 3rd bid: second account
        let res3: Result<ActionsTree, _> = auction_bid(&ctx2, amount2, &mut state);
        res3.expect("Bidding 3 should pass");
        highest_bid = amount2;
        bid_map.insert(account2, highest_bid);
        assert_eq!(state, dummy_state(highest_bid, bid_map.clone()));

        // trying to finalize auction with wrong owner
        let finres: Result<ActionsTree, _> = auction_finalize(&ctx3, &mut state);
        expect_error(finres, FinalizeError::SenderMustBeOwner, "Finalizing auction should fail with the wrong sender");

        // trying to finalize auction after expiry time
        let finres: Result<ActionsTree, _> = auction_finalize(&ctx_expired, &mut state);
        expect_error(finres, FinalizeError::AuctionExpired, "Finalizing auction should fail when it's expired");

        // finalizing auction
        ctx4.set_self_balance(highest_bid);
        let finres2: Result<ActionsTree, _> = auction_finalize(&ctx4,  &mut state);
        let actions = finres2.expect("Finalizing auction should work");
        assert_eq!(actions, ActionsTree::simple_transfer(&owner, amount2)
                            .and_then(ActionsTree::simple_transfer(&account1, amount1 + amount1)));
        assert_eq!(state, State{
            auction_state: AuctionState::SOLD,
            highest_bid: highest_bid,
            item: "test".to_string(),
            expiry: Timestamp::from_timestamp_millis(1),
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
        let mut ctx0 = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        let mut ctx2 = ReceiveContextTest::empty();
        let mut bid_map = BTreeMap::new();

        let parameter = InitParameter {
            item: "test".to_string(),
            expiry: Timestamp::from_timestamp_millis(1)
        };
        let parameter_bytes = to_bytes(&parameter);
        ctx0.set_parameter(&parameter_bytes);

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
        assert_eq!(state, dummy_state(highest_bid, bid_map.clone()));

        // 2nd bid: account2 bids amount1
        // should fail because amount is equal to highest bid
        let res2: Result<ActionsTree, _> = auction_bid(&ctx2, amount, &mut state);
        expect_error(res2, ReceiveError::BidTooLow { bidded: amount, highest_bid: highest_bid }, "Bidding 2 should fail because bidded amount must be higher than highest bid");
    }

    #[test]
    fn test_auction_bid_zero() {
        let account = AccountAddress([1u8; 32]);
        let mut ctx = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        ctx1.set_sender(Address::Account(account));
        let parameter = InitParameter {
            item: "test".to_string(),
            expiry: Timestamp::from_timestamp_millis(1)
        };
        let parameter_bytes = to_bytes(&parameter);
        ctx.set_parameter(&parameter_bytes);

        let mut state = auction_init(&ctx).expect("Init results in error");

        let res: Result<ActionsTree, _> = auction_bid(&ctx1, Amount::zero(), &mut state);
        expect_error(res, ReceiveError::ZeroTransfer, "Bidding zero should fail");
    }
}
// todo factor out common code in tests