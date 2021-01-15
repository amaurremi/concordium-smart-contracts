use concordium_std::*;
use std::collections::btree_map::BTreeMap;

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

// todo: send notification to participants if higher bid received?
// todo send name?

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
) -> ReceiveResult<A> {

    ensure!(state.active);

    let sender_address = match ctx.sender() {
        Address::Contract(_) => bail!(),
        Address::Account(account_address) => account_address,
    };
    let existing_bid = *state.bids.entry(sender_address).or_insert_with(|| Amount::zero());

    let new_bid = existing_bid + amount;
    // Ensure that the new bid exceeds the highest bid so far
    ensure!(new_bid > state.highest_bid);
    state.highest_bid = new_bid;
    state.bids.insert(sender_address, existing_bid + amount);

    Ok(A::acceppt())
}

#[receive(contract = "auction", name = "finalize")]
fn auction_finalize<A: HasActions>(
    ctx: &impl HasReceiveContext,
    state: &mut AuctionState,
) -> ReceiveResult<A> {
    let owner = ctx.owner();
    let sender = ctx.sender();

    ensure!(sender.matches_account(&owner));
    ensure!(state.active);
    state.active = false;

    let balance = ctx.self_balance();
    if balance == Amount::zero() {
        Ok(A::accept())
    } else {
        let mut return_action = A::simple_transfer(&owner, balance);
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
        ensure!(remaining_bids.len() == 1);
        match remaining_bids.iter().next() {
            Some((_, amount)) => {
                ensure!(*amount == state.highest_bid);
                Ok(return_action)
            },
            None =>
                bail!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_infrastructure::*;

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
    fn test_auction_bid() {
        let account1 = AccountAddress([2u8; 32]);
        let account2 = AccountAddress([3u8; 32]);
        // let account3 = AccountAddress([4u8; 32]);

        let ctx0 = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        let mut ctx2 = ReceiveContextTest::empty();
        // let mut ctx3 = ReceiveContextTest::empty();

        ctx1.set_sender(Address::Account(account1));
        ctx2.set_sender(Address::Account(account2));
        // ctx3.set_sender(Address::Account(account3));

        let amount1 = Amount::from_micro_gtu(100);
        let amount2 = Amount::from_micro_gtu(300);
        // let amount3 = Amount::from_micro_gtu(300);
        // let amount4 = Amount::from_micro_gtu(400);

        let mut bid_map = BTreeMap::new();

        // initializing auction
        let mut state = auction_init(&ctx0).expect("Init results in error");
        let mut highest_bid = Amount::zero();

        // 1st bid: account1 bids amount1
        let res1: ReceiveResult<ActionsTree> = auction_bid(&ctx1, amount1, &mut state);
        res1.expect("Bidding 1 results in error");
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
        let res2: ReceiveResult<ActionsTree> = auction_bid(&ctx1, amount1, &mut state);
        res2.expect("Bidding 2 results in error");
        highest_bid += amount1;
        bid_map.insert(account1, highest_bid);
        assert_eq!(state, AuctionState{
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // 3rd bid: second account
        let res3: ReceiveResult<ActionsTree> = auction_bid(&ctx2, amount2, &mut state);
        res3.expect("Bidding 2 results in error");
        highest_bid = amount2;
        bid_map.insert(account2, highest_bid);
        assert_eq!(state, AuctionState{
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

    }

    #[test]
    fn test_auction_bid_fail() {
        let account1 = AccountAddress([2u8; 32]);
        let account2 = AccountAddress([3u8; 32]);

        let ctx0 = InitContextTest::empty();
        let mut ctx1 = ReceiveContextTest::empty();
        let mut ctx2 = ReceiveContextTest::empty();

        ctx1.set_sender(Address::Account(account1));
        ctx2.set_sender(Address::Account(account2));

        let amount1 = Amount::from_micro_gtu(100);

        let mut bid_map = BTreeMap::new();

        // initializing auction
        let mut state = auction_init(&ctx0).expect("Init results in error");
        let mut highest_bid = Amount::zero();

        // 1st bid: account1 bids amount1
        let res1: ReceiveResult<ActionsTree> = auction_bid(&ctx1, amount1, &mut state);
        res1.expect("Bidding 1 results in error");
        // now highest bid should be amount1
        bid_map.insert(account1, amount1);
        highest_bid += amount1;
        assert_eq!(state, AuctionState {
            active: true,
            highest_bid: highest_bid,
            bids: bid_map.clone() // todo bad
        });

        // 2nd bid: account2 bids amount1
        // should fail because amount is equal to highest bid
        let res2: ReceiveResult<ActionsTree> = auction_bid(&ctx2, amount1, &mut state);
        res2.expect_err("Bidding 2 should fail");
    }

}