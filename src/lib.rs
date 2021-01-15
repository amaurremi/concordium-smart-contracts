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

#[init(contract = "auction")]
fn auction_init(_ctx: &impl HasInitContext) -> InitResult<AuctionState> {

    Ok(AuctionState {
        active: true,
        highest_bid: Amount::zero(), // todo: take parameter for highest bid
        bids: BTreeMap::new(),
    })
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

    Ok(A::accept())
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
                   AuctionState {
                       active: true,
                       highest_bid: Amount::zero(), // todo: take parameter for highest bid
                       bids: BTreeMap::new()
                   },
                   "Auction state should be new after initialization"
        );
    }

}