
# adds a column to the player data of the chosen offer.
add_chosen_offer <- function (player_data) {
  output_data <- player_data %>%
    mutate(OfferTaken = case_when(
      ChosenOffer == LowerOffer ~ "Lower",
      ChosenOffer == CashBuilder ~ "Middle",
      ChosenOffer == HigherOffer ~ "Higher" ))
  return(output_data)
}


# generate total offer wins table
get_total_offer_wins <- function(players){
  Total_offers <- players %>%
    add_chosen_offer () %>%
    mutate(win = ifelse(HTH_Result >0, TRUE, FALSE)) %>%
    group_by(OfferTaken, win) %>%
    summarise(
      total_count = n()
    ) %>%
    ungroup() %>%
    group_by(OfferTaken) %>%
    summarise(
      total_total = sum(total_count)
    )

  Total_offers_wins <- players %>%
    add_chosen_offer () %>%
    mutate(win = ifelse(HTH_Result >0, TRUE, FALSE)) %>%
    group_by(OfferTaken, win) %>%
    summarise(
      total_count = n()
    )

  Total_offers_wins <- left_join(Total_offers_wins, Total_offers, by = "OfferTaken") %>%
    filter(win == TRUE) %>%
    mutate(pct = total_count/total_total)

  return(Total_offers_wins)
}
