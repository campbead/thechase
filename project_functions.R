
# adds a column to the player data of the chosen offer.
add_chosen_offer <- function (player_data) {
  output_data <- player_data %>%
    mutate(OfferTaken = case_when(
      ChosenOffer == LowerOffer ~ "Lower",
      ChosenOffer == CashBuilder ~ "Middle",
      ChosenOffer == HigherOffer ~ "Higher" ))
  return(output_data)
}
