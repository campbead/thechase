
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
    mutate(pct = total_count/total_total) %>%
    mutate(
      OfferTaken = factor(OfferTaken,
                          levels = c("Higher", "Middle", "Lower")
      )
    )

  return(Total_offers_wins)
}

calc_win_pct_by_chosen_offer_taken_offer_binned <- function(data, bin_size){
  # modify player data for win information
  temp_data <- data %>%
    add_chosen_offer() %>%
    # add column for win
    mutate(win = ifelse(HTH_Result >0, TRUE, FALSE))

  # compute total times each chosen offer and offer taken pair is selected
  total_data <- temp_data %>%
    # round chosen offer to nearest 1000
    mutate(ChosenOffer = round(ChosenOffer / bin_size)* bin_size) %>%
    group_by(ChosenOffer, OfferTaken) %>%
    # count all times a pair of chosen offer and offer take is selected
    summarise(total_time_selected= n())

  # count
  ind_data <- temp_data %>%
    mutate(ChosenOffer = round(ChosenOffer / bin_size)* bin_size) %>%
    group_by(ChosenOffer, OfferTaken, win) %>%
    summarise(wins = n())

  plot_data <- left_join(ind_data,total_data,
                         by = c("ChosenOffer" = "ChosenOffer",
                                "OfferTaken" = "OfferTaken" )) %>%
    filter (win == TRUE) %>%
    mutate(win_pct = wins/total_time_selected) %>%
    mutate(OfferTaken = factor(
      OfferTaken,
      levels = c("Higher", "Middle", "Lower")
    )) %>%
    select(-win)
  return(plot_data)
}


calc_win_pct_by_correct_answers_taken_offer <- function(data){

  temp_data <- data %>%
    mutate(CorrectAnswers = CashBuilder /1000) %>%
    add_chosen_offer() %>%
    mutate(win = ifelse(HTH_Result > 0, TRUE, FALSE))

  total_data <- temp_data %>%
    group_by(CorrectAnswers, OfferTaken) %>%
    summarise(total_count = n())

  ind_data <- temp_data %>%
    group_by(CorrectAnswers, OfferTaken, win) %>%
    summarise(count = n())

  plot_data <- left_join(ind_data,total_data, by = c("CorrectAnswers" = "CorrectAnswers", "OfferTaken" = "OfferTaken" )) %>%
    filter(win == TRUE) %>%
    mutate(win_pct = count/total_count) %>%
    mutate(OfferTaken = factor(
      OfferTaken,
      levels = c("Higher", "Middle", "Lower")
    ))
}
