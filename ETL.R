library(rvest)
library(tidyverse)
library(stringi)

## scrape player data

getPlayerData <- function(){
  url <- "http://onequestionshootout.xyz/players/series_all.htm"

  players <- url %>%
    xml2::read_html() %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)

  data <- players [[1]]

  # clean player data
  player_data <-data %>%
    mutate(PlayerNo. = as.numeric(str_replace(PlayerNo., "P",""))) %>%

    mutate(CashBuilder = parse_number(CashBuilder)) %>%

    mutate(ChosenOffer = str_replace(ChosenOffer, "=", "")) %>%
    mutate(ChosenOffer = str_replace(ChosenOffer, "/", "")) %>%
    mutate(ChosenOffer = str_replace(ChosenOffer, "£", "")) %>%
    mutate(ChosenOffer = gsub("([\\])", "", ChosenOffer) ) %>%
    mutate(ChosenOffer = parse_number(ChosenOffer)) %>%

    mutate(LowerOffer = str_replace(LowerOffer, "£", "")) %>%
    mutate(LowerOffer = parse_number(LowerOffer)) %>%

    mutate(HigherOffer = parse_number(HigherOffer)) %>%

    mutate(HTHResult = parse_number(HTHResult)) %>%

    mutate(FC_Correct_Answers_Player = stri_extract_first_regex(`FC CorrectAnswers`,"[0-9]+")) %>%

    mutate(FC_team_size = case_when(grepl("solo", `FC CorrectAnswers`, fixed = TRUE) == TRUE ~ "1",
                                    grepl("solo", `FC CorrectAnswers`, fixed = TRUE) == FALSE ~ stri_extract_last_regex(`FC CorrectAnswers`,"[0-9]+")
    )
    ) %>%

    mutate(
      FC_Winner = case_when(
        grepl("Chaser", `FC Winner`, fixed = TRUE) == TRUE ~ "Chaser",
        grepl("Chaser", `FC Winner`, fixed = TRUE) == FALSE ~ "Team"
      )
    ) %>%
    mutate(
      Team_Win_Amount = case_when(
        FC_Winner == "Team" ~ stri_extract_last_regex(`FC Winner`,"[0-9]+")
      )
    ) %>%

    mutate(
      Chaser_Win_Amount = case_when(
        FC_Winner == "Chaser" ~ stri_extract_last_regex(`FC Winner`,"[0-9]:[0-9]+")
      )
    ) %>%

    mutate(
      Special_round = case_when(
        grepl("✝", `FC CorrectAnswers`, fixed = TRUE) == TRUE ~ TRUE,
        grepl("✝", `FC CorrectAnswers`, fixed = TRUE) == FALSE ~ FALSE,
      )
    ) %>%

    mutate(`Amount WonBy Player`= parse_number(`Amount WonBy Player`)) %>%

    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%

    select(-c(`FC CorrectAnswers`,`FC Winner`,bgc)) %>%

    rename(PlayerNumber = `PlayerNo.`,
           FC_CorrectAnswers = FC_Correct_Answers_Player,
           FC_TeamSize = FC_team_size,
           TeamWinAmount = Team_Win_Amount,
           ChaserWinAmount = Chaser_Win_Amount,
           SpecialRound = Special_round,
           HTH_Result = HTHResult)

  # clean a data error
  player_data$ChosenOffer[2073]=5000

  return(player_data)
}

getEpisodeData <- function(){

  url <- "http://onequestionshootout.xyz/episodes/series_all.htm"

  episode <- url %>%
    xml2::read_html() %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)

  data_episode <- episode [[1]]

  data_episode <- data_episode %>%
    separate(`Series &Episode`, c("Series","Episode")) %>%
    mutate(PrizeFund = str_replace(PrizeFund, "£", "")) %>%
    mutate(PrizeFund = parse_number(PrizeFund)) %>%
    mutate(
      FC_Winner = case_when(
        grepl("Chaser", `Winner`, fixed = TRUE) == TRUE ~ "Chaser",
        grepl("Chaser", `Winner`, fixed = TRUE) == FALSE ~ "Team"
      )
    ) %>%
    mutate(
      Team_Win_Amount = case_when(
        FC_Winner == "Team" ~ stri_extract_last_regex(`Winner`,"[0-9]+")
      )
    ) %>%

    mutate(
      Chaser_Win_Amount = case_when(
        FC_Winner == "Chaser" ~ stri_extract_last_regex(`Winner`,"[0-9]:[0-9]+")
      )
    ) %>%

    mutate(ChaserAccuracy = str_replace(ChaserAccuracy, "%", "")) %>%
    mutate(ChaserAccuracy = as.numeric(ChaserAccuracy) / 100) %>%

    # mark episode as special if it has asterix in chaser name
    mutate(
      SpecialEpisode = case_when(
        grepl("*", `Chaser`, fixed = TRUE) == TRUE ~ TRUE,
        TRUE ~ FALSE
      )
    )%>%

    # Change asterixed Anne Hegerty episodes
    mutate(
      Chaser = replace(Chaser, Chaser == 'Anne Hegerty *', 'Anne Hegerty')
    ) %>%

    # Separate Target Column
    tidyr::separate("Target", into = c("InitialTarget", "InitialPushbacks"), sep = "\\+" ) %>%

    mutate(InitialTarget = as.numeric(InitialTarget)) %>%

    tidyr::replace_na(list(InitialPushbacks = 0)) %>%
    mutate(InitialPushbacks = as.numeric(InitialPushbacks)) %>%

    mutate(InitialTotal = InitialTarget + InitialPushbacks, .before = InitialTarget) %>%

    select(-c(`Winner`,`FinalChaseVideo`)) %>%

    # change datatypes
    mutate(
      Date = as.Date(Date, "%d/%m/%Y"),
      Chaser = as.factor(Chaser),
      FC_Winner = as.factor(FC_Winner)
    )

  return(data_episode)
}
