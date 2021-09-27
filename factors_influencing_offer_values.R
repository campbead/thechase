library(tidyverse)
library(extrafont)
library(scales)

source("ETL.R")
source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData.Rdata")

# custom colors
my_colors = c("#207561", "#60A16D", "#a0cc78")

# round number theory
players_through_before_4 <- players %>%
  group_by(Date) %>%
  filter(PlayerNumber != 4) %>%
  mutate(win =
           case_when(
             HTH_Result > 0 ~ 1,
             HTH_Result < 0 ~ 0
           ),
         won_amount = win * ChosenOffer
  ) %>%
  mutate(player_through_before_4 = sum(win),
         sum_before_4 = sum(won_amount)) %>%
  filter(PlayerNumber
         == 1) %>%
  select(c(Date, player_through_before_4, sum_before_4))

player_4_data <- players %>%
  filter(PlayerNumber == 4) %>%
  left_join(players_through_before_4, by = "Date") %>%
  mutate(
    higher_team_split = (sum_before_4 + HigherOffer) / (player_through_before_4 + 1),
    lower_team_split = (sum_before_4 + LowerOffer) / (player_through_before_4 + 1)
  )


