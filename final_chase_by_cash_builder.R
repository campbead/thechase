library(tidyverse)
library(extrafont)
library(scales)
library(camptheme)
library(RColorBrewer)

source("ETL.R")
source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData_06_10_2021.Rdata")

player_episodes = left_join(episodes, players, by = "Date") %>%
  filter(HTH_Result > 0) %>%
  group_by(Date) %>%
  summarise(
    total_cash_builder = sum(CashBuilder/1000),
    Chaser = Chaser.x,
    NumPlayersFC = `No. OfPlayers InFinal Chase`,
    InitialTotal = InitialTotal,
    FC_Winner = FC_Winner.x

  ) %>%
  distinct()

average_initial_total <- player_episodes %>%
  group_by(total_cash_builder, NumPlayersFC) %>%
  summarise(
    ave_initial_total = mean(InitialTotal),
    count_initial_total = n()
  )
