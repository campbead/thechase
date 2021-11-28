# winning final chase


library(tidyverse)
library(extrafont)
library(scales)
library(camptheme)
library(RColorBrewer)
library(here)
library(ggbeeswarm)

source("ETL.R")
source("project_functions.R")

# load data
load(file = here("data","ChaseData_06_10_2021.Rdata"))

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


# figure 1 - final chase total vs cash builder total by number of players
ggplot(
  data = player_episodes,
  aes( y= InitialTotal, fill = as.factor(NumPlayersFC))
  ) +
  geom_density(alpha = 0.3)+
  theme_campbead()

ggsave(filename = here("figures", "FC_total_vs_cashbuilder.png"))

