library(tidyverse)
library(extrafont)
library(scales)
library(camptheme)
library(RColorBrewer)

source("ETL.R")
source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData_27_09_2021.Rdata")

# custom colors
my_colors = c("#207561", "#60A16D", "#a0cc78")

chaser_wins <- episodes %>%
  filter(FC_Winner == "Chaser") %>%
  group_by(Chaser, InitialTarget) %>%
  summarise(
    wins_total = n()
  )
chaser_total <- episodes %>%
  group_by(Chaser, InitialTarget) %>%
  summarise(
    total = n()
  )

chaser <- left_join(chaser_total, chaser_wins, by= c("Chaser", "InitialTarget")) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(pct = wins_total / total)

chaser_average_wins <- episodes %>%
  filter(FC_Winner == "Chaser") %>%
  group_by(InitialTarget) %>%
  summarise(
    wins_total = n()
  )

chaser_average_total <- episodes %>%
  group_by(InitialTarget) %>%
  summarise(
    total = n()
  )

chaser_average <- left_join(chaser_average_total, chaser_average_wins, by= c("InitialTarget")) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(pct = wins_total / total)

plot <- ggplot() +
  geom_line(data = chaser_average, aes(x = InitialTarget, y = pct), size = 1, colour = "Snow4") +
  geom_point(data = chaser, aes(x = InitialTarget, y = pct, color = Chaser, size = total))+

  labs(size = "TOTAL ATTEMPTS", color = "CHASER") +
  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  scale_colour_manual(values = brewer_pal(palette = "Dark2")(6)) +
  scale_x_continuous(
    name = "Number of correct answers by team in the Final Chase",
    limits = c(10,26),
    breaks = seq(10,26,2),
    labels = as.character(seq(10,26,2))
  ) +
  scale_y_continuous(
    name = "Percentage of Chaser wins in the Final Chase",
    labels = percent) +
  guides(
    color = guide_legend(
      override.aes = list(size = 5),
      title.vjust = 0.78
    ),
    size = guide_legend(
      title.vjust = 0.5,
      label.vjust = 0.5
    )) +
  camptheme::theme_campbead()

ggsave("figures/Chaser_Accuracy.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)



# Calculate a few stats

# overall chaser accuracy

chaser_wins_overall <- episodes %>%
  filter(FC_Winner == "Chaser") %>%
  group_by(Chaser) %>%
  summarise(
    wins_total = n()
  )

chaser_total_overall <- episodes %>%
  group_by(Chaser) %>%
  summarise(
    total = n()
  )

chaser_overall <- left_join(chaser_total_overall, chaser_wins_overall, by= c("Chaser")) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(pct = wins_total / total)

# weighted chaser accuracy
weighted_chaser_accuracy <- chaser %>%
  group_by(Chaser) %>%
  summarise(weighted_total = sum(InitialTarget * total),
            weighted_wins = sum(InitialTarget * wins_total)) %>%
  mutate(weighted_accuracy = weighted_wins / weighted_total)


chaser_overall <- left_join(chaser_overall, weighted_chaser_accuracy, by = c("Chaser"))
