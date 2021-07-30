library(tidyverse)
library(extrafont)
library(scales)

source("ETL.R")
source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData.Rdata")

# custom colors
my_colors = c("#207561", "#60A16D", "#a0cc78")

plot_data <- players %>%
  add_chosen_offer()

plot_data$OfferTaken <- factor(plot_data$OfferTaken, levels = c("Higher", "Middle", "Lower"))

plot <- ggplot(plot_data, aes(x = OfferTaken, fill = OfferTaken)) +
  scale_fill_manual(values = my_colors) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  scale_y_continuous(
    "Percent of time offer is choosen",
    labels = scales::percent) +
  xlab("") +
  ggtitle("The Chase's Head-to-Head Offers:",
          subtitle = "How often are lower, middle, and higher offers picked in the Chase?") +
  theme_campbead() +
  theme(legend.position = "none")

ggsave(plot, filename = "figures/Offer_choice_frequency.png", width = 8, height = 4, dpi = 300)

plot_data <- players %>%
  add_chosen_offer() %>%
  mutate(CB_Correct_Answers = CashBuilder / 1000) %>%
  group_by(CB_Correct_Answers, OfferTaken) %>%
  summarise(count = n())

plot_data$OfferTaken <- factor(plot_data$OfferTaken, levels = c("Higher", "Middle", "Lower"))



plot <- ggplot(plot_data, aes(fill = OfferTaken, y = count, x = CB_Correct_Answers)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = my_colors) +
  labs(fill = "OFFER TAKEN") +
  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  scale_x_continuous(
    name = "Number of correct answers in Cash Builder",
    breaks = seq(0,14),
    labels = as.character(seq(0,14))
  ) +
  scale_y_continuous(
    name = "Percentage of time offer is taken",
    labels = percent,
    expand = c(-1,1) ) +
  ggtitle("The Chase's Head-to-Head Offers:",
          subtitle = "Does the number of correct answers influence the offer choice?") +
  theme_campbead()

ggsave(plot, filename = "figures/Offer_choice_frequency_by_correct_answer.png", width = 8, height = 4, dpi = 300)
