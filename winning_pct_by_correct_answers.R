library(tidyverse)
library(extrafont)
library(scales)

source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData.Rdata")

# set colors
my_colors = c("#207561", "#60A16D", "#a0cc78")

# get total offer wins table
Total_offers_wins <- get_total_offer_wins(players)

plot_data <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 1000)

plot_data <- plot_data %>%
  filter(OfferTaken != "Higher")


plot <- ggplot() +
  geom_hline(data = Total_offers_wins,
             aes(color = OfferTaken, yintercept = pct),
             size = 1,
             show.legend = FALSE,
             linetype = "dotted") +
  geom_point(data = plot_data,
             aes(fill = OfferTaken,
                 y = win_pct,
                 x = ChosenOffer,
                 size = total_time_selected),
             colour = "black",
             pch = 21) +
  geom_text(
    data = Total_offers_wins,
    aes(x = 10400,
        y = pct,
        label = paste0("average for ", OfferTaken, " offer: ", round(pct * 100,0), "%")),
    family = "Fira Mono",
    size = 3,
    position = position_nudge(y = -0.025)
  ) +
  scale_fill_manual(values = my_colors[2:3]) +
  scale_color_manual(values = my_colors) +

  xlab("Chosen offer (£) rounded to nearest thousand") +
  scale_x_continuous(
    breaks = seq(-6000, 12000,2000),
    limits = c(-6000,12000),
    labels = comma(seq(-6000, 12000,2000))
  ) +
  scale_y_continuous(
    name = "Win Percentage",
    labels = percent,
    expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +
  labs(fill = "OFFER TAKEN", size = "TOTAL ATTEMPTS") +
  guides(colour = guide_legend(override.aes = list(size = 7))) +
  theme_campbead()

ggsave("figures/WinningPCT_Zoomed.png", plot, width = 10, height = 5.5, dpi = 300)

plot_data_unzoom <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 5000)

plot_data <- plot_data_unzoom %>%
  filter(OfferTaken == "Higher")

plot <- ggplot() +
  geom_hline(data = Total_offers_wins,
             aes(color = OfferTaken, yintercept = pct),
             size = 1,
             show.legend = FALSE,
             linetype = "dotted") +
  geom_text(
    data = Total_offers_wins,
    aes(x = 90000,
        y = pct,
        label = paste0("average for ", OfferTaken, " offer: ", round(pct * 100,0), "%")),
    family = "Fira Mono",
    size = 3,
    position = position_nudge(y = -0.025)
  ) +
  geom_point(data = plot_data,
             aes(fill = OfferTaken,
                 y = win_pct,
                 x = ChosenOffer,
                 size = total_time_selected),
             colour = "black",
             pch = 21) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  #ylim(0,1)+
  xlab("Chosen offer (£) rounnded to nearest 5000") +
  scale_x_continuous(
    breaks = seq(15000, 100000,5000),
    limits = c(15000, 100000),
    labels = comma(seq(15000, 100000,5000))
  ) +
  scale_y_continuous(
    name = "Win Percentage",
    labels = percent,
    expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +
  labs(fill = "OFFER TAKEN", size = "TOTAL ATTEMPTS") +
  guides(colour = guide_legend(override.aes = list(size = 7))) +
  theme_campbead()

ggsave("figures/WinningPCT.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)

plot_data <- calc_win_pct_by_correct_answers_taken_offer(players)


plot <- ggplot(
  data = plot_data,
  aes(x = CorrectAnswers, y = win_pct, color = OfferTaken, size = total_count)
  ) +

  geom_smooth(
    method = "lm",
    mapping = aes(color = OfferTaken, weight = total_count, fill = OfferTaken),
    show.legend = FALSE
    ) +

  geom_point(
    aes(fill = OfferTaken),
    colour = "black",
    pch = 21) +

  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +

  labs(fill = "OFFER TAKEN", size = "TOTAL ATTEMPTS") +

  scale_x_continuous(
    name = "Number of correct answers in cash builder",
    breaks = seq(0,5) * 2,
    limits = c(0,10),
    expand = expansion(mult = c(0.02,0.02) )
    ) +
  scale_y_continuous(
    name = "Chance to win head-to-head",
    labels = percent,
    #expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +

  guides(fill = guide_legend(override.aes = list(size = 7))) +

  theme_campbead()

ggsave("figures/WinningPCT_by_correct_and_offer.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)
