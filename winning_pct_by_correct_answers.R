library(tidyverse)
library(extrafont)
library(scales)

source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData.Rdata")

# set colors
my_colors = c("#207561", "#60A16D", "#a0cc78")




## Lower Offer
# get total offer wins table
Total_offers_wins <- get_total_offer_wins(players)

plot_data <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 1000)

plot_data_regression <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 1)

plot_data <- plot_data %>%
  filter(OfferTaken == "Lower")

## plot lower offer

plot <- ggplot() +

  geom_smooth(
    data = plot_data_regression %>% filter(OfferTaken == "Lower"),
    mapping = aes(x = ChosenOffer,
                  y = win_pct,
                  weight = total_time_selected,
                  color = OfferTaken
                  ),
    method = "lm",
    show.legend = FALSE
  ) +

  geom_hline(data = Total_offers_wins %>% filter(OfferTaken == "Lower"),
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
    data = Total_offers_wins %>% filter(OfferTaken == "Lower"),
    aes(x = 7400,
        y = pct,
        label = paste0("average for ", OfferTaken, " offer: ", round(pct * 100,0), "%")),
    family = "Fira Mono",
    size = 3,
    position = position_nudge(y = -0.025)
  ) +
  scale_fill_manual(values = my_colors[3]) +
  scale_color_manual(values = my_colors[3]) +

  xlab("Chosen offer (£) rounded to nearest thousand") +
  scale_x_continuous(
    breaks = seq(-6000, 9000,2000),
    limits = c(-6000,9000),
    labels = comma(seq(-6000, 9000,2000))
  ) +
  scale_y_continuous(
    name = "Chance to win Head-to-Head",
    labels = percent,
    expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +
  labs(fill = "OFFER TAKEN", size = "TOTAL ATTEMPTS") +
  guides(
    fill = guide_legend(
      override.aes = list(size = 7),
      order = 1
    ),
    size = guide_legend(
      size = guide_legend(order = 2)
    )) +
  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  ggtitle("Player's Success in the Chase's Head-to-Head Round:",
          subtitle = "How often do players taking the lower offer win the Head-to-Head based on the offer value?") +

  theme_campbead()

ggsave("figures/WinningPCT_Lower.png", plot, width = 10, height = 5.5, dpi = 300)



## Middle offer plot

# get total offer wins table
Total_offers_wins <- get_total_offer_wins(players)

plot_data <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 1000)

plot_data <- plot_data %>%
  filter(OfferTaken == "Middle")

## plot middle offer

plot <- ggplot() +
  geom_smooth(
    data = plot_data_regression %>% filter(OfferTaken == "Middle"),
    mapping = aes(x = ChosenOffer,
                  y = win_pct,
                  weight = total_time_selected,
                  color = OfferTaken
    ),
    method = "lm",
    show.legend = FALSE
  ) +
  geom_hline(data = Total_offers_wins %>% filter(OfferTaken == "Middle"),
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
    data = Total_offers_wins %>% filter(OfferTaken == "Middle"),
    aes(x = 10400,
        y = pct,
        label = paste0("average for ", OfferTaken, " offer: ", round(pct * 100,0), "%")),
    family = "Fira Mono",
    size = 3,
    position = position_nudge(y = -0.025)
  ) +
  scale_fill_manual(values = my_colors[2]) +
  scale_color_manual(values = my_colors[2]) +

  xlab("Chosen offer (£) rounded to nearest thousand") +
  scale_x_continuous(
    breaks = seq(0, 12000,2000),
    limits = c(0,12000),
    labels = comma(seq(0, 12000,2000))
  ) +
  scale_y_continuous(
    name = "Chance to win Head-to-Head",
    labels = percent,
    expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +
  labs(fill = "OFFER TAKEN", size = "TOTAL ATTEMPTS") +
  guides(
    fill = guide_legend(
      override.aes = list(size = 7),
      order = 1
    ),
    size = guide_legend(
      size = guide_legend(order = 2)
    )) +
  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  ggtitle("Player's Success in the Chase's Head-to-Head Round:",
          subtitle = "How often do players taking the middle offer win the Head-to-Head based on the offer value?") +

  theme_campbead()

ggsave("figures/WinningPCT_Middle.png", plot, width = 10, height = 5.5, dpi = 300)




## Higher offer plot

plot_data_unzoom <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 5000)

plot_data <- plot_data_unzoom %>%
  filter(OfferTaken == "Higher")

plot <- ggplot() +
  geom_smooth(
    data = plot_data_regression %>% filter(OfferTaken == "Higher"),
    mapping = aes(x = ChosenOffer,
                  y = win_pct,
                  weight = total_time_selected,
                  color = OfferTaken
    ),
    method = "lm",
    show.legend = FALSE
  ) +

  geom_hline(data = Total_offers_wins %>% filter(OfferTaken == "Higher"),
             aes(color = OfferTaken, yintercept = pct),
             size = 1,
             show.legend = FALSE,
             linetype = "dotted") +
  geom_text(
    data = Total_offers_wins %>% filter(OfferTaken == "Higher"),
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
  xlab("Chosen offer (£) rounded to nearest five-thousand") +
  scale_x_continuous(
    breaks = seq(15000, 100000,5000),
    limits = c(15000, 100000),
    labels = comma(seq(15000, 100000,5000))
  ) +
  scale_y_continuous(
    name = "Chance to win Head-to-Head",
    labels = percent,
    expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +
  labs(fill = "OFFER TAKEN", size = "TOTAL ATTEMPTS") +
  guides(
    fill = guide_legend(
      override.aes = list(size = 7),
      order = 1
      ),
    size = guide_legend(
      size = guide_legend(order = 2)
    )) +
  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  ggtitle("Player's Success in the Chase's Head-to-Head Round:",
          subtitle = "How often do players taking the higher offer win the Head-to-Head based on the offer value?") +
  theme_campbead()

ggsave("figures/WinningPCT_Higher.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)

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
    name = "Number of correct answers in Cash Builder",
    breaks = seq(0,5) * 2,
    limits = c(0,10),
    expand = expansion(mult = c(0.02,0.02) )
    ) +
  scale_y_continuous(
    name = "Chance to win Head-to-Head",
    labels = percent,
    #expand = expansion(mult = c(0.5,0.05)),
    breaks = seq(0,1, 0.20)
  ) +

  guides(
    fill = guide_legend(
      override.aes = list(size = 7),
      order = 1
    ),
    size = guide_legend(
      size = guide_legend(order = 2)
    )) +

  labs(caption = "Data: onequestionshootout.xyz | Plot: @campbead") +
  ggtitle("Player's Success in the Chase's Head-to-Head Round:",
          subtitle = "How often do players win the Head-to-Head based on Cash Builder performance?") +
  theme_campbead()

ggsave("figures/WinningPCT_by_correct_and_offer.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)
