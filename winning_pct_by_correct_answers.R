library(tidyverse)
library(extrafont)
library(scales)

source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData.Rdata")

# set colors
my_colors = c("#207561", "#589167", "#a0cc78")

# get total offer wins table
Total_offers_wins <- get_total_offer_wins(players)

plot_data <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 1000)

plot_data <- plot_data %>%
  filter(OfferTaken != "Higher")


plot <- ggplot() +
  geom_hline(data = Total_offers_wins,
             aes(color=OfferTaken, yintercept = pct),
             size = 1,
             show.legend = FALSE,
             linetype = "dotted") +
  geom_point(data = plot_data,
             aes(fill=OfferTaken,
                 y=win_pct,
                 x=ChosenOffer,
                 size = total_time_selected),
             colour="black",
             pch=21) +
  geom_text(
    data = Total_offers_wins,
    aes(x=12000, y= pct, label = paste0(round(pct * 100,0), "%")),
    family = "Fira Mono",
    size = 3,
    position = position_nudge(y = 0.02)
  ) +
  scale_fill_manual(values = my_colors[2:3]) +
  scale_color_manual(values = my_colors) +

  xlab("Chosen Offer (£) rounded to nearest thousand") +
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
  labs(color = "OFFER TAKEN", size = "TOTAL ATTEMPTS")+
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme_campbead()

ggsave("figures/WinningPCT_Zoomed.png", plot, width = 10, height = 5.5, dpi = 300)

plot_data_unzoom <- calc_win_pct_by_chosen_offer_taken_offer_binned(players, 5000)

plot_data <- plot_data_unzoom %>%
  filter(OfferTaken == "Higher")

plot <- ggplot(plot_data_unzoom,
               aes(color=OfferTaken,
                   y=win_pct,
                   x=ChosenOffer,
                   size = total_time_selected)) +
  geom_point() +
  scale_color_manual(values = my_colors) +

  #ylim(0,1)+
  xlab("Chosen Offer (£)") +
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
  labs(color = "OFFER TAKEN", size = "TOTAL ATTEMPTS")+
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme_campbead()

ggsave("figures/WinningPCT.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)


# temp_data <- players %>%
#   add_chosen_offer() %>%
#   mutate(win = ifelse(HTH_Result >0, TRUE, FALSE))
#
# total_data <- temp_data %>%
#   mutate(ChosenOffer = round(ChosenOffer/5000) * 5000) %>%
#   group_by(ChosenOffer, OfferTaken) %>%
#   summarise(total_count = n())
#
# ind_data <- temp_data %>%
#   mutate(ChosenOffer = round(ChosenOffer/5000) * 5000) %>%
#   group_by(ChosenOffer, OfferTaken, win) %>%
#   summarise(count = n())
#
# plot_data <- left_join(ind_data,total_data, by = c("ChosenOffer" = "ChosenOffer", "OfferTaken" = "OfferTaken" )) %>%
#   filter (win == TRUE) %>%
#   mutate(win_pct = count/total_count)
#
# plot_data$OfferTaken <- factor(plot_data$OfferTaken, levels = c("Higher", "Middle", "Lower"))
# plot_data <- plot_data %>%
#   filter(OfferTaken == "Higher")
#
#
# plot <- ggplot(plot_data, aes(color=OfferTaken, y=win_pct, x=ChosenOffer, size = total_count)) +
#   geom_point() +
#   scale_color_manual(values = my_colors) +
#
#   #ylim(0,1)+
#   xlab("Chosen Offer (£)") +
#   scale_x_continuous(
#     breaks = seq(15000, 100000,5000),
#     limits = c(15000, 100000),
#     labels = comma(seq(15000, 100000,5000))
#   ) +
#   scale_y_continuous(
#     name = "Win Percentage",
#     labels = percent,
#     expand = expansion(mult = c(0.5,0.05)),
#     breaks = seq(0,1, 0.20)
#   ) +
#   labs(color = "OFFER TAKEN", size = "TOTAL ATTEMPTS")+
#   guides(colour = guide_legend(override.aes = list(size=7))) +
#   theme_campbead()
#
# ggsave("figures/WinningPCT.png", plot, width = 10, height = 5.5, units = "in",  dpi = 300)
