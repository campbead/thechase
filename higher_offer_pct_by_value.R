library(tidyverse)
library(extrafont)
library(scales)

source("ETL.R")
source("custom_gg.R")
source("project_functions.R")

load(file = "data/ChaseData.Rdata")

# custom colors
my_colors = c("#207561", "#589167", "#a0cc78")

# cuts to use in bar graph
high_offer_cuts =  c(
  0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, Inf)
high_offer_breaks = c("(0,1e+04]","(1e+04,2e+04]","(2e+04,3e+04]","(3e+04,4e+04]", "(4e+04,5e+04]",
                      "(5e+04,6e+04]", "(6e+04,7e+04]", "(7e+04,8e+04]", "(8e+04,9e+04]",
                      "(9e+04,1e+05]", "(1e+05,Inf]")
high_offer_labels = c("≤10k", "10-20k", "20-30k","30-40k","40-50k","50-60k","60-70k","70-80k",
                      "80-90k","90-100k",">100k")

plot_data <- players %>%
  add_chosen_offer() %>%
  mutate(cuts = cut(HigherOffer, high_offer_cuts)) %>%
  group_by(cuts, OfferTaken) %>%
  summarise(count = n())

plot_data$OfferTaken <- factor(plot_data$OfferTaken, levels = c("Higher", "Middle", "Lower"))

plot <- ggplot(plot_data, aes(fill=OfferTaken, y=count, x=cuts)) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = my_colors)+
  scale_x_discrete(breaks = high_offer_breaks,
                   labels= high_offer_labels)+
  xlab("Higher Offer Value (£)") +
  labs(fill = "OFFER TAKEN")+
  ggtitle("How enticing is the higher offer?") +

  scale_y_continuous(name = "Percentage of time offer is taken", labels = percent, expand = c(-1,1) )+
  theme_campbead()

ggsave(plot, filename = "figures/How_enticing_is_the_higher_offer.png")
