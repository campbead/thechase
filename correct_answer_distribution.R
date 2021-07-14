# load package
library(tidyverse)
library(extrafont)
library(scales)

# load custom scripts
source("custom_gg.R")
source("project_functions.R")

# load CHase Data
load(file = "data/ChaseData.Rdata")

# create a correct answers column
answer_distribution <- players %>%
  mutate(CorrectAnswers = CashBuilder / 1000)

# plot
plot <- ggplot(data = answer_distribution) +
  geom_bar(aes(x = CorrectAnswers), stat, fill = "#60A16D") +
  xlab("Number of correct answers in Cash Builder") +
  ylab("Total count") +
  theme_campbead()

ggsave(plot,
       width = 5,
       height = 3,
       filename = "figures/correct_answer_distribution.png")

## Cummulative distribuion
plot <-
  ggplot(
    data = answer_distribution,
    aes(x = CorrectAnswers)
  ) +
  stat_ecdf(
    geom = "step",
    position = position_nudge(x = -0.5, y = 0),
    color = "#60A16D",
    size = 1.5
  ) +
  scale_x_continuous(
    name = "Number of correct answers in Cash Builder",
    breaks = seq(0,7) * 2,
    limits = c(0,14),
    expand = expansion(mult = c(0.02,0.02) )
  ) +
  scale_y_continuous(
    name = "Players scoring at least",
    breaks = seq(from = 0, to = 1, by = 0.2),

    labels = percent
  ) +
  theme_campbead()

ggsave(plot,
       width = 5,
       height = 3,
       filename = "figures/correct_answer_distribution_cummulative.png")
