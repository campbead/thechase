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
