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
  mutate(CorrectAnswers = CashBuilder /1000)

# plot
plot <- ggplot(data = answer_distribution) +
  geom_bar(aes(x=CorrectAnswers), stat) +
  theme_campbead()

ggsave(plot, filename = "figures/correct_answer_distrobution.png")