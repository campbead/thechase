# 4-5-6

player_chance = .7
chaser_chance = .9

c_0 = 7
p_0 = 4

positions = c(p_0, c_0)

get_positions <- function(positions) {
  if (runif(1) < player_chance) {
    positions[1] = positions[1] - 1
  }
  if (runif(1) < chaser_chance) {
    positions[2] = positions[2] - 1
  }
  return(positions)
}

get_h2h_result <- function(c_0, p_0, player_chance, chaser_chance){
  positions = c(p_0, c_0)
  while ((positions[1] > 0) && (positions[2] != positions[1]))
  {
    positions <- get_positions(positions)
  }
  if (positions[1] == 0) {
    win <- TRUE
  } else {
    win <- FALSE
  }
  return(win)
}

get_h2h_result(c_0, p_0, player_chance, chaser_chance)

wins <- replicate(100000, get_h2h_result(c_0, p_0, player_chance, chaser_chance))
win_df <- data.frame(wins)

win_df %>%
  filter(wins == TRUE) %>%
  summarise(wins = n())

