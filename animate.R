# Use this file to animate plays in the tracking data
# reference: https://cran.r-project.org/web/packages/sportyR/vignettes/animating-tracking-data.html


# Set Up ------------------------------------------------------------------

setwd("C:/Users/43jmi/Documents/AnalysisbyComet/BDB2024")

library(tidyverse)
#library(sportyR)
library(gganimate)
library(nflfastR)


# Data --------------------------------------------------------------------

plays_df <- read_csv("Data/plays.csv")

week1_df <- read_csv("Data/tracking_week_1.csv") %>% 
  left_join(teams_colors_logos %>% select(team_abbr, team_color, team_color2),
            by = c("club" = "team_abbr")) %>% 
  mutate(team_color = ifelse(is.na(team_color), "#825736", team_color),
         team_color2 = ifelse(is.na(team_color2), "#825736", team_color2))

tackles_df <- read_csv("Data/tackles.csv")

players_df <- read_csv("Data/players.csv")

# Visualize ---------------------------------------------------------------

# Create the field
nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

# Display the field
nfl_field

play_anim <- nfl_field +
  geom_point(
    data = example_play,
    aes(x, y),
    shape = 21,
    fill = example_play$team_color,
    color = example_play$team_color2
  ) +
  transition_time(example_play$frameId)

play_anim


# Different Approach ------------------------------------------------------

# General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

gameID <- 2022090800
playID <- 80

example_play <- week1_df %>% 
  filter(gameId == gameID, playId == playID)

plt_title <- plays_df %>% 
  filter(gameId == gameID & playId == playID) %>%
  pull(playDescription)

tackles_df %>% 
  filter(gameId == gameID & playId == playID) %>% 
  left_join(players_df %>% select(nflId, position, displayName),
            by = c("nflId")) %>% View()

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

# hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

anim_plt <- ggplot() +
  
  #setting size and color parameters
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  #scale_fill_manual(values = cols_fill, guide = FALSE) + 
  #scale_colour_manual(values = cols_col, guide = FALSE) +
  
  #adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  
  #adding yard lines
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  
  #adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  
  #adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") +
  theme_void() +
  
  #adding players
  geom_point(data = example_play, aes(x = (xmax-y), y = x),
             fill = example_play$team_color,
             colour = example_play$team_color2, 
             shape = 21,
             alpha = 0.7, size = 7) +
  #adding jersey numbers
  geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) +
  #applying plot limits
  ylim(ymin, ymax) + 
  #coord_fixed() +
  theme(plot.title = element_text()) +
  
  #titling plot with play description
  labs(title = str_wrap(plt_title, 80))
  

anim_plt +
  #setting animation parameters
  transition_time(example_play$frameId)
