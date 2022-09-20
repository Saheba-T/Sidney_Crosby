Data Storytelling of Sidney Crosby’s NHL Career
================
Saheba Tegally
2022-09-03

## Introduction

There probably is not probably is not a single person in this generation
who is a fan of hockey and watches NHL games that has not heard of
Sidney Crosby at least once.

Sidney Crosby is an ice hockey player who plays Centre and is the
Captain of the Pittsburgh Penguins. Born in Cole Harbour, Nova Scotia,
Canada, he was drafted first overall in the 2005 NHL Entry Draft by the
Pittsburgh Penguins with a lot of hype around him as “The Next One”.

The goal of this project is to look at Sidney Crosby’s NHL career
through the lens of data. There are many other great hockey players out
there and the point of this project is not to argue or emphasize the
greatness of Sidney Crosby over other players. Thus, no comparison to
any specific player will be made.

``` r
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(rmarkdown)
```

``` r
games_reg = read.csv(file = "data/games_regular.csv", header = TRUE)
games_playoffs = read.csv(file = "data/games_playoffs.csv", header = TRUE)
```

SELF-REMINDER: I need to see what the data tells me, so I know what the
story really is. Only then, I can do a data story-telling.

Things to do: Feature Engineering:

-   proportion of powerplay points out of total points.
-   points per game.
-   

``` r
# 
nhl_games_reg <- games_reg %>% 
                 dplyr::filter(team.name == "Pittsburgh Penguins") %>%
                 dplyr::select(season:plusMinus) %>%
                 dplyr::mutate(season = year(as.Date(str_sub(season,1,4), format = "%Y")))
  

nhl_games_playoffs <- games_playoffs %>% 
                      dplyr::filter(team.name == "Pittsburgh Penguins") %>%
                      dplyr::select(season:blocked) %>%
                      dplyr::mutate(season = year(as.Date(str_sub(season,1,4),format = "%Y")))

Reg_Season <- games_reg %>% 
              dplyr::select(season) %>%
              dplyr::mutate(season = paste(str_sub(season,1,4), str_sub(season,5,-1), sep = '-'))
```

## Regular Season Career Performance

``` r
ggplot(data = nhl_games_reg) +
  geom_col(aes(x = season, y = games)) +
  guides(x = guide_axis(angle = 45))
```

![](data_exploration_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(data = nhl_games_reg) +
  geom_col(aes(x = season, y = points, fill = "#ffb303")) +
  geom_col(aes(x = season, y = assists, fill = "#000000")) +
  guides(x = guide_axis(angle = 45)) +
  labs(title = "Total Points By Season",
       x = "NHL Season", 
       y = "Total points") +
  scale_fill_identity(name = "Point Type",
                      labels = c("Assists", "Goals"), 
                      guide = "legend")
```

![](data_exploration_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(data = nhl_games_reg) + 
  geom_col(aes(x = season, y = points/games)) +
  guides(x = guide_axis(angle = 45)) +
  labs(title = "Points Per Game By Season",
       x = "NHL Season",
       y = "Points Per Games (PPG)")
```

![](data_exploration_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

A goal is considered game winning when the team would win the game
without scoring any more goals

``` r
ggplot(data = nhl_games_reg) +
  geom_col(aes(x = season, y = (gameWinningGoals/goals)*100)) +
  guides(x = guide_axis(angle = 45)) +
  labs(title = "Percentage of Game Wining Goals By Season",
       subtitle = "",
       x = "NHL Season",
       y = "% of Game Winning Goals")
```

![](data_exploration_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Sidney Crosby is a player who shows up during big moments and seals the
win for his team by scoring the game winning goal.

This is one of the reasons he is the Captain and why he is such a
valuable player. Scoring goals is always nice, but scoring them when the
team needs it the most, in this case when victory is at stake, makes a
skater akin to a hero to their team, coach and fans.

``` r
ggplot(data = nhl_games_reg) +
  geom_col(mapping = aes(x = season, y = faceOffPct)) +
  labs(title = "FaceOff Percentage For each Season",
       x = "NHL Season",
       y = "FaceOff Percentage %") + 
  guides(x = guide_axis(angle = 45)) 
```

![](data_exploration_files/figure-gfm/FaceOff%20Percentage%20Over%20time-1.png)<!-- -->

In Hockey, a player’s Shooting Percentage is the percentage of their
attempted shots that resulted in goals. For this calculation, a goal is
any shot that sent the puck into the net, including empty net goals but
not goals from a shootout. A shot attempt is any action that directs the
puck towards the net, however shots blocked by a non-goalie player,
shots that missed the net or hit the goal post, or shots in a shootout
are not included.

``` r
ggplot(data = nhl_games_reg) +
  geom_col(mapping = aes(x = season, y = shotPct)) +
  labs(title = "Shot Percentage By Regular Season",
       x = "NHL Season",
       y = "Shot Percentage %") + 
  guides(x = guide_axis(angle = 45)) 
```

![](data_exploration_files/figure-gfm/Shot%20Percentage%20Over%20time-1.png)<!-- -->