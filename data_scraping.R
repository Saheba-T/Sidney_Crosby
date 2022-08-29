#load required libraries
library(dplyr)
library(tidyr)
library(jsonlite)
library(httr)
library(stringr)
library(readr)

## Get raw data from NHL API
# Curl of api containing relevant data from Sidney Crosby's NHL page
url_string <- paste0("https://statsapi.web.nhl.com/api/v1/people/8471675?expand=person.stats&",
                     "stats=yearByYear,yearByYearPlayoffs,careerRegularSeason&expand=stats.team&site=en_nhlCA")
# get the response
r <- httr::VERB("GET", url = url_string)


## Access the data
# get the json content of the response
r_content <- httr::content(r, as = 'text')
# get the iterative json data into a usable format
data <- jsonlite::fromJSON(r_content, flatten = TRUE)
# get all the player's data
player_data <- data$people
#get all the player game stats
player_stats <- player_data$stats


## Get all 3 dataframes
# Regular Season: year by year stats
games_data_regular <- (player_stats[[1]])[[1]][[1]]
# Playoffs: year by year stats
games_data_playoffs <- (player_stats[[1]])[[1]][[2]]
# Career Regular Season Stats
career_data_regular <- (player_stats[[1]])[[1]][[3]]


## Clean the column names of all 3 dataframes
colnames(games_data_regular) <- stringr::str_remove(colnames(games_data_regular),
                                                    pattern = 'stat\\.')

colnames(games_data_playoffs) <- stringr::str_remove(colnames(games_data_playoffs),
                                                     pattern = 'stat\\.')

colnames(career_data_regular) <- stringr::str_split(colnames(career_data_regular),
                                                    pattern = '\\.',
                                                    simplify = TRUE)[,-1]


## Save dataframes as csv files in data folder of current project directory
readr::write_csv(games_data_regular, 'data\\games_regular.csv')
readr::write_csv(games_data_playoffs,'data\\games_playoffs.csv')
readr::write_csv(career_data_regular,'data\\career_regular.csv')