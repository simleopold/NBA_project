###################################################################################
###################################################################################
#
#this script is used to prepare the data for the bt model
#
###################################################################################
###################################################################################

library(dplyr)
library(tidyverse)
library(stringr)

##################################################################################
##################################################################################


################################################
################################################
#Importing nba tournament games
################################################
################################################

data <- read.csv("/Users/icesim/Desktop/games.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
head(data)
data <- data[,c(-1,-6,-7)] #remove cols
colnames(data) <- data[1, ] #put the right names of the cols 
data <- data[-1, ]
head(data)
data$Date <- as.Date(data$Date,format = "%d/%m/%Y")

data <- data %>% arrange(Date)
head(data)
data$id <- seq(1:length(data$Date))

#separate away and home score in data df
tmp <- strsplit(data$Score, "-")
split_score <- do.call(rbind,tmp)
data$AwayScore <- split_score[,1]
data$HomeScore <- split_score[,2]
games <- data[,-4]
games$AwayScore <- as.integer(games$AwayScore)
games$HomeScore <- as.integer(games$HomeScore)
head(games)


########################
#importing stats (basic)
########################

# Define the folder containing your .txt files
folder_path <- "/Users/icesim/Desktop/tcd/cours/dissertation/data" # Replace with your folder path

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty data frame to store the last rows
combined_data <- data.frame()

# Loop through each file and process
for (file in file_list) {
  # Read the file into a data frame
  temp_data <- read.csv(file, header = F)
  
  # Extract the last row
  last_row <- temp_data[nrow(temp_data), ]
  
  # Add the file name as a column (optional, for tracking)
  last_row$file_name <- basename(file)
  # Combine the last row into the final dataset
  combined_data <- bind_rows(combined_data, last_row)
}
names(combined_data)[1:23] <- read.csv(file_list[1], header=F, skip=1, nrows=1)

# View the combined dataset
head(combined_data)

#remove GmSc and +/- (not relevant here as we study the whole team)
basic_gamestat <- combined_data[,-c(23,21,22)]
head(basic_gamestat)

#######################
#joining basic gamestats into games dataset based on the id of the game
#######################

new_basic_gamestat <- basic_gamestat %>%
  mutate(
    id = as.numeric(gsub("[^0-9]", "", file_name)),  # Extract numeric ID
    team_type = ifelse(grepl("a", file_name), "away", "home")  # Determine team type
  )
head(new_basic_gamestat)

#rename the stats col according to home or away statu
away_basic_stats <- new_basic_gamestat %>%
  filter(team_type == "away") %>%
  select(-file_name, -team_type) %>%
  rename_with(~ paste0(., "_a"), -id)  # Append "_a" for away stats
head(away_basic_stats)

home_basic_stats <- new_basic_gamestat %>%
  filter(team_type == "home") %>%
  select(-file_name, -team_type) %>%
  rename_with(~ paste0(., "_h"), -id)  # Append "_h" for home stats
head(home_basic_stats)

#merge the df into games df
fullgame_basic <- games %>%
  left_join(away_basic_stats, by = "id") %>%
  left_join(home_basic_stats, by = "id")
head(fullgame_basic)

#importing advanced stats (same process than above)

# Define the folder containing your .txt files
folder_path <- "/Users/icesim/Desktop/tcd/cours/dissertation/ad_stats" # Replace with your folder path

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty data frame to store the last rows
combined_data <- data.frame()

# Loop through each file and process
for (file in file_list) {
  # Read the file into a data frame
  temp_data <- read.csv(file, header = F)
  
  # Extract the last row
  last_row <- temp_data[nrow(temp_data), ]
  
  # Add the file name as a column (optional, for tracking)
  last_row$file_name <- basename(file)
  # Combine the last row into the final dataset
  combined_data <- bind_rows(combined_data, last_row)
}
names(combined_data)[1:23] <- read.csv(file_list[1], header=F, skip=1, nrows=1)

# View the combined dataset
head(combined_data)

#remove GmSc and +/- (not relevant here as we study the whole team)
ad_gamestat <- combined_data[,-c(24,23,22,21,20,18,17)]
head(ad_gamestat)

#######################
#joining advanced gamestats into games dataset based on the id of the game
#######################

new_ad_gamestat <- ad_gamestat %>%
  mutate(
    id = as.numeric(gsub("[^0-9]", "", Starters.1)),  # Extract numeric ID
    team_type = ifelse(grepl("a", Starters.1), "away", "home")  # Determine team type
  )
head(new_ad_gamestat)

#rename the stats col according to home or away statu
ad_away_stats <- new_ad_gamestat %>%
  filter(team_type == "away") %>%
  select(-Starters.1, -team_type) %>%
  rename_with(~ paste0(., "_a"), -id)  # Append "_a" for away stats
head(ad_away_stats)

ad_home_stats <- new_ad_gamestat %>%
  filter(team_type == "home") %>%
  select(-Starters.1, -team_type) %>%
  rename_with(~ paste0(., "_h"), -id)  # Append "_h" for home stats
head(ad_home_stats)

#merge the df into games df
fullgame_advanced <- games %>%
  left_join(ad_away_stats, by = "id") %>%
  left_join(ad_home_stats, by = "id")
head(fullgame_advanced)

write.csv(fullgame_basic,"fullgame_basic.csv", row.names = FALSE)
write.csv(fullgame_advanced,"fullgame_advanced.csv", row.names = FALSE)


#########################################################################
#########################################################################
#same process but for a whole season datasets (without covariates for now)
#########################################################################
#########################################################################

#1 file per month --> merge all the files in one 
folder_path <- "/Users/icesim/Desktop/tcd/cours/dissertation/data 9:10 szn"
file_list <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

#combined dataset
combined_data <- data.frame()
for (file in file_list) {
  temp_data <- read.csv(file, header = F)
  combined_data <- bind_rows(combined_data, temp_data)
}

colnames(combined_data) <- c(combined_data[1,])
combined_data <- combined_data[-1,]

write.csv(combined_data, "/Users/icesim/Desktop/tcd/cours/dissertation/data 9:10 szn/season.csv", row.names = FALSE)


##########################################################
#importing covariates 
##########################################################

test <- read.csv("/Users/icesim/Desktop/nba_2023_2024_match_stats.csv", header = TRUE, skip=1)
head(test)
test$MP <- as.numeric(test$MP)

#date column 
test <- test %>%
  separate(X, into = c("Matchup", "dategame"), sep = " Box Score, ", extra = "merge")
test$dategame <- as.Date(test$dategame, format = "%B %d, %Y")

#keep only regular season games
cutoff_date <- as.Date("2024-04-16")
df_filtered <- test %>%
  filter(dategame < cutoff_date)

#retrieve only team totals stats 
team_stats <- df_filtered[df_filtered$Starters == "Team Totals" & df_filtered$MP >= 240,]
head(team_stats)

#create col for team names
team_stats <- team_stats %>%
  mutate(
    team_name = ifelse(
      row_number() %% 2 == 1,  
      str_extract(Matchup, "^(.*?) at") %>% str_replace(" at$", "") %>% str_trim(),
      str_extract(Matchup, "at (.*)$") %>% str_replace("^at ", "") %>% str_trim()
    )
  )

team_stats <- team_stats %>%
  mutate(team_name = str_replace(team_name, "^at ", ""))

team_stats <- team_stats %>%
  mutate(team_name = str_replace(team_name, "In-Season Tournament Final: ", ""))
head(team_stats)

team_stats <- team_stats %>%
  mutate(game_id = rep(1:(n()/2), each = 2))

#merge stats from both teams in one row
home_stats <- team_stats %>%
  filter(row_number() %% 2 == 0) %>%
  rename(home_team = team_name)

away_stats <- team_stats %>%
  filter(row_number() %% 2 == 1) %>%
  rename(away_team = team_name) %>%
  rename_with(~ paste0(., "a"), -c(game_id, away_team, dategame, Matchup))  # Add "a" suffix to stats

merged_stats <- home_stats %>%
  inner_join(away_stats, by = c("game_id", "dategame", "Matchup")) %>%
  select(dategame, Matchup, home_team, away_team, everything(), -game_id)

head(merged_stats)
merged_stats[,colnames(merged_stats)=="Starter"]

merged_stats <- merged_stats %>%
  select(-c(Startersa, GmSc, GmSca, X..., X...a, X.1, X.1a))
head(merged_stats)

write_csv(merged_stats,"fullseason.csv")
