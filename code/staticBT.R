#######################################################
######################################################
#
# this script is used to compute the static BT model
#
######################################################
#####################################################

library(BradleyTerry2)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)

#####################################################
#####################################################

######################
# nba tournament (first try but not enough games)
######################

data <- read.csv("/Users/icesim/Desktop/tcd/cours/dissertation/data/fullgame_basic.csv")
dataBT <- data[,c(1:6)]
head(dataBT)
dataBT <- dataBT %>%
  mutate(outcome = ifelse(AwayScore > HomeScore, 0, 1)) #adding win/loss

#Arrange the data properly
dataBT$away <- trimws(dataBT$away)
dataBT$Home <- trimws(dataBT$Home)

dataBT$away <- tolower(dataBT$away)
dataBT$Home <- tolower(dataBT$Home)

dataBT <- dataBT %>%
  mutate(
    away = ifelse(away == "ny", "knicks", away),
    Home = ifelse(Home == "ny", "knicks", Home)
  )

dataBT <- dataBT %>%
  mutate(
    away = ifelse(away == "indiana", "pacers", away),
    Home = ifelse(Home == "indiana", "pacers", Home)
  )

dataBT <- dataBT %>%
  mutate(
    Home = ifelse(Home == "rockets", "houston", Home)
  )

dataBT$away <- as.factor(dataBT$away)
dataBT$Home <- as.factor(dataBT$Home)

#home advantage
dataBT<- dataBT %>% mutate(homead = ifelse(outcome == 1, 1,-1))

dataBT$outcome <- as.factor(dataBT$outcome)
dataBT$tov_a <- data$TOV_a
dataBT$tov_h <- data$TOV_h
dataBT$x3pt <- data$X3P._h
dataBT$x3pta <- data$X3P._a

# Fit the Bradley-Terry model
bt_model <- BTm(outcome = dataBT$outcome,
                player1 = dataBT$Home,
                player2 = dataBT$away)
summary(bt_model)

#BT model with covariates
bt_model_cov <- BTm(
  outcome = dataBT$outcome, 
  player1 = dataBT$Home, 
  player2 = dataBT$away,
  formula = ~ x3pt[..] + x3pta[..] + tov_h[..] + tov_a[..],
  data = dataBT
)

summary(bt_model_cov)
BTabilities(bt_model_cov)

btvar <- data.frame(tov = data$TOV_a,  row.names = levels(data$away))
bt_model2 <- BTm(dataBT[,c(3,2,7)] ~ data$TOV_a, data)


###########################################################################################
##########################################################################################
# whole season 
###########################################################################################
##########################################################################################

data <- read.csv("/Users/icesim/Desktop/tcd/cours/dissertation/data 23:24 szn/season.csv")
str(data)
data$PTS <- as.integer(data$PTS)
data$PTS.1 <- as.integer(data$PTS.1)
data <- data %>%
  mutate(outcome = ifelse(PTS > PTS.1, 0, 1)) #adding win/loss
head(data)
data <- na.omit(data)
data$Visitor.Neutral <- as.factor(data$Visitor.Neutral)
data$Home.Neutral <- as.factor(data$Home.Neutral)

#fit the static bt model
bt_model <- BTm(outcome = data$outcome,
                player1 = Home.Neutral,
                player2 = Visitor.Neutral,
                data = data)
summary(bt_model)

df <- BTabilities(bt_model)
df <- as.data.frame(df)
df <- df[order(df$ability, decreasing = TRUE),]
df$team <- rownames(df)
df$team <- factor(df$team, levels = df$team[order(df$ability, decreasing = TRUE)])

#plots of ability + error bars
ggplot(data = df) +
  geom_col(aes(x = team, y = ability, fill = - ability)) +
  scale_fill_distiller(palette = "Reds") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(x = team, ymin = ability - s.e., ymax = ability + s.e.), width = 0.2) +
  labs(x = "Teams", y = "Ability")

# Extract team abilities from the BT model
team_abilities <- bt_model$coefficients 
names(team_abilities) <- gsub("^\\.\\.", "", names(team_abilities))
team_abilities <- c(0, team_abilities)   # Add baseline team with ability 0
names(team_abilities)[1] <- levels(data$Home)[1]  # Assign baseline name
team_abilities <- sort(team_abilities, decreasing = TRUE) 

# Compute probability matrix
teams <- names(team_abilities)
n <- length(teams)
prob_matrix <- matrix(0, nrow=n, ncol=n, dimnames=list(teams, teams))

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      prob_matrix[i, j] <- exp(team_abilities[i]) / (exp(team_abilities[i]) + exp(team_abilities[j]))
    } else {
      prob_matrix[i, j] <- NA  # No probability for a team vs itself
    }
  }
}

#for ggplot format
prob_data <- melt(prob_matrix)
colnames(prob_data) <- c("Team_A", "Team_B", "Win_Prob")

# Plot the heatmap with teams ranked
ggplot(prob_data, aes(x = Team_B, y = Team_A, fill = Win_Prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Probability of Team A beating Team B", fill = "P(A beats B)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for better readability


###################################################################
#with covariates (with modified package)
###################################################################

data <- read.csv("/Users/icesim/Desktop/tcd/cours/dissertation/fullseason.csv")
data$PTS <- as.numeric(data$PTS)
data$PTSa <- as.numeric(data$PTSa)

data[,6:43] <- lapply(data[,6:43], as.numeric)
data <- data %>%
  mutate(outcome = ifelse(PTS > PTSa, 1, 0)) #adding win/loss
head(data)

data$home_team <- as.factor(data$home_team)
data$away_team <- as.factor(data$away_team)
btmod2 <- BTm(outcome = outcome,
              player1 = home_team,
              player2 = away_team,
              formula = ~  AST[..] + TOV[..]  + X3P.[..] +  TRB[..] + (1 |..),
              data = data)
summary(btmod2)
BTabilities(btmod2)

#plots with covariates
beta_estimates <- as.data.frame(coef(btmod2[1]))
beta_estimates$metric <- rownames(beta_estimates)
beta_estimates$se <- c(0.01845,0.02611,1.18540,0.01523)
ggplot(beta_estimates, aes(y = metric, x = beta_estimates)) +
  geom_col()

ggplot(beta_estimates, aes(y = metric, x = beta_estimates$`coef(btmod2)`)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(y = "Game Metric", x = "Estimated Effect (β)")

beta_estimates$`coef(btmod2[1])`[rownames(beta_estimates) == "X3P."] <- beta_estimates$`coef(btmod2[1])`[rownames(beta_estimates) == "X3P."]/10
beta_estimates$se[rownames(beta_estimates) == "X3P."] <- beta_estimates$se[rownames(beta_estimates) == "X3P."]/10

ggplot(beta_estimates, aes(x = metric, y = beta_estimates$`coef(btmod2[1])`)) +
  geom_col(fill = "steelblue") +
  #coord_flip() +
  labs(x = "Game Metric", y = "Estimated Effect (β)") +
  geom_errorbar(aes(x = metric, ymin = beta_estimates$`coef(btmod2[1])` - se, ymax = beta_estimates$`coef(btmod2[1])` + se), width = 0.2)

########################################
cor_matrix <- cor(data[,6:46])
print(cor_matrix)
str(data)
melted_cor <- melt(cor_matrix)

ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(x = "", y="")

#ranking to compare the two models (with and without predictors) 
nopred <- as.data.frame(BTabilities(bt_model))
nopred <- nopred[order(nopred$ability, decreasing = TRUE),]
nopred$team <- rownames(nopred)
nopred$staticrank <- rank(-nopred$ability)
rank_nopred <- rank(-nopred$ability)

pred <- as.data.frame(BTabilities(btmod2))
rank_pred <- rank(-pred$ability)

ggplot()+
  geom_point(aes(x = rank_nopred, y = rank_pred))+
  labs(x = "Ranking without predictors", y = "Ranking with predictors") +
  geom_abline(slope = 1, intercept = 0, color = "red")

#comparing with the official standing
standing <- read.csv("/Users/icesim/Desktop/tcd/cours/dissertation/ranking.txt", header = TRUE)
standing <- standing[-1,]
standing$X <- as.numeric(standing$X)

pred$X.1 <- rownames(pred)
rank_standing_arrange <- join(pred,standing,by = "X.1")
rank_standing_arrange <- rank_standing_arrange[,-c(6:18)]

rank_standing_arrange$rank_nopred <- rank_nopred
rank_standing_arrange$rank_pred <- rank_pred
head(rank_standing_arrange)

#without predictors
ggplot(rank_standing_arrange, aes(x = rank_nopred, y = X))+
  geom_point()+
  labs(x = "Ranking without predictors", y = "Official ranking") +
  geom_text(aes(label = X.1), hjust = -0.1, vjust = 0.5, size = 2.5)+
  geom_abline(slope = 1, intercept = 0, color = "red")


#with predictors 
ggplot(rank_standing_arrange, aes(x = rank_pred, y = X))+
  geom_point()+
  labs(x = "Ranking with predictors", y = "Official ranking") +
  geom_text(aes(label = X.1), hjust = -0.1, vjust = 0.5, size = 2.5)+
  geom_abline(slope = 1, intercept = 0, color = "red")

##############################################################
#test on 9/10 season 
##############################################################

data <- read.csv('/Users/icesim/Desktop/tcd/cours/dissertation/data 9:10 szn/season.csv')
data$PTS <- as.integer(data$PTS)
data$PTS.1 <- as.integer(data$PTS.1)
data <- data %>%
  mutate(outcome = ifelse(PTS > PTS.1, 0, 1)) #adding win/loss
head(data)
data <- na.omit(data)
data$Visitor.Neutral <- as.factor(data$Visitor.Neutral)
data$Home.Neutral <- as.factor(data$Home.Neutral)

head(data)

bt <- BTm(outcome = data$outcome,
          player1 = data$Home.Neutral,
          player2 = data$Visitor.Neutral)
summary(bt)
abilities <- BTabilities(bt)
ref <- "Chicago Bulls"
abilites <- as.data.frame(abilities)
abilites$ability <- abilites$ability - abilities[rownames(abilities)=="Chicago Bulls"][1]
abilites[order(abilites$ability, decreasing = TRUE),]

####################################################
####################################################

######################################################
##test
######################################################

data2 <- read.csv("/Users/icesim/Downloads/matchs.txt")
data2 <- data2[-1,]
data2 <- data2 %>% mutate(outcome = ifelse(PTS > PTS.1, 1,0))
data2$Visitor.Neutral <- as.factor(data2$Visitor.Neutral)
data2$Home.Neutral <- as.factor(data2$Home.Neutral)
bttest <- BTm(outcome = data2$outcome,
              player1 = data2$Visitor.Neutral,
              player2 = data2$Home.Neutral)
summary(bttest)

data3 <- read.csv("/Users/icesim/Downloads/matchs2.txt")
data3 <- data3 %>% mutate(outcome = ifelse(PTS > PTS.1, 1,0))
data3$Visitor.Neutral <- as.factor(data3$Visitor.Neutral)
data3$Home.Neutral <- as.factor(data3$Home.Neutral)

dataT <- rbind(data2, data3)
bttest <- BTm(outcome = dataT$outcome,
              player1 = dataT$Visitor.Neutral,
              player2 = dataT$Home.Neutral)
summary(bttest)


##############################################################################
#heatmap : prob of each team of winning against others (without covariates)
##############################################################################

# Extract team abilities from the BT model
team_abilities <- bt_model$coefficients 
team_abilities <- c(0, team_abilities)   # Add baseline team with ability 0
names(team_abilities)[1] <- levels(dataBT$Home)[1]  # Assign baseline name
team_abilities <- sort(team_abilities, decreasing = TRUE) 

# Compute probability matrix
teams <- names(team_abilities)
n <- length(teams)
prob_matrix <- matrix(0, nrow=n, ncol=n, dimnames=list(teams, teams))

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      prob_matrix[i, j] <- exp(team_abilities[i]) / (exp(team_abilities[i]) + exp(team_abilities[j]))
    } else {
      prob_matrix[i, j] <- NA  # No probability for a team vs itself
    }
  }
}

#for ggplot format
prob_data <- melt(prob_matrix)
colnames(prob_data) <- c("Team_A", "Team_B", "Win_Prob")

# Plot the heatmap with teams ranked
ggplot(prob_data, aes(x = Team_B, y = Team_A, fill = Win_Prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Probability of Team A beating Team B", fill = "P(A beats B)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for better readability

#heatmap with team unranked 
team_abilities <- bt_model$coefficients 
team_abilities <- c(0, team_abilities)   # Add baseline team with ability 0
names(team_abilities)[1] <- levels(dataBT$Home)[1]  # Assign baseline name

teams <- names(team_abilities)
n <- length(teams)
prob_matrix <- matrix(0, nrow=n, ncol=n, dimnames=list(teams, teams))

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      prob_matrix[i, j] <- exp(team_abilities[i]) / (exp(team_abilities[i]) + exp(team_abilities[j]))
    } else {
      prob_matrix[i, j] <- NA  # No probability for a team vs itself
    }
  }
}

#for ggplot format
prob_data <- melt(prob_matrix)
colnames(prob_data) <- c("Team_A", "Team_B", "Win_Prob")


ggplot(prob_data, aes(x = Team_B, y = Team_A, fill = Win_Prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Probability of Team A beating Team B", fill = "P(A beats B)", x = "Team B", y = "Team A") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################################################
#
#test2 with covariates
####################################################

data2 <- data.frame(winner = character(nrow(data)), loser = character(nrow(data)))
data2$winner <- ifelse(data$outcome == 1, as.character(data$home_team), as.character(data$away_team))
data2$loser  <- ifelse(data$outcome == 1, as.character(data$away_team), as.character(data$home_team))

predictors <- data.frame(
  game_id = 1:nrow(data),  # Unique identifier for each game
  home_team = data$home_team,
  away_team = data$away_team,
  home_PTS = data$PTS,  # Points scored by home team in this game
  away_PTS = data$PTSa,  # Points scored by away team in this game
  home_FG = data$FG,  # Field Goals made by home team in this game
  away_FG = data$FGa,  # Field Goals made by away team in this game
  home_AST = data$AST,  # Assists by home team in this game
  away_AST = data$ASTa  # Assists by away team in this game
)

data2$winner <- as.factor(data2$winner)
data2$loser <- as.factor(data2$loser)

bt_model <- BTm(
  outcome = 1,  # Always 1 because the winner is compared to the loser
  player1 = winner,
  player2 = loser,
  formula = ~ home_PTS[..] + away_PTS[..] + home_FG[..] + away_FG[..] + (1 | ..),
  data = list(contests = data2, predictors = predictors)
)
summary(bt_model)
BTabilities(bt_model)

lizModel <- BTm(1, winner, loser, ~ SVL[..] + (1|..), data = flatlizards)
summary(lizModel)
BTabilities(lizModel)

devtools::install_local("/Users/icesim/Desktop/BT2-modified/package/BradleyTerry2/", force = TRUE)


##############################################################
#likelihood 
#############################################################

teams <- unique(c(data$home_team, data$away_team))

# Initialize lambdas 
lambda <- rep(0.1, length(teams))
names(lambda) <- teams

log_likelihood <- function(lambda) {
  log_likelihood_value <- 0
  
  for (k in 1:nrow(data)) {
    home_team <- data$home_team[k]
    away_team <- data$away_team[k]
    outcome <- data$outcome[k]
    
    lambda_home <- lambda[home_team]
    lambda_away <- lambda[away_team]
    
    if (outcome == 1) {
      # Home team wins
      log_likelihood_value <- log_likelihood_value + log(exp(lambda_home) / (exp(lambda_home) + exp(lambda_away)))
    } else {
      # Away team wins
      log_likelihood_value <- log_likelihood_value + log(exp(lambda_away) / (exp(lambda_home) + exp(lambda_away)))
    }
  }
  return(log_likelihood_value) #to use optim function
}

initial_lambda <- rep(0, length(teams))  
names(initial_lambda) <- teams

result <- optim(par = initial_lambda, 
                fn = log_likelihood)

result

#with Hawks as the baseline 
ref <- "Atlanta Hawks"
ref_ability <- result$par[ref]
ref_ability
result$par <- result$par - ref_ability
result


remove.packages("BradleyTerry2")
 


