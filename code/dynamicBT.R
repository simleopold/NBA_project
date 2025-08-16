#########################################################
#########################################################
#
#this script is used for the dynamic bradley terry model 
#
########################################################
########################################################

library(dplyr)
library(glmnet)
library(parallel)
library(tidyr)
library(ggplot2)

#########################################################
#########################################################

#########################################################
#preparing data
#########################################################

data <- read.csv("/Users/icesim/Desktop/tcd/cours/dissertation/23_24season_cov.csv")
data <- data %>%
  mutate(outcome = ifelse(PTS > PTSa, 1, 0))
data <- data %>%
  mutate(outcome_h = ifelse(PTS > PTSa, 1, 0))
data <- data %>%
  mutate(outcome_a = ifelse(PTS < PTSa, 1, 0))
data2 <- data %>% select(home_team,away_team,outcome)
data2$match_id <- 1:nrow(data2)
teams <- unique(c(data$home_team, data$away_team))

#data from the 09/10 season (processed in staticBT.R)
data <- data %>% rename(away_team = Visitor.Neutral,
                home_team = Home.Neutral)

#########################################################
#ewma 
#########################################################

ewma <- function(team_name, match_index, match_data, lambda, beta, is_home, avg_result) {
  past_matches <- match_data[1:(match_index - 1), ]
  if (is_home) {
    team_matches <- past_matches %>% filter(home_team == team_name)
    results <- team_matches$outcome
  } else {
    team_matches <- past_matches %>% filter(away_team == team_name)
    results <- 1 - team_matches$outcome
  }
  
  K <- length(results)
  if (K == 0) return(beta * avg_result)
  
  weights <- lambda * (1 - lambda)^(0:(K - 1))
  
  ewma_sum <- sum(weights * rev(results))
  ewma_ability <- beta * (ewma_sum + (1 - lambda)^K * avg_result)
  
  return(ewma_ability)
}

#################################################################
#beta and lambda estimations
#################################################################

#Logistic Regression with fixed lambdas
estimate_betas <- function(lambda1, lambda2, data, avg_home, avg_away) {
  home_ability <- numeric(nrow(data))
  away_ability <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    hi <- data$home_team[i]
    vi <- data$away_team[i]
    
    #betas set to 1
    home_ability[i] <- ewma(hi, i, data, lambda1, 1, TRUE, avg_home)
    away_ability[i] <- ewma(vi, i, data, lambda2, 1, FALSE, avg_away)
  }
  
  model <- glm(data$outcome ~ home_ability + away_ability, family = binomial(link = "logit"))
  return(model)
}

#estimating lambdas via MLE
loglike <- function(lambdas, data, avg_home, avg_away) {
  lambda1 <- lambdas[1]
  lambda2 <- lambdas[2]
  home_ability <- numeric(nrow(data))
  away_ability <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    hi <- data$home_team[i]
    vi <- data$away_team[i]
    home_ability[i] <- ewma(hi, i, data, lambda1, 1, TRUE, avg_home)
    away_ability[i] <- ewma(vi, i, data, lambda2, 1, FALSE, avg_away)
  }
  model <- glm(data$outcome ~ home_ability + away_ability, family = binomial(link = "logit"))
  return(-logLik(model))
}

data$away_team <- as.character(data$away_team)
data$home_team <- as.character(data$home_team)
initial_lambdas <- c(0.1, 0.1)
avg_home <- 0.608 #0.5743 home win rate from previous season
avg_away <- 1 - avg_home

#estimation of betas
initial_model <- estimate_betas(initial_lambdas[1], initial_lambdas[2], data, avg_home, avg_away)
summary(initial_model)

#estimation of lambdas 
opt_lambdas <- optim(
  par = initial_lambdas,
  fn = loglike,
  data = data,
  avg_home = avg_home,
  avg_away = avg_away,
)

logn1 <- loglike(lambdas = c(0.1,0.1), data, avg_home, avg_away)
logn2 <- loglike(lambdas = c(opt_lambdas$par[1], opt_lambdas$par[2]), data, avg_home, avg_away)

i <- 0
while(abs((logn1 - logn2) /logn2) >  10e-6 ){
  initial_lambdas <- opt_lambdas$par
  logn1 <- logn2
  opt_lambdas <- optim(  par = initial_lambdas,
                         fn = loglike,
                         data = data,
                         avg_home = avg_home,
                         avg_away = avg_away,
                         method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
  logn2 <- loglike(opt_lambdas$par, data, avg_home, avg_away)
  i <- i + 1
}  


#final estimation
final_model <- estimate_betas(opt_lambdas$par[1], opt_lambdas$par[2], data, avg_home, avg_away)

opt_lambdas$par[1] #lambda1
opt_lambdas$par[2] #lambda2
coef(final_model)[2] #beta1
coef(final_model)[3] #beta2


#compute the final ability of each team at the end of the season 

all_teams <- unique(c(data$home_team, data$away_team))
#all_teams <- as.character(all_teams)
team_ability_history <- setNames(vector("list", length(all_teams)), all_teams)

beta1 <- coef(final_model)["home_ability"] #5.503 coef(final_model)["home_ability"]
beta2 <- abs(coef(final_model)["away_ability"]) #7.379  
names(beta1) <- "home_ability"
names(beta2) <- "away_ability"
lambda1 <- opt_lambdas$par[1] #0.043 
lambda2 <- opt_lambdas$par[2] #0.025 opt_lambdas$par[2]

for (i in 1:nrow(data)) {
  home <- data$home_team[i]
  away <- data$away_team[i]
  
  # Compute and store home ability
  home_ability <- ewma(home, i, data, lambda1, beta1, TRUE, avg_home)
  team_ability_history[[home]] <- c(team_ability_history[[home]], home_ability)
  
  # Compute and store away ability
  away_ability <- ewma(away, i, data, lambda2, beta2, FALSE, avg_away)
  team_ability_history[[away]] <- c(team_ability_history[[away]], away_ability)
}

final_abilities <- data.frame(
  team = all_teams,
  final_ability = sapply(all_teams, function(team) {
    mean(team_ability_history[[team]])
  })
)

final_abilities$final_ability <- final_abilities$final_ability - mean(final_abilities$final_ability)
print(final_abilities[order(final_abilities$final_ability, decreasing = TRUE),])
final_abilities <- final_abilities[order(final_abilities$final_ability, decreasing = TRUE),]

#ranking
final_abilities$rank <- rank(- final_abilities$final_ability)
standing <- standing %>% rename("team" = "X.1")
rank_standing_arrange <- dplyr::left_join(standing, final_abilities, by = "team")
rank_standing_arrange <- dplyr::left_join(rank_standing_arrange, nopred, by = "team")
cor(rank_standing_arrange$X, rank_standing_arrange$rank, method = "spearman")
cor(rank_standing_arrange$X, rank_standing_arrange$staticrank, method = "spearman")

###################################
#plots for the 09/10 season
###################################

#comparison between abilites with the paper 
dynamic_abilities <- c(
  "Cleveland Cavaliers"       =  0.769,
  "Orlando Magic"             =  0.568,
  "Los Angeles Lakers"        =  0.650,
  "Dallas Mavericks"          =  0.451,
  "Phoenix Suns"              =  0.382,
  "Atlanta Hawks"             =  0.424,
  "Denver Nuggets"            =  0.482,
  "Utah Jazz"                 =  0.348,
  "Boston Celtics"            =  0.447,
  "Oklahoma City Thunder"     =  0.268,
  "Portland Trail Blazers"    =  0.253,
  "San Antonio Spurs"         =  0.270,
  "Miami Heat"                =  0.070,
  "Milwaukee Bucks"           =  0.054,
  "Charlotte Bobcats"         =  0.019,
  "Houston Rockets"           =  0.080,
  "Chicago Bulls"             = -0.065,
  "Memphis Grizzlies"         =  0.019,
  "Toronto Raptors"           =  0.017,
  "New Orleans Hornets"       = -0.037,
  "Indiana Pacers"            = -0.382,
  "Los Angeles Clippers"      = -0.300,
  "New York Knicks"           = -0.415,
  "Detroit Pistons"           = -0.446,
  "Philadelphia 76ers"        = -0.457,
  "Golden State Warriors"     = -0.586,
  "Washington Wizards"        = -0.494,
  "Sacramento Kings"          = -0.436,
  "Minnesota Timberwolves"    = -0.834,
  "New Jersey Nets"           = -1.119
)

dynamic_df <- data.frame(
  team = names(dynamic_abilities),
  dynamic_ability = as.numeric(dynamic_abilities),
  stringsAsFactors = FALSE
)

joined_df <- final_abilities %>%
  left_join(dynamic_df, by = "team")

ggplot(data = joined_df, aes(x = final_ability, y = dynamic_ability)) +
  geom_point() +
  geom_text(aes(label = team), hjust = -0.1, vjust = 0.5, size = 3) + 
  geom_abline(slope = 1, intercept = 0, color = "red")+
  labs(
    x = "Abilities computed",
    y = "Abilities from the article"
  ) +
  theme_minimal()

#correlation
cor(joined_df$final_ability, joined_df$dynamic_ability)

#tracking the ability over games 
#cavs
home_vals <- team_ability_history$`Boston Celtics`[ which(names(team_ability_history$`Boston Celtics`) == "home_ability")]
away_vals <- team_ability_history$`Oklahoma City Thunder`[ which(names(team_ability_history$`Oklahoma City Thunder`) == "away_ability")]

if (length(home_vals) > length(away_vals)) {
  away_vals <- c(away_vals, rep(tail(away_vals, 1), length(home_vals) - length(away_vals)))
} else if (length(away_vals) > length(home_vals)) {
  home_vals <- c(home_vals, rep(tail(home_vals, 1), length(away_vals) - length(home_vals)))
}

df <- data.frame(
  home = home_vals,
  away = away_vals,
  home_id = 1:length(home_vals),
  away_id = 1:length(away_vals))


ggplot(data = df) +
  geom_line(aes(x = 1:length(home_vals), home)) +
  geom_line(aes(x = 1:length(away_vals), away), linetype = "dashed") +
  scale_y_continuous(limits = c(0, 5.2)) +
  labs(
    x = "games",
    y = "ability",
    title = "Oklahoma City Thunder"
  ) +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )


df <- data_frame("home" = c(team_ability_history$`Boston Celtics`[ which(names(team_ability_history$`Boston Celtics`) == "home_ability")]),
                 "away" = c(team_ability_history$`Boston Celtics`[which(names(team_ability_history$`Boston Celtics`) == "away_ability")]),
                 "home_id" =which(names(team_ability_history$`Boston Celtics`) == "home_ability"),
                 "away_id" = which(names(team_ability_history$`Boston Celtics`) == "away_ability"))

ggplot(data = df)+
  geom_line(aes(x = home_id, y = home))+
  geom_line(aes(x =away_id, y = away, color = "red"))+
  labs(color = "away",
       title = "Away and home abilities (without covariates): Boston Celtics",
       x = "games",
       y= "ability")

###################################################################################################################
#accounting for covariates now
###################################################################################################################

################################
# Function to compute EWMA vector for a team and set of features
################################

compute_ewma_vector <- function(team_name, match_index, match_data, lambda, is_home, avg_features, feature_cols_home , feature_cols_away, smoothed = TRUE) {
  past_matches <- match_data[1:(match_index - 1), ]
  
  if (is_home) {
    team_matches <- past_matches %>% filter(home_team == team_name)
    results <- team_matches[, feature_cols_home]
  } else {
    team_matches <- past_matches %>% filter(away_team == team_name)
    results <- team_matches[, feature_cols_away] #, drop = FALSE
  }
  
  K <- nrow(results)
  if (K == 0) {
    return(avg_features)
  }
  #weights <- lambda * (1 - lambda)^(0:(K - 1))
  #weights <- rev(weights)  # match the order with results (most recent last)
  
  #ewma_sum <- colSums(sweep(results, 1, weights, `*`)) # multiply each row by its corresponding weight (1 weight per row)
  #ewma_ability <- ewma_sum + (1 - lambda)^K * avg_features
  
  if (smoothed == TRUE) {
    #return(ewma_ability)  #vector of the sum of the each covariates based on the ewma on all previous games 
    weights <- lambda * (1 - lambda)^(0:(K - 1))
    weights <- rev(weights)  # match the order with results (most recent last)
    weighted_avg <- colSums(sweep(results, 1, weights, `*`))  # multiply each row by its corresponding weight
    return(weighted_avg)
  }
  else {
    weights <- lambda * (1 - lambda)^(0:(K - 1))
    weights <- rev(weights)
    results[,length(feature_cols_away)] <- weights * results[,length(feature_cols_away)] #smoothed the outcomes only
    weigthed_avg <- colSums(results) + (1 - lambda)^K * avg_features
    return(weigthed_avg)
  }
}

##########################################################
# betas estimation with logistic regression (lambdas fixed)
##########################################################

estimate_betas_vector <- function(data, feature_cols_home,feature_cols_away, lambda_home, lambda_away, avg_features) {
  n <- nrow(data)
  p <- length(feature_cols_home)
  
  home_matrix <- matrix(0, n, p)
  away_matrix <- matrix(0, n, p)
  
  for (i in 1:n) {
    hi <- data$home_team[i]
    ai <- data$away_team[i]
    
    home_matrix[i, ] <- compute_ewma_vector(hi, i, data, lambda_home, TRUE, avg_features, feature_cols_home, feature_cols_away,smoothed = FALSE)
    away_matrix[i, ] <- compute_ewma_vector(ai, i, data, lambda_away, FALSE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
  }
  
  colnames(home_matrix) <- paste0("home_", feature_cols_home)
  colnames(away_matrix) <- paste0("away_", feature_cols_away)
  
  # Combine into one data frame
  design_df <- as.data.frame(cbind(home_matrix, away_matrix))
  design_df$outcome <- data$outcome
  
  # Fit logistic regression
  formula_str <- paste("outcome ~ ", paste(colnames(home_matrix), collapse = " + "), 
                       "+", paste(colnames(away_matrix), collapse = " + "))
  model <- glm(as.formula(formula_str), data = design_df, family = binomial(link = "logit"))
  
  return(model)
}

# Log-likelihood for optimization
loglike_vector <- function(lambdas, data, feature_cols_home,feature_cols_away, avg_features) {
  lambda_home <- lambdas[1]
  lambda_away <- lambdas[2]
  
  model <- estimate_betas_vector(data, feature_cols_home,feature_cols_away, lambda_home, lambda_away, avg_features)
  return(-logLik(model))
}


feature_cols_home <- c("X3P.", "TOV", "TRB","AST","outcome_h") #"outcome_h" 
feature_cols_away <- c("X3P.a", "TOVa", "TRBa","ASTa","outcome_a") #"outcome_a" 
data <- data %>%
  mutate(across(c(feature_cols_home, feature_cols_away), scale))

#avg_features <- c(mean(data$outcome_h))
avg_features <- colMeans(data[, feature_cols_home], na.rm = TRUE)

#if scaled, run :
avg_features <- c(rep(avg_home,length(feature_cols_home)), rep(avg_away, length(feature_cols_away)))
initial_lambdas <- c(0.1, 0.1)

#################################################
#joint likelihood estimation 
lambda1 <- 0.1
lambda2 <- 0.1
n_iter <- 20  # Number of alternating optimization steps
p <- length(feature_cols_home)  # number of features
intercept <- 0  # can start at 0
beta1 <- rep(0, p+1)
beta2 <- rep(0, p)

# Step 1: Estimate β1 and β2 with fixed lambdas
precomputed_ewma <- lapply(1:nrow(data), function(i) {
  hi <- data$home_team[i]
  ai <- data$away_team[i]
  
  x_home <- compute_ewma_vector(hi, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
  x_away <- compute_ewma_vector(ai, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
  
  list(x_home = x_home, x_away = x_away)
})

joint_loglikelihood_fixed_ewma <- function(beta, precomputed_ewma, data) {
  intercept <- beta[1]                        # first element is intercept
  beta1 <- beta[2:(p+1)]                      # next p elements are beta1
  beta2 <- beta[(p+2):(2*p+1)]                # next p elements are beta2
  
  linear_preds <- sapply(1:nrow(data), function(i) {
    x_home <- precomputed_ewma[[i]]$x_home
    x_away <- precomputed_ewma[[i]]$x_away
    intercept + sum(beta1 * x_home) - sum(beta2 * x_away)  # add intercept
  })
  
  y <- data$outcome
  loglik <- sum(y * linear_preds - log(1 + exp(linear_preds)))
  return(-loglik)  # negative log-likelihood for minimization
}

log1 <- joint_loglikelihood_fixed_ewma(beta = c(beta1,beta2),precomputed_ewma,data = data )
opt_beta <- optim(
  par = c(beta1, beta2),
  fn = joint_loglikelihood_fixed_ewma,
  precomputed_ewma = precomputed_ewma,
  data = data,
  #method = "L-BFGS-B",
  hessian = TRUE
)

beta1 <- opt_beta$par[1:(p+1)]
beta2 <- opt_beta$par[(p+2):(2*p+1)]

log0 <- 1

while (abs((log1-log0)/ log1) > 10e-6) {
  #cat("Iteration", iter, "\n")
  
  # Step 1: Estimate β1 and β2 with fixed lambdas
  precomputed_ewma <- lapply(1:nrow(data), function(i) {
    hi <- data$home_team[i]
    ai <- data$away_team[i]
    
    x_home <- compute_ewma_vector(hi, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
    x_away <- compute_ewma_vector(ai, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
    
    list(x_home = x_home, x_away = x_away)
  })
  
  log0 <- log1
  log1 <- joint_loglikelihood_fixed_ewma(beta = c(beta1,beta2), precomputed_ewma, data)
  
  opt_beta <- optim(
    par = c(beta1, beta2),
    fn = joint_loglikelihood_fixed_ewma,
    precomputed_ewma = precomputed_ewma,
    data = data,
    #method = "L-BFGS-B",
    hessian = TRUE
  )
  
  beta1 <- opt_beta$par[1:(p+1)]
  beta2 <- opt_beta$par[(p+2):(2*p+1)]
  
  # Step 2: Estimate lambda1 and lambda2 with fixed β1 and β2
  joint_loglikelihood_lambda <- function(theta, data, feature_cols_home, feature_cols_away, avg_features) {
    lambda1 <- theta[1]
    lambda2 <- theta[2]
    intercept <- beta1[1]
    beta1 <- beta1[2:(p+1)]
    
    
    n <- nrow(data)
    linear_preds <- mclapply(1:n, function(i) {
      hi <- data$home_team[i]
      ai <- data$away_team[i]
      
      x_home <- compute_ewma_vector(hi, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
      x_away <- compute_ewma_vector(ai, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
      
      intercept + sum(beta1 * x_home) - sum(beta2 * x_away)
    }, mc.cores = detectCores() - 1)
    
    linear_preds <- unlist(linear_preds)
    y <- data$outcome
    loglik <- sum(y * linear_preds - log(1 + exp(linear_preds)))
    return(-loglik)
  }
  
  opt_lambda <- optim(
    par = c(lambda1, lambda2),
    fn = joint_loglikelihood_lambda,
    data = data,
    feature_cols_home = feature_cols_home,
    feature_cols_away = feature_cols_away,
    avg_features = avg_features,
    method = "L-BFGS-B",
    lower = c(0.001, 0.001),  # to avoid zero
    upper = c(1, 1),
    hessian = TRUE
  )
  
  lambda1 <- opt_lambda$par[1]
  lambda2 <- opt_lambda$par[2]
  
  cat(" -> lambda1 =", round(lambda1, 4), ", lambda2 =", round(lambda2, 4), "\n")
}

beta1 <- beta1[2:(p+1)]
all_teams <- unique(c(data$home_team, data$away_team))
team_ability_history <- setNames(vector("list", length(all_teams)), all_teams)

for (i in 1:nrow(data)) {
  home <- data$home_team[i]
  away <- data$away_team[i]
  
  home_x <- compute_ewma_vector(home, i, data, lambda1, TRUE, avg_features, feature_cols_home,feature_cols_away, smoothed = FALSE)
  away_x <- compute_ewma_vector(away, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away, smoothed = FALSE)
  
  home_ability <- sum(beta1 * home_x)
  away_ability <- sum(beta2 * away_x)
  
  team_ability_history[[home]] <- c(team_ability_history[[home]], home_ability)
  team_ability_history[[away]] <- c(team_ability_history[[away]], away_ability)
}

# Compute final average abilities
final_abilities <- data.frame(
  team = all_teams,
  final_ability = sapply(all_teams, function(team) {
    mean(team_ability_history[[team]], na.rm = TRUE)
  })
)

final_abilities$final_ability <- final_abilities$final_ability - mean(final_abilities$final_ability)
print(final_abilities[order(final_abilities$final_ability, decreasing = TRUE), ])

#tracking the ability over games 

team_game <- data[data$home_team == "Boston Celtics" | data$away_team == "Boston Celtics",]
home_indice <- which(team_game$home_team =="Boston Celtics")
away_indice <- which(team_game$away_team == "Boston Celtics")
home_vals <- team_ability_history$`Boston Celtics`[home_indice]
away_vals <- team_ability_history$`Boston Celtics`[away_indice]

if (length(home_vals) > length(away_vals)) {
  away_vals <- c(away_vals, rep(tail(away_vals, 1), length(home_vals) - length(away_vals)))
} else if (length(away_vals) > length(home_vals)) {
  home_vals <- c(home_vals, rep(tail(home_vals, 1), length(away_vals) - length(home_vals)))
}

df <- data.frame(
  home = home_vals,
  away = away_vals,
  home_id = 1:length(home_vals),
  away_id = 1:length(away_vals))


ggplot(data = df) +
  geom_line(aes(x = 1:length(home_vals), home)) +
  geom_line(aes(x = 1:length(away_vals), away), linetype = "dashed") +
  #scale_y_continuous(limits = c(0, 10)) +
  labs(
    x = "games",
    y = "ability",
    title = "Oklahoma City Thunder"
  ) +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )


##########################################################################
#confidence interval 
#########################################################################

#betas
cov_matrix_b <- solve(opt_beta$hessian) # Inverse
std_errors_b <- sqrt(diag(cov_matrix_b))

# 95% CI for each parameter
beta_hat <- opt_beta$par
beta_hat[7:11] <- - beta_hat[7:11] #change if intercept 
names(beta_hat) <- c("intercept",paste0("beta1_", feature_cols_home), paste0("beta2_", feature_cols_away))
ci_lower_b <- beta_hat - 1.96 * std_errors_b
ci_upper_b <- beta_hat + 1.96 * std_errors_b

conf_ints_b <- data.frame(
  parameter = names(beta_hat),
  estimate = beta_hat,
  lower_95 = ci_lower_b,
  upper_95 = ci_upper_b
)
conf_ints_b

#lambdas
cov_matrix <- solve(opt_lambda$hessian) #inverse
std_errors <- sqrt(diag(cov_matrix))

#95% CI for each parameter
lambda_hat <- opt_lambda$par
names(lambda_hat) <- c("lambda1", "lambda2")
ci_lower <- lambda_hat - 1.96 * std_errors
ci_upper <- lambda_hat + 1.96 * std_errors

#print CIs
conf_ints <- data.frame(
  parameter = names(lambda_hat),
  estimate = lambda_hat,
  lower_95 = ci_lower,
  upper_95 = ci_upper
)
print(conf_ints)

conf_ints <- rbind(conf_ints, conf_ints_b)

#centipede plot

ggplot(conf_ints, aes(x = parameter, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs( y = "Estimate", x = "Parameters")


################################################################################################################
################################################################################################################

#plots 
temp <- which(data$home_team == "Boston Celtics" | data$away_team == "Boston Celtics")
temp_df <- data[temp,]
df_d <- data_frame("home" = team_ability_history$`Boston Celtics`[which(temp_df$home_team == "Boston Celtics")],
                 "away" = team_ability_history$`Boston Celtics`[which(temp_df$away_team == "Boston Celtics")],
                 "home_id" =which(temp_df$home_team == "Boston Celtics"),
                 "away_id" = which(temp_df$away_team == "Boston Celtics")) 

ggplot(data = df_d)+
  geom_line(aes(x = home_id, y = home))+
  geom_line(aes(x =away_id, y = away, color = "red"))+
  labs(color = "away",
       title = "Away and home abilities (with covariates): Boston Celtics",
       x = "games",
       y= "ability")

temp <- which(data$home_team == "Washington Wizards" | data$away_team == "Washington Wizards")
temp_df <- data[temp,]
df_d2 <- data_frame("home" = team_ability_history$`Washington Wizards`[which(temp_df$home_team == "Washington Wizards")],
                   "away" = team_ability_history$`Washington Wizards`[which(temp_df$away_team == 
                                                                              "Washington Wizards")],
                   "home_id" =which(temp_df$home_team == "Washington Wizards"),
                   "away_id" = which(temp_df$away_team == "Washington Wizards")) 

ggplot(data = df_d2)+
  geom_line(aes(x = home_id, y = home))+
  geom_line(aes(x =away_id, y = away, color = "red"))+
  labs(color = "away",
       title = "Away and home abilities (with covariates): Wizards",
       x = "games",
       y= "ability")


#with the intercept
# Extract coefficients (intercept + betas)
coefs <- coef(final_model)
intercept <- coefs[1]

# Adjust for the intercept when slicing betas
p <- (length(coefs) - 1) / 2
beta1 <- coefs[2:(1 + p)]           # home coefficients
beta2 <- coefs[(2 + p):(1 + 2 * p)] # away coefficients

# Lambda values from optimization
lambda1 <- final$par[1]
lambda2 <- final$par[2]

# Prepare to track team abilities over time
all_teams <- unique(c(data$home_team, data$away_team))
team_ability_history <- setNames(vector("list", length(all_teams)), all_teams)

for (i in 1:nrow(data)) {
  home <- data$home_team[i]
  away <- data$away_team[i]
  
  home_x <- compute_ewma_vector(home, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away)
  away_x <- compute_ewma_vector(away, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away)
  
  home_ability <- sum(beta1 * home_x)
  away_ability <- sum(beta2 * away_x)
  
  # Include the intercept in the ability difference (logit scale)
  ability_with_intercept_home <- intercept + home_ability
  ability_with_intercept_away <- away_ability
  
  team_ability_history[[home]] <- c(team_ability_history[[home]], ability_with_intercept_home)
  team_ability_history[[away]] <- c(team_ability_history[[away]], ability_with_intercept_away)
}

# Compute average ability per team and mean-center
final_abilities <- data.frame(
  team = all_teams,
  final_ability = sapply(all_teams, function(team) {
    mean(team_ability_history[[team]], na.rm = TRUE)
  })
)

# Mean-center for interpretability
final_abilities$final_ability <- final_abilities$final_ability - mean(final_abilities$final_ability)

# Display sorted abilities
print(final_abilities[order(final_abilities$final_ability, decreasing = TRUE), ])


#plots with intercept 
temp <- which(data$home_team == "Boston Celtics" | data$away_team == "Boston Celtics")
temp_df <- data[temp,]
df_d <- data_frame("home" = team_ability_history$`Boston Celtics`[which(temp_df$home_team == "Boston Celtics")],
                   "away" = team_ability_history$`Boston Celtics`[which(temp_df$away_team == "Boston Celtics")],
                   "home_id" =which(temp_df$home_team == "Boston Celtics"),
                   "away_id" = which(temp_df$away_team == "Boston Celtics")) 

ggplot(data = df_d)+
  geom_line(aes(x = home_id, y = home))+
  geom_line(aes(x =away_id, y = away, color = "red"))+
  labs(color = "away",
       title = "Away and home abilities (with covariates): Boston Celtics",
       x = "games",
       y= "ability")

temp <- which(data$home_team == "Washington Wizards" | data$away_team == "Washington Wizards")
temp_df <- data[temp,]
df_d2 <- data_frame("home" = team_ability_history$`Washington Wizards`[which(temp_df$home_team == "Washington Wizards")],
                    "away" = team_ability_history$`Washington Wizards`[which(temp_df$away_team == 
                                                                               "Washington Wizards")],
                    "home_id" =which(temp_df$home_team == "Washington Wizards"),
                    "away_id" = which(temp_df$away_team == "Washington Wizards")) 

ggplot(data = df_d2)+
  geom_line(aes(x = home_id, y = home))+
  geom_line(aes(x =away_id, y = away, color = "red"))+
  labs(color = "away",
       title = "Away and home abilities (with covariates): Wizards",
       x = "games",
       y= "ability")

##############################################################################################################################################
##############################################################################################################################################

##################################################
#adding a penalty 
##################################################

estimate_betas_vector_p <- function(data, feature_cols_home, feature_cols_away, lambda_home, lambda_away, avg_features) {
  n <- nrow(data)
  p <- length(feature_cols_home)
  
  home_matrix <- matrix(0, n, p)
  away_matrix <- matrix(0, n, p)
  
  for (i in 1:n) {
    hi <- data$home_team[i]
    ai <- data$away_team[i]
    
    home_matrix[i, ] <- compute_ewma_vector(hi, i, data, lambda_home, TRUE, avg_features, feature_cols_home, feature_cols_away)
    away_matrix[i, ] <- compute_ewma_vector(ai, i, data, lambda_away, FALSE, avg_features, feature_cols_home, feature_cols_away)
  }
  
  colnames(home_matrix) <- paste0("home_", feature_cols_home)
  colnames(away_matrix) <- paste0("away_", feature_cols_away)
  
  X <- cbind(home_matrix, away_matrix)
  y <- data$outcome
  
  # Apply LASSO (alpha = 1). Cross-validation helps select best lambda.
  cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, standardize = TRUE)
  
  # Return the cross-validated model
  return(cv_fit)
}


loglike_vector_lasso <- function(lambdas, data, feature_cols_home, feature_cols_away, avg_features) {
  lambda_home <- lambdas[1]
  lambda_away <- lambdas[2]
  
  n <- nrow(data)
  p <- length(feature_cols_home)
  
  home_matrix <- matrix(0, n, p)
  away_matrix <- matrix(0, n, p)
  
  for (i in 1:n) {
    hi <- data$home_team[i]
    ai <- data$away_team[i]
    
    home_matrix[i, ] <- compute_ewma_vector(hi, i, data, lambda_home, TRUE, avg_features, feature_cols_home, feature_cols_away)
    away_matrix[i, ] <- compute_ewma_vector(ai, i, data, lambda_away, FALSE, avg_features, feature_cols_home, feature_cols_away)
  }
  
  colnames(home_matrix) <- paste0("home_", feature_cols_home)
  colnames(away_matrix) <- paste0("away_", feature_cols_away)
  
  X <- cbind(home_matrix, away_matrix)
  y <- data$outcome
  
  # Fit model using glmnet with fixed lambda
  lambda_value <- 0.01  # or make this tunable
  model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = lambda_value, standardize = TRUE)
  
  # Get predictions
  eta <- predict(model, newx = X, type = "link")[, 1]
  p_hat <- 1 / (1 + exp(-eta))
  
  # Compute negative log-likelihood
  eps <- 1e-15  # avoid log(0)
  neg_loglik <- -sum(y * log(p_hat + eps) + (1 - y) * log(1 - p_hat + eps))
  
  # Add L1 penalty
  beta <- as.numeric(coef(model))[-1]  # drop intercept
  penalty <- lambda_value * sum(abs(beta))
  
  return(neg_loglik + penalty)
}


feature_cols_home <- c("TOV", "X3P")
feature_cols_away <- c("TOVa", "X3Pa")
avg_features <- colMeans(data[, feature_cols_home], na.rm = TRUE)

initial_lambdas <- c(0.1, 0.1)

# Optimisation
final <- optim(par = initial_lambdas, fn = loglike_vector_lasso,
               data = data, feature_cols_home = feature_cols_home,
               feature_cols_away = feature_cols_away,
               avg_features = avg_features)

# Modèle final
final_model <- estimate_betas_vector_p(data, feature_cols_home, feature_cols_away,
                                           final$par[1], final$par[2], avg_features)

# Extraction des coefficients
coefs <- coef(final_model, s = "lambda.min")
coefs <- as.numeric(coefs)[-1]  # enlever l’intercept
p <- length(feature_cols_home)
beta1 <- coefs[1:p]
beta2 <- coefs[(p + 1):(2 * p)]

lambda1 <- final$par[1]
lambda2 <- final$par[2]

all_teams <- unique(c(data$home_team, data$away_team))
team_ability_history <- setNames(vector("list", length(all_teams)), all_teams)

for (i in 1:nrow(data)) {
  home <- data$home_team[i]
  away <- data$away_team[i]
  
  home_x <- compute_ewma_vector(home, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away)
  away_x <- compute_ewma_vector(away, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away)
  
  home_ability <- sum(beta1 * home_x)
  away_ability <- sum(beta2 * away_x)
  
  team_ability_history[[home]] <- c(team_ability_history[[home]], home_ability)
  team_ability_history[[away]] <- c(team_ability_history[[away]], away_ability)
}

# Résumé des habiletés finales
final_abilities <- data.frame(
  team = all_teams,
  final_ability = sapply(all_teams, function(team) {
    mean(team_ability_history[[team]], na.rm = TRUE)
  })
)

final_abilities$final_ability <- final_abilities$final_ability - mean(final_abilities$final_ability)
print(final_abilities[order(final_abilities$final_ability, decreasing = TRUE), ])


##############################################
#trash (joint likelihood wihtout covariates)
###############################################

##########################################################################################################################################
#fixed lambdas
joint_loglikelihood_fixed_lambdas <- function(beta, precomputed_ewma, data) {
  beta1 <- beta[1]
  beta2 <- beta[2]
  
  linear_preds <- sapply(1:nrow(data), function(i) {
    x_home <- precomputed_ewma[[i]]$x_home
    x_away <- precomputed_ewma[[i]]$x_away
    sum(beta1 * x_home) - sum(beta2 * x_away)
  })
  
  y <- data$outcome
  loglik <- sum(y * linear_preds - log(1 + exp(linear_preds)))
  return(-loglik)
}

#with fixed betas
joint_loglikelihood_bfixed <- function(lambda, data) {
  lambda1 <- lambda[1]
  lambda2 <- lambda[2]
  
  linear_preds <- sapply(1:nrow(data), function(i) {
    x_home <- precomputed_ewma[[i]]$x_home
    x_away <- precomputed_ewma[[i]]$x_away
    sum(beta1 * x_home) - sum(beta2 * x_away)
  })
  
  # Convert the result to a numeric vector
  linear_preds <- unlist(linear_preds)
  y <- data$outcome
  
  # Logistic log-likelihood
  loglik <- sum(y * linear_preds - log(1 + exp(linear_preds)))
  
  return(-loglik)  # For minimization
}

lambda1 <- 0.1
lambda2 <- 0.1
#precompute ewma 
precomputed_ewma <- lapply(1:nrow(data), function(i) {
  hi <- data$home_team[i]
  ai <- data$away_team[i]
  
  x_home <- ewma(hi, i, data, lambda1, beta = 1,TRUE, avg_home)
  x_away <- ewma(ai, i, data, lambda2, beta = 1, FALSE, avg_away)
  
  list(x_home = x_home, x_away = x_away)
})

beta <- c(1,1)

for (i in 1:50) {
  
  opt_lambdas <- optim(
    par = beta,
    fn = joint_loglikelihood_fixed_lambdas,
    precomputed_ewma = precomputed_ewma,
    data = data
  )
  
  beta1 <- opt_lambdas$par[1]
  beta2 <- opt_lambdas$par[2]
  
  lambda_v <- c(lambda1i, lambda2i)
  
  opt <- optim(
    par = lambda,
    fn = joint_loglikelihood_bfixed,
    data = data
  )
  
  lambda1i <- opt$par[1]
  lambda2i <- opt$par[2]
  print(lambda1i)
  
  beta <- c(beta1,beta2)
  
  precomputed_ewma <- lapply(1:nrow(data), function(i) {
    hi <- data$home_team[i]
    ai <- data$away_team[i]
    
    x_home <- ewma(hi, i, data, lambda1, beta = 1,TRUE, avg_home)
    x_away <- ewma(ai, i, data, lambda2, beta = 1, FALSE, avg_away)
    
    list(x_home = x_home, x_away = x_away)
  })
  
}
##################################################################################################################################################

# Initialization
lambda1 <- 0.1
lambda2 <- 0.1
n_iter <- 20  # Number of alternating optimization steps
p <- length(feature_cols_home)  # number of features
beta1 <- rep(1, p)
beta2 <- rep(1, p)

for (iter in 1:n_iter) {
  cat("Iteration", iter, "\n")
  
  # Step 1: Estimate β1 and β2 with fixed lambdas
  precomputed_ewma <- lapply(1:nrow(data), function(i) {
    hi <- data$home_team[i]
    ai <- data$away_team[i]
    
    x_home <- compute_ewma_vector(hi, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away)
    x_away <- compute_ewma_vector(ai, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away)
    
    list(x_home = x_home, x_away = x_away)
  })
  
  joint_loglikelihood_fixed_ewma <- function(beta, precomputed_ewma, data) {
    beta1 <- beta[1:p]
    beta2 <- beta[(p + 1):(2*p)]
    
    linear_preds <- sapply(1:nrow(data), function(i) {
      x_home <- precomputed_ewma[[i]]$x_home
      x_away <- precomputed_ewma[[i]]$x_away
      sum(beta1 * x_home) - sum(beta2 * x_away)
    })
    
    y <- data$outcome
    loglik <- sum(y * linear_preds - log(1 + exp(linear_preds)))
    return(-loglik)
  }
  
  opt_beta <- optim(
    par = c(beta1, beta2),
    fn = joint_loglikelihood_fixed_ewma,
    precomputed_ewma = precomputed_ewma,
    data = data,
    method = "L-BFGS-B",
    hessian = TRUE
  )
  
  beta1 <- opt_beta$par[1:p]
  beta2 <- opt_beta$par[(p + 1):(2*p)]
  
  # Step 2: Estimate lambda1 and lambda2 with fixed β1 and β2
  joint_loglikelihood_lambda <- function(theta, data, feature_cols_home, feature_cols_away, avg_features) {
    lambda1 <- theta[1]
    lambda2 <- theta[2]
    
    n <- nrow(data)
    linear_preds <- mclapply(1:n, function(i) {
      hi <- data$home_team[i]
      ai <- data$away_team[i]
      
      x_home <- compute_ewma_vector(hi, i, data, lambda1, TRUE, avg_features, feature_cols_home, feature_cols_away)
      x_away <- compute_ewma_vector(ai, i, data, lambda2, FALSE, avg_features, feature_cols_home, feature_cols_away)
      
      sum(beta1 * x_home) - sum(beta2 * x_away)
    }, mc.cores = detectCores() - 1)
    
    linear_preds <- unlist(linear_preds)
    y <- data$outcome
    loglik <- sum(y * linear_preds - log(1 + exp(linear_preds)))
    return(-loglik)
  }
  
  opt_lambda <- optim(
    par = c(lambda1, lambda2),
    fn = joint_loglikelihood_lambda,
    data = data,
    feature_cols_home = feature_cols_home,
    feature_cols_away = feature_cols_away,
    avg_features = avg_features,
    method = "L-BFGS-B",
    lower = c(0.001, 0.001),  # to avoid zero
    upper = c(1, 1),
    hessian = TRUE
  )
  
  lambda1 <- opt_lambda$par[1]
  lambda2 <- opt_lambda$par[2]
  
  cat(" -> lambda1 =", round(lambda1, 4), ", lambda2 =", round(lambda2, 4), "\n")
}


