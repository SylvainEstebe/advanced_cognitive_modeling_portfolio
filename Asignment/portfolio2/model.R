pacman::p_load(tidyverse,
               here,
               posterior,
               cmdstanr,
               brms, tidybayes)


# Loading model
file <- file.path("~/Code/advanced_cognitive_modeling/portfolio2/stan_RL.stan")
# Loading simulation
source("~/Code/advanced_cognitive_modeling/portfolio2/rl_agent.R")
# Create combinatory dataframe of each parameters
param <- expand_grid(tau = exp(linspace(-1, 1, 5)) , alpha = seq(0,1,0.1))

# Run simulation for each parameter combination and save them in a dataframe
data_sim <- NULL
for (i in 1:nrow(param)) {
  data_sim <- rbind(data_sim,do_sim(trials = 100, rate = 0.8, alpha = param$alpha[i], tau = param$tau[i]))
}
# Load stan model
mod <- cmdstan_model(file)

# function which take simulation data as an argument and return an estimation of parameters
model <- function(data_sim){

  data <- list(
    trials = nrow(data_sim),
  choice_RL = data_sim$Agent1_choice+1,
  feedback = data_sim$feedback,
  prior_only = 0,
  initValue = 0,parallel_chains = 4
)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

draws_df <- as_draws_df(samples$draws()) 
return(draws_df)
}

# Dataframe which save true value and estimated value of each parameters (alpha & tau)
results <- tibble('true_alpha' = rep(NA,nrow(param)),
                  'true_tau' = rep(NA,nrow(param)), 
                  'estimated_alpha' = rep(NA,nrow(param)),
                  'estimated_tau' = rep(NA,nrow(param)))
        
# Run simulation for each parameter combination
recovery_df <- NULL

for (i in 1:nrow(param)){
  
data <- read_csv("~/Code/advanced_cognitive_modeling/simdata/rl_sim.csv")
  
  dd <- data_sim %>% subset(
    alpha == param$alpha[i] & tau == param$tau[i]
  )
  draws_df <- model(dd)
  
  results$true_alpha[i] <- param$alpha[i]
  results$true_tau[i] <- log(param$tau[i])
  results$estimated_alpha[i] <- mean(draws_df$alpha)
  results$estimated_tau[i] <- mean(draws_df$logInvTemperature)
  
  temp <- tibble(estimated_alpha = draws_df$alpha, 
                 true_alpha = param$alpha[i], tau = log(param$tau[i]),estimated_tau = draws_df$logInvTemperature, 
                 true_tau = log(param$tau[i]), alpha = param$alpha[i])
  
  
  if (exists("recovery_df")) {recovery_df <- rbind(recovery_df, temp)} else {recovery_df <- temp}
  
}

# Plot 
ggplot(results, aes(true_alpha, estimated_alpha)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Added label line
  facet_wrap(.~true_tau,scales="free_y")+
  theme_minimal() +
  scale_colour_viridis_c() +
  theme(panel.grid.minor = element_blank())+
  ylim(0,1) + 
labs(title = "True alpha against estimated alpha for each Tau (500 trials recovery)",  
       x = "True Alpha",  # Added label for x-axis
       y = "Estimated Alpha",  # Added label for y-axis)
)

ggplot(recovery_df, aes(true_alpha, estimated_alpha)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Added label line
  facet_wrap(.~true_tau,scales="free_y") +
  theme_classic() +
  ylim(0,10) +
labs(title = "True alpha against estimated alpha for each Tau (500 trials recovery)",  
     x = "True Alpha",  # Added label for x-axis
     y = "Estimated Alpha",  # Added label for y-axis)
)


ggplot(recovery_df, aes(true_tau, estimated_tau)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Added label line
  facet_wrap(.~true_alpha,scales="free_y") +
  theme_classic() +
  labs(title = "True log(tau) against estimated log(tau) for each alpha (500 trials recovery)",  # Added label for plot title
       x = "True log(tau)",  # Added label for x-axis
       y = "Estimated log(tau)",  # Added label for y-axis)
  )

ggplot(results, aes(true_tau, estimated_tau),scale = "free_y") +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Added label line
  facet_wrap(.~true_alpha, scales="free_y")+
  theme_minimal() +
  scale_colour_viridis_c() +
  theme(panel.grid.minor = element_blank())+
  ylim(-10,10) +   labs(title = "True log(tau) against estimated log(tau) for each alpha (500 trials recovery)",  # Added label for plot title
       x = "True log(tau)",  # Added label for x-axis
       y = "Estimated log(tau)",  # Added label for y-axis)
  )
