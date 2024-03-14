

library(tidyverse)
library(cmdstanr)
library(shinystan)
library(posterior)
library(boot)
library(bayesplot)
library(ggdist)
set_cmdstan_path("~/.cmdstan/cmdstan-2.34.1")

# diagnostic plots for a model fitted on data from a simulated matching pennies game with an RL agent

source("Asignment/portfolio2/RL_sim.R")
sim_data <- do_sim(trials = 100, # Number of times the game is played
                   rate = 0.8, # Bias of the random agent
                   alpha = 0.4, # Learning rate
                   bias = 0, # Bias of the RL to choose right before any games are played
                   tau = 1 # Inverse temperature
                   )

# Loading in the model
m1 <- cmdstan_model("Asignment/portfolio2/stan_RL.stan")


fit <- m1$sample(data = list(trials = nrow(sim_data),
                             choice_RL = sim_data$Agent1_choice+1,
                             prior_only = 0,
                             feedback = sim_data$feedback), parallel_chains = 4)

fit_prior <- m1$sample(data = list(trials = nrow(sim_data),
                             choice_RL = sim_data$Agent1_choice+1,
                             prior_only = 1,
                             feedback = sim_data$feedback), parallel_chains = 4)

# Traceplots
### caterpillar
mcmc_trace(fit$draws(c("alpha", "logInvTemperature")))

## pairs
mcmc_pairs(fit$draws(c("alpha", "logInvTemperature")))

## parameter estimates
mcmc_intervals(fit$draws(c("alpha", "invTemperature")))


## Prior and posterior predictive checks
# Save model results without learning from the data (prior)
yrep_prior <- as_draws_df(fit_prior) %>% 
  select(!c("lp__", "alpha", "logInvTemperature", "invTemperature", "predError"))  %>% 
  pivot_longer(starts_with("value")) %>%
  mutate(trial = parse_number(str_split_i(name, ",", 2)),
         choice = parse_number(str_split_i(name, ",", 1))
         ) %>%
  select(-name) %>%
  group_by(trial, .chain, .iteration, .draw) %>%
  pivot_wider(names_from = choice, values_from = value, names_prefix = "choice_") %>%
  mutate(exp_1 = exp(choice_1 * 1),
         exp_2  = exp(choice_2 * 1),
         softmax_2 = exp_2 / (exp_1 + exp_2)) %>%
  mutate(source = "prior")


# Save model result fitted to the data (posterior)
yrep_posterior <- as_draws_df(fit) %>%
  select(!c("lp__", "alpha", "logInvTemperature", "invTemperature", "predError")) %>%
  pivot_longer(starts_with("value")) %>%
  mutate(trial = parse_number(str_split_i(name, ",", 2)),
         choice = parse_number(str_split_i(name, ",", 1))
         ) %>%
  select(-name) %>%
  group_by(trial, .chain, .iteration, .draw) %>%
  pivot_wider(names_from = choice, values_from = value, names_prefix = "choice_") %>%
  mutate(exp_1 = exp(choice_1 * 1),
         exp_2  = exp(choice_2 * 1),
         softmax_2 = exp_2 / (exp_1 + exp_2))%>%
  mutate(source = "posterior")


# Prior- and posterior predictive checks
# --- This plot shows the distributions of predictions of the probablity of picking the right hand for both the prior and posterior, respectively
bind_rows(yrep_prior, yrep_posterior) %>%
  filter(trial <= 15) %>%
  group_by(trial) %>%
  ungroup() %>%
  ggplot(aes(color = source)) +
  geom_density(aes(softmax_2, after_stat(scaled))) +
  facet_wrap(~trial) +
  geom_vline(aes(xintercept = exp(right_v2)/(exp(right_v2)+exp(left_v1))), data=filter(sim_data, trial <= 15))+ # We calculate the probability of picking right from the true values in the data
  theme_minimal()+
  ggtitle("Prior- and posterior predictive checks")

### Posterior predictive check
yrep_posterior %>%
  group_by(trial) %>%
  summarise(mean_qi(softmax_2)) %>%
  ggplot(aes(trial, y)) +
  geom_line(aes(y = exp(right_v2)/(exp(right_v2)+exp(left_v1))), data=sim_data) +
  geom_line(aes(y=y), color = "red")

### Prior predictive check
yrep_prior%>%
  group_by(trial) %>%
  summarise(mean_qi(softmax_2)) %>%
  ggplot(aes(trial, y)) +
  geom_line(aes(y = exp(right_v2)/(exp(right_v2)+exp(left_v1))), data=sim_data) +
  geom_line(aes(y=y), color = "red")


## prior-posterior update plot

### Extract parameter estimates (prior)
prior = as_draws_df(fit_prior) %>%
  select(alpha, logInvTemperature) %>%
  pivot_longer(everything()) %>%
  mutate(source = "prior")

### Extract parameter estimates (posterior)
posterior = as_draws_df(fit) %>%
  select(alpha, logInvTemperature) %>%
  pivot_longer(everything()) %>%
  mutate(source = "posterior")

### Plot the posterior against the prior for both alpha and logInvTemperature 
bind_rows(prior,posterior) %>%
ggplot() +
  geom_density(aes(value, color = source)) +
  facet_wrap(~name, scales="free") +
  theme_minimal() +
  labs(title = "Prior-Posterior update plot")
