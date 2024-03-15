

library(tidyverse)
library(cmdstanr)
library(shinystan)
library(posterior)
library(boot)
library(bayesplot)
library(ggdist)
set_cmdstan_path("~/.cmdstan/cmdstan-2.34.0")


# diagnostic plots
## example_data <- list(
##   trials = 10,
##   initial_value = 0.5,
##   choice = sample(c(0, 1), 10, replace = TRUE),
##   feedback = sample(c(-1, 1), 10, replace = TRUE)
## )

source("portfolio2/RL_sim.R")
sim_data <- do_sim(trials = 100, rate = 0.8, alpha = 0.4)

## m1 <- cmdstan_model("portfolio2/RL_single.stan")
m1 <- cmdstan_model("portfolio2/stan_RL.stan")

fit <- m1$sample(data = list(trials = nrow(sim_data),
                                 ## initial_value = 0.5,
                             choice_RL = sim_data$Agent1_choice+1,
                             prior_only = 0,
                             feedback = sim_data$feedback), parallel_chains = 4)

fit_prior <- m1$sample(data = list(trials = nrow(sim_data),
                                 ## initial_value = 0.5,
                             choice_RL = sim_data$Agent1_choice+1,
                             prior_only = 1,
                             feedback = sim_data$feedback), parallel_chains = 4)




### catarpillar
mcmc_trace(fit$draws(c("alpha", "logInvTemperature")))

## pairs
mcmc_pairs(fit$draws(c("alpha", "logInvTemperature")))

## parameter estimates
mcmc_intervals(fit$draws(c("alpha", "invTemperature")))


## posterior predictive

## TEMPERATURE=1

yrep_prior <- as_draws_df(fit_prior) |>
  select(!c("lp__", "alpha", "logInvTemperature", "predError")) |>
  pivot_longer(starts_with("value")) |>
  mutate(trial = parse_number(str_split_i(name, ",", 2)),
         choice = parse_number(str_split_i(name, ",", 1))#,
         #value = inv.logit(value)
         ) |>
  select(-name) |>
  group_by(trial, .chain, .iteration, .draw) |>
  pivot_wider(names_from = choice, values_from = value, names_prefix = "choice_") |>
  mutate(exp_1 = exp(choice_1 * invTemperature),
         exp_2  = exp(choice_2 * invTemperature),
         softmax_2 = exp_2 / (exp_1 + exp_2)) |>
  mutate(source = "prior")



yrep_posterior <- as_draws_df(fit) |>
  select(!c("lp__", "alpha", "logInvTemperature", "predError")) |>
  pivot_longer(starts_with("value")) |>
  mutate(trial = parse_number(str_split_i(name, ",", 2)),
         choice = parse_number(str_split_i(name, ",", 1))#,
         #value = inv.logit(value)
         ) |>
  select(-name) |>
  group_by(trial, .chain, .iteration, .draw) |>
  pivot_wider(names_from = choice, values_from = value, names_prefix = "choice_") |>
  mutate(exp_1 = exp(choice_1 * invTemperature),
         exp_2  = exp(choice_2 * invTemperature),
         softmax_2 = exp_2 / (exp_1 + exp_2))|>
  mutate(source = "posterior")


bind_rows(yrep_prior, yrep_posterior) |>
  ## yrep_posterior |>
  filter(trial <= 16) |>
  ## mutate(label = str_c("trial ", trial, ", feedback "))
  ## group_by(trial) |>
  ## #sample_n(10) |>
  ## ungroup() |>
  ggplot(aes(color = source)) +
  #geom_density(aes(softmax_2, after_stat(scaled))) +
  geom_histogram(aes(softmax_2, after_stat(ncount)), fill = NA, position="identity", binwidth=0.025) +
  scale_x_continuous(limits=c(0,1), labels = scales::percent_format()) +
  facet_wrap(~trial, labeller=label_both) +
  geom_vline(aes(xintercept = exp(right_v2)/(exp(right_v2)+exp(left_v1))), data=filter(sim_data, trial <= 16)) +
  ggthemes::theme_tufte() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  labs(x = "Probability of chosing option 2",
       title = "Predictive check", y = "")
ggsave("portfolio2/predictive_check.png")



yrep_posterior |>
  group_by(trial) |>
  summarise(mean_qi(choice_2)) |>
  ggplot(aes(trial, y)) +
  #scale_y_continuous(limits=c(0,1)) +
  #geom_line(aes(y=right_v2), color = "blue", data=mutate(sim_data, y = exp(right_v2)/(exp(left_v1)+exp(right_v2))))
  ## geom_line()
  ## geom_density(aes(value, after_stat(scaled))) +
  #scale_y_continuous(limits=c(0.5,1)) +
  ## facet_wrap(~trial) +
  #geom_line(aes(y = exp(right_v2)/(exp(right_v2)+exp(left_v1))), data=sim_data) +
  geom_line(aes(y=y), color = "red")


### prior predictive check



### prior posteriuor update plot

prior = as_draws_df(fit_prior) |>
  select(alpha, logInvTemperature) |>
  pivot_longer(everything()) |>
  mutate(source = "prior")

posterior = as_draws_df(fit) |>
  select(alpha, logInvTemperature) |>
  pivot_longer(everything()) |>
  mutate(source = "posterior")

bind_rows(prior,posterior) |>
ggplot() +
  geom_density(aes(value, color = source)) +
  facet_wrap(~name, scales="free") +
  theme_minimal() +
  labs(title = "Prior-Posterior update plot")
