

library(tidyverse)
library(cmdstanr)
library(shinystan)
library(posterior)
set_cmdstan_path("~/.cmdstan/cmdstan-2.34.0")


# diagnostic plots
## example_data <- list(
##   trials = 10,
##   initial_value = 0.5,
##   choice = sample(c(0, 1), 10, replace = TRUE),
##   feedback = sample(c(-1, 1), 10, replace = TRUE)
## )

source("Asignment/portfolio2/RL_sim.R")
sim_data <- do_sim(trials = 20, rate = .9, alpha = 0.1)

## m1 <- cmdstan_model("Asignment/portfolio2/RL_single.stan")
m1 <- cmdstan_model("Asignment/portfolio2/stan_RL.stan")

fit <- m1$sample(data = list(trials = nrow(sim_data),
                                 ## initial_value = 0.5,
                                 choice_RL = sim_data$Agent1_choice+1,
                                 feedback = (sim_data$feedback*2)-1), parallel_chains = 4)

## posterior predictive

yrep <- as_draws_df(fit) |>
  select(starts_with("value[2")) |>
  pivot_longer(everything()) |>
  mutate(trial = parse_number(str_split_i(name, ",", 2)),
         value = inv.logit(value))

yrep |>
  group_by(trial) |>
  ## sample_n(100) |>
  ggplot() +
  geom_density(aes(value, after_stat(scaled))) +
  #scale_x_continuous(limits=c(0,1)) +
  facet_wrap(~trial) +
  geom_vline(aes(xintercept = right_v2), data=sim_data)
