
# Produce a text (+ plots) document (linked to a github repo) in which you:
# Describe the model you are working on : Reinforcement Learning model
# A commented version of the stan model :
# Discuss model quality of the model (predictive prior and posterior checks, prior-posterior update checks, etc.)
# Describe a process of parameter recovery (why are you doing it?, how are you doing it?)
# Discuss the results: can you recover the parameter values? How many trials should be used at least to properly recover the parameters? What’s the role of priors? Add relevant plot(s)
###
###


pacman::p_load(tidyverse,
               here,
               posterior,
               cmdstanr,
               brms, tidybayes)

file <- file.path("~/Code/advanced_cognitive_modeling/portfolio2/stan_RL.stan")
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
data <- read_csv("Code/advanced_cognitive_modeling/simdata/rl_sim.csv")
data$feedback
trials = 30

data <- list(
  trials = trials,
  choice_RL = data$Agent1_choice,
  choice_Random = data$Agent2_choice,
  feedback = data$feedback
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

samples$summary()