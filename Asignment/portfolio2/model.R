
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
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE),
                     stanc_options = list("O1"), pedantic = TRUE)
data <- read_csv("~/Code/advanced_cognitive_modeling/simdata/rl_sim.csv")
data$feedback
trials = 30

data <- list(
  trials = trials,
  choice_RL = data$Agent1_choice+1,
  feedback = data$feedback,
  initValue = c(0, 2)
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

samples$save_object("~/Code/advanced_cognitive_modeling/portfolio2/rl.rds")
samples$cmdstan_diagnose()
samples$summary()

draws_df <- as_draws_df(samples$draws()) 
ggplot(draws_df, aes(.iteration, alpha, group = .chain, color = .chain)) +
  geom_line() +
  theme_classic()

ggplot(draws_df, aes(.iteration, logInvTemperature, group = .chain, color = .chain)) +
  geom_line() +
  theme_classic()


ggplot(draws_df) +
  geom_density(aes(alpha), fill = "blue", alpha = 0.3) +
  geom_density(aes(alpha_prior), fill = "red", alpha = 0.3) +
  xlab("Learning Rate") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(logInvTemperature), fill = "blue", alpha = 0.3) +
  geom_density(aes(temperature_prior), fill = "red", alpha = 0.3) +
  xlab("(inverse) temperature") +
  ylab("Posterior Density") +
  theme_classic()
    
