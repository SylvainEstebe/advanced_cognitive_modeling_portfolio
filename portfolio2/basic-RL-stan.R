
library(tidyverse)
library(cmdstanr)
set_cmdstan_path("~/.cmdstan/cmdstan-2.34.0")


example_data <- list(
  trials = 10,
  initial_value = 0.5,
  choice = sample(c(0,1), 10, replace = TRUE),
  feedback = sample(c(-1,1), 10, replace = TRUE)
)

m1 <- cmdstan_model("portfolio2/RL_single.stan")
m1$sample(data = example_data)



m2 <- cmdstan_model("portfolio2/WSLS_single.stan")

X <- tibble(choice = example_data$choice,
            feedback = example_data$feedback) |>
  mutate(intercept = 1,
         winstay = case_when(choice & feedback ~ 1,
                             !choice & feedback ~ -1,
                             .default = 0),
         loseshift = case_when(choice & !feedback ~ -1,
                               !choice & !feedback ~ 1,
                               .default = 0)) |>
  select(-choice, -feedback) |>
  as.matrix()

m2$sample(data = c(example_data, list(X=X, k = ncol(X))))



# multilevel
example_data <- list(
  trials = 20,
  initial_value = 0.5,
  choice = sample(c(0,1), 20, replace = TRUE),
  feedback = sample(c(-1,1), 20, replace = TRUE),
  id = rep(1:2, each=10),
  k = 2,
  trial = rep(1:10, times = 2)
)

m3 <- cmdstan_model("portfolio2/RL_multi.stan")
samples <- m3$sample(data = c(as.list(d), trials = nrow(d), initial_value = 0.5,
                              k = length(unique(d$id))), parallel_chains = 4)
launch_shinystan(samples)
