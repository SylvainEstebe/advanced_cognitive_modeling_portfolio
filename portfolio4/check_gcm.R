
library(tidyverse)
library(cmdstanr)



source("portfolio4/forward_GCM.R")

df <- df |>
  mutate(true_category = ifelse(feedback==1, decision, 1-decision))

standata <- list(
  N_trials = nrow(df),
  N_features = 5,
  N_categories = 2,
  features = unnest(df, features) |> select(f1:f5) |> as.matrix(),
  true_category = df$true_category + 1,
  decision = df$decision + 1,
  trial_start_sampling = 1 + first(which(df$true_category == lead(df$true_category)))
)



gcm_single <- cmdstan_model("portfolio4/GCM_single.stan")
s <- gcm_single$sample(data = standata,
                       iter_warmup = 100,
                       iter_sampling = 100,
                       parallel_chains = 4)
