
library(tidyverse)
library(cmdstanr)
library(bayesplot)



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
  trial_start_sampling = 1 + first(which(df$true_category != lag(df$true_category))),
  weight_prior_precision = 1
)



gcm_single <- cmdstan_model("portfolio4/GCM_single.stan")

s <- gcm_single$sample(data = standata,
                       iter_warmup = 500,
                       iter_sampling = 500,
                       parallel_chains = 4)



mcmc_pairs(s$draws(c("scaling", "weights")))



prior_cauchy <- tibble(x = seq(0, 20, by = 0.01),
                       y = dcauchy(x, 0, 2),
                       source = "prior")
s$draws("scaling", format = "df") |>
  mutate(source = "posterior") |>
  ggplot() +
  geom_density(aes(scaling, color = source)) +
  geom_line(aes(x=x,y=y, color=source), data = prior_cauchy) +
  theme_minimal()


s$draws(c("weights", "weights_prior"), format = "df") |>
  pivot_longer(starts_with("scaling")) |>
  mutate(name = ifelse(name == "scaling", "posterior", "prior")) |>
  ggplot() +
  geom_density(aes(value, color = name), n = 2**11) +
  coord_cartesian(xlim = c(0, 100))


s$draws(c("weights", "weights_prior"), format = "df") |>
  pivot_longer(starts_with("weights"), names_pattern = "([a-zA-Z_]+)\\[(.+)\\]", names_to = c("source", "feature")) |>
  mutate(source = ifelse(source == "weights", "posterior", "prior")) |>
  ggplot() +
  geom_density(aes(value, color = source)) +
  facet_wrap(~feature)
