
library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(glue)


## n_trial = 32 * 5

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
  weight_prior_precision = 1,
  prior_only = 0
)



gcm_single <- cmdstan_model("portfolio4/GCM_single.stan")

s <- gcm_single$sample(data = standata,
                       iter_warmup = 1000,
                       iter_sampling = 2000,
                       parallel_chains = 4)


## write_rds(s, glue("portfolio4/gcm_{n_trial}_samples.rds"))
## s <- read_rds("portfolio4/gcm_160_samples.rds")

pairs <- mcmc_pairs(s$draws(c("scaling", "weights")))
ggsave(glue("portfolio4/gcm_{n_trial}_pairplot.png"), plot = pairs)

mcmc_trace(s$draws(c("scaling", "weights")))
ggsave(glue("portfolio4/gcm_{n_trial}_traceplot.png"))


## prior_cauchy <- tibble(x = seq(0, 20, by = 0.01),
##                        y = dcauchy(x, 0, 2),
##                        source = "prior")
## s$draws("scaling", format = "df") |>
##   mutate(source = "posterior") |>
##   ggplot() +
##   geom_density(aes(scaling, color = source)) +
##   geom_line(aes(x=x,y=y, color=source), data = prior_cauchy) +
##   theme_minimal()


s$draws(c("scaling", "scaling_prior"), format = "df") |>
  pivot_longer(starts_with("scaling")) |>
  mutate(name = ifelse(name == "scaling", "posterior", "prior")) |>
  #filter(value < 60) |>
  ggplot() +
  geom_density(aes(value, color = name), n = 2**11) +
  coord_cartesian(xlim = c(0, 50)) +
  labs(title = glue("Prior-Posterior update plot for scaling, n={n_trial}"))
ggsave(glue("portfolio4/gcm_{n_trial}_priorposterior_scaling.png"))


s$draws(c("weights", "weights_prior"), format = "df") |>
  pivot_longer(starts_with("weights"), names_pattern = "([a-zA-Z_]+)\\[(.+)\\]", names_to = c("source", "feature")) |>
  mutate(source = ifelse(source == "weights", "posterior", "prior")) |>
  ggplot() +
  geom_density(aes(value, color = source)) +
  facet_grid(~feature, labeller = label_both) +
  theme_minimal() +
  labs(title = glue("Prior-Posterior update plot for weights, n={n_trial}"))
ggsave(glue("portfolio4/gcm_{n_trial}_priorposterior_weights.png"))


#### attempt at posterior predictive

s$draws("yrep", format = "matrix") %>%
  ppc_stat(y = df$decision+1,
                   stat = "mean", binwidth = 0.03) +
  labs(title = glue("Posterior predictive (average decision), n={n_trial}"))
ggsave(glue("portfolio4/gcm_{n_trial}_posterior_predictive.png"))


s$draws("yrep", format = "matrix") %>%
  ppc_stat_grouped(y = df$decision+1,
                   group = df$true_category,
                   stat = "mean",
                   binwidth=0.06,
                   facet_args = list(labeller=as_labeller(\(x) str_c("True category: ", as.numeric(x)+1)))) +
  labs(title = glue("Posterior predictive (average decision), n={n_trial}"))
ggsave(glue("portfolio4/gcm_{n_trial}_posterior_predictive_facet.png"))

### prior predictive
standata_2 <- standata
standata_2$prior_only <- 1
prior_s <- gcm_single$sample(data = standata_2,
                       iter_warmup = 1000,
                       iter_sampling = 2000,
                       parallel_chains = 4)


prior_s$draws("yrep", format = "matrix") %>%
  ppc_stat_grouped(y = df$decision+1,
                   group = df$true_category,
                   stat = "mean",
                   binwidth=0.06,
                   facet_args = list(labeller=as_labeller(\(x) str_c("True category: ", as.numeric(x)+1)))) +
  labs(title = glue("Prior predictive (average decision), n={n_trial}"))
ggsave(glue("portfolio4/gcm_{n_trial}_prior_predictive_facet.png"))
