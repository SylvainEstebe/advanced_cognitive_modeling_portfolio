

library(tidyverse)
library(brms)
library(cmdstanr)


data_patients <- read_csv("portfolio3/data/Simonsen_clean.csv")



one_participant <- data_patients |>
  filter(ID == sample(unique(ID), 1))


sim_simple_betabinomial <- function(df, lb = 1, ub = 8, invTemperature = 1) {

  N <- nrow(df)
  shape1 <- (df$FirstRating + df$GroupRating - 2 * lb)
  shape2 <- (2 * (ub - lb))
  belief <- rbeta(N, 1 + shape1 * invTemperature, 1 + (shape2 - shape1) * invTemperature)
  df$SecondRating <- lb + rbinom(N, ub - lb, belief)

  return(df)
}

sim_weighted_betabinomial <- function(df, lb = 1, ub = 8, invTemperature = 1, log_weight_delta = 0) {
  log_weight_mu <- 0

  N <- nrow(df)
  weight1 <- exp(log_weight_mu + log_weight_delta / 2)
  weight2 <- exp(log_weight_mu - log_weight_delta / 2)
  shape1 <- (df$FirstRating-lb) * weight1 + (df$GroupRating-lb) * weight2
  shape2 <- ((ub - lb) * (weight1 + weight2))
  belief <- rbeta(N, 1 + shape1 * invTemperature, 1 + (shape2 - shape1) * invTemperature)
  df$SecondRating <- lb + rbinom(N, ub - lb, belief)

  return(df)

}

## compile models
simple_betabayes <- cmdstan_model("portfolio3/betabinomial-simple-single.stan")
weighted_betabayes <- cmdstan_model("portfolio3/betabinomial-weighted-single.stan")

simple_bayes <- cmdstan_model("portfolio3/simple_Bayes_model.stan")
weighted_bayes <- cmdstan_model("portfolio3/weighted_Bayes_model.stan")

betashift <- cmdstan_model("portfolio3/betashift.stan")

#weighted_temp_betabayes <- cmdstan_model("portfolio3/betabinomial-weighted-single-temp.stan")

fit_betabayes <- function(model, data, lb = 1, ub = 8, fixed_param=FALSE, iter=100) {
    model$sample(
        data = list(
            N = nrow(data),
            lb = lb,
            ub = ub,
            FirstRating = data$FirstRating,
            GroupRating = data$GroupRating,
            SecondRating = data$SecondRating
        ),
        iter_sampling=iter,
        parallel_chains = 4,
        adapt_delta=.95,
        fixed_param=fixed_param
    )
}

fit_bayes <- function(model, data, iter=100) {
    model$sample(
        data = list(
            trials = nrow(data),
            FirstRating = data$FirstRating,
            GroupRating = data$GroupRating,
            SecondRating = data$SecondRating
        ),
        iter_sampling=iter,
        parallel_chains = 4,
        adapt_delta=.95
    )
}

fit_betashift <- function(model, data, iter=100) {
    model$sample(
        data = list(
            N = nrow(data),
            FirstRating = data$FirstRating,
            GroupRating = data$GroupRating,
            SecondRating = data$SecondRating
        ),
        iter_sampling=iter,
        parallel_chains = 4,
        adapt_delta=.95
    )
}

## ## simulate data from the model to assess whether the models can be fit.
## sim_simple <- one_participant |>
##   sim_simple_betabinomial()
## m1 <- fit_gumball(simple_betabayes, sim_simple, fixed_param=TRUE)
## m2 <- fit_gumball(weighted_betabayes, sim_simple)
## loo_compare(m1$loo(), m2$loo()) ## comparison should favor m1


## sim_weighted_1 <- one_participant |>
##   sim_weighted_betabinomial()
## m1 <- fit_gumball(simple_betabayes, sim_weighted_1, fixed_param=TRUE)
## m2 <- fit_gumball(weighted_betabayes, sim_weighted_1)
## loo_compare(m1$loo(), m2$loo()) ## comparison should favor m1 or be inconclusive


## sim_weighted_2 <- one_participant |>
##   sim_weighted_betabinomial(log_weight_delta = 1)
## m1 <- fit_gumball(simple_betabayes, sim_weighted_2, fixed_param=TRUE)
## m2 <- fit_gumball(weighted_betabayes, sim_weighted_2)
## loo_compare(m1$loo(), m2$loo()) ## comparison should favor m2



## sim_weighted_3 <- one_participant |>
##   sim_weighted_betabinomial(log_weight_delta = 2)
## m1 <- fit_gumball(simple_betabayes, sim_weighted_3, fixed_param=TRUE)
## m2 <- fit_gumball(weighted_betabayes, sim_weighted_3)
## loo_compare(m1$loo(), m2$loo()) ## comparison should favor m2



model_names <- c(
  "Beta-Binomial (Simple)",
  "Beta-Binomial (Weighted)",
  "Add-logits (Simple)",
  "Add-logits (weighted)",
  "Beta Shift"
)

results <- data_patients |>
  filter(ID %in% c("201", "203")) |>
  group_by(ID) |>
  group_modify(function(data, participant) {
    m1 <- fit_betabayes(simple_betabayes, data, fixed_param = TRUE)
    m2 <- fit_betabayes(weighted_betabayes, data)
    m3 <- fit_bayes(simple_bayes, data)
    m4 <- fit_bayes(weighted_bayes, data)
    m5 <- fit_betashift(betashift, data)
    models <- list(m1, m2, m3, m4, m5)
    loo_result <- map(models, \(x) x$loo()) |>
      loo_compare() |>
      as_tibble(rownames = NA) |>
      rownames_to_column() |>
      rename(model = rowname) |>
      arrange(model)
    y_rep <- map(models, \(x) as_draws_df(x$draws("y_rep")))
    distinct(data, Condition) |>
      mutate(loo = list(loo_result),
             y_rep = list(y_rep))
  }) |>
  ungroup()

## model comparison result, by ID
## unnest(x, loo) |>
##   mutate(ymin = elpd_diff - 2 * se_diff,
##          ymax = elpd_diff + 2 * se_diff) |>
##   mutate(top_model = model[])
##   ggplot(aes(ID, elpd_diff, ymin=ymin, ymax=ymax, color=model)) +
##   geom_pointrange()


unnest(results, loo) |>
  filter(elpd_diff == 0) |>
  ggplot(aes(model)) +
  geom_histogram(stat="count")


## posterior predictive check
y <- data_patients |>
  filter(ID %in% c("201", "203")) |>
  select(ID, Condition, FaceID, SecondRating) |>
  mutate(trial = FaceID + 1)

yrep <- unnest(results, c(loo, y_rep)) |>
  select(-c(elpd_diff, se_diff, elpd_loo, se_elpd_loo, p_loo, se_p_loo, looic, se_looic)) |>
  mutate(yrep = map(y_rep, \(x) spread_draws(x, y_rep[trial], ndraws=5))) |>
  select(-y_rep) |>
  unnest(yrep)

ggplot(y) +
  geom_freqpoly(aes(y_rep, after_stat(density), group = str_c(.draw, model), color = model), data = yrep, binwidth=1, alpha=.5) +
  geom_freqpoly(aes(SecondRating, after_stat(density)), binwidth=1) +
  scale_color_discrete(labels = model_names) +
  scale_x_continuous(breaks = seq(0,8)) +
  facet_wrap(~ID)
