

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

#weighted_temp_betabayes <- cmdstan_model("portfolio3/betabinomial-weighted-single-temp.stan")

fit_betabayes <- function(model, data, lb = 1, ub = 8, fixed_param=FALSE) {
    model$sample(
        data = list(
            N = nrow(data),
            lb = lb,
            ub = ub,
            FirstRating = data$FirstRating,
            GroupRating = data$GroupRating,
            SecondRating = data$SecondRating
        ),
        parallel_chains = 4,
        adapt_delta=.95,
        fixed_param=fixed_param
    )
}

fit_bayes <- function(model, data) {
    model$sample(
        data = list(
            trials = nrow(data),
            FirstRating = data$FirstRating,
            GroupRating = data$GroupRating,
            SecondRating = data$SecondRating
        ),
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




x <- data_patients |>
  head(200) |>
  group_by(ID) |>
  group_modify(function(data, participant) {
    m1 <- fit_betabayes(simple_betabayes, data, fixed_param = TRUE)
    m2 <- fit_betabayes(weighted_betabayes, data)
    m3 <- fit_bayes(simple_bayes, data)
    m4 <- fit_bayes(weighted_bayes, data)
    distinct(data, Condition) |>
      mutate(loo   = list(rownames_to_column(as_tibble(loo_compare(m1$loo(), m2$loo(), m3$loo(), m4$loo()), rownames = NA))),
             y_rep = list(as_draws_df(m1$draws("y_rep"))))
  })

## model comparison result, by ID
unnest(x, loo) |>
  mutate(ymin = elpd_diff - 2 * se_diff,
         ymax = elpd_diff + 2  *se_diff) |>
  mutate(top_model = rowname[])
  ggplot(aes(ID, elpd_diff, ymin=ymin, ymax=ymax, color=rowname)) +
  geom_pointrange()


unnest(x, loo) |>
  filter(elpd_diff == 0) |>
  ggplot(aes(rowname)) +
  geom_histogram(stat="count")


## posterior predictive check
y <- head(data_patients, 200)$SecondRating

yrep <- unnest(x, y_rep) |>
  ungroup() |>
  select(-ID, -Condition, yrep-loo)

yrep_groups <- unnest(x, y_rep) |>
  ungroup() |>
  select(ID)


ppc_dens_overlay_grouped(y, as.matrix(yrep), yrep_groups)



## old code
posterior <- m2$draws(variable = c("log_weight_mu", "log_weight_delta"))

mcmc_trace(posterior)
mcmc_pairs(posterior)


y <- one_participant$SecondRating
yrep <- as_draws(m3, variable = "yrep", format="matrix")[1:50,]
ppc_dens_overlay(y, yrep)


### try the whole dataset with no pooling
## m1 <- fit_gumball(simple_betabayes, data_patients, fixed_param=TRUE)
## m2 <- fit_gumball(weighted_betabayes, data_patients)
## ## m3 <- fit_gumball(weighted_temp_betabayes, data_patients)

## l1 <- m1$loo()
## l2 <- m2$loo()
## ## l3 <- m3$loo()

## loo_compare(l1, l2, l3)

## y <- data_patients$SecondRating
## yrep <- as_draws(m2, variable = "yrep", format="matrix")[1:10,]
## ppc_dens_overlay(y, yrep)
