

library(tidyverse)
library(brms)
library(cmdstanr)


data_patients <- read_csv("portfolio3/data/Simonsen_clean.csv")



one_participant <- data_patients |>
  filter(ID == sample(unique(ID), 1))


simple_beta_bayes <- function(df, lb = 1, ub = 8, invTemperature = 1) {

  N <- nrow(df)
  shape1 <- invTemperature * (df$FirstRating + df$GroupRating - 2 * lb)
  shape2 <- invTemperature * (2 * (ub - lb))
  belief <- rbeta(N, 1 + shape1 * invTemperature, 1 + (shape2 - shape1) * invTemperature)
  df$SecondRating <- lb + rbinom(N, ub - lb, belief)

  return(df)
}

## compile models
simple_betabayes <- cmdstan_model("portfolio3/betabinomial-simple-single.stan")
weighted_betabayes <- cmdstan_model("portfolio3/betabinomial-weighted-single.stan")

fit_gumball <- function(model, data, lb = 1, ub = 8) {
    model$sample(
        data = list(
            N = nrow(data),
            lb = lb,
            ub = ub,
            FirstRating = data$FirstRating,
            GroupRating = data$GroupRating,
            SecondRating = data$SecondRating
        ),
        parallel_chains = 4
    )
}

## simulate data from the model to assess whether the models can be fit.
sim_simple <- one_participant |>
  simple_beta_bayes()

m1 <- fit_gumball(simple_betabayes, sim_simple)
m2 <- fit_gumball(weighted_betabayes, sim_simple)

loo_compare(m1$loo(), m2$loo())

## simple_beta_bayes(one_participant, invTemperature = 0.1) |>
##   mutate(error = SecondRating_predicted - SecondRating) |>
##   ggplot() +
##   geom_point(aes(FaceID, error))

simple_betabayes <- cmdstan_model("portfolio3/betabinomial-simple-single.stan")

fit <- simple_betabayes$sample(data = list(N = nrow(one_participant),
                                           lb = 1,
                                           ub = 8,
                                           FirstRating = one_participant$FirstRating,
                                           GroupRating = one_participant$GroupRating,
                                           SecondRating = one_participant$SecondRating),
                               parallel_chains = 4)


fit2 <- weighted_betabayes$sample(data = list(N = nrow(one_participant),
                                           lb = 1,
                                           ub = 8,
                                           FirstRating = one_participant$FirstRating,
                                           GroupRating = one_participant$GroupRating,
                                           SecondRating = one_participant$SecondRating),
                               parallel_chains = 4)

loo_compare(fit$loo(), fit2$loo())
