

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

## simulate data from the model to assess whether the models can be fit.
sim_simple <- one_participant |>
  simple_beta_bayes()


## compile models
simple_betabayes <- cmdstan_model("portfolio3/betabinomial-simple-single.stan")

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

m1 <- fit_gumball(simple_betabayes, sim_simple)

## simple_beta_bayes(one_participant, invTemperature = 0.1) |>
##   mutate(error = SecondRating_predicted - SecondRating) |>
##   ggplot() +
##   geom_point(aes(FaceID, error))

