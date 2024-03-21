

library(tidyverse)
library(brms)
library(cmdstanr)


data_patients <- read_csv("portfolio3/data/Simonsen_clean.csv")



one_participant <- data_patients |>
  filter(ID == sample(unique(ID), 1))


simple_beta_bayes <- function(df, lb = 1, ub = 8, invTemperature = 1) {

  N <- nrow(df)
  shape1 <- invTemperature * (df$FirstRating + df$GroupRating - 2 * lb)
  shape2 <- invTemperature * (2 * (ub - lb)) - shape1
  belief <- rbeta(N, shape1, shape2)
  df$SecondRating_predicted <- lb + rbinom(N, ub - lb, belief)

  return(df)
}


## simple_beta_bayes(one_participant, invTemperature = 0.1) |>
##   mutate(error = SecondRating_predicted - SecondRating) |>
##   ggplot() +
##   geom_point(aes(FaceID, error))

simple_betabayes <- cmdstan_model("portfolio3/betabayes.stan")
fit <- simple_betabayes$sample(data = list(N = nrow(one_participant),
                                           lb = 1,
                                           ub = 8,
                                           FirstRating = one_participant$FirstRating,
                                           GroupRating = one_participant$GroupRating,
                                           SecondRating = one_participant$SecondRating))
