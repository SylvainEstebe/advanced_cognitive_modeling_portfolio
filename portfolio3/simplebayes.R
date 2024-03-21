

library(tidyverse)
library(brms)
library(cmdstanr)


data_patients <- read_csv("portfolio3/data/Simonsen_clean.csv")



one_participant <- data_patients |>
  filter(ID == sample(unique(ID), 1))


simple_beta_bayes <- function(df, lb = 1, ub = 8, invTemperature = 1) {

  df$belief <- NA

  for (i in seq_len(nrow(df))) {
    shape1 <- invTemperature * (pluck(df, "FirstRating", i) + pluck(df, "GroupRating", i) - 2 * lb)
    shape2 <- invTemperature * (2 * (ub-lb)) - shape1
    print(c(i, shape1, shape2))
    df[i, "belief"] <- rbeta(1, shape1, shape2)
    df[i, "SecondRating_predicted"] <- rbinom(1, ub - lb, pluck(df, "belief", i)) + lb
  }
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
