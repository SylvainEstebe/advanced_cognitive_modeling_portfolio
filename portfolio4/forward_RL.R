
library(tidyverse)


n_trial <- 32

n_features <- 5
## features <- c(1,1,0,0,1)
alpha <- 0.2

df <- tibble(
  trial = 1:n_trial,
  features = map(1:n_trial, \(x) sample(0:1, n_features, replace=TRUE)),
  decision = NA,
  EV1 = list(NA),
  EV0 = list(NA))

feedback_1 <- function(features, decision) {

  return(features)
  ## if(decision) {
  ##   return(features)
  ## } else {
  ##   return(1-features)
  ## }
}

feedback_0 <- function(features, decision) {
  return(1-features)
}

softmax <- function(v1, v2) {
  return(v1/(v1+v2))
}

distance <- function(v1, v2) {
  sqrt(sum((v1-v2)^2))
}

similarity <- function(x) {
  1/exp(x)
}

pluck(df, "EV1", 1) <- c(0,0,0,0,0)
pluck(df, "EV0", 1) <- c(0,0,0,0,0)

for (i in 1:n_trial) {
  d1 <- distance(pluck(df, "EV1", i), pluck(df, "features", i))
  d0 <- distance(pluck(df, "EV0", i), pluck(df, "features", i))
  pluck(df, "decision", i) <- rbinom(1, 1, softmax(similarity(d1), similarity(d0)))
  prediction_error <- pluck(df, "EV1", i) - feedback(pluck(df, "features", i), pluck(df, "decision", i))
  if (i != n_trial) {
    pluck(df, "EV1", i+1) <- pluck(df, "EV1", i) + alpha * (pluck(df, "EV1", i) - feedback_1(pluck(df, "features", i),  pluck(df, "decision", i) ))
    pluck(df, "EV0", i+1) <- pluck(df, "EV0", i) + alpha * (pluck(df, "EV1", i) - feedback_0(pluck(df, "features", i),  pluck(df, "decision", i) ))
  }
}
