

library(tidyverse)


n_trial <- 32
n_features <- 5


df <- tibble(
  trial = 1:n_trial,
  features = map(1:n_trial, \(x) tibble(f1 = sample(0:1, 1, replace = TRUE),
                                        f2 = sample(0:1, 1, replace = TRUE),
                                        f3 = sample(0:1, 1, replace = TRUE),
                                        f4 = sample(0:1, 1, replace = TRUE),
                                        f5 = sample(0:1, 1, replace = TRUE))),
  decision = NA,
  feedback = NA)



feedback_easy <- function(features, decision) {
  if (features[[1]] == 1) {
    return(as.numeric(decision == 1))
  } else {
    return(as.numeric(decision != 1))
  }
}

## feedback_medium <- function(features, decision) {
##   selection <- features[c(2,3,5)]
##   if (sum(selection) >= 2) {
##     return(as.numeric(decision == 1))
##   } else {
##     return(as.numeric(decision != 0))
##   }
## }

gcm <- function(weights, scaling, features, memory_danger, memory_friendly) {
  if ((nrow(memory_danger) == 0) | (nrow(memory_friendly) == 0)) {
    return(rbinom(1,1,.5))
  }

  ## print(features)
  ## print(memory_danger)
  ## print(memory_friendly)

  distance_danger <- rep(NA, nrow(memory_danger))
  for (i in 1:nrow(memory_danger)) {
    distance_danger[i] <- list(weights * abs(features - memory_danger[i, ]))
  }
  distance_danger <- mean(unlist(distance_danger))

  distance_friendly <- rep(NA, nrow(memory_friendly))
  for (i in 1:nrow(memory_friendly)) {
    distance_friendly[i] <- list(weights * abs(features - memory_friendly[i, ]))
  }
  distance_friendly <- mean(unlist(distance_friendly))

  ## print(list(dan = distance_danger, frie=distance_friendly))

  similarity_danger <- exp(-scaling * distance_danger)
  similarity_friendly <- exp(-scaling * distance_friendly)
  theta <- similarity_danger / (similarity_danger + similarity_friendly)# softmax
  ## print(theta)
  return(rbinom(1, 1, theta))
}

weights <- c(1, 0, 0, 0, 0)
# weights <- c(0.8,0.05,0.05,0.05,0.05)
scaling <- 5
memory_danger <- tribble(~f1, ~f2, ~f3, ~f4, ~f5)
memory_friendly <- tribble(~f1, ~f2, ~f3, ~f4, ~f5)

for (i in 1:n_trial) {
  print(i)
  pluck(df, "decision", i) <- gcm(weights, scaling, pluck(df, "features", i), memory_danger, memory_friendly)
  feedback <- feedback_easy(pluck(df, "features", i), pluck(df, "decision", i))
  pluck(df, "feedback", i)<- feedback
  true_category <- ifelse(feedback == 1, pluck(df, "decision", i), 1-pluck(df, "decision", i))
  if (true_category == 1) {
    memory_danger <- bind_rows(memory_danger, pluck(df, "features", i))
  } else {
    memory_friendly <- bind_rows(memory_friendly, pluck(df, "features", i))
  }
}
