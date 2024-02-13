

library(tidyverse)
library(furrr)

plan(multicore(workers = 4))

# an agent is a function from (history, parameters) -> output

# history
# the complete history of trials

# output
# 0 = left, 1 = right

## wsls <- function(prevchoice, feedback) {
##   if (feedback) {
##     return(prevchoice)
##   } else {
##     return(1-prevchoice)
##   }
## }

## noisy_wsls <- function(prevchoice, feedback, theta) {
##   if (feedback) {
##     return(rbinom(1,1, theta))
##   } else {
##     return(rbinom(1,1, 1-theta))
##   }
## }

## wsls(1, 1)
## wsls(0, 1)
## wsls(1, 0)
## wsls(0, 0)

## noisy_wsls(1,1,0.95)




noisy_wsls <- function(history, params, feedback) {
  history <- feedback(history)
  last_round <- history[nrow(history),]
  if (last_round$feedback == 1) {
    # win
    choice <- rbinom(1, 1, params$theta)
    if (choice) {
      return(last_round$choice_self)
    } else {
      return(1-last_round$choice_self)
    }
  } else {
    # lose
    choice <- rbinom(1, 1, 1-params$theta)
    if (choice) {
      return(last_round$choice_self)
    } else {
      return(1-last_round$choice_self)
    }
  }
}

example_history <- tibble(matcher    = c(1,0,0,0),
                          capitalist = c(1,1,1,0))


example_history_2 <- tibble(matcher    = c(1,0,0,0),
                            capitalist = c(1,1,1,1))

## noisy_wsls(example_history, tibble(theta=1), feedback_matcher)
## noisy_wsls(example_history, tibble(theta=1), feedback_capitalist)

## noisy_wsls(example_history_2, tibble(theta=1), feedback_matcher)
## noisy_wsls(example_history_2, tibble(theta=1), feedback_capitalist)

feedback_matcher <- function(history) {
  mutate(history, feedback = as.numeric(matcher == capitalist)) |>
    rename(choice_self = matcher,
           choice_other = capitalist)
}

feedback_capitalist <- function(history) {
  mutate(history, feedback = as.numeric(matcher != capitalist)) |>
    rename(choice_self = capitalist,
           choice_other = matcher)
}

## rowwise(example_history) |>
##   group_split() |>
##   map(noisy_wsls, tibble(theta = 0.9), feedback_matcher)


step_matching_pennies <- function(history, agent_matcher, agent_capitalist) {
  new <- tibble(matcher = agent_matcher(history),
                capitalist = agent_capitalist(history))
  ## print(new)
  ## result <- bind_rows(history, new)
  return(new)
}


make_agent <- function(agent, params, feedback_function) {
  return(function(history) {
    agent(history, params, feedback_function)
  ##   rowwise(history) |>
  ##     group_split() |>
  ##     map(agent, params, feedback_function)
  ## })
  })
  }

## x <- make_agent(noisy_wsls, tibble(theta=0.95), feedback_matcher)
## x(example_history)

## step_matching_pennies(example_history,
##                       make_agent(noisy_wsls, tibble(theta=0.95), feedback_matcher),
##                       make_agent(noisy_wsls, tibble(theta=0.95), feedback_capitalist))

step_n_matching_pennies <- function(history, n, agent_matcher, agent_capitalist) {
  result <- tibble(matcher = rep(NA, n),
                   capitalist = rep(NA, n))
  result <- bind_rows(history, result)
  for (i in 1:n) {
    row <- nrow(history) + i
    ## print(row)
    ## print(result[1:row,])
    ## print(result[1:(row-1),])
    result[row,] <- step_matching_pennies(result[(row-1),], agent_matcher, agent_capitalist)
    #print(result[1:row,])
  }
  return(result)
}

init_random <- function() {
  tibble(matcher = rbinom(1,1,.5),
         capitalist = rbinom(1,1,.5))
}

init_random() |>
  step_n_matching_pennies(5,
                          make_agent(noisy_wsls, tibble(theta=1), feedback_matcher),
                          make_agent(noisy_wsls, tibble(theta=1), feedback_capitalist))



tom <- function(model, other_feedback) {
  function(history, params, feedback) {
    estimate <- model(history, params, other_feedback)

    noise <- rbinom(1,1, params$sigma)
    if (noise) {
      return(estimate)
    } else {
      return(1-estimate)
    }
  }
}



do_sim <- function(theta1, theta2, sigma, n) {
  init_random() |>
    step_n_matching_pennies(
        n,
        make_agent(noisy_wsls, tibble(theta=theta1), feedback_matcher),
        make_agent(tom(noisy_wsls, feedback_matcher), tibble(theta=theta2, sigma=sigma), feedback_capitalist)
    ) |>
    mutate(winner = matcher == capitalist)
}


sim_res <- expand_grid(
  theta1 = seq(0.5, 1, by = 0.1),
  theta2 = seq(0.5, 1, by = 0.1),
  sigma = seq(0.5, 1, by = 0.1)) |>
  #sample_n(100) |>
  mutate(result = future_pmap(list(theta1, theta2, sigma), do_sim, 500, .options = furrr_options(seed = TRUE)))


cum_winrate <- function(res) {
  cumsum(res) / 1:length(res)
}

## mutate(sim_res, winrate = map(result, cum_winrate)) |>
##   unnest(winrate)

sim_res |>
  unnest(result) |>
  group_by(theta1, theta2, sigma) |>
  mutate(i = 1:n(),
         winrate = cum_winrate(winner)) |>
  ggplot() +
  geom_line(aes(i, winrate, color = factor(sigma), group = sigma)) +
  facet_grid(theta1 ~ theta2)


sim_res |>
  unnest(result) |>
  group_by(theta, sigma) |>
  summarise(wins = mean(winner)) |>
  ggplot() +
  geom_tile(aes(theta, sigma, fill = wins))
