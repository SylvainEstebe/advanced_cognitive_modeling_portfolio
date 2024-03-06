pacman::p_load(tidyverse)

# Creation of agents
# Random bias agent
randomAgent <- function(bias){
  choice <- rbinom(1,1,bias)
  return(choice)
}

# Reinforcement Learning agent
RLAgent <- function(alpha, # Learning rate - how much weight does the agent put on new experience
                    tau,
                    value,
                    choice,
                    feedback
){
  # Update values based on previous trial
  values <- updateValue(value,choice, alpha, feedback)
  
  # Softmax
  probs <- softmax_func(values,tau)
  
  newChoice <- rbinom(1,1,probs[2]) # We get the new choice by sampling from a binomial distribution where the probability of getting one corresponds to the probability of picking the right hand
  outcome <- c(newChoice,values)
  
  return(outcome)
}

# Helper functionsz
updateValue <- function(value, # A vector of values for either hand
                        choice, # The choice from the previous round
                        alpha, # The learning rate
                        feedback # The feedback from the previous round 
){
  
  #PE <- feedback - value
  
  left_v1 <-  value[1] + alpha * (1-choice) * (feedback - value[1])
  right_v2 <- value[2] + alpha * (choice) * (feedback - value[2])
  
  updated_values <- c(left_v1,right_v2)
  
  return(updated_values)
}


softmax_func <-  function(x, tau) { 
  exp_values <- exp(x / (1/tau))
  prob <- exp_values / sum(exp_values)
  return(prob)
}


# Matching pennie game simulation

# parameters
trials <- 30 # number of trial in the game
alpha <- 0.6 # learning rate
tau <- 0.8 # inverse temperature
rate <- 0.7 # biais for random agent

# Create tibble to store results

results <- tibble('trial' = rep(NA,trials),
                  'Agent1_choice' = rep(NA,trials), 
                  'Agent2_choice' = rep(NA,trials),
                  'left_v1' = rep(NA,trials),
                  'right_v2' = rep(NA,trials),
                  'feedback' = rep(NA,trials))

# Get a random choice on the first trial
results$Agent1_choice[1] <- rbinom(1,1,0.5)
results$Agent2_choice[1] <- randomAgent(rate)

# Initiate values for RL agent - for simplicity, we set it to .5 which means that the agent is unbiased
results$left_v1[1] <- .5
results$right_v2[1] <- .5


# play differents trials and save the result
for (trial in 2:trials){
  
  # Get feedback from previous round
  feedback <- ifelse(results$Agent1_choice[trial-1] == results$Agent2_choice[trial-1],1,0)
  
  # Update values based on previous trial and make choice
  Agent1 <- RLAgent(alpha = alpha,
                    tau = tau,
                    value = c(results$left_v1[trial-1],results$right_v2[trial-1]),
                    choice = results$Agent1_choice[trial-1],
                    feedback = feedback)
  
  Agent1_choice <- Agent1[1]
  
  # Make choice for random bias agent
  Agent2_choice <- randomAgent(rate)
  
  # Save results in tibble
  results$feedback[trial-1] <- feedback
  
  results$Agent1_choice[trial] <- Agent1_choice
  results$Agent2_choice[trial] <- Agent2_choice
  
  results$left_v1[trial] <- Agent1[2]
  results$right_v2[trial] <- Agent1[3]
  
}
results$feedback[30] <- feedback
feedback <- ifelse(results$Agent1_choice[30] == results$Agent2_choice[30],1,0)

results <- results %>% mutate(
  trial = seq(trials))

write_csv(results, "~/Code/advanced_cognitive_modeling/simdata/rl_sim.csv")

# plot
p1 <- ggplot(results) + 
  geom_line(aes(trial, left_v1), color = "green") + 
  geom_line(aes(trial, right_v2), color = "blue") +
  theme_bw()
p1

