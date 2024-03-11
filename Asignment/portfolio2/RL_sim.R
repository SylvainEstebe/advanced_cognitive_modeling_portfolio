randomAgent <- function(bias){
  choice <- rbinom(1,1,bias)
  return(choice)
}

# Helper functions
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


softmax_func <-  function(x, tau) { # x is the expected value for each choice, tau is the temperature parameter, the higher the temperature, the more random the choice (more exploration)
  exp_values <- exp(x / (1/tau))
  prob <- exp_values / sum(exp_values)
  return(prob)
}

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

  #New choice (deterministic)
  #newChoice <- ifelse(probs[1]>0, 0, 1)

  newChoice <- rbinom(1,1,1-probs[1]) # We get the new choice by sampling from a binomial distribution where the probability of getting one corresponds to the probability of picking the right hand
  outcome <- c(newChoice,values)

  return(outcome)
}


do_sim <- function(trials=25, alpha=0.1, tau = 1, bias = 0.5, rate = 0.7) {

## # Globals
## trials <- 25

## # RL Agent
## alpha <- 0.1
## tau <- 1
## bias <- 0.5 # for the initial choice, so its your bias for choosing left/rigth
## rate <- 0.7

# Create tibble to store results
results <- tibble('trial' = rep(NA, trials),
                  'Agent1_choice' = rep(NA,trials),
                  'Agent2_choice' = rep(NA,trials),
                  'left_v1' = rep(NA,trials),
                  'right_v2' = rep(NA,trials),
                  'feedback' = rep(NA,trials),
                  'cumulative' = rep(NA,trials))

# Get a random choice on the first trial (addding bias)
results$Agent1_choice[1] <- rbinom(1,1,0.5+bias)
results$Agent2_choice[1] <- randomAgent(rate)

# Initiate values for RL agent - for simplicity, we set it to .5 which means that the agent is unbiased
results$left_v1[1] <- 0
results$right_v2[1] <- 0

results$trial[1] <- 1

for (trial in 2:trials){

  # Get feedback from previous round
  feedback <- ifelse(results$Agent1_choice[trial-1] == results$Agent2_choice[trial-1],1,-1)

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
  results$Agent1_choice[trial] <- Agent1_choice # choice of agent1 (REL)
  results$Agent2_choice[trial] <- Agent2_choice # choice of agent2 (random)

  # the value of left and right
  results$left_v1[trial] <- Agent1[2]
  results$right_v2[trial] <- Agent1[3]

  # trials
  results$trial[trial] <- trial

  # logging the cumulative feedback of agent1
  results$cumulative <- cumsum(results$Agent1_choice) / seq_along(results$Agent1_choice)

}
  results <- filter(results, complete.cases(feedback))
  return(results)
}


##### malte solution


## do_sim <- function(trials=25, alpha=0.1, tau = 1, bias = 0.5, rate = 0.7) {
