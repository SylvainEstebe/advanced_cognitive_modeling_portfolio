// The input data contains 'choice_RL', 'feedback' and the number of trials "trials" extracted from the data 
data {
  int<lower=1> trials;
  array[trials] int<lower=1,upper=2>  choice_RL; // An array containing the choices from the data
  array[trials] int<lower=-1,upper=1> feedback ; // An array of feedback on each trial, which is used for calculating the prediction error
  int<lower=0,upper=1> prior_only; // A variable that specifies whether the priors should be updated (posterior), or remain the original priors (for prior predictive checks)
}

// defining variables that do not need to be changed when running the program.
transformed data {
  vector[2] initValue;  // A vector containing the initial expected values before any evidence is seen.
  initValue = rep_vector(0, 2); // The initial values are set to 0, to indicate equal value for both hands
  }

// The parameters that the model needs to estimate (alpha and lovInvTemperature)
parameters {
  real<lower=0, upper=1> alpha; // The learning rate
  real logInvTemperature; // A parameter that describes how consistent the agent is in their strategy. We use a tranformed parameter to be able to set a normal unbounded prior.
}

transformed parameters{
  real<lower=0> invTemperature; // Inverse temperature
  invTemperature = exp(logInvTemperature); // We transform the parameter to ensure a boundary at 0
  real predError; // We specify the prediction error to be a real number
  
  // Values
  matrix[2, trials] value;

  value[,1] = initValue;

  // model
  for (t in 2:trials){

    predError = feedback[t-1] - value[choice_RL[t-1],t-1];
    value[choice_RL[t-1],t] = value[choice_RL[t-1],t-1] + alpha * predError; // update value of the chosen hand
    value[3-choice_RL[t-1],t] = value[3-choice_RL[t-1],t-1];            // Save the value of the non-chosen hand as well 

  }
}


// The model to be estimated (binomial distribution, parameter alpha and inv, prior on alpha and inv)
model {

 // local parameters
  vector[2]  prob; // The probabilty of choosen right hand as a function of expected value and inverse temperature

 // priors
  target += uniform_lpdf(alpha | 0, 1); // We set a uniform prior for alpha
  target += normal_lpdf(logInvTemperature | 0, 3); // Because we have transformed the parameter, we can set a normal prior

 // model
 if (!prior_only) {
  for (t in 2:trials){

    prob = softmax(invTemperature * value[,t]);
    target += categorical_lpmf(choice_RL[t] | prob);

    }
  } 

}

// generated quantities{
//   We save the model's predicted expected values directly in the transformed parameters
//   and introduce an if else statement in the model that determines whether or not to update the prior based on the data.
//   This setup means that we don't need to include this in the generated quantities
//   }

// }
