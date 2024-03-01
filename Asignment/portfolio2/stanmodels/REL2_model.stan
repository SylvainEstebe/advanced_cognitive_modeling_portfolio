// Laura, 01/03/2024

// The input data for the model
data {
  int<lower=0> trials; // Number of trials
  vector[trials] choiceREL; // Relative choice made in each trial
  vector[trials] feedback; // Feedback received in each trial
  vector[2] initialValue; // Initial values for choosing left and right
}






// The parameters to be estimated by the model
parameters {
  real<lower=0, upper=1> alpha; // Learning rate, constrained between 0 and 1
  real<lower=0> logTau; // Log-transformed temperature parameter to ensure it's positive
}

// Transformation of parameters to their original scale or form
transformed parameters {
  real<lower=0> tau = exp(logTau); // Converting logTau back to tau on the original scale
}






// The model specification, including prior distributions and the likelihood
model {
  vector[2] Values; // Vector to hold the values for left and right options
  vector[2] probabilities; // Vector to hold the computed probabilities for making choices

  // Specify PRIORS for the parameters to express initial beliefs about their possible values
  
  // Prior for logTau, assuming it's normally distributed around 0 with a standard deviation of 1 I HAVE NO FREAKING CLUE IF THIS IS A GOOD IDEA???
  target += normal_lpdf(logTau | 0, 1); 
  // Prior for alpha, assuming a beta distribution which is appropriate for parameters bounded between 0 and 1
  target += beta_lpdf(alpha | 1, 1); 

  // Initializing the Values vector with the initial values provided in the data
  Values = initialValue; 
  

  int Choice = 1; // Variable to store the initial choice, initialized to 1 for the first option CAN WE APPLY SOME SORT OF LOGIC THAT CHOOSES IT RANODMLY??? 
  
  for (t in 2:trials) {
    real pred_error; // Variable to store the prediction error, declaration moved here for clarity

    // Calculate prediction error based on the choice made in the previous trial
    pred_error = feedback[t-1] - Values[Choice];

    // Update the value for the chosen option based on the prediction error
    Values[Choice] = Values[Choice] + alpha * pred_error;

    // Compute probabilities for the next choice by applying the softmax function to the tau-scaled Values
    probabilities = softmax(tau * Values);

  }
}







// The generated quantities block for posterior predictions and additional outputs
generated quantities {
  // specifying the priors we want to assess: 
  real alpha_prior;
  real tau_prior; 
  
  // FOR ALPHA::::
  int<lower=0, upper=1> alpha_prior_preds03;
  int<lower=0, upper=1> alpha_prior_preds07;
  int<lower=0, upper=1> alpha_prior_preds09;
  
  int<lower=0, upper=1> alpha_post_preds03;
  int<lower=0, upper=1> alpha_post_preds07;
  int<lower=0, upper=1> alpha_post_preds09;
  
  
  // computing the prior and posterior prediction values to explore the nature of the different priors:
  // it is sampling from a binomial distribution that makes it into a choice of either 0 or 1. 
  alpha_prior = normal_rng(.5,.5);
  alpha_prior_preds03 = binomial_rng(1,inv_logit(alpha_prior*logit(.3))); // remember that inv_logit(alpha_prior) = the pior 
  alpha_prior_preds07 = binomial_rng(1,inv_logit(alpha_prior*logit(.7)));
  alpha_prior_preds09 = binomial_rng(1,inv_logit(alpha_prior*logit(.9)));
  
  alpha_post_preds03 = binomial_rng(1, inv_logit(alpha*logit(.3))); // remember that inv_logit(alpha) = the posterior 
  alpha_post_preds07 = binomial_rng(1, inv_logit(alpha*logit(.7)));
  alpha_post_preds09 = binomial_rng(1, inv_logit(alpha*logit(.9)));
  
  
   // FOR TAU::::
  int<lower=0, upper=1> tau_prior_preds03;
  int<lower=0, upper=1> tau_prior_preds07;
  int<lower=0, upper=1> tau_prior_preds09;
  
  int<lower=0, upper=1> tau_post_preds03;
  int<lower=0, upper=1> tau_post_preds07;
  int<lower=0, upper=1> tau_post_preds09;
  
  
  // computing the prior and posterior prediction values to explore the nature of the different priors:
  // it is sampling from a binomial distribution that makes it into a choice of either 0 or 1. 
  tau_prior = normal_rng(.5,.5);
  tau_prior_preds03 = binomial_rng(1,inv_logit(alpha_prior*logit(.3))); // remember that inv_logit(tau_prior) = the pior 
  tau_prior_preds07 = binomial_rng(1,inv_logit(alpha_prior*logit(.7)));
  tau_prior_preds09 = binomial_rng(1,inv_logit(alpha_prior*logit(.9)));
  
  tau_post_preds03 = binomial_rng(1, inv_logit(alpha*logit(.3))); // remember that inv_logit(tau) = the posterior 
  tau_post_preds07 = binomial_rng(1, inv_logit(alpha*logit(.7)));
  tau_post_preds09 = binomial_rng(1, inv_logit(alpha*logit(.9)));
  
  
}
