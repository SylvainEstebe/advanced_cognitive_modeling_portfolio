data {
    int<lower=1> trials;
  //vector[trials] choiceREL; 
  array[trials] int<lower=1,upper=2> choiceREL;
  //vector[trials] feedback; 
  array[trials] int<lower=0,upper=1> feedback;
  vector[2] initialValue; //the initial values for choosing left and rigth specified in model fitting 
} 

// the parameters are what the model should accept, here we use "alpha" = learning rate and "tau" = inverse temperature for softmax
parameters {
    real<lower=0, upper=1> alpha; 
    real<lower=0, upper=15> log_inv_tau; 
}

transformed parameters{
  real invTau; // Inverse temperature, for the boundaries 
  invTau = exp(log_inv_tau);
}



model {
    real pred_error; // predicted error
    vector[2] Values;
    vector[2] prob; 
    
    // priors for alpha and tau 
    target += uniform_lpdf(alpha | 0, 1); // beta distribution cause...  
    target += uniform_lpdf(log_inv_tau | 0, 15); //uniform distribution
    
    // Initializinng the values vector with the initial values provided outside
    Values = initialValue;
    
    // looping over estimating for all trials: 
    for (t in 1:trials) {
      
       // Compute probabilities for the next choice by the softmax given inverse temperature and previous values for left rigth 
        prob = softmax(invTau * Values); 
        // Update choice given the probabilities calculated above
        target += categorical_lpmf(choiceREL[t] | prob);
        
        pred_error = feedback[t] - Values[choiceREL[t]]; // compute prediction error based on the choice and the corresponding feedback
        Values[choiceREL[t]] = Values[choiceREL[t]] + alpha * pred_error; // update the value of the choice just made 
    }
    
}

generated quantities{
  // --------PRIORS---------
  real<lower=0, upper=1> alpha_prior;
  real<lower=0, upper=15> tau_prior;
  
  // generating our priors: 
  alpha_prior = uniform_rng(0,1);
  tau_prior = uniform_rng(0,15);
  
  
  // --------PRIOR PREDICTIONS--------
  real<lower=0, upper=1> alpha_prior_preds;
  real<lower=0, upper=1> tau_prior_preds;
  
  // generating our prior predictioinss: 
  alpha_prior_preds = binomial_rng(1, inv_logit(alpha_prior));
  tau_prior_preds = binomial_rng(1, inv_logit(tau_prior));
  
  
  // --------POSTERIOIR PREDICTIONS---------
    real<lower=0, upper=1> alpha_post_preds;
  real<lower=0, upper=1> tau_post_preds;
  
  // generating our prior predictioinss: 
  alpha_post_preds = binomial_rng(1,inv_logit(alpha));
  tau_post_preds = binomial_rng(1,inv_logit(invTau));
  
  
 
  
  //--------- GENERATING THE CHOICES GIVEN THE MODEL---------
   // initializing model values to get them our 
  real pred_error;
  vector[2] Values;
  vector[2] prob;
  
  real log_lik;
  
  Values = initialValue;
  log_lik = 0;
  
  //--------- RUNNING ---------
  for (t in 1:trials) {
        prob = softmax(invTau * Values); // action prob. computed via softmax
        log_lik = log_lik + categorical_lpmf(choiceREL[t] | prob);
        
        pred_error = feedback[t] - Values[choiceREL[t]]; // compute pred_error for chosen value only
        Values[choiceREL[t]] = Values[choiceREL[t]] + alpha * pred_error; // update chosen V
    }
  
}




