data {
    int<lower=1> trials; // Number of trials in the experiment or dataset.
    array[trials] int<lower=1,upper=2> choiceREL; // Observed choices made by the agent. Each choice is either 1 (e.g., left) or 2 (e.g., right).
    array[trials] int<lower=0,upper=1> feedback; // Feedback received after each choice. Encoded as 0 or 1, representing negative or positive feedback, respectively.
    vector[2] initialValue; // Initial values for making left and right choices. 
} 


parameters {
    real<lower=0, upper=1> alpha; // Learning rate parameter, confined between 0 and 1. Determines how quickly the agent updates its value estimates based on received feedback.
    real<lower=0, upper=20> log_inv_tau; // Logarithm of the inverse temperature parameter. This parameter controls the randomness of the choice: higher values make choices more deterministic.
}



transformed parameters{
    real invTau; // Inverse temperature, transformed from its logarithmic scale to its natural scale for usage in the softmax function.
    invTau = exp(log_inv_tau);
}




model {
    real pred_error; // Placeholder for the prediction error computed in the loop.
    vector[2] Values; // Vector to hold the current estimates of the values for left and right choices.
    vector[2] prob; // Vector to hold the probabilities of choosing left or right, computed using the softmax function.

    
    // Specify uniform priors for alpha and tau directly on the log scale. 
    // using uniform priors suggest a lack of strong prior beliefs .... 
    target += beta_lpdf(alpha | 2, 5); 
    target += normal_lpdf(log_inv_tau | 0, 1);
    

    // Initialize the Values vector with the provided initial values.
    Values = initialValue;
    
    // Loop through each trial to estimate the model parameters based on observed data.
    for (t in 1:trials) {
  
          prob = softmax(invTau * Values); // Compute choice probabilities using the softmax function, scaled by the inverse temperature to account for decision stochasticity.
          target += categorical_lpmf(choiceREL[t] | prob); // Update the model's target log probability based on the likelihood of observed choices, given the computed probabilities.
        
          pred_error = feedback[t] - Values[choiceREL[t]]; // Compute the prediction error as the difference between received feedback and the current value estimate for the chosen option.
          Values[choiceREL[t]] = Values[choiceREL[t]] + alpha * pred_error; // Update the value estimate for the chosen option using the prediction error scaled by the learning rate.
        
    }
    
}

generated quantities{
  // --------PRIORS---------
  real alpha_prior = beta_rng(2, 5); // Simulate a prior sample for alpha from a Beta distribution.
  real log_inv_tau_prior = normal_rng(0, 1); // Simulate a prior sample for log_inv_tau from a Normal distribution. 
  
  // Convert simulated log_inv_tau_prior to inv_tau_prior for interpretability 
  real inv_tau_prior = exp(log_inv_tau_prior);
  
  
  // --------PRIOR PREDICTIONS--------
  int alpha_prior_preds = binomial_rng(trials, alpha_prior);
  //int tau_prior_preds = binomial_rng(trials, inv_tau_prior); COMMENTED OUT CAUSE TAU ISNT BETWEEN 0 AND 1 SO NOT SURE HOW TO TREAT IT 
  
  // ------- POSTERIOR PREDICTIONS------------
  int alpha_post_preds = binomial_rng(trials, alpha);
  //int tau_post_preds = binomial_rng(trials, inv_tau_prior); COMMENTED OUT CAUSE TAU ISNT BETWEEN 0 AND 1 SO NOT SURE HOW TO TREAT IT 
  
  
  // ------- COMPUTING PREDICTITIONS TO POSTERIOR PREDICTIVE CHECKS ------------
  // An array to store choices based on posterior probabilities 
  array[trials] int<lower=1,upper=2> simulated_choices;
  
  
  // Temporary variables for prediction error and probabilities calculation within the loop.
  real temp_pred_error;
  vector[2] temp_Values;
  vector[2] temp_prob;
  
  // Initialize temp_Values with the initial values provided to the model.
  temp_Values = initialValue;
  
  for (t in 1:trials) {
    // Compute choice probabilities using softmax function, based on current value estimates and inv_tau.
    temp_prob = softmax(invTau * temp_Values);
    
    // Simulate choices based on computed probabilities.
    simulated_choices[t] = categorical_rng(temp_prob);
    
    //  calculate prediction error for the simulated choice.
    if (t < trials) { // Skip the last trial cause no feedback is available.
      temp_pred_error = feedback[t] - temp_Values[simulated_choices[t]];
      temp_Values[simulated_choices[t]] = temp_Values[simulated_choices[t]] + alpha * temp_pred_error;
    }
    
    //okay im kinda torned, cause below i guess the actual post pred check is - no data and new
    // probabilities for choices given the prior and post tau 
    // however, should the same be done for alpha, we need feedback (data), so we 
    // are no longer in completely simulation-no-data land, which i think we are supposed to be???
    
    // my intuitioin: 
    // The aim is to assess the predictive performance of the model as it was fitted
    // to the observed data. This means looking at how well the model, using the parameters 
    // it learned (invTau), can predict the choices (choiceREL) we observed, without 
    // further modifying its understanding or "learning" from those observations during the simulation.
    //If we update the value estimates based on feedback during the simulation,
    // we're effectively allowing the model to "learn" from the observed data again.
    // This conflates prediction (estimating what will happen based on what the model has learned) 
    // with a form of continued learning (updating the model's estimates based on new data),
    // which isn't the goal during predictive performance assessment. ????? I THINK ???!?!??!?!
    
  // ------------- ASSESSING HOW POST AND PRIOR PARAMETERS AFFECT PROBABILITES ------------- 
  // Because tau influences choice via softmax probabilities, 
  // we simulate how choice probabilities might vary across trials for **prior** and **posterior** tau values.
  
  ///// THIS DOESNT WORK AND I CAN'T FIGURE OUT HOW ON EARTH I AM TO STORE THE VARIABLES
    // vector[2] choice_prob_prior_tau[trials]; 
    // vector[2] choice_prob_post_tau[trials];


  
    // for (i in 1:trials) {
    // Simulating choice probabilities under prior tau:
      // vector[2] prob_prior_tau = softmax(inv_tau_prior * temp_Values); // Assuming temp_Values reflects some baseline value estimates.
      // choice_prob_prior_tau[1][i] = prob_prior_tau[1];
      // choice_prob_prior_tau[2][i] = prob_prior_tau[2];
    
    // Simulating choice probabilities under estimated tau:
      // vector[2] prob_post_tau = softmax(invTau * temp_Values); // Use model-estimated invTau.
      // choice_prob_post_tau[1][i] = prob_post_tau[1];
      // choice_prob_post_tau[2][i] = prob_post_tau[2];
    // }
  
  }
}  
  
  
  
//I think ???, the key to Bayesian modeling in Stan is distinguishing between fitting the model 
//to observed data (in the model block) and using the fitted model to make predictions or 
//conduct simulations (in the generated quantities block).
