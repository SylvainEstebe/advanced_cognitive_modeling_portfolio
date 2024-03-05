// The input data are 3 vector 'choice_RL', 'choice_Random', 'feedback' and the number of trials "trials"
data {
  int<lower=1> trials;
  array[trials] int<lower=0,upper=1> choice_RL; // outcome - NB make the previous choice from choice
  array[trials] int<lower=0,upper=1> choice_Random; // outcome - NB make the previous choice from choice
  array[trials] int<lower=-1,upper=1> feedback; // input
}

// defiing variables that do not need to be changed when running the program.
transformed data {
  vector[2] initValue;  // initial values for V
  initValue = rep_vector(0, 2);
}

// The parameters that the model needs to estimate (alpha and lovInvTemperature)
parameters {
  real<lower=0, upper=1> alpha; // the learning rate
  real logInvTemperature; // 
}

transformed parameters{
  real<lower=0.001> invTemperature; // Inverse temperature, for the boundaries 
  invTemperature = exp(logInvTemperature);
}


// The model to be estimated (binomial distribution, parameter alpha and inv, prior on alpha and inv)
model {

 // local parameters
  real predError;
  vector[2]  value;
  vector[2]  prob;

  // The prior for alpha is 
 // target += normal_lpdf(alpha | 0, .5);
  // The prior for invTemp is 
  //target += normal_lpdf(alpha | 0, .5);

  target += uniform_lpdf(alpha | 0, 1); //     alpha ~ beta(2,2);
  target += uniform_lpdf(invTemperature | 0, 20); //   invTemperature ~ norml(0,1);

    value = initValue;
  
  // model
  for (t in 1:trials){
  prob = softmax(invTemperature * value);
  target += categorical_lpmf(choice_RL[t] | prob);
  
  predError = feedback[t] - value[choice_RL[t]];
  value[choice_RL[t]] = value[choice_RL[t]] + alpha * predError; // update chosen V
  }
}


