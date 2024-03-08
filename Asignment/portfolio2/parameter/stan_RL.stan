// The input data are 3 vector 'choice_RL', 'choice_Random', 'feedback' and the number of trials "trials"
data {
  int<lower=1> trials;
  array[trials] int<lower=0,upper=2>  choice_RL; // outcome - NB make the previous choice from choice
  array[trials] int<lower=0,upper=1> feedback ; // input
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
  real invTemperature; // Inverse temperature, for the boundaries 
  invTemperature = exp(logInvTemperature);
}


// The model to be estimated (binomial distribution, parameter alpha and inv, prior on alpha and inv)
model {

 // local parameters
  real predError;
  vector[2]  value;
  vector[2]  prob;

 // priors
  target += beta_lpdf(alpha | 2, 5); //     alpha ~ uni(0,1);
  target += normal_lpdf(logInvTemperature | 0, 1); //   invTemperature ~ uni(0,20);

  value = initValue;

 // model
for (t in 1:trials){
  prob = softmax(invTemperature * value);
  // problem here
  target += categorical_lpmf(choice_RL[t] | prob);

  predError = feedback[t] - value[choice_RL[t]];
  value[choice_RL[t]] = value[choice_RL[t]] + alpha * predError; // update chosen V


}
 
}


