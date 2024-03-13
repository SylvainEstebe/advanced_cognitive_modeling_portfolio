// The input data are 3 vector 'choice_RL', 'choice_Random', 'feedback' and the number of trials "trials"
data {
  int<lower=1> trials;
  array[trials] int<lower=1,upper=2>  choice_RL; // outcome - NB make the previous choice from choice
  array[trials] int<lower=-1,upper=1> feedback ; // input
  int<lower=0,upper=1> prior_only;
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
  real<lower=0> invTemperature; // Inverse temperature, for the boundaries
  invTemperature = exp(logInvTemperature);
  real predError;
  // vector[2]  value;
  matrix[2, trials] value;

  value[,1] = initValue;

  // model
  for (t in 2:trials){

    predError = feedback[t-1] - value[choice_RL[t-1],t-1];
    value[choice_RL[t-1],t] = value[choice_RL[t-1],t-1] + alpha * predError; // update chosen V
    value[3-choice_RL[t-1],t] = value[3-choice_RL[t-1],t-1];            // keep the non-chosen V

  }
}


// The model to be estimated (binomial distribution, parameter alpha and inv, prior on alpha and inv)
model {

 // local parameters
  vector[2]  prob;

 // priors
  //target += beta_lpdf(alpha | 2, 2); //     alpha ~ uni(0,1);
  //target += normal_lpdf(logInvTemperature | 0, 1); //   invTemperature ~ uni(0,20);
  target += uniform_lpdf(alpha | 0, 1); //     alpha ~ uni(0,1);
  target += normal_lpdf(logInvTemperature | 0, 3); //   invTemperature ~ uni(0,20);

  // value[,1] = initValue;

 // model
 if (!prior_only) {
  for (t in 2:trials){

  // predError = feedback[t-1] - value[choice_RL[t-1],t-1];
  // value[choice_RL[t-1],t] = value[choice_RL[t-1],t-1] + alpha * predError; // update chosen V
  // value[3-choice_RL[t-1],t] = value[3-choice_RL[t-1],t-1];
  // keep the non-chosen V


   prob = softmax(invTemperature * value[,t]);
  // problem here
  target += categorical_lpmf(choice_RL[t] | prob);

}

 }
 
}

// generated quantities{
//   // real<lower=0, upper=1> alpha_prior;
//   // real<lower=0, upper=20> temperature_prior;

//   real pe;
//   matrix[2, trials] value;
//   // vector[2] prob;

//   // real log_lik;

//   // alpha_prior = uniform_rng(0,1);
//   // temperature_prior = uniform_rng(0,20);

//   value[,1] = initValue;
//   // log_lik = 0;

//   for (t in 2:trials) {
//         // log_lik = log_lik + categorical_lpmf(choice_RL[t] | prob);

//         pe = feedback[t-1] - value[choice_RL[t-1], t-1]; // compute pe for chosen value only
//         value[choice_RL[t-1],t] = value[choice_RL[t-1],t-1] + alpha * pe; // update chosen V
//         value[3-choice_RL[t-1],t] = value[3-choice_RL[t-1],t-1];            // keep the non-chosen V
//                                                                 //
//         // prob = softmax( logInvTemperature * value[,t]); // action prob. computed via softmax
//     }

// }
