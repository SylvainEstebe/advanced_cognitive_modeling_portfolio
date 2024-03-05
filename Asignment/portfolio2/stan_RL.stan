// The input data are 3 vector 'choice', 'prev_choice', 'feedback' of length 'trials'.
data {
  int<lower=1> trials;
  array[trials] int<lower=0,upper=1> choice_RL; // outcome - NB make the previous choice from choice
  array[trials] int<lower=0,upper=1> choice_Random; // outcome - NB make the previous choice from choice
  array[trials] int<lower=-1,upper=1> feedback; // input
}

transformed data {
  vector[2] initvalue;  // initial values for V
  initvalue = rep_vector(0.0, 2); // exepected value before starting
}
// The parameters accepted by the model. Our model

parameters {
  real<lower=0, upper=1> alpha; // the learning rate
  real logInvTemperature;
}

transformed parameters{
  real<lower=0.001> invTemperature; // Inverse temperature, for the boundaries 
  invTemperature = exp(logInvTemperature);
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

// https://en.wikipedia.org/wiki/Binomial_distribution
// https://en.wikipedia.org/wiki/Bernoulli_distribution
// https://fr.wikipedia.org/wiki/Fonction_softmax
// https://en.wikipedia.org/wiki/Logit
// https://en.wikipedia.org/wiki/Exponentially_modified_Gaussian_distribution
// https://en.wikipedia.org/wiki/Logit-normal_distribution

model {
  
  // Prior
  target += normal_lpdf(alpha | 0, .5);

  // model
  real predError;
  vector[2]  value;
  vector[2]  prob;
  
  target += beta_lpdf(alpha | 2, 2); //     alpha ~ beta(2,2);
  target += normal_lpdf(invTemperature | 0, 1); //   invTemperature ~ norml(0,1);

  value = initvalue;
  
  // construct this
  for (t in 1:trials){
  prob = softmax(invTemperature * value);
  target += categorical_lpmf(choice_RL[t] | prob);
  
  predError = feedback[t] - value[choice_RL[t]];
  value[choice_RL[t]] = value[choice_RL[t]] + alpha * predError; // update chosen V

// left_v1 <-  value[1] + alpha * (1-choice) * (feedback - value[1])
//  right_v2 <- value[2] + alpha * (choice) * (feedback - value[2])
  }
}

generated quantities{
  real<lower=0, upper=1> alpha_prior;
  real<lower=0, upper=20> temperature_prior;
  
  real pe;
  vector[2] value;
  vector[2] theta;
  
  real log_lik;
  
  alpha_prior = uniform_rng(0,1);
  temperature_prior = uniform_rng(0,20);
  
  value = initvalue;
  log_lik = 0;
  
  for (t in 1:trials) {
        theta = softmax( invTemperature * value); // action prob. computed via softmax
        log_lik = log_lik + categorical_lpmf(choice_RL[t] | theta);
        
        pe = feedback[t] - value[choice_RL[t]]; // compute pe for chosen value only
        value[choice_RL[t]] = value[choice_RL[t]] + alpha * pe; // update chosen V
    }
  
}
