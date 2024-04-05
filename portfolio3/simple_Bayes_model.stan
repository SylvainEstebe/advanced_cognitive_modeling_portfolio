
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> trials;
  array[trials] int<lower=1,upper=8> FirstRating;
  array[trials] int<lower=1,upper=8> GroupRating;
  array[trials] int<lower=1,upper=8> SecondRating;
}

transformed data {
  array[trials] real<lower=0,upper=1> FirstRating_scaled;
  array[trials] real<lower=0,upper=1> GroupRating_scaled;
  
  for (t in 1:trials){
    FirstRating_scaled[t] = (FirstRating[t]+1)/10.0;
    GroupRating_scaled[t] = (GroupRating[t]+1)/10.0;
  }
}

// The parameters accepted by the model. Our model
// accepts the bias parameter
parameters {
  real bias;
}

// ----------- The model to be estimated
model {
  
  // Specify variables
  vector[trials] updated_belief;
  
  // Specify prior for bias
  target += normal_lpdf(bias | 0,2);
  
  for (t in 1:trials){
    updated_belief[t] = inv_logit(bias + logit(FirstRating_scaled[t])+logit(GroupRating_scaled[t]));
    target += binomial_lpmf(SecondRating[t]-1 | 7, updated_belief[t]);
  }
}

generated quantities {
  vector[trials] log_lik;
  array[trials] int y_rep;
  
  for (t in 1:trials){
    real belief;
    belief = inv_logit(bias + logit(FirstRating_scaled[t])+logit(GroupRating_scaled[t]));
    log_lik[t] = binomial_lpmf(SecondRating[t]-1 | 7, belief);
    y_rep[t] = binomial_rng(7,belief);
  }
}





