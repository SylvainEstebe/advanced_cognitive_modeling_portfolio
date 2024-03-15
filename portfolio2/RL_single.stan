
data {
  int<lower=0> trials;
  real initial_value;
  array[trials] int choice;
  vector[trials] feedback;
}

parameters {
  real<lower=0, upper=1> alpha; // learning rate
  real log_tau;                 // inverse temperature, unconstrained
}

transformed parameters {
  real <lower=0> tau;
  vector[trials] EV;
  vector<lower=0, upper=1>[trials] theta;
  
  tau = exp(log_tau);
  
  EV[1] = initial_value;
  for (t in 2:trials)  {
    EV[t] = EV[t-1] + alpha * (feedback[t-1] - EV[t-1]);
  }
  
  theta = softmax(tau * EV);
    
}

model {
  alpha ~ beta(2, 1);
  log_tau ~ normal(0, 1);
  target += beta_lpdf(alpha | 2, 1);
  target += normal_lpdf(log_tau | 0, 1);

  // choice ~ bernoulli(theta);
  target += bernoulli_lpmf(choice | theta);
}
