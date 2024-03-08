
data {
  int<lower=0> trials;
  int<lower=1> k; // number of parameters
  matrix[trials, k] X; // design matrix encoding WSLS-strategy
  // real initial_value;
  array[trials] int choice;
  vector[trials] feedback;
}

parameters {
  vector[k] beta;
}

// transformed parameters {
//   // vector<lower=0, upper=1>[trials] theta;

//   // tau = inv_logit(tau_u);

//   // EV[1] = initial_value;
//   // for (t in 2:trials)  {
//   //   EV[t] = EV[t-1] + alpha * (feedback[t-1] - EV[t-1]);
//   // }

//   // theta = inv_logit(beta * X)

// }

model {
  beta ~ normal(0, 1);
  // alpha ~ uniform(0, 1);
  // tau_u ~ normal(0, 1);

  // choice ~ bernoulli(theta);
  target += bernoulli_logit_lpmf(choice | X * beta);
  // target += bernoulli_lpmf(choice | theta);
}
