
data {
  int<lower=0> trials;
  real initial_value;
  array[trials] int choice;
  vector[trials] feedback;

  vector[trials] trial; // which trial are we in
  int k; // number of participants
  array[trials] int id;    // participant
}

parameters {
  real logit_alpha_mu;             // unconstrained learning rate, group mean
  real<lower=0> logit_alpha_sd;             // unconstrained learning rate, group sd
  vector[k] logit_alpha_id;                // unconstrained learning rate, random intercepts
  real log_tau;                    // inverse temperature, unconstrained
}

transformed parameters {
  real <lower=0> tau;
  vector[k] alpha; // learning rate
  vector[trials] EV;
  vector<lower=0, upper=1>[trials] theta;

  alpha = inv_logit(logit_alpha_id * logit_alpha_sd + logit_alpha_mu);
  tau = exp(log_tau);


  for (t in 1:trials)  {
    if (trial[t] == 1) {
      // new participant
      EV[t] = initial_value;
    } else {
      EV[t] = EV[t-1] + alpha[id[t]] * (feedback[t-1] - EV[t-1]);
    }
  }

  theta = softmax(tau * EV);

}

model {
  // alpha ~ beta(2, 1);
  // log_tau ~ normal(0, 1);
  // target += beta_lpdf(alpha | 2, 1);
  logit_alpha_id ~ normal(0, 1);
  target += normal_lpdf(logit_alpha_mu | 0, 3);
  target += normal_lpdf(logit_alpha_sd | 0, 1);

  // target += normal_lpdf(logit_alpha | 0, 3);
  target += normal_lpdf(log_tau | 0, 1);

  // choice ~ bernoulli(theta);
  target += bernoulli_lpmf(choice | theta);
}
