
data {
  // count and bounds of the data
  // (the same for firstrating, grouprating, and secondrating in this case)
  int N;
  int lb;
  int<lower=lb+1> ub;
  // these 3 are the main data
  vector<lower=lb,upper=ub>[N] FirstRating;  // x1
  vector<lower=lb,upper=ub>[N] GroupRating;  // x2
  array[N] int<lower=lb,upper=ub> SecondRating; // y
}

transformed data {
  vector[N] shape1;
  vector[N] shape2;
  array[N] int y;
  // assume participants combine the information by adding two beta
  // distributions beta(y1, ub) and beta(y2, ub) beta(y1+y2, ub+ub) essentially.
  // This is a (fun) property of the beta distribution.
  //
  // Subtract lb because in the end, we want binomial(...) = 0 to correspond to
  // the smallest rating option, ie 1

  shape1 = FirstRating + GroupRating - 2 * lb; // how many ..
  shape2 = rep_vector(2 * (ub-lb), N);         // out of how many total
  for (i in 1:N) {
    y[i] = SecondRating[i] - lb;
  }
}

parameters {
  // real logInvTemperature;
}

transformed parameters {
  // real<lower=0> invTemperature = exp(logInvTemperature);
}

model {
  // logInvTemperature ~ normal(0, 1);

  // beta_binomial(shape1, shape2) means binomial(beta(shape1, shape2))
  y ~ beta_binomial(rep_array(ub - lb, N), 1 + shape1, 1 + (shape2 - shape1));
  // y ~ beta_binomial(rep_array(ub - lb, N), 1 + shape1 * invTemperature, 1 + (shape2 - shape1) * invTemperature);
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;

  for (i in 1:N) {
    log_lik[i]    = beta_binomial_lpmf(y[i] | ub - lb, 1 + shape1[i], 1 + shape2[i] - shape1[i]);
    y_rep[i] = lb + beta_binomial_rng(        ub - lb, 1 + shape1[i], 1 + shape2[i] - shape1[i]);
  }
}
