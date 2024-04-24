
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
  array[N] int y;
  // assume participants combine the information by adding two beta
  // distributions beta(y1, ub) and beta(y2, ub) beta(y1+y2, ub+ub) essentially.
  // This is a (fun) property of the beta distribution.
  //
  // Subtract lb because in the end, we want binomial(...) = 0 to correspond to
  // the smallest rating option, ie

  for (i in 1:N) {
    y[i] = SecondRating[i] - lb;
  }
}

parameters {
  real log_weight_1;
  real log_weight_2;
  // real<lower=0> weight1;
  // real<lower=0> weight2;
}

transformed parameters {
  vector[N] shape1;
  vector[N] shape2;
  // real<lower=0> weight1 = exp(log_weight_mu + log_weight_delta/2);
  // real<lower=0> weight2 = exp(log_weight_mu - log_weight_delta/2);
  real<lower=0> weight1 = exp(log_weight_1);
  real<lower=0> weight2 = exp(log_weight_2);
 // how many ..
  shape1 = (FirstRating-lb) * weight1 + (GroupRating-lb) * weight2;
  // out of how many total
  shape2 = rep_vector((ub-lb) * weight1 + (ub-lb) * weight2, N);
}

model {
  log_weight_1 ~ normal(0, 2);
  log_weight_2 ~ normal(0, 2);
  // weight1 ~ cauchy(1, 30);
  // weight2 ~ cauchy(1, 30);

  // beta_binomial(shape1, shape2) means binomial(beta(shape1, shape2))
  y ~ beta_binomial(rep_array(ub - lb, N), 1 + shape1, 1 + (shape2 - shape1));
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;

  for (i in 1:N) {
    log_lik[i] =    beta_binomial_lpmf(y[i] | ub - lb, 1 + shape1[i], 1 + shape2[i] - shape1[i]);
    y_rep[i] = lb + beta_binomial_rng(        ub - lb, 1 + shape1[i], 1 + shape2[i] - shape1[i]);
  }
}
