data {
    // Integers
    int<lower = 1> N;  // Total number of trials

    // Observed data
    vector<lower = 1, upper = 8>[N] FirstRating;  // Initial rating
    vector<lower = 1, upper = 8>[N] GroupRating;  // Social rating
    vector<lower = 1, upper = 8>[N] SecondRating;  // Updated rating
}

transformed data {
    vector<lower = 0, upper = 1>[N] mu0 = (FirstRating)/9;
    vector<lower = 0, upper = 1>[N] muS = (GroupRating)/9;
    vector<lower = 0, upper = 1>[N] mu1 = (SecondRating)/9;
    vector<lower = -1, upper = 1>[N] delta = muS - mu0;
}

parameters {
    real<lower = 0, upper = 1> k;
    real<lower = 2> c;
}

transformed parameters {
    vector[N] mode = mu0 + k*delta;
    vector[N] a = mode*(c-2) + 1;
    vector[N] b = (1-mode)*(c-2) + 1;
}

model {
    // Priors
    target += gamma_lpdf(c | 2, 1.0/10);
    target += beta_lpdf(k | 1, 1);

    // Likelihood
    target += beta_lpdf(mu1 | a, b);
}

generated quantities {
    // Likelihood per sample
    vector[N] log_lik;
    vector[N] y_rep;
    for (i in 1:N){
        log_lik[i] = beta_lpdf(mu1[i] | a[i], b[i]);
        y_rep[i] = round(beta_rng(a[i], b[i]) * 9);
    }
}
