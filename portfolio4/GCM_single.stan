functions {
  vector gcm(int trial, int ncat, array[] int true_category, vector weights, matrix features, real scaling) {
    // Compute gcm (logit) probabilities of picking each category given the
    // weights, scaling, and features so far.
    //
    // In trial 10 (for example), we compute the distance to the previous 9
    // trials, and add them up to the category they belong to. Then divide at
    // the end by how many in that category to get the mean.

    vector[ncat] distance_acc  = rep_vector(0, ncat);
    vector[ncat] n_example_acc = rep_vector(0, ncat);
    for (t in 1:trial-1) {
      // for each previous trial, accumulate the distance to the current trial
      distance_acc [true_category[t]] += sum(weights * abs(features[t,] - features[trial,]));
      n_example_acc[true_category[t]] += 1;
    }
    return rep_vector(-scaling, ncat) .* (distance_acc ./ n_example_acc);
  }
}

data {
  int N_trials; // 32
  int N_features; // 5
  int N_categories; // 2
  matrix[N_trials, N_features] features;
  array[N_trials] int true_category;
  array[N_trials] int decision;
  int trial_start_sampling; // which trial is the first one they've seen examplars from all categories
  real weight_prior_precision;
}

parameters {
  simplex[N_features] weights;
  real<lower=0> scaling;
}

transformed parameters {
  matrix[N_categories, N_trials] theta;

  // the first couple of trials, we don't have enough examples to compare to...
  theta[1:N_categories, 1:trial_start_sampling-1] = rep_matrix(0.5, N_categories, trial_start_sampling-1);

  // for the rest of the trials, use the gcm()
  for (t in 1:N_trials) {
    if (t >= trial_start_sampling) {
      theta[,t] = gcm(t, N_categories, true_category, weights, features, scaling);
    }
  }

  
}

model {
  weights ~ dirichlet(rep_vector(weight_prior_precision, N_features));
  scaling ~ cauchy(0,2);

  for (t in 1:N_trials) {
    decision[t] ~ categorical_logit(theta[,t]);
  }
}


generated quantities {
  vector[N_trials] log_lik;
  array[N_trials] int yrep;
  for (t in 1:N_trials) {
    yrep[t] = categorical_logit_rng(theta[,t]);
    log_lik[t] = categorical_logit_lpmf(decision[t] | theta[,t]);
  }
}
