data {
  int N_trials; // 32
  int N_features; // 5
  int N_categories; // 2
  matrix[N_trials, N_features] features;
  array[N_trials] int true_category;
  array[N_trials] int decision;
  int trial_start_sampling; // which trial is the first one they've seen examplars from all categories
  real weight_prior_precision;
  int<lower=0,upper=1> prior_only;
}

parameters {
  simplex[N_features] weights;
  real<lower=0> scaling;
}

transformed parameters {
  matrix[N_categories, N_trials] theta;
  vector[N_categories] distance_acc;
  vector[N_categories] n_example_acc;

  // the first couple of trials, we don't have enough examples to compare to...
  // theta[1:N_categories, 1:trial_start_sampling-1] = rep_matrix(0.5, N_categories, trial_start_sampling-1);

  // for the rest of the trials, use the gcm()
  for (t in 1:N_trials) {
    if (t >= trial_start_sampling) {
      distance_acc  = rep_vector(0, N_categories);
      n_example_acc = rep_vector(0, N_categories);

      for (q in 1:t-1) {
        // for each previous trial, accumulate the distance to the current trial
        distance_acc [true_category[q]] += sum(weights .* to_vector(abs(features[q] - features[t,])));
        n_example_acc[true_category[q]] += 1;
      }

      theta[,t] = rep_vector(-scaling, N_categories) .* (distance_acc ./ n_example_acc);
    } else {
      theta[,t] = rep_vector(0.5, N_categories);
    }
  }

  
}

model {
  weights ~ dirichlet(rep_vector(weight_prior_precision, N_features));
  scaling ~ cauchy(0,2);

  if (!prior_only) {
    for (t in 1:N_trials) {
      decision[t] ~ categorical_logit(theta[,t]);
    }
  }
}


generated quantities {
  vector[N_trials] log_lik;
  array[N_trials] int yrep;

  simplex[N_features] weights_prior;
  real scaling_prior;
  weights_prior = dirichlet_rng(rep_vector(weight_prior_precision, N_features));
  scaling_prior = abs(cauchy_rng(0, 2));

  for (t in 1:N_trials) {
    yrep[t] = categorical_logit_rng(theta[,t]);
    log_lik[t] = categorical_logit_lpmf(decision[t] | theta[,t]);
  }
}
