functions {
  // real aliens_feedback_easy(vector fs, int d) {
  //   // features fs
  //   // decision d
  //   if (fs[1] == 1) {
  //       if (d == 1) {
  //           return 1;
  //       } else {
  //           return 0;
  //           }
  //   } else {
  //     if (d == 1) {
  //       return 0;
  //     } else {
  //       return 1;
  //       }
  //     }
  // }

  vector gcm(int trial, int ncat, array[] int true_category, vector weights, matrix features, real scaling) {
    vector[ncat] distance_acc = rep_vector(0, ncat);
    vector[ncat] n_example_acc = rep_vector(0, ncat);
    for (t in 1:trial-1) {
      distance_acc [true_category[t]] += sum(weights * abs(features[t,] - features[trial,]));
      n_example_acc[true_category[t]] += 1;
    }
    return exp(rep_vector(-scaling, ncat) .* (distance_acc ./ n_example_acc));
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
}

parameters {
  simplex[N_features] weights;
  real<lower=0> scaling;
}

transformed parameters {
  // matrix[N_categories, N_features] memory;
  matrix[N_categories, N_trials] theta;
  theta[1:N_categories, 1:trial_start_sampling-1] = rep_matrix(0.5, N_categories, trial_start_sampling-1);
  
  for (t in 1:N_trials) {
    if (t >= trial_start_sampling) {
      theta[,t] = gcm(t, N_categories, true_category, weights, features, scaling);
    }
  }

  
}

model {
  weights ~ dirichlet(rep_vector(2, N_features));
  scaling ~ cauchy(0,2);

  for (t in 1:N_trials) {
    decision[t] ~ categorical(softmax(theta[,t]));
  }
}
