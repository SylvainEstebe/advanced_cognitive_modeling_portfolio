
source('betashift_sim_functions.R')
N = 1

PR = tibble(
  k_tru = rep(NA,N),
  k_est = rep(NA,N),
  c_tru = rep(NA,N),
  c_est = rep(NA,N))

for (i in 1:N){
  nTrials = 150
  params = list()
  params$k = runif(1)
  params$c = runif(1)*50+2
  
  df = simBetaShift(params,nTrials)
  ggplot(df) + geom_density(aes(x=(mu1-mu0)/(muS-mu0))) + xlim(c(0,1))
  
  # COMPILE AND SAMPLE ####
  
  # Compile STAN model
  mod <- cmdstan_model("betashift.stan",
                       cpp_options = list(stan_threads = TRUE),
                       stanc_options = list("O1"))
  
  data_stan <- list(N = nTrials,
                    FirstRating = df$FirstRating,
                    GroupRating = df$GroupRating,
                    SecondRating = df$SecondRating)
  
  # Draw prior and posterior samples
  samples <- mod$sample(data = data_stan, 
                        seed = 123, 
                        chains = 2, 
                        parallel_chains = 2,
                        threads_per_chain = 1, 
                        iter_warmup = 1000, 
                        iter_sampling = 2000, 
                        refresh = 1000, 
                        max_treedepth = 10, 
                        adapt_delta = 0.99)
  
  draws_df <- as_draws_df(samples$draws())
  
  PR$k_tru[i] = params$k
  PR$k_est[i] = mean(draws_df$k)
  PR$c_tru[i] = params$c
  PR$c_est[i] = mean(draws_df$c)
}

#plotTrace(draws_df,c("k","c"))
ggplot(PR,aes(x = k_tru, y = k_est)) + geom_point() + geom_smooth() + geom_abline(slope = 1, intercept = 0)
ggplot(PR,aes(x = c_tru, y = c_est)) + geom_point() + geom_smooth() + geom_abline(slope = 1, intercept = 0)

