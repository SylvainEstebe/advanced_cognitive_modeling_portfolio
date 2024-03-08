pacman::p_load(tidyverse,
               here,
               posterior,
               cmdstanr,
               brms, tidybayes)

file <- file.path("~/Code/advanced_cognitive_modeling/portfolio2/stan_RL.stan")
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE),
                     stanc_options = list("O1"), pedantic = TRUE)
data <- read_csv("~/Code/advanced_cognitive_modeling/simdata/rl_sim.csv")
trials = 30
recovery_df <- NULL

#for (taulv in unique(data$tau)){
  #for (alphalv in unique(data$alpha)){
    data <- read_csv("~/Code/advanced_cognitive_modeling/simdata/rl_sim.csv")
    dd <- data %>% subset(
      alpha == 0.8 & tau == 0.5
    )
    
  data <- list(
  trials = trials,
  choice_RL = dd$Agent1_choice+1,
  feedback = dd$feedback,
  initValue = c(0, 2)
)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

draws_df <- as_draws_df(samples$draws()) 

temp <- tibble(alphaEst = inv_logit_scaled(draws_df$alpha), 
               alphaTrue = alphalv)

if (exists("recovery_df")) {recovery_df <- rbind(recovery_df, temp)} else {recovery_df <- temp}

#  }
#}

write_csv(recovery_df, "~/Code/advanced_cognitive_modeling/simdata/recov.csv")

ggplot(recovery_df, aes(alphaTrue, alphaEst)) +
  geom_point() +
  geom_smooth() +
  theme_classic()
