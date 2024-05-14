library(tidyverse)
library(cmdstanr)

source("forward_GCM.R")

df_sim <- df |>
  mutate(true_category = ifelse(feedback==1, decision, 1-decision))

standata_sim <- list(
  N_trials = nrow(df_sim),
  N_features = 5,
  N_categories = 2,
  features = unnest(df_sim, features) |> select(f1:f5) |> as.matrix(),
  true_category = df_sim$true_category + 1,
  decision = df_sim$decision + 1,
  trial_start_sampling = 1 + first(which(df_sim$true_category != lag(df_sim$true_category))),
  weight_prior_precision = 1
)

source("catego.R")
cnt = 0
Nsubs = 5
Nsess = 3
N = Nsubs*Nsess
plt_data = tibble(sub = rep(NA,N),
                  ses = rep(NA,N),
                  w1 = rep(NA,N),
                  w2 = rep(NA,N),
                  w3 = rep(NA,N),
                  w4 = rep(NA,N),
                  w5 = rep(NA,N))

for (subject in 1:Nsubs){
  for (session in 1:Nsess){
    cnt = cnt+1
    print(cnt)
    df_exp = data_xp[data_xp$subject == subject & 
                       data_xp$session == session & 
                       data_xp$condition == 2,]
    # df_exp = df_exp[1:50,]
    
    standata_exp <- list(
      N_trials = nrow(df_exp),
      N_features = 5,
      N_categories = 2,
      features = df_exp$feature,
      true_category = df_exp$true_category + 1,
      decision = df_exp$decision + 1,
      trial_start_sampling = 1 + max(c(first(which(df_exp$true_category == 1)),first(which(df_exp$true_category == 0)))),
      weight_prior_precision = 1
    )
    
    gcm_single <- cmdstan_model("GCM_single.stan")
    
    s <- gcm_single$sample(data = standata_exp,
                           iter_warmup = 500,
                           iter_sampling = 1000,
                           chains = 2,
                           parallel_chains = 2, 
                           refresh = 500,
                           max_treedepth = 10, 
                           adapt_delta = 0.9)
    ddf = as_draws_df(s)
    plt_data$w1[cnt] = mean(ddf$`weights[1]`)
    plt_data$w2[cnt] = mean(ddf$`weights[2]`)
    plt_data$w3[cnt] = mean(ddf$`weights[3]`)
    plt_data$w4[cnt] = mean(ddf$`weights[4]`)
    plt_data$w5[cnt] = mean(ddf$`weights[5]`)
    plt_data$sub[cnt] = subject
    plt_data$ses[cnt] = session
  }
}

plt_df = plt_data %>% pivot_longer(cols=starts_with("w"),names_to = "feature",names_prefix = "w",values_to = "weight")

ggplot(plt_df, aes(x=feature,y=weight)) + geom_bar(stat = "identity") + facet_grid(rows = vars(sub), cols = vars(ses))
