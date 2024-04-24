
source("/work/Home/R/init.R")
setwd("/work/Home/advanced_cognitive_modeling_portfolio1")

library(tidyverse)
# library(brms)
library(cmdstanr)
library(loo)
library(posterior)
library(tidybayes)
library(ggthemes)
library(bayesplot)
options(mc.cores = 8)

data_patients <- read_csv("portfolio3/data/Simonsen_clean.csv")

one_participant <- data_patients |>
  filter(ID == sample(unique(ID), 1))

## compile models
simple_betabayes <- cmdstan_model("portfolio3/betabinomial-simple-single.stan")
weighted_betabayes <- cmdstan_model("portfolio3/betabinomial-weighted-single.stan")


fit_betabayes <- function(model, data, lb = 1, ub = 8, fixed_param=FALSE, iter=500) {
  model$sample(
    data = list(
      N = nrow(data),
      lb = lb,
      ub = ub,
      FirstRating = data$FirstRating,
      GroupRating = data$GroupRating,
      SecondRating = data$SecondRating
    ),
    iter_sampling=iter,
    parallel_chains = 4,
    adapt_delta=.95,
    fixed_param=fixed_param
  )
}

m2 <- fit_betabayes(weighted_betabayes, one_participant, iter=2000)

mcmc_trace(m2$draws(c("weight1", "weight2")))
ggsave("portfolio3/betabinomial-traceplot.png")

mcmc_dens(m2$draws(c("weight1", "weight2")), facet_args = list(scales = "free_x")) +
  geom_density(data = tibble(Parameter = c("weight1", "weight2"),
                             Value = list(exp(rnorm(1000, 0, 2)), exp(rnorm(1000, 0, 2)))) |> 
                 unnest(Value)) +
  scale_x_continuous(limits = c(0,10), breaks = seq(0,10, 2)) +
  labs(subtitle = "Black line = Prior. Blue filled = Posterior.",
       title = "Prior-posterior update plot for weighted beta-binomial on participant 234")
ggsave("portfolio3/betabinomial-priorposterior.png")
