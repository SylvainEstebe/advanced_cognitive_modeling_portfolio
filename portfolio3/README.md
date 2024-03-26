# Beta-binomial model of gumball guesses

This model has two tricks:
- With beta distributions you can do posterior updating "by hand" analytically. `beta(1+α1,1+β1)` and `beta(1+α2,1+β2)` become `beta(1+α1+α2, 1+β1+β2)`
- Giving ratings of 1-8 can be thought of as `1 + binomial(k points out of 7)`

Files:
- stan model: `betabinomial-simple-single.stan`
- reprex: `betabinomial-demonstration.R`

## Beta distribution
I model the belief corresponding to each rating as a beta distribution. If `α` is the number of successes and `β` is the number of failures, that corresponds to a `beta(1+α, 1+β)` distribution [1].

A rating of 1 is the lowest possible (`beta(1+0, 1+7)`), a rating of 8 is the highest possible (`beta(1+7, 1+0)`), and for example a rating of 4 is then `beta(1+4, 1+3)`. Once this evidence has been combined (we assume the participants do something similar in their head/brain), the distribution of rates `θ` that come out can be used as input to a `binomial` likelihood, which models `k` successes out of `n` bernoulli trials with probability `θ`. This second trick is built into stan with the `beta_binomial` distribution, which skips the `θ` and goes straight from `α,β` to the `k` of `n` "successes". 

Simplifying a bit to ignore the boundaries, the model is then:
`target += beta_binomial_lpmf(SecondRating | 7, 1 + FirstRating + GroupRating, 14 - (FirstRating + GroupRating))`


## Details
This model in itself has no parameters to estimate, and makes the assumption that one rating corresponds to "7 data points" worth of evidence. 


## Links
[1] https://en.wikipedia.org/wiki/Beta_distribution
