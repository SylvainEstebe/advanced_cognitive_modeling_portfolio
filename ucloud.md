# R environments

Just like python, environments are a mess in R but you can sort of get
an OK solution if you’re lucky that your use case lines up with the
developers of the tools. I use `renv` and think about 3 distinct actions
that are possible.

1.  Initialize the environment and save it somewhere it’s not going to
    get deleted when you shut down the ucloud instance. This can be in
    `/work/Home/` or in a project folder, but you probably don’t want to
    commit this to git.
2.  Install packages into the environment
3.  Activate the environment to use the packages

In order to set up your environment, do step 1 and 2 (which are both in
the `install.R` script below). This should only be needed once. When you
have your environment set up and want to run something in a fresh ucloud
container, do step 3 (and optionally step 2) which is in the
`activate.R` script below. The easiest way to do that is to just add
`source("/path/to/activate.R")` at the top of your R scripts.

# install.R

``` r
workdir = "/work/Home/R"

setwd(workdir)
Sys.setenv(RENV_PATHS_CACHE = paste0(workdir, '/renv/cache'))
options("renv.consent" = TRUE)

renv::init() ## takes several minutes
install.packages(c("tidyverse", "brms"))
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan(dir = workdir)
renv::snapshot(prompt = FALSE)
```

# activate.R

``` r
workdir = '/work/Home/R'

Sys.setenv(RENV_PATHS_CACHE = paste0(workdir, '/renv/cache'))
options("renv.consent" = TRUE)
renv::load(project = workdir)
cmdstanr::set_cmdstan_path(paste0(workdir, '/cmdstan-2.34.1')) # adapt for newer versions of cmdstanr


Sys.setenv(
  MAKEFLAGS = paste0(
    "-j", 
    parallel::detectCores(logical = FALSE)
  ))
```

# Installing new packages

if you end up installing new packages, finish with

``` r
install.packages(...)
renv::snapshot()
```

# Extra explanations

-   `RENV_PATHS_CACHE` is where renv saves the packages you install –
    might as well put them somewhere not-so-temporary
-   `options("renv.consent" = TRUE)` tells renv not to ask you to
    confirm that you want to use environments every time (it forgets
    your answer when you make a new ucloud session otherwise)
