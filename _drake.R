## Load functions in R folder
devtools::load_all()

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake::drake_config(get_analysis_plan(),
             lock_envir = TRUE)

