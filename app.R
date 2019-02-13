#############################
## Emergent constraint app ##
#############################

## Preamble
library(MASS)
library(rstan)
library(shiny)
options(mc.cores = 4)             ## Set the number of CPU cores to use
rstan_options(auto_write = TRUE)  ## Prevent STAN recompiling unnecessarily
model = stan_model("model.stan")  ## Load STAN model

## Load user interface
source("ui.R")

## Load server logic
source("server.R")

## Run app
shinyApp(ui = ui, server = server)
