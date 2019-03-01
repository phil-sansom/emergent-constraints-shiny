## Preamble
library(MASS)
library(rstan)
library(shiny)
options(mc.cores = 4)             ## Set the number of CPU cores to use
rstan_options(auto_write = TRUE)  ## Prevent STAN recompiling unnecessarily

## Load UI
source("ecui.R")

## Load server
source("ecserver.R")

shinyApp(ui = ui, server = server)
