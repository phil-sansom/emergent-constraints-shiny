## Preamble
model = stan_model("model.stan")  ## Load STAN model

## Server function
server = function(input, output, session) {

  ## Load source
  source("constants.R"  , local = TRUE)      ## Constants
  source("functions.R"  , local = TRUE)      ## Functions
  source("handlers.R"   , local = TRUE)      ## Condition handlers
  source("couplers.R"   , local = TRUE)      ## Reactive couplers
  source("observers.R"  , local = TRUE)      ## Passive observers
  source("computation.R", local = TRUE)      ## Computation

  ## Load tabs
  source("data_tab.R"       , local = TRUE)  ## Data tab
  source("priors_tab.R"     , local = TRUE)  ## Priors tab
  source("projections_tab.R", local = TRUE)  ## Projections tab
  source("diagnostics_tab.R", local = TRUE)  ## Diagnostics tab
  
}
