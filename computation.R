##########################
## Data and computation ##
##########################

## Read user data
data = reactive({
  # print("Computation 1: data")

  input_file = input$file

  ## Check for input file
  if (is.null(input_file))
    return(NULL)

  ## Read data
  read.csv(file   = input_file$datapath,
           header = input$header,
           sep    = input$sep,
           quote  = input$quote
  )

})

## Subset samples for plotting
mask = reactive({
  # print("Computation 2: mask")
  sample.int(input$N, 1e3)
})

## Predictor points for plotting
xx = reactive({
  # print("Computation 3: xx")
  seq(from       = xlim()$min,
      to         = xlim()$max,
      length.out = 101)
})

## Sample posterior with reference priors
reference_posterior = reactive({
  # print("Computation 4: reference_posterior")

  ## Extract data
  x       = data()[,input$x]
  y       = data()[,input$y]
  z       = input$z
  sigma_z = input$sigma_z

  ## Model
  model = lm(y ~ x)

  ## Sample size
  cores = getOption("mc.cores")
  n     = input$N %/% cores
  N     = n * cores
  
  ## Initialise storage
  samples = array(data     = NA,
                  dim      = c(n, cores, 7),
                  dimnames = list(iterations = NULL,
                                  chains     = paste("chain:",
                                                     1:getOption("mc.cores"),
                                                     sep = ""),
                                  parameters = c("alpha","beta", "log_sigma",
                                                 "xstar","sigma","ystar",
                                                 "lp__")
                  ) ## dimnames
  ) ## samples

  ## Sample posterior
  theta = mvrnorm(N, model$coef, vcov(model))
  samples[,,"alpha"]     = theta[,1]
  samples[,,"beta" ]     = theta[,2]
  samples[,,"sigma"]     = sqrt(sum(model$residuals^2) /
                                  rchisq(N, df.residual(model)))
  samples[,,"log_sigma"] = log(samples[,,"sigma"])
  samples[,,"xstar"]     = rnorm(N, z, sigma_z)
  samples[,,"ystar"]     = samples[,,"alpha"] +
    samples[,,"beta" ] * samples[,,"xstar"] +  samples[,,"sigma"] * rnorm(N)

  ## Compute log posterior probability
  samples[,,"lp__"] = apply(samples, c(1,2), function(s)
    sum(dnorm(y, s["alpha"] + s["beta"]*x, s["sigma"], log = TRUE)) +
      dnorm(z, s["xstar"], sigma_z, log = TRUE))

  ## Return posterior samples
  return(samples)

})

## Sample posterior with informative priors
informative_posterior = reactive({
  # print("Computation 5: informative_posterior")

  ## Extract data
  x       = data()[,input$x]
  y       = data()[,input$y]
  z       = input$z
  sigma_z = input$sigma_z
  
  ## Sample size
  cores = getOption("mc.cores")
  n     = input$N %/% cores
  N     = n * cores
  
  ## Package data
  data = list(M = length(x), x = x, y = y, z = z, sigma_z = sigma_z,
              mu_alpha = input$mu_alpha, sigma_alpha = input$sigma_alpha,
              mu_beta  = input$mu_beta , sigma_beta  = input$sigma_beta ,
              mu_sigma = input$mu_sigma, sigma_sigma = input$sigma_sigma,
              mu_xstar = input$mu_xstar, sigma_xstar = input$sigma_xstar,
              rho = input$rho)

  ## Fit STAN model
  buffer = sampling(model, data = data, chains = getOption("mc.cores"),
                    iter = 2*N/getOption("mc.cores"),
                    warmup = N/getOption("mc.cores"),
                    verbose = FALSE, show_messages = FALSE)

  ## Extract posterior samples
  buffer = as.array(buffer)
  dimnames(buffer)$parameters = c("alpha","beta","sigma","xstar","ystar","lp__")
  return(buffer)

})

## Posterior distribution
posterior = reactive({
  # print("Computation 6: posterior")
  if (input$priors == "informative") {
    informative_posterior()
  } else {
    reference_posterior()
  }
})

## Compute discrepancy
discrepancy = reactive({
  # print("Computation 7: discrepancy")

  ## Initialise storage
  samples = posterior()[,,c("alpha","beta","sigma","xstar","ystar")]
  dimnames(samples)$parameters = c("alphastar","betastar","sigmastar",
                                   "xstar","ystar")
  
  ## Check discrepancy parameters
  if (! any(is.null(input$mu_delta_alpha) | is.null(input$sigma_delta_alpha) |
            is.null(input$mu_delta_beta ) | is.null(input$sigma_delta_beta ) |
            is.null(input$mu_delta_sigma) | is.null(input$sigma_delta_sigma) |
            is.null(input$rho_delta)) &
      ! any(is.na(input$mu_delta_alpha) | is.na(input$sigma_delta_alpha) |
            is.na(input$mu_delta_beta ) | is.na(input$sigma_delta_beta ) |
            is.na(input$mu_delta_sigma) | is.na(input$sigma_delta_sigma) |
            is.na(input$rho_delta))) {
    
    ## Set flags
    new_mean = FALSE
    new_sd   = FALSE
    
    ## Alpha bias
    if (input$mu_delta_alpha != 0) {
      samples[,,"alphastar"] = samples[,,"alphastar"] + input$mu_delta_alpha
      new_mean = TRUE
    }
    
    ## Beta bias
    if (input$mu_delta_beta != 0) {
      samples[,,"betastar" ] = samples[,,"betastar" ] + input$mu_delta_beta
      new_mean = TRUE
    }
    
    ## Alpha/Beta uncertainty
    if (input$sigma_delta_alpha > 0 | input$sigma_delta_beta > 0 | 
        input$rho_delta != 0) {
      
      ## If discrepancy uncertainty non-zero then sample discrepancies
      Sigma = matrix(c(input$sigma_delta_alpha^2,
                       input$rho_delta*
                         input$sigma_delta_alpha*input$sigma_delta_beta,
                       input$rho_delta*
                         input$sigma_delta_alpha*input$sigma_delta_beta,
                       input$sigma_delta_beta^2),
                     nrow = 2, 
                     ncol = 2
      )
      delta = mvrnorm(input$N, c(0,0), Sigma)
      samples[,,"alphastar"] = samples[,,"alphastar"] + delta[,1]
      samples[,,"betastar" ] = samples[,,"betastar" ] + delta[,2]
      
      new_mean = TRUE
      
    } ## Alpha/Beta uncertainty
    
    ## Sigma bias and uncertainty
    if (input$mu_delta_sigma != 0 | input$sigma_delta_sigma  > 0) {
      
      samples[,,"sigmastar"] = samples[,,"sigmastar"]^2
      
      if (input$mu_delta_sigma != 0)
        samples[,,"sigmastar"] = 
          samples[,,"sigmastar"] + input$mu_delta_sigma^2
      
      if (input$sigma_delta_sigma > 0)
        samples[,,"sigmastar"] = 
          samples[,,"sigmastar"] + (input$sigma_delta_sigma * rnorm(input$N))^2
      
      samples[,,"sigmastar"] = sqrt(samples[,,"sigmastar"])
      
      new_sd = TRUE
      
    } ## Sigma bias and uncertainty
    
    ## Posterior predictive distribution
    if (new_mean | new_sd) {
      
      samples[,,"ystar"] = posterior()[,,"alpha"] + 
        posterior()[,,"beta"] * posterior()[,,"xstar"]
      epsilon = posterior()[,,"ystar"] - samples[,,"ystar"]
      
      if (new_mean)
        samples[,,"ystar"] = samples[,,"alphastar"] + 
        samples[,,"betastar"] * samples[,,"xstar"]
      
      if (new_sd)
        epsilon = epsilon * samples[,,"sigmastar"] / posterior()[,,"sigma"]
      
      samples[,,"ystar"] =  samples[,,"ystar"] + epsilon
      
    }    

  } ## Check discrepancy parameters
    
  ## Return predictions
  return(samples)
  
})

## Sample posterior predictive
predictive = reactive({
  # print("Computation 8: predictive")

  posterior_predictive(x     = xx(),
                       alpha = posterior()[,,"alpha"],
                       beta  = posterior()[,,"beta" ],
                       sigma = posterior()[,,"sigma"],
                       gamma = gamma()
  )

})

## Sample posterior predictive discrepancy
discrepancy_predictive = reactive({
  # print("Computation 9: discrepancy_predictive")

  posterior_predictive(x     = xx(),
                       alpha = discrepancy()[,,"alphastar"],
                       beta  = discrepancy()[,,"betastar" ],
                       sigma = discrepancy()[,,"sigmastar"],
                       gamma = gamma()
  )

})

## Sample posterior predictive from basic model with reference priors
reference_predictive = reactive({
  # print("Computation 10: reference_predictive")

  posterior_predictive(x     = xx(),
                       alpha = reference_posterior()[,,"alpha"],
                       beta  = reference_posterior()[,,"beta" ],
                       sigma = reference_posterior()[,,"sigma"],
                       gamma = gamma()
  )

})

