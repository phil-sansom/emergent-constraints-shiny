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

## Posterior distribution
posterior = reactive({
  # print("Computation 4: posterior")
  
  if (input$model_priors == "informative" & ! bad_model_prior()) {
    
    ## Extract data
    x       = data()[,input$x]
    y       = data()[,input$y]
    
    ## Sample size
    cores = getOption("mc.cores")
    n     = input$N %/% cores
    N     = n * cores
    
    ## Package data
    data = list(M = length(x), x = x, y = y,
                mu_alpha = input$mu_alpha, sigma_alpha = input$sigma_alpha,
                mu_beta  = input$mu_beta , sigma_beta  = input$sigma_beta ,
                mu_sigma = input$mu_sigma, sigma_sigma = input$sigma_sigma,
                rho = input$rho)
    
    ## Fit STAN model
    buffer = sampling(model, data = data, chains = getOption("mc.cores"),
                      iter = 2*N/getOption("mc.cores"),
                      warmup = N/getOption("mc.cores"),
                      verbose = FALSE, show_messages = FALSE)
    
    ## Extract posterior samples
    buffer = as.array(buffer)
    dimnames(buffer)$parameters = c("alpha","beta","sigma","lp__")
    return(buffer)
    
  } else {
    
    reference_posterior()
    
  }
  
})

## Sample posterior with reference priors
reference_posterior = reactive({
  # print("Computation 5: reference_posterior")

  ## Extract data
  x       = data()[,input$x]
  y       = data()[,input$y]
 
  ## Model
  model = lm(y ~ x)

  ## Sample size
  cores = getOption("mc.cores")
  n     = input$N %/% cores
  N     = n * cores
  
  ## Initialise storage
  samples = array(data     = NA,
                  dim      = c(n, cores, 4),
                  dimnames = list(iterations = NULL,
                                  chains     = paste("chain:",
                                                     1:getOption("mc.cores"),
                                                     sep = ""),
                                  parameters = c("alpha","beta","sigma","lp__")
                  ) ## dimnames
  ) ## samples

  ## Sample posterior
  theta = mvrnorm(N, model$coef, vcov(model))
  samples[,,"alpha"]     = theta[,1]
  samples[,,"beta" ]     = theta[,2]
  samples[,,"sigma"]     = sqrt(sum(model$residuals^2) /
                                  rchisq(N, df.residual(model)))

  ## Compute log posterior probability
  samples[,,"lp__"] = apply(samples, c(1,2), function(s)
    sum(dnorm(y, s["alpha"] + s["beta"]*x, s["sigma"], log = TRUE)))

  ## Return posterior samples
  return(samples)

})

## Posterior for xstar
xstar = reactive({
  # print("Computation 6: xstar")
  
  if (input$real_priors == "informative" & ! bad_real_prior()) {
 
    ## Sample size
    cores = getOption("mc.cores")
    n     = input$N %/% cores
    N     = n * cores
    
    ## Data
    mux  = input$mu_xstar
    taux = input$sigma_xstar^-2
    z    = input$z
    tauz = input$sigma_z^-2
    
    ## Compute posterior
    mu    = taux*mux + tauz*z
    sigma = 1/(taux + tauz)
    mu    = mu*sigma
    sigma = sqrt(sigma)
    
    ## Sample posterior
    xstar = rnorm(input$N, mu, sigma)
    dim(xstar) = c(n,cores)
    
    ## Return samples
    return(xstar)
    
    
  } else {
    
    xstar_reference()
    
  }
})

## Posterior for real predictor
xstar_reference = reactive({
  # print("Computation 7: xstar_reference")
  
  ## Sample size
  cores = getOption("mc.cores")
  n     = input$N %/% cores
  N     = n * cores
  
  ## Sample posterior
  xstar = rnorm(input$N, input$z, input$sigma_z)
  dim(xstar) = c(n,cores)
  
  ## Return samples
  return(xstar)
  
})

## Predictive deviations
epsilon = reactive({
  # print("Computation 8: epsilon)
  
  rnorm(input$N)
  
})

## Posterior predictive distribution
ystar = reactive({
  # print("Computation 9: ystar")
  
  alphastar = discrepancy()[,,"alphastar"]
  betastar  = discrepancy()[,,"betastar" ]
  sigmastar = discrepancy()[,,"sigmastar"]

  alphastar + betastar * xstar() + sigmastar * epsilon()
  
})

## Reference posterior predictive
ystar_reference = reactive({
  # print("Computation 10: ystar_reference")
  
  alpha = reference_posterior()[,,"alpha"]
  beta  = reference_posterior()[,,"beta" ]
  sigma = reference_posterior()[,,"sigma"]

  alpha + beta * xstar_reference() + sigma * epsilon()
  
})

## Compute discrepancy
discrepancy = reactive({
  # print("Computation 11: discrepancy")

  ## Initialise storage
  samples = posterior()[,,c("alpha","beta","sigma")]
  dimnames(samples)$parameters = c("alphastar","betastar","sigmastar")
  
  ## Check discrepancy parameters
  if (! any(is.null(input$mu_delta_alpha) | is.null(input$sigma_delta_alpha) |
            is.null(input$mu_delta_beta ) | is.null(input$sigma_delta_beta ) |
            is.null(input$rho_delta)      | is.null(input$sigma_delta_sigma) ) &
      ! any(is.na(input$mu_delta_alpha) | is.na(input$sigma_delta_alpha) |
            is.na(input$mu_delta_beta ) | is.na(input$sigma_delta_beta ) |
            is.na(input$rho_delta)      | is.na(input$sigma_delta_sigma) )) {
    
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
    
    ## Sigma uncertainty
    if (input$sigma_delta_sigma  > 0) {
      
      samples[,,"sigmastar"] = 
        abs(rnorm(input$N, samples[,,"sigmastar"], input$sigma_delta_sigma))
      
      new_sd = TRUE
      
    } ## Sigma bias and uncertainty
    
  } ## Check discrepancy parameters
    
  ## Return samples
  return(samples)
  
})

## Sample posterior predictive
predictive = reactive({
  # print("Computation 12: predictive")

  posterior_predictive(x     = xx(),
                       alpha = posterior()[,,"alpha"],
                       beta  = posterior()[,,"beta" ],
                       sigma = posterior()[,,"sigma"],
                       gamma = gamma()
  )

})

## Sample posterior predictive discrepancy
discrepancy_predictive = reactive({
  # print("Computation 13: discrepancy_predictive")

  posterior_predictive(x     = xx(),
                       alpha = discrepancy()[,,"alphastar"],
                       beta  = discrepancy()[,,"betastar" ],
                       sigma = discrepancy()[,,"sigmastar"],
                       gamma = gamma()
  )

})

## Sample posterior predictive from basic model with reference priors
reference_predictive = reactive({
  # print("Computation 14: reference_predictive")

  posterior_predictive(x     = xx(),
                       alpha = reference_posterior()[,,"alpha"],
                       beta  = reference_posterior()[,,"beta" ],
                       sigma = reference_posterior()[,,"sigma"],
                       gamma = gamma()
  )

})

