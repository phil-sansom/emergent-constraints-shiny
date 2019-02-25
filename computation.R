##########################
## Data and computation ##
##########################

## Read user data
data = reactive({

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
mask = reactive(
  sample.int(input$N, 1e3)
)

## Predictor points for plotting
xx = reactive(
  seq(from       = xlim()$min,
      to         = xlim()$max,
      length.out = 101)
)

## Sample posterior with reference priors
reference_posterior = reactive({

  ## Extract data
  x       = data()[,input$x]
  y       = data()[,input$y]
  z       = input$z
  sigma_z = input$sigma_z

  ## Model
  model = lm(y ~ x)

  ## Initialise storage
  samples = array(data     = NA,
                  dim      = c(input$N/getOption("mc.cores"),
                               getOption("mc.cores"),
                               7),
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
  theta = mvrnorm(input$N, model$coef, vcov(model))
  samples[,,"alpha"]     = theta[,1]
  samples[,,"beta" ]     = theta[,2]
  samples[,,"sigma"]     = sqrt(sum(model$residuals^2) /
                                  rchisq(input$N, df.residual(model)))
  samples[,,"log_sigma"] = log(samples[,,"sigma"])
  samples[,,"xstar"]     = rnorm(input$N, z, sigma_z)
  samples[,,"ystar"]     = samples[,,"alpha"] +
    samples[,,"beta" ] * samples[,,"xstar"] +
    samples[,,"sigma"] * rnorm(input$N)

  ## Compute log posterior probability
  samples[,,"lp__"] = apply(samples, c(1,2), function(s)
    sum(dnorm(y, s["alpha"] + s["beta"]*x, s["sigma"], log = TRUE)) +
      dnorm(z, s["xstar"], sigma_z, log = TRUE))

  ## Return posterior samples
  return(samples)

})

## Sample posterior with informative priors
informative_posterior = reactive({

  ## Extract data
  x       = data()[,input$x]
  y       = data()[,input$y]
  z       = input$z
  sigma_z = input$sigma_z

  ## Package data
  data = list(M = length(x), x = x, y = y, z = z, sigma_z = sigma_z,
              mu_alpha = input$mu_alpha, sigma_alpha = input$sigma_alpha,
              mu_beta  = input$mu_beta , sigma_beta  = input$sigma_beta ,
              mu_sigma = input$mu_sigma, sigma_sigma = input$sigma_sigma,
              mu_xstar = input$mu_xstar, sigma_xstar = input$sigma_xstar,
              rho = input$rho)

  ## Fit STAN model
  buffer = sampling(model, data = data, chains = getOption("mc.cores"),
                    iter = 2*input$N/getOption("mc.cores"),
                    warmup = input$N/getOption("mc.cores"),
                    verbose = FALSE, show_messages = FALSE)

  ## Extract posterior samples
  buffer = as.array(buffer)
  dimnames(buffer)$parameters = c("alpha","beta","sigma","xstar","ystar","lp__")
  return(buffer)

})

## Posterior distribution
posterior = reactive(
  if (input$reference) {
    reference_posterior()
  } else {
    informative_posterior()
  }
)

## Compute discrepancy
discrepancy = reactive({

  ## Initialise storage
  samples = array(data     = NA,
                  dim      = c(input$N/getOption("mc.cores"),
                               getOption("mc.cores"),
                               5),
                  dimnames = list(iterations = NULL,
                                  chains     = paste("chain:",
                                                     1:getOption("mc.cores"),
                                                     sep = ""),
                                  parameters = c("alphastar","betastar",
                                                 "sigmastar","xstar","ystar")
                  ) ## dimnames
  ) ## samples

  ##
  samples[,,"xstar"    ] = posterior()[,,"xstar"]

  ## Sample discrepancies
  mu    = c(input$mu_delta_alpha,input$mu_delta_beta)
  Sigma = matrix(c(input$sigma_delta_alpha^2,
                   input$rho_delta*input$sigma_delta_alpha*input$sigma_delta_beta,
                   input$rho_delta*input$sigma_delta_alpha*input$sigma_delta_beta,
                   input$sigma_delta_beta^2),
                 2, 2)
  delta = mvrnorm(input$N, mu, Sigma)
  samples[,,"alphastar"] = posterior()[,,"alpha"] + delta[,1]
  samples[,,"betastar" ] = posterior()[,,"beta" ] + delta[,2]
  samples[,,"sigmastar"] = sqrt(posterior()[,,"sigma"]^2 +
                                  input$sigma_sigma_star^2)

  ## Posterior predictive distribution
  samples[,,"ystar"] = samples[,,"alphastar"] +
    samples[,,"betastar"] * samples[,,"xstar"] +
    samples[,,"sigmastar"] * rnorm(input$N)

  ## Return predictions
  return(samples)

})

## Sample posterior predictive
predictive = reactive(

  posterior_predictive(x     = xx(),
                       alpha = posterior()[,,"alpha"],
                       beta  = posterior()[,,"beta" ],
                       sigma = posterior()[,,"sigma"],
                       gamma = as.numeric(input$gamma)
  )

)

## Sample posterior predictive discrepancy
discrepancy_predictive = reactive(

  posterior_predictive(x     = xx(),
                       alpha = discrepancy()[,,"alphastar"],
                       beta  = discrepancy()[,,"betastar" ],
                       sigma = discrepancy()[,,"sigmastar"],
                       gamma = as.numeric(input$gamma)
  )

)

## Sample posterior predictive from basic model with reference priors
reference_predictive = reactive(

  posterior_predictive(x     = xx(),
                       alpha = reference_posterior()[,,"alpha"],
                       beta  = reference_posterior()[,,"beta" ],
                       sigma = reference_posterior()[,,"sigma"],
                       gamma = as.numeric(input$gamma)
  )

)
