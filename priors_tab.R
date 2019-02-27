################
## Priors tab ##
################

## Plot intercept prior
output$alpha_prior = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if (is.na(input$mu_alpha) | is.na(input$sigma_alpha) |
      input$sigma_alpha <= 0 | input$reference)
    return(NULL)

  normal_plot(mu    = input$mu_alpha,
              sigma = input$sigma_alpha,
              gamma = gamma(),
              xlab  = parameter_labels["alpha"],
              ylab  = "Density",
              par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## alpha_prior

## Plot slope prior
output$beta_prior = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if (is.na(input$mu_beta) | is.na(input$sigma_beta) |
      input$sigma_beta <= 0 | input$reference)
    return(NULL)

  normal_plot(mu    = input$mu_beta,
              sigma = input$sigma_beta,
              gamma = gamma(),
              xlab  = parameter_labels["beta"],
              ylab  = "Density",
              par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## beta_prior

## Plot spread prior
output$sigma_prior = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if (is.na(input$mu_sigma) | is.na(input$sigma_sigma) |
      input$sigma_sigma <= 0 | input$reference)
    return(NULL)

  folded_normal_plot(mu    = input$mu_sigma,
                     sigma = input$sigma_sigma,
                     gamma = gamma(),
                     xlab  = parameter_labels["sigma"],
                     ylab  = "Density",
                     par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## sigma_prior

## Plot predictor prior
output$xstar_prior = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if (is.na(input$mu_xstar) | is.na(input$sigma_xstar) |
      input$sigma_xstar <= 0 | input$reference)
    return(NULL)

  normal_plot(mu    = input$mu_xstar,
              sigma = input$sigma_xstar,
              gamma = gamma(),
              xlab  = parameter_labels["xstar"],
              ylab  = "Density",
              par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## xstar_prior

## Plot prior predictive
output$prior_predictive = renderPlot({

  ## Skip plotting if error condition
  if(no_data() | input$reference | bad_prior())
    return(NULL)

  ## Simulate from parameter priors
  mu    = c(input$mu_alpha,input$mu_beta)
  Sigma = matrix(c(input$sigma_alpha^2,
                   input$rho*input$sigma_alpha*input$sigma_beta,
                   input$rho*input$sigma_alpha*input$sigma_beta,
                   input$sigma_beta^2),
                 2, 2)
  theta =   mvrnorm(input$N, mu, Sigma)
  sigma = abs(rnorm(input$N, input$mu_sigma, input$sigma_sigma))

  ## Plotting points
  xx = xx()
  nn = length(xx)

  ## Simulate from prior predictive distribution
  pp = matrix(NA, nn, 3, dimnames = list(NULL, c("fit","lwr","upr")))
  for (i in 1:nn) {
    buffer = theta[,1] + theta[,2] * xx[i] + sigma * rnorm(input$N)
    pp[i,"fit"]          = mean(buffer)
    pp[i,c("lwr","upr")] =
      quantile(buffer, 0.5*(1 + c(-1,+1)*gamma()))
  }

  ## Graphical parameters
  graphical_parameters()

  ## Plot prior predictive mean
  plot(xx, pp[,"fit"], type = "l", lty = "dotdash", lwd = 2,
       xlim = xlim()$value, ylim = range(pp), yaxs = "r")

  ## Add prior predictive interval
  lines(xx, pp[,"lwr"], lty = "dashed" , lwd = 2)
  lines(xx, pp[,"upr"], lty = "dashed" , lwd = 2)

  ## Add labels
  title(xlab = input$xlab)
  title(ylab = input$ylab)

}) ## prior_predictive
