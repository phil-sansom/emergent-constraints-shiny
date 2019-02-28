################
## Priors tab ##
################

## Input selection
output$prior_input_select = renderUI({
  
  if (input$priors == "informative") {
    radioButtons(
      inputId  = "prior_input_select",
      label    = "Input style",
      choices  = list(Sliders = "sliders", Numerical = "numerical"),
      selected = "sliders",
      inline   = FALSE
    )
  } else {
    return(NULL)
  }
  
})

## Intercept prior
output$alpha_prior = renderUI({

  if (input$priors == "reference" | is.null(input$prior_input_select))
    return (NULL)
  
  if (no_data()) {
    min  = -1.0
    max  = +1.0
    step =  0.1
  } else {
    x    = c(ylim()$min,ylim()$max)
    diff = diff(x)
    step = 10^(floor(log10(diff))-1)
    min  = - diff / 2
    max  = + diff / 2
    min  = floor  (min / step) * step
    max  = ceiling(max / step) * step
  }

  if (input$prior_input_select == "numerical") {
    tagList(
      withMathJax(),
      hr(),
      h5("Intercept \\(\\alpha\\)"),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_alpha",
                            label   = "Mean \\(\\mu_\\alpha\\)",
                            value   = 0.5 * (min + max),
                            min     = NA,
                            max     = NA,
                            step    = step
               ) ## mu_alpha
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_alpha",
                            label   = "Standard deviation \\(\\sigma_\\alpha\\)",
                            value   = max,
                            min     = 0,
                            max     = NA,
                            step    = step
               ) ## sigma_alpha
        ) ## column
      ) ## fluidRow
    ) ## tagList
  } else {
    tagList(
      withMathJax(),
      hr(),
      h5("Intercept \\(\\alpha\\)"),
      sliderInput(inputId = "mu_alpha",
                  label   = "Mean \\(\\mu_\\alpha\\)",
                  value   = 0.5 * (min + max),
                  min     = min,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ), ## mu_alpha
      sliderInput(inputId = "sigma_alpha",
                  label   = "Standard deviation \\(\\sigma_\\alpha\\)",
                  value   = max,
                  min     = 0,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ) ## sigma_alpha
    ) ## tagList
  }
  
}) ## alpha_prior

## Slope prior
output$beta_prior = renderUI({
  
  if (input$priors == "reference" | is.null(input$prior_input_select))
    return (NULL)
  
  if (no_data()) {
    min  = -1
    max  = +1
    step = 0.1
  } else {
    x     = c(xlim()$min,xlim()$max)
    y     = c(ylim()$min,ylim()$max)
    diff = diff(y)/diff(x)
    step = 10^(floor(log10(diff))-1)
    min  = - diff / 2
    max  = + diff / 2
    min  = floor  (min / step) * step
    max  = ceiling(max / step) * step
  } 
  
  if (input$prior_input_select == "numerical") {
    tagList(
      withMathJax(),
      hr(),
      h5("Slope \\(\\beta\\)"),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_beta",
                            label   = "Mean \\(\\mu_\\beta\\)",
                            value   = 0,
                            min     = NA,
                            max     = NA,
                            step    = step
               ) ## mu_beta
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_beta",
                            label   = "Standard deviation \\(\\sigma_\\beta\\)",
                            value   = max,
                            min     = 0,
                            max     = NA,
                            step    = step
               ) ## sigma_beta
        ) ## column
      ) ## fluidRow
    ) ## tagList
  } else {
    tagList(
      withMathJax(),
      hr(),
      h5("Slope \\(\\beta\\)"),
      sliderInput(inputId = "mu_beta",
                  label   = "Mean \\(\\mu_\\beta\\)",
                  value   = 0,
                  min     = min,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ), ## mu_beta
      sliderInput(inputId = "sigma_beta",
                  label   = "Standard deviation \\(\\sigma_\\beta\\)",
                  value   = max,
                  min     = 0,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ) ## sigma_beta
    ) ## tagList
  }
  
}) ## beta_prior

## Prior correlation
output$rho_prior = renderUI({
  
  if (input$priors == "reference" | is.null(input$prior_input_select))
    return(NULL)
  
  inputId = "rho"
  label   = "Corr(\\(\\alpha,\\beta\\))"
  value   =  0
  min     = -1
  max     = +1
  step    = 0.01
  
  if (input$prior_input_select == "numerical") {
    tagList(
      withMathJax(),
      hr(),
      h5("Correlation \\(\\rho\\)"),
      fluidRow(
        column(width = 6,
               numericInput(inputId = inputId,
                            label   = label,
                            value   = value,
                            min     = min,
                            max     = NA,
                            step    = step
               )
        ) ## column
      ) ## fluidRow
    ) ## tagList
  } else {
    tagList(
      withMathJax(),
      hr(),
      h5("Correlation \\(\\rho\\)"),
      sliderInput(inputId = inputId,
                  label   = label,
                  value   = value,
                  min     = min,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      )
    ) ## tagList
  }

}) ## rho

## Response uncertainty prior
output$sigma_prior = renderUI({
  
  if (input$priors == "reference" | is.null(input$prior_input_select))
    return (NULL)
  
  if (no_data()) {
    min  = -1
    max  = +1
    step = 0.1
  } else {
    x    = c(ylim()$min,ylim()$max)
    diff = diff(x)
    step = 10^(floor(log10(diff))-1)
    min  = - diff / 2
    max  = + diff / 2
    min  = floor  (min / step) * step
    max  = ceiling(max / step) * step
  }
  
  if (input$prior_input_select == "numerical") {
    tagList(
      withMathJax(),
      hr(),
      h5("Response spread \\(\\sigma\\)"),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_sigma",
                            label   = "Mean \\(\\mu_\\sigma\\)",
                            value   = 0,
                            min     = NA,
                            max     = NA,
                            step    = step
               ) ## mu_sigma
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_sigma",
                            label   = "Standard deviation \\(\\sigma_\\sigma\\)",
                            value   = max,
                            min     = 0,
                            max     = NA,
                            step    = step
               ) ## sigma_sigma
        ) ## column
      ) ## fluidRow
    ) ## tagList
  } else {
    tagList(
      withMathJax(),
      hr(),
      h5("Response spread \\(\\sigma\\)"),
      sliderInput(inputId = "mu_sigma",
                  label   = "Mean \\(\\mu_\\sigma\\)",
                  value   = 0,
                  min     = min,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ), ## mu_sigma
      sliderInput(inputId = "sigma_sigma",
                  label   = "Standard deviation \\(\\sigma_\\sigma\\)",
                  value   = max,
                  min     = 0,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ) ## sigma_sigma
    ) ## tagList
  }
  
}) ## sigma_prior

## Real world predictor prior
output$xstar_prior = renderUI({
  
  if (input$priors == "reference" | is.null(input$prior_input_select))
    return (NULL)
  
  if (no_data()) {
    min  = -1.0
    max  = +1.0
    step =  0.1
  } else {
    x    = c(ylim()$min,ylim()$max)
    diff = diff(x)
    step = 10^(floor(log10(diff))-1)
    min  = - diff / 2
    max  = + diff / 2
    min  = floor  (min / step) * step
    max  = ceiling(max / step) * step
  }
  
  if (input$prior_input_select == "numerical") {
    tagList(
      withMathJax(),
      hr(),
      h5("Real world predictor \\(X_\\star\\)"),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_xstar",
                            label   = "Mean \\(\\mu_{X_\\star}\\)",
                            value   = 0.5 * (min + max),
                            min     = NA,
                            max     = NA,
                            step    = step
               ) ## mu_xstar
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_xstar",
                            label   = "Standard deviation \\(\\sigma_{X_\\star}\\)",
                            value   = max,
                            min     = 0,
                            max     = NA,
                            step    = step
               ) ## sigma_xstar
        ) ## column
      ) ## fluidRow
    ) ## tagList
  } else {
    tagList(
      withMathJax(),
      hr(),
      h5("Real world predictor \\(X_\\star\\)"),
      sliderInput(inputId = "mu_xstar",
                  label   = "Mean \\(\\mu_{X_\\star}\\)",
                  value   = 0.5 * (min + max),
                  min     = min,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ), ## mu_xstar
      sliderInput(inputId = "sigma_xstar",
                  label   = "Standard deviation \\(\\sigma_{X_\\star}\\)",
                  value   = max,
                  min     = 0,
                  max     = max,
                  step    = step,
                  ticks   = FALSE
      ) ## sigma_xstar
    ) ## tagList
  }
  
}) ## xstar_prior

## Plot intercept prior
output$alpha_prior_plot = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if(input$priors == "reference" | is.null(input$prior_input_select))
    return(NULL)
  if (is.null(input$mu_alpha) | is.null(input$sigma_alpha))
    return(NULL)
  if (is.na  (input$mu_alpha) | is.na  (input$sigma_alpha))
    return(NULL)
  if (input$sigma_alpha <= 0)
    return(NULL)
  
  normal_plot(mu    = input$mu_alpha,
              sigma = input$sigma_alpha,
              gamma = gamma(),
              xlab  = parameter_labels["alpha"],
              ylab  = "Density",
              par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## alpha_prior_plot

## Plot slope prior
output$beta_prior_plot = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if(input$priors == "reference" | is.null(input$prior_input_select))
    return(NULL)
  if (is.null(input$mu_beta) | is.null(input$sigma_beta))
    return(NULL)
  if (is.na  (input$mu_beta) | is.na  (input$sigma_beta))
    return(NULL)
  if (input$sigma_beta <= 0)
    return(NULL)
  
  normal_plot(mu    = input$mu_beta,
              sigma = input$sigma_beta,
              gamma = gamma(),
              xlab  = parameter_labels["beta"],
              ylab  = "Density",
              par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## beta_prior_plot

## Plot spread prior
output$sigma_prior_plot = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if(input$priors == "reference" | is.null(input$prior_input_select))
    return(NULL)
  if (is.null(input$mu_sigma) | is.null(input$sigma_sigma))
    return(NULL)
  if (is.na  (input$mu_sigma) | is.na  (input$sigma_sigma))
    return(NULL)
  if (input$sigma_sigma <= 0)
    return(NULL)

  folded_normal_plot(mu    = input$mu_sigma,
                     sigma = input$sigma_sigma,
                     gamma = gamma(),
                     xlab  = parameter_labels["sigma"],
                     ylab  = "Density",
                     par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## sigma_prior_plot

## Plot predictor prior
output$xstar_prior_plot = renderPlot({

  ## Skip plotting if bad prior or reference prior
  if(input$priors == "reference" | is.null(input$prior_input_select))
    return(NULL)
  if (is.null(input$mu_xstar) | is.null(input$sigma_xstar))
    return(NULL)
  if (is.na  (input$mu_xstar) | is.na  (input$sigma_xstar))
    return(NULL)
  if (input$sigma_xstar <= 0)
    return(NULL)
  
  normal_plot(mu    = input$mu_xstar,
              sigma = input$sigma_xstar,
              gamma = gamma(),
              xlab  = parameter_labels["xstar"],
              ylab  = "Density",
              par   = list(mar = c(2.5,4.0,1,1)+0.1))

}) ## xstar_prior_plot

## Plot prior predictive
output$prior_predictive_plot = renderPlot({

  ## Skip plotting if error condition
  if(no_data() | input$priors == "reference" | bad_prior())
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

}) ## prior_predictive_plot
