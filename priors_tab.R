################
## Priors tab ##
################

## Input selection
output$prior_input_select = renderUI({
  if (input$priors == "informative") {
    radioButtons(
      inputId  = "prior_input_select",
      label    = "Select inputs",
      choices  = list(Sliders = "sliders", Numerical = "numerical"),
      selected = "sliders",
      inline   = FALSE
    )
  } else {
    return(NULL)
  }
})

## Intercept mean
output$mu_alpha = renderUI({
  
  inputId = "mu_alpha"
  label   = "Mean \\(\\mu_\\alpha\\)"
  value   = 0
  
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
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = NA,
                     max     = NA,
                     step    = step
        ) ## numericInput
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        ) ## sliderInput
      }
    }
  ) ## tagList
  
}) ## mu_alpha

## Intercept SD
output$sigma_alpha = renderUI({
  
  inputId = "sigma_alpha"
  label   = "Standard deviation \\(\\sigma_\\alpha\\)"
  value   = 1
  min     = 0
  
  if (no_data()) {
    max     = 1.0
    step    = 0.1
  } else {
    x     = c(ylim()$min,ylim()$max)
    diff  = diff(x)
    step  = 10^(floor(log10(diff))-1)
    max   = diff / 2
    max   = ceiling(max / step) * step
  }
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = min,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList
  
}) ## sigma_alpha

## Slope mean
output$mu_beta = renderUI({
  
  inputId = "mu_beta"
  label   = "Mean \\(\\mu_\\beta\\)"
  value   = 0
  
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
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = NA,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList
  
}) ## mu_beta

## Slope SD
output$sigma_beta = renderUI({
  
  inputId = "sigma_beta"
  label   = "Standard deviation \\(\\sigma_\\beta\\)"
  value   = 1
  min     = 0
  
  if (no_data()) {
    max  = 1.0
    step = 0.1
  } else {
    x     = c(xlim()$min,xlim()$max)
    y     = c(ylim()$min,ylim()$max)
    diff = diff(y)/diff(x)
    step = 10^(floor(log10(diff))-1)
    max  = diff / 2
    max  = ceiling(max / step) * step
  }
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = min,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList
  
}) ## sigma_beta

## Prior correlation
output$rho = renderUI({
  
  inputId = "rho"
  label   = "Corr(\\(\\alpha,\\beta\\))"
  value   =  0
  min     = -1
  max     = +1
  step    = 0.01
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = min,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList
  
}) ## rho

## Response spread mean
output$mu_sigma = renderUI({
  
  inputId = "mu_sigma"
  label   = "Mean \\(\\mu_\\sigma\\)"
  value   = 0
  
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
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = NA,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList
  
}) ## mu_sigma

## Response spread SD
output$sigma_sigma = renderUI({
  
  inputId = "sigma_sigma"
  label   = "Standard deviation \\(\\sigma_\\sigma\\)"
  value   = 1
  min     = 0
  
  if (no_data()) {
    max  = 1.0
    step = 0.1
  } else {
    x    = c(ylim()$min,ylim()$max)
    diff = diff(x)
    step = 10^(floor(log10(diff))-1)
    max  = diff / 2
    max  = ceiling(max / step) * step
  }
  
  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = min,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList
  
}) ## sigma_sigma

## Real world predictor mean
output$mu_xstar = renderUI({

  inputId = "mu_xstar"
  label   = "Mean \\(\\mu_{X_\\star}\\)"
  value   = 0

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

  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = NA,
                     max     = NA,
                     step    = step
        ) ## numericInput
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        ) ## sliderInput
      }
    }
  ) ## tagList

}) ## mu_xstar

## Real world predictor SD
output$sigma_xstar = renderUI({

  inputId = "sigma_xstar"
  label   = "Standard deviation \\(\\sigma_{X_\\star}\\)"
  value   = 1
  min     = 0

  if (no_data()) {
    max     = 1.0
    step    = 0.1
  } else {
    x     = c(ylim()$min,ylim()$max)
    diff  = diff(x)
    step  = 10^(floor(log10(diff))-1)
    max   = diff / 2
    max   = ceiling(max / step) * step
  }

  tagList(
    withMathJax(),
    if (input$priors == "reference" | is.null(input$prior_input_select)) {
      NULL
    } else {
      if (input$prior_input_select == "numerical") {
        numericInput(inputId = inputId,
                     label   = label,
                     value   = value,
                     min     = min,
                     max     = NA,
                     step    = step
        )
      } else {
        sliderInput(inputId = inputId,
                    label   = label,
                    min     = min,
                    max     = max,
                    value   = value,
                    step    = step,
                    ticks   = FALSE
        )
      }
    }
  ) ## tagList

}) ## sigma_xstar

## Plot intercept prior
output$alpha_prior = renderPlot({

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

}) ## alpha_prior

## Plot slope prior
output$beta_prior = renderPlot({

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

}) ## beta_prior

## Plot spread prior
output$sigma_prior = renderPlot({

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

}) ## sigma_prior

## Plot predictor prior
output$xstar_prior = renderPlot({

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

}) ## xstar_prior

## Plot prior predictive
output$prior_predictive = renderPlot({

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

}) ## prior_predictive
