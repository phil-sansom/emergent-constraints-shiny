################
## Priors tab ##
################

## Model input selection
output$model_input_select = renderUI({
  
  if (input$model_priors == "informative") {
    radioButtons(
      inputId  = "model_input_select",
      label    = "Input style",
      choices  = list(Sliders = "sliders", Numerical = "numerical"),
      selected = "sliders",
      inline   = FALSE
    )
  } else {
    return(NULL)
  }
  
})

## Real world input selection
output$real_input_select = renderUI({
  
  if (input$real_priors == "informative") {
    radioButtons(
      inputId  = "real_input_select",
      label    = "Input style",
      choices  = list(Sliders = "sliders", Numerical = "numerical"),
      selected = "sliders",
      inline   = FALSE
    )
  } else {
    return(NULL)
  }
  
})

## Model prior interface
output$model_prior_interface = renderUI({

  if (input$model_priors == "reference" | is.null(input$model_input_select))
    return (NULL)
  
  if (no_data()) {
    astep =  0.1
    amval =  0.0
    ammin = -1.0
    ammax = +1.0
    asmax = +1.0
    bstep =  0.1
    bmmin = -1.0
    bmmax = +1.0
    bsmax = +1.0
    rval  =  0.0
    sstep =  0.1
    smmax = +1.0
    ssmax = +1.0
  } else {
    
    ## Data
    x      = data()[,input$x]
    xrange = diff(range(x))
    xorder = 10^round(log10(xrange))
    
    y      = data()[,input$y]
    yrange = diff(range(y))
    yorder = 10^round(log10(yrange))
    
    ## Slope parameter
    border = yorder/xorder
    bstep  = border/10
    
    bmmin  = -10*border/2
    bmmax  = +10*border/2

    bsmax  = +10*border/2

    ## Intercept parameter
    astep  = yorder/10
    
    amval  = round(mean(y)/yorder)*yorder
    ammin  = amval - 10*yorder/2
    ammax  = amval + 10*yorder/2
    
    asmax  = (10*yorder/2)^2 + mean(x)^2*bsmax^2
    asmax  = ceiling(sqrt(asmax)/yorder)*yorder
    
    ## Correlation
    rval   = -mean(x)*bsmax/asmax
    
    ## Response spread
    sstep  = yorder/10
    
    smmax  = 10*yorder/2
    
    ssmax  = 10*yorder/2
    
  }
  
  numerical = input$model_input_select == "numerical"
  
  tagList(
    withMathJax(),
    hr(),
    h5("Intercept \\(\\alpha\\)"),
    if (numerical) {
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_alpha",
                            label   = "Mean \\(\\mu_\\alpha\\)",
                            value   = amval,
                            min     = NA,
                            max     = NA,
                            step    = astep
               ) ## mu_alpha
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_alpha",
                            label   = "Standard deviation \\(\\sigma_\\alpha\\)",
                            value   = asmax,
                            min     = 0,
                            max     = NA,
                            step    = astep
               ) ## sigma_alpha
        ) ## column
      ) ## fluidRow
    } else {
      tagList(
        sliderInput(inputId = "mu_alpha",
                    label   = "Mean \\(\\mu_\\alpha\\)",
                    value   = amval,
                    min     = ammin,
                    max     = ammax,
                    step    = astep,
                    ticks   = FALSE
        ), ## mu_alpha
        sliderInput(inputId = "sigma_alpha",
                    label   = "Standard deviation \\(\\sigma_\\alpha\\)",
                    value   = asmax,
                    min     = 0,
                    max     = asmax,
                    step    = astep,
                    ticks   = FALSE
        ) ## sigma_alpha
      ) ## tagList        
    },
    hr(),
    h5("Slope \\(\\beta\\)"),
    if (numerical) {
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_beta",
                            label   = "Mean \\(\\mu_\\beta\\)",
                            value   = 0,
                            min     = NA,
                            max     = NA,
                            step    = bstep
               ) ## mu_beta
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_beta",
                            label   = "Standard deviation \\(\\sigma_\\beta\\)",
                            value   = bsmax,
                            min     = 0,
                            max     = NA,
                            step    = bstep
               ) ## sigma_beta
        ) ## column
      ) ## fluidRow
    } else {
      tagList(
        sliderInput(inputId = "mu_beta",
                    label   = "Mean \\(\\mu_\\beta\\)",
                    value   = 0,
                    min     = bmmin,
                    max     = bmmax,
                    step    = bstep,
                    ticks   = FALSE
        ), ## mu_beta
        sliderInput(inputId = "sigma_beta",
                    label   = "Standard deviation \\(\\sigma_\\beta\\)",
                    value   = bsmax,
                    min     = 0,
                    max     = bsmax,
                    step    = bstep,
                    ticks   = FALSE
        ) ## sigma_beta
      ) ## tagList
    },
    hr(),
    h5("Correlation \\(\\rho\\)"),
    if (numerical) {
      fluidRow(
        column(width = 6,
               numericInput(inputId = "rho",
                            label   = "Corr(\\(\\alpha,\\beta\\))",
                            value   = rval,
                            min     = -1,
                            max     = +1,
                            step    = 0.01
               ) ## rho
        ) ## column
      ) ## fluidRow
    } else {
      tagList(
        sliderInput(inputId = "rho",
                    label   = "Corr(\\(\\alpha,\\beta\\))",
                    value   = rval,
                    min     = -1,
                    max     = +1,
                    step    = 0.01,
                    ticks   = FALSE
        ) ## rho
      ) ## tagList
    },
    hr(),
    h5("Response spread \\(\\sigma\\)"),
    if (numerical) {
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_sigma",
                            label   = "Fold \\(\\mu_\\sigma\\)",
                            value   = 0,
                            min     = NA,
                            max     = NA,
                            step    = sstep
               ) ## mu_sigma
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_sigma",
                            label   = "Scale \\(\\sigma_\\sigma\\)",
                            value   = ssmax,
                            min     = 0,
                            max     = NA,
                            step    = sstep
               ) ## sigma_sigma
        ) ## column
      ) ## fluidRow
    } else {
      tagList(
        sliderInput(inputId = "mu_sigma",
                    label   = "Fold \\(\\mu_\\sigma\\)",
                    value   = 0,
                    min     = 0,
                    max     = smmax,
                    step    = sstep,
                    ticks   = FALSE
        ), ## mu_sigma
        sliderInput(inputId = "sigma_sigma",
                    label   = "Scale \\(\\sigma_\\sigma\\)",
                    value   = ssmax,
                    min     = 0,
                    max     = ssmax,
                    step    = sstep,
                    ticks   = FALSE
        ) ## sigma_sigma
      ) ## tagList
    }
  ) ## tagList
  
}) ## model_prior_interface

## Real world prior interface
output$real_prior_interface = renderUI({
  
  if (input$real_priors == "reference" | is.null(input$real_input_select))
    return (NULL)
  
  if (no_data()) {
    
    xstep =  0.1
    xmval =  0.0
    xmmin = -1.0
    xmmax = +1.0
    xsmax = +1.0
    
  } else {
    
    x      = data()[,input$x]
    xrange = diff(range(x))
    xorder = 10^round(log10(xrange))
    
    xstep  = xorder/10
    
    xmval  = round(mean(x)/xorder)*xorder
    xmmin  = xmval - 10*xorder/2
    xmmax  = xmval + 10*xorder/2
    
    xsmax  = 10*xorder/2
    
  }
  
  tagList(
    withMathJax(),
    hr(),
    h5("Real world predictor \\(X_\\star\\)"),
    if (input$real_input_select == "numerical") {
      fluidRow(
        column(width = 6,
               numericInput(inputId = "mu_xstar",
                            label   = "Mean \\(\\mu_{X_\\star}\\)",
                            value   = xmval,
                            min     = NA,
                            max     = NA,
                            step    = xstep
               ) ## mu_xstar
        ), ## column
        column(width = 6,
               numericInput(inputId = "sigma_xstar",
                            label   = "Standard deviation \\(\\sigma_{X_\\star}\\)",
                            value   = xsmax,
                            min     = 0,
                            max     = NA,
                            step    = xstep
               ) ## sigma_xstar
        ) ## column
      ) ## fluidRow
    } else {
      tagList(
        sliderInput(inputId = "mu_xstar",
                    label   = "Mean \\(\\mu_{X_\\star}\\)",
                    value   = xmval,
                    min     = xmmin,
                    max     = xmmax,
                    step    = xstep,
                    ticks   = FALSE
        ), ## mu_xstar
        sliderInput(inputId = "sigma_xstar",
                    label   = "Standard deviation \\(\\sigma_{X_\\star}\\)",
                    value   = xsmax,
                    min     = 0,
                    max     = xsmax,
                    step    = xstep,
                    ticks   = FALSE
        ) ## sigma_xstar
      ) ## tagList
    }
  ) ## tagList
  
}) ## real_prior_interface

## Plot intercept prior
output$alpha_prior_plot = renderPlot({
  
  ## Skip plotting if bad prior or reference prior
  if(input$model_priors == "reference" | is.null(input$model_input_select))
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
  if(input$model_priors == "reference" | is.null(input$model_input_select))
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
  if(input$model_priors == "reference" | is.null(input$model_input_select))
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
  if(input$real_priors == "reference" | is.null(input$real_input_select))
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
  if(no_data() | input$model_priors == "reference" | bad_model_prior())
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
    pp[i,c("lwr","upr")] = quantile(buffer, 0.5*(1 + c(-1,+1)*gamma()))
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
