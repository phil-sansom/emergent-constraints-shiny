server = function(input, output, session) {

  ###############
  ## Constants ##
  ###############
  
  ## Parameter labels
  parameter_labels = c(expression(paste("Intercept ", alpha)),
                       expression(paste("Slope ", beta)),
                       expression(paste("Response spread ", sigma)),
                       expression(paste("Real world predictor ", X["*"])),
                       "Log Posterior")
  names(parameter_labels) = c("alpha","beta","sigma","xstar","lp__")
  
  ## User defined colours
  alpha_black = rgb(0, 0, 0, 0.5)
  alpha_red   = rgb(1, 0, 0, 0.5)
  alpha_green = rgb(0, 1, 0, 0.5)
  alpha_blue  = rgb(0, 0, 1, 0.5)
  
  
  ###############
  ## Functions ##
  ###############
  
  ## Graphical parameters
  graphical_parameters = function()
    par(ann = FALSE, las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0),
        ps = 12, tcl = -1/3, xaxs = "i", yaxs = "i")
  
  ## Compute useful plotting limits
  limits = function(...) {
    
    vrange  = range(..., na.rm = TRUE)
    vdiff   = diff(vrange)
    vstep   = 10^floor(log10(vdiff))
    vmin    = vrange[1] - vdiff
    vmax    = vrange[2] + vdiff
    vminval = vrange[1] - 0.04 * vdiff
    vmaxval = vrange[2] + 0.04 * vdiff
    vmin    = floor  (vmin   /vstep)*vstep
    vmax    = ceiling(vmax   /vstep)*vstep
    vminval = floor  (vminval/vstep)*vstep
    vmaxval = ceiling(vmaxval/vstep)*vstep
    vstep   = 10^floor(log10(vdiff)-1)
    
    return(list(value = c(vminval,vmaxval), min = vmin, max = vmax,
                step = vstep))
    
  }  
  
  ## Compute useful limits for posterior plots
  posterior_limits = function(...) {
    
    vrange  = range(..., na.rm = TRUE)
    vdiff   = diff(vrange)
    vminval = vrange[1] - 0.04 * vdiff
    vmaxval = vrange[2] + 0.04 * vdiff
    vmin    = vminval
    vmax    = vmaxval
    vstep   = 10^floor(log10(vdiff)  )
    vmin    = floor  (vmin   /vstep)*vstep
    vmax    = ceiling(vmax   /vstep)*vstep
    vstep   = 10^floor(log10(vdiff)-1)
    vminval = floor  (vminval/vstep)*vstep
    vmaxval = ceiling(vmaxval/vstep)*vstep
    
    return(list(value = c(vminval,vmaxval), min = vmin, max = vmax, 
                step = vstep))
    
  } ## posterior_limits
  
  ## Function to sample posterior predictive distribution
  posterior_predictive = function(x, alpha, beta, sigma, gamma, N) {
    
    ## x    : numeric(p) - predictor values for sampling
    ## alpha: numeric(n) - intercept values
    ## beta : numeric(n) - slope values
    ## sigma: numeric(n) - spread values
    
    ## Dimensions
    p = length(x)
    n = length(alpha)
    
    ## Initialise storage
    results = matrix(NA, p, 3, dimnames = list(x = x, y = c("fit","lwr","upr")))
    
    ## Loop over predictor values
    for (i in 1:p) {
      buffer                    = rnorm(n, alpha + beta*x[i], sigma)
      results[i,"fit"]          = mean(buffer)
      results[i,c("lwr","upr")] = quantile(buffer, 0.5*(1 + c(-1,+1)*gamma))
    }
    
    ## Return results
    return(results)
    
  }
  
  ## Function to plot a normal distribution
  normal_plot = function(mu, sigma, xlab, ylab, ...) {
    
    ## Compute plotting limits and probability density
    xmin  = qnorm(pnorm(-4), mu, sigma)
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(from = xmin, to = xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma)
    ymax  = 1.04*max(yy)
    
    ## Compute limits of prior interval
    xp = seq(from       = qnorm(0.5*(1 - as.numeric(input$gamma)), mu, sigma),
             to         = qnorm(0.5*(1 + as.numeric(input$gamma)), mu, sigma), 
             length.out = 101)
    yp = dnorm(xp, mu, sigma)
    
    ## Graphical parameters
    graphical_parameters()
    dots = list(...)
    if (exists("par", where = dots))
      par(dots$par)
    
    ## Plot density
    plot (xx, yy, type = "n", xlim = c(xmin,xmax), ylim = c(0,ymax))
    
    ## Add prior interval
    polygon(x = c(xp[1],xp,xp[101]), y = c(0,yp,0), 
            border = NA, col = alpha_red)
    lines(xx, yy, col = "red", lwd = 2)
    
    ## Add labels
    title(xlab = xlab)
    title(ylab = ylab, line = 3.0)
    
  } ## normal_plot
  
  ## Function to plot a folded normal distribution
  folded_normal_plot = function(mu, sigma, xlab, ylab, ...) {
    
    ## Compute plotting limits and probability density
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(0, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma) + dnorm(xx, -mu, sigma)
    ymax  = 1.04*max(yy)
    
    ## Compute limits of prior interval
    cdf   = pnorm(xx, mu, sigma) + pnorm(xx, -mu, sigma) - 1
    xp    = seq(max(which(cdf < 0.5*(1 - as.numeric(input$gamma)))),
                min(which(cdf > 0.5*(1 + as.numeric(input$gamma)))), 1)
    
    ## Graphical parameters
    graphical_parameters()
    dots = list(...)
    if (exists("par", where = dots))
      par(dots$par)
    
    ## Plot density
    plot (xx, yy, type = "n", xlim = c(0,xmax), ylim = c(0,ymax))
    
    ## Add prior interval
    polygon(x = c(xx[xp[1]],xx[xp],xx[xp[length(xp)]]), y = c(0,yy[xp],0),
            border = NA, col = alpha_red)
    lines(xx, yy, col = "red", lwd = 2)
    
    ## Add labels
    title(xlab = xlab)
    title(ylab = ylab, line = 3.0)
    
  } ## folded_normal_plot
  
  ## Function to plot time series of MCMC samples
  plot_samples = function(diag_var) {
    
    ## Extract data
    x = posterior()[,,diag_var]
    
    ## Graphical parameters
    graphical_parameters()
    
    ## Plot samples
    plot(x[,1], type = "n", xlim = c(0,nrow(x)), ylim = range(x), yaxs = "r")
    for (i in 1:ncol(x))
      lines(x[,i], col = i+1)
    
    ## Add labels
    title(xlab = "Sample")
    title(ylab = parameter_labels[diag_var])
    
  } ## plot_samples
  
  ## Function to plot parameter densities
  plot_density = function(diag_var){
    
    ## Extract data
    x = posterior()[,,diag_var]
    
    ## Compute densities
    dd = list()
    for (i in 1:ncol(x))
      dd[[i]] = density(x[,i])
    
    ## Compute limits for credible intervals
    xx = list()
    for (i in 1:ncol(x)) {
      qq = quantile(x[,i], 0.5 + c(-0.5,+0.5)*as.numeric(input$gamma))
      xx[[i]] = seq(from = max(which(dd[[i]]$x < qq[1])), 
                    to   = min(which(dd[[i]]$x > qq[2])), 
                    by   = 1)
    }
    
    ## Compute plotting limits
    ylim = c(0,1.04*max(sapply(1:ncol(x), function(i) max(dd[[i]]$y))))
    
    ## Graphical parameters
    graphical_parameters()
    
    ## Plot densities and credible intervals
    plot(dd[[1]], type = "n", ylim = ylim)
    for (i in 1:ncol(x)) {
      icol = col2rgb(palette()[i%%8+1])/255
      icol = rgb(icol[1], icol[2], icol[3], 0.5)
      polygon(x = c(dd[[i]]$x[xx[[i]][1]],dd[[i]]$x[xx[[i]]],
                    dd[[i]]$x[xx[[i]][length(xx[[i]])]]),
              y = c(0,dd[[i]]$y[xx[[i]]],0), border = NA, col = icol)
      lines(dd[[i]], col = palette()[i%%8+1], lwd = 2)
    }
    
    ## Add labels
    title(xlab = parameter_labels[diag_var])
    title(ylab = "Density")
    
  } ## plot_density

    
  ########################
  ## Condition handling ##
  ########################
  
  ## Check for data
  no_data = reactive(
    any(is.null(data()), 
        ! input$x %in% names(data()), 
        ! input$y %in% names(data())
    )
  )
  
  ## Check observation parameters
  bad_obs = reactive(
    any(is.na(input$z), is.na(input$sigma_z), input$sigma_z <= 0)
  )
  
  ## Check prior parameters
  bad_prior = reactive(
    if (input$reference) {
      FALSE
    } else {
      any(
        is.na(input$mu_alpha), is.na(input$sigma_alpha), input$sigma_alpha <= 0,
        is.na(input$mu_beta ), is.na(input$sigma_beta ), input$sigma_beta  <= 0,
        is.na(input$mu_sigma), is.na(input$sigma_sigma), input$sigma_sigma <= 0,
        is.na(input$mu_xstar), is.na(input$sigma_xstar), input$sigma_xstar <= 0
      )
    }
  )
  
  
  #######################
  ## Reactive couplers ##
  #######################
  
  ## Update X limits
  xlim = reactive({
    
    if (no_data())
      return(list(value = c(0,1), min = 0, max = 1, step = 0.1))
    
    limits(data()[,input$x], input$z + c(-2,+2)*input$sigma_z)
    
  })
  
  ## Update Y limits
  ylim = reactive({
    
    if (no_data())
      return(list(val = c(0,1), min = 0, max = 1, step = 0.1))
    
    limits(data()[,input$y])
    
  })
  
  
  ###############
  ## Observers ##
  ###############

  ## Update variable choices when data loaded
  observe({
    
    choices = names(data())
    
    updateSelectInput(session  = session,
                      inputId  = "x",
                      choices  = choices,
                      selected = choices[1]
    ) ## x
    
    updateSelectInput(session  = session,
                      inputId  = "y",
                      choices  = choices,
                      selected = choices[2]
    ) ## y
    
  }) ## observe

  ## Update predictor label when predictor changes
  observe(
    updateTextInput(session = session,
                    inputId = "xlab",
                    value   = input$x
    )
  ) ## observe

  ## Update response label when response changes
  observe(
    updateTextInput(session = session,
                    inputId = "ylab",
                    value   = input$y
    )
  ) ## observe

  ## Update observation input controls
  observe({
    
    if (no_data()) {
      
      vstep = 1
      
    } else {
      
      vstep  = floor(log10(diff(range(data()[,input$x]))))

    }
      
    updateNumericInput(session = session,
                       inputId = "z",
                       step    = 10^(vstep-1)
    ) ## z

    updateNumericInput(session = session,
                       inputId = "sigma_z",
                       step    = 10^(vstep-2)
    ) ## sigma_z

  }) ## observe

  ## Update intercept bias range
  observe({
    
    v     = c(ylim()$min,ylim()$max)
    vdiff = diff(v)
    vstep = 10^(floor(log10(vdiff))-1)
    vmin  = - vdiff / 2
    vmax  = + vdiff / 2
    vmin  = floor  (vmin / vstep) * vstep
    vmax  = ceiling(vmax / vstep) * vstep

    updateSliderInput(session = session,
                      inputId = "mu_delta_alpha",
                      value   = 0,
                      min     = vmin,
                      max     = vmax,
                      step    = vstep
    ) ## mu_delta_alpha
    
  }) ## observe

  ## Update intercept uncertainty range
  observe({
    
    v     = c(ylim()$min,ylim()$max)
    vdiff = diff(v)
    vstep = 10^(floor(log10(vdiff))-1)
    vmax  = vdiff / 2
    vmax  = ceiling(vmax / vstep) * vstep

    updateSliderInput(session = session,
                      inputId = "sigma_delta_alpha",
                      value   = 0,
                      min     = 0,
                      max     = vmax,
                      step    = vstep
    ) ## sigma_delta_alpha
    
  }) ## observe

  ## Update slope bias range
  observe({
    
    x     = c(xlim()$min,xlim()$max)
    y     = c(ylim()$min,ylim()$max)
    xdiff = diff(x)
    ydiff = diff(y)

    vdiff = ydiff/xdiff
    vstep = 10^(floor(log10(vdiff))-1)
    vmin  = - vdiff / 2
    vmax  = + vdiff / 2
    vmin  = floor  (vmin / vstep) * vstep
    vmax  = ceiling(vmax / vstep) * vstep

    updateSliderInput(session = session,
                      inputId = "mu_delta_beta",
                      value   = 0,
                      min     = vmin,
                      max     = vmax,
                      step    = vstep
    ) ## mu_delta_beta
    
  }) ## observe

  ## Update slope uncertainty range
  observe({
    
    x     = c(xlim()$min,xlim()$max)
    y     = c(ylim()$min,ylim()$max)
    xdiff = diff(x)
    ydiff = diff(y)

    vdiff = ydiff/xdiff
    vstep = 10^(floor(log10(vdiff))-1)
    vmax  = vdiff / 2
    vmax  = ceiling(vmax / vstep) * vstep

    updateSliderInput(session = session,
                      inputId = "sigma_delta_beta",
                      value   = 0,
                      min     = 0,
                      max     = vmax,
                      step    = vstep
    ) ## sigma_delta_beta
    
  }) ## observe

  ## Update response uncertainty range
  observe({
    
    v     = c(ylim()$min,ylim()$max)
    vdiff = diff(v)
    vstep = 10^(floor(log10(vdiff))-1)
    vmax  = vdiff / 2
    vmax  = ceiling(vmax / vstep) * vstep

    updateSliderInput(session = session,
                      inputId = "sigma_sigma_star",
                      value   = 0,
                      min     = 0,
                      max     = vmax,
                      step    = vstep
    ) ## sigma_sigma_star
    
  }) ## observe

  ## Update posterior plot limits
  observe ({

    if (no_data() | bad_obs() | bad_prior()) {
      
      xlims = list(value = c(0,1), min = 0, max = 1, step = 0.1)
      ylims = xlims
      
    } else {

      xlims = posterior_limits(data()[,input$x], discrepancy()[,,"xstar"])
      ylims = posterior_limits(data()[,input$y], discrepancy()[,,"ystar"])
      
    }    

    updateSliderInput(session = session,
                      inputId = "xlim_marginal",
                      value   = ylims$value,
                      min     = ylims$min,
                      max     = ylims$max,
                      step    = ylims$step
    ) ## xlim_marginal
    
    updateSliderInput(session = session,
                      inputId = "xlim_joint",
                      value   = xlims$value,
                      min     = xlims$min,
                      max     = xlims$max,
                      step    = xlims$step
    ) ## xlim_joint
    
    updateSliderInput(session = session,
                      inputId = "ylim_joint",
                      value   = ylims$value,
                      min     = ylims$min,
                      max     = ylims$max,
                      step    = ylims$step
    ) ## ylim_joint

  })

  ## Disable informative priors
  observeEvent(input$reference, {
    toggleState(id = "mu_alpha")
    toggleState(id = "sigma_alpha")
    toggleState(id = "mu_beta")
    toggleState(id = "sigma_beta")
    toggleState(id = "mu_sigma")
    toggleState(id = "sigma_sigma")
    toggleState(id = "mu_xstar")
    toggleState(id = "sigma_xstar")
  })
  

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
                mu_xstar = input$mu_xstar, sigma_xstar = input$sigma_xstar)
    
    ## Fit STAN model
    buffer = sampling(model, data = data, chains = getOption("mc.cores"),
                      iter = 2*input$N/getOption("mc.cores"),
                      warmup = input$N/getOption("mc.cores"),
                      verbose = FALSE, show_messages = FALSE)

    ## Extract posterior samples
    as.array(buffer)

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
    samples[,,"alphastar"] = posterior()[,,"alpha"] + input$mu_delta_alpha +
      input$sigma_delta_alpha * rnorm(input$N)
    samples[,,"betastar" ] = posterior()[,,"beta"]  + input$mu_delta_beta  +
      input$sigma_delta_beta  * rnorm(input$N)
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

  
  ##############
  ## Data tab ##
  ##############
  
  ## Visualise data
  output$data_plot = renderPlot({

    ## Skip plotting if no data is loaded
    if (no_data())
      return(NULL)

    ## Extract data
    x = data()[,input$x]
    y = data()[,input$y]
    z = input$z
    
    ## Graphical parameters
    graphical_parameters()

    ## Plot data
    plot(x, y, xlim = xlim()$value, ylim = ylim()$value, pch = 19)

    ## Add observations
    if (!is.na(input$z))
      abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    
    ## Add labels
    title(xlab = input$xlab)
    title(ylab = input$ylab)

    ## Add legend
    legend("bottomright", legend = c("Models","Observation"),
           col = c("black","blue"), lty = c(NA,"dotdash"), lwd = c(2,2),
           pch = c(19,NA), bty = "n")

    # ## Create plot
    # p = ggplot() +
    #   geom_point(mapping = aes_string(x = input$x, y = input$y), 
    #              data = data()) +
    #   labs(x = input$xlab, y = input$ylab)
    # 
    # ## Add observations
    # if (is.numeric(input$z))
    #   p = p + geom_vline(mapping = aes_string(xintercept = input$z),
    #                      na.rm = TRUE, colour   = "blue", linetype = "dotdash")
    # 
    # ## Plot data
    # p

  })
  
  
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
    alpha =     rnorm(input$N, input$mu_alpha, input$sigma_alpha)
    beta  =     rnorm(input$N, input$mu_beta , input$sigma_beta )
    sigma = abs(rnorm(input$N, input$mu_sigma, input$sigma_sigma))
    
    ## Plotting points
    xx = xx()
    nn = length(xx)
    
    ## Simulate from prior predictive distribution
    pp = matrix(NA, nn, 3, dimnames = list(NULL, c("fit","lwr","upr")))
    for (i in 1:nn) {
      buffer = alpha + beta * xx[i] + sigma * rnorm(input$N)
      pp[i,"fit"]          = mean(buffer)
      pp[i,c("lwr","upr")] = 
        quantile(buffer, 0.5*(1 + c(-1,+1)*as.numeric(input$gamma)))
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
  
  
  #####################
  ## Projections tab ##
  #####################
  
  ## Marginal posterior predictive plot
  output$marginal_plot = renderPlot({
    
    ## Skip plotting if no data is loaded
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    ## Extract data
    y      = data()[,input$y]
    ystar1 = reference_posterior()[,,"ystar"]
    ystar2 = discrepancy()[,,"ystar"]
    probs  = 0.5*(1 + c(-1,+1)*as.numeric(input$gamma))
    
    ## Reference predictive density interval
    dens1 = density (ystar1)
    x1m   = quantile(ystar1, probs = probs)
    x1m   = seq(max(which(dens1$x < x1m[1])), min(which(dens1$x > x1m[2])), 1)
    
    ## Discrepancy predictive density interval
    dens2 = density (ystar2)
    x2m   = quantile(ystar2, probs = probs)
    x2m   = seq(max(which(dens2$x < x2m[1])), min(which(dens2$x > x2m[2])), 1)
    
    ## Plotting limiis
    ymax = max(dens1$y,dens2$y)*1.04
    xlim = input$xlim_marginal
    ylim = c(0,ymax)
    
    ## Graphical parameters
    graphical_parameters()
    
    ## Plot data as rug
    plot(y, type = "n", xlim = xlim, ylim = ylim)
    rug (y, ticksize = 0.02, side = 1, lwd = 2, col = "black", quiet = TRUE)
    
    ## Add predictive densities
    polygon(x = c(dens1$x[x1m[1]],dens1$x[x1m],dens1$x[x1m[length(x1m)]]),
            y = c(0,dens1$y[x1m],0), border = NA, col = alpha_black)
    polygon(x = c(dens2$x[x2m[1]],dens2$x[x2m],dens2$x[x2m[length(x2m)]]),
            y = c(0,dens2$y[x2m],0), border = NA, col = alpha_red)
    lines(dens1, col = "black", lwd = 2)
    lines(dens2, col = "red"  , lwd = 2)
    
    ## Add labels
    title(xlab = input$ylab)
    title(ylab = "Density")
    
    ## Add legend
    legend("topright",
           legend = c("Reference model","Conditionally exchangeable model"),
           col = c("black","red"), lty = c("solid","solid"), lwd = c(2,2),
           bty = "n")
    
  })
  
  ## Download marginal plot
  output$save_marginal_plot = downloadHandler(
    filename = "marginal.pdf",
    content = function(file) {
      pdf(file = file, width = 210/25.4, height = 148/25.4,
          title = "Marginal distribution", pointsize = 12)
      marginal_plot()
      dev.off()
    },
    contentType = "application/pdf"
  )
  
  ## Download samples
  output$save_samples = downloadHandler(
    filename = "samples.csv",
    content = function(file) {
      ## Skip plotting if no data is loaded
      if (no_data() | bad_obs() | bad_prior()) {
        write.csv(NULL, file = file, row.names = FALSE)
      } else {
        write.csv(data.frame(alpha     = as.numeric(posterior()[,,"alpha"]),
                             beta      = as.numeric(posterior()[,,"beta" ]),
                             sigma     = as.numeric(posterior()[,,"sigma"]),
                             alphastar = as.numeric(discrepancy()[,,"alphastar"]),
                             betastar  = as.numeric(discrepancy()[,,"betastar"]),
                             sigmastar = as.numeric(discrepancy()[,,"sigmastar"]),
                             xstar     = as.numeric(discrepancy()[,,"xstar"]),
                             ystar     = as.numeric(discrepancy()[,,"ystar"])
        ),
        file = file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )
  
  ## Print predictive intervals
  output$predictive_intervals = renderTable({   
    
    ## Skip table if no data is loaded
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    ## Extract data
    ystar1 = posterior()  [,,"ystar"]
    ystar2 = discrepancy()[,,"ystar"]
    
    ## Interval width
    probs = 0.5*(1 + c(-1,+1)*as.numeric(input$gamma))
    
    ## Initialise storage
    pred_int = data.frame(numeric(2),numeric(2),numeric(2))
    colnames(pred_int) = c("Mean", paste0(probs, "%"))
    rownames(pred_int) = c("Reference model", 
                           "Conditionally exchangeable model")
    
    ## Compute intervals
    pred_int[1,1  ] = mean(ystar1)
    pred_int[2,1  ] = mean(ystar2)
    pred_int[1,2:3] = quantile(ystar1, probs = probs)
    pred_int[2,2:3] = quantile(ystar2, probs = probs)
    
    ## Return intervals
    return(pred_int)
    
  },
  rownames = TRUE
  )
  
  ## Joint posterior preditive plot
  output$joint_plot = renderPlot({
    
    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    ## Extract data
    x         = data()[,input$x]
    y         = data()[,input$y]
    xstar     = as.numeric(discrepancy()[,,"xstar"])
    ystar     = as.numeric(discrepancy()[,,"ystar"])
    xstar_ref = as.numeric(reference_posterior()[,,"xstar"])
    ystar_ref = as.numeric(reference_posterior()[,,"ystar"])
    
    ## Extract predictive intervals
    reference   = reference_predictive()
    discrepancy = discrepancy_predictive()
    
    ## Points to sample predictive distribution
    xx = xx()
    
    ## Mask to limit number of points plotted
    mask = mask()
    
    ## Interval width
    gamma = as.numeric(input$gamma)
    probs = 0.5*(1 + c(-1,+1)*gamma)
    
    ## Compute joint density under reference priors
    reference_density = kde2d(x = xstar_ref, y = ystar_ref, n = 25)
    z = reference_density$z           ## Extract density
    z = z/sum(z)                      ## Normalise
    o = order(z, decreasing = TRUE)   
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]   ## Compute cumulative density
    reference_density$z = z
    
    ## Compute joint density with discrepancy
    discrepancy_density = kde2d(x = xstar, y = ystar, n = 25)
    z = discrepancy_density$z           ## Extract density
    z = z/sum(z)                        ## Normalise
    o = order(z, decreasing = TRUE)   
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]     ## Compute cumulative density
    discrepancy_density$z = z
    
    ## Plotting limits
    xlim = input$xlim_joint
    ylim = input$ylim_joint
    
    ## Graphical parameters
    graphical_parameters()
    
    ## Plot predictive point cloud
    plot(xstar[mask], ystar[mask], col = gray(0.75, alpha = 0.25), pch = 19,
         xlim = xlim, ylim = ylim)
    
    ## Add data
    points(x, y, col = "black", pch = 19)
    
    ## Add reference predictions
    lines(xx, reference[,"fit"], col = "black", lty = "dotdash", lwd = 2)
    lines(xx, reference[,"lwr"], col = "black", lty = "dashed" , lwd = 2)
    lines(xx, reference[,"upr"], col = "black", lty = "dashed" , lwd = 2)
    
    ## Add discrepancy predictions
    lines(xx, discrepancy[,"fit"], col = "red", lty = "dotdash", lwd = 2)
    lines(xx, discrepancy[,"lwr"], col = "red", lty = "dashed" , lwd = 2)
    lines(xx, discrepancy[,"upr"], col = "red", lty = "dashed" , lwd = 2)
    
    ## Add observations
    abline(v = mean    (xstar)       , col = "blue", lty = "dotdash", lwd = 2)
    abline(v = quantile(xstar, probs), col = "blue", lty = "dashed" , lwd = 2)
    
    ## Add reference density
    contour(reference_density$x, reference_density$y, reference_density$z,
            levels = gamma, drawlabels = FALSE,
            lwd = 2, col = "black", lty = "dotted", add = TRUE)
    
    ## Add discrepancy density
    contour(discrepancy_density$x, discrepancy_density$y, discrepancy_density$z,
            levels = gamma, drawlabels = FALSE,
            lwd = 2, col = "red", lty = "dotted", add = TRUE)
    
    ## Add labels
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    
    ## Add legend
    legend("bottomright",
           legend = c("Reference model","Conditionally exchangeable model",
                      "Observational constraint"),
           col = c("black","red","blue"),
           lty = c("dotdash","dotdash","dotdash"), lwd = c(2,2,2), bty = "n")
    
  })
  
  ## Download joint plot
  output$save_joint_plot = downloadHandler(
    filename = "joint.pdf",
    content = function(file) {
      pdf(file = file, width = 210/25.4, height = 148/25.4,
          title = "Joint distribution", pointsize = 12)
      joint_plot()
      dev.off()
    },
    contentType = "application/pdf"
  )

    
  #####################
  ## Diagnostics tab ##
  #####################
  
  ## Print summary of MCMC output
  output$summary = renderTable({   
    
    ## Skip table if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    monitor(posterior(), warmup = 0, print = FALSE)[c("alpha","beta",
                                                      "sigma","xstar"),]
    
  },
  rownames = TRUE
  ) ## summary

  ## Plot time series of model parameter samples
  output$sample_plot = renderPlot({
    
    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    plot_samples(input$diag_var)
    
  }) ## sample_plot
  
  ## Plot density of model parameter samples
  output$density_plot = renderPlot({
    
    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    plot_density(input$diag_var)
    
  }) ## density_plot

  ## Plot samples vs log posterior
  output$log_posterior_plot = renderPlot({
    
    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)

    ## Extract data    
    x = posterior()[,,input$diag_var]
    y = posterior()[,,"lp__"]
    
    ## Graphical parameters
    graphical_parameters()

    ## Plot log posterior
    plot(x[,1], y[,1], type = "n", xlim = range(x), ylim = range(y),
         xaxs = "r", yaxs = "r")
    for (i in 1:ncol(x))
      points(x[,i], y[,i], col = i+1)

    ## Add labels
    title(xlab = parameter_labels[input$diag_var])
    title(ylab = "Log Posterior")
    
  }) ## log_posterior_plot

  ## Plot sample autocorrelation functions of posterior samples
  output$autocorrelation_plot = renderPlot({

    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)

    ## Extract data
    x = posterior()[,,input$diag_var]
    
    ## Compute autocorrelation functions
    acfs = list()
    for (i in 1:ncol(x))
      acfs[[i]] = as.numeric(acf(x[,i], lag.max = 50, plot = FALSE)$acf)
    
    ## Compute plotting limits
    ylim = c(1.04*min(0,sapply(1:ncol(x), function(i) min(acfs[[i]]))),1)
    xlim = c(0,1.2*51+0.2)

    ## Graphical parameters
    graphical_parameters()
    
    ## Plot autocorrelation functions
    i = 1
    icol = col2rgb(palette()[i%%8+1])/255
    icol = rgb(icol[1], icol[2], icol[3], 0.5)
    barplot(acfs[[i]], col = icol, border = NA, xlim = xlim, ylim = ylim)
    for (i in 2:ncol(x)) {
      icol = col2rgb(palette()[i%%8+1])/255
      icol = rgb(icol[1], icol[2], icol[3], 0.5)
      barplot(acfs[[i]], col = icol, border = NA, add = TRUE, axes = FALSE)
    }
    
    ## Add axis
    axis(side = 1, at = seq(from = 0.7, by = 1.2*5, length.out = 11),
         labels = seq(0,50,5))
    
    ## Add labels
    title(xlab = "Lag")
    title(ylab = "Autocorrelation")
    
  }) ## autocorrelation_plot
 
  ## Plot log posterior time series
  output$log_posterior_samples = renderPlot({
    
    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    plot_samples("lp__")
    
  }) ## log_posterior_samples

  ## Plot density of log posterior
  output$log_posterior_density = renderPlot({
    
    ## Skip plotting if error condition
    if (no_data() | bad_obs() | bad_prior())
      return(NULL)
    
    plot_density("lp__")
    
  }) ## log_posterior_density
  
}
