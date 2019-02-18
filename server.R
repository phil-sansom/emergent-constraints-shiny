server = function(input, output, session) {

  ##########################
  ## Reactive UI elements ##
  ##########################

  observe({
    choices = names(data())
    updateSelectInput(session  = session,
                      inputId  = "x",
                      choices  = choices,
                      selected = choices[1]
    )
  })

  observe({
    choices = names(data())
    updateSelectInput(session  = session,
                      inputId  = "y",
                      choices  = choices,
                      selected = choices[2]
    )
  })

  observe(
    updateTextInput(session  = session,
                    inputId  = "xlab",
                    value    = input$x
    )
  )

  observe(
    updateTextInput(session  = session,
                    inputId  = "ylab",
                    value    = input$y
    )
  )

  ## Update X limits
  observe({
      xy      = data()
      choices = names(xy)
      if (is.null(xy) | ! input$x %in% choices)
        return(NULL)
      v       = xy[,input$x]

      vrange  = range(v)
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

      updateSliderInput(session = session,
                        inputId = "xlim",
                        value   = c(vminval,vmaxval),
                        min     = vmin,
                        max     = vmax,
                        step    = vstep
      )
  })

  ## Update Y limits
  observe({
    xy = data()
    choices = names(xy)
    if (is.null(xy) | ! input$y %in% choices)
      return(NULL)
    v       = data()[,input$y]

    vrange  = range(v)
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

    updateSliderInput(session = session,
                      inputId = "ylim",
                      value   = c(vminval,vmaxval),
                      min     = vmin,
                      max     = vmax,
                      step    = vstep
    )
  })

  ## Update observation inputs
  observe({
    xy      = data()
    choices = names(xy)
    if (is.null(xy) | ! input$x %in% choices)
      return(NULL)
    v       = xy[,input$x]

    vrange  = range(v)
    vdiff   = diff(vrange)
    vstep   = floor(log10(vdiff))

    updateNumericInput(session = session,
                       inputId = "z",
                       step    = 10^(vstep-1)
    )

    updateNumericInput(session = session,
                       inputId = "sigma_z",
                       step    = 10^(vstep-2)
    )

  })

  observe({
    v     = input$ylim
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
    ) ## updateSliderInput
  }) ## observe

  observe({
    v     = input$ylim
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
    ) ## updateSliderInput
  }) ## observe

  observe({
    v     = input$ylim
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
    ) ## updateSliderInput
  }) ## observe

  observe({
    x     = input$xlim
    y     = input$ylim
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
    ) ## updateSliderInput
  }) ## observe

  observe({
    x     = input$xlim
    y     = input$ylim
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
    ) ## updateSliderInput
  }) ## observe


  ###############
  ## Downloads ##
  ###############

  output$save_main_plot = downloadHandler(
    filename = "joint.pdf",
    content = function(file) {
      pdf(file = file, width = 210/25.4, height = 148/25.4,
          title = "Joint distribution", pointsize = 12)
      main_plot()
      dev.off()
    },
    contentType = "application/pdf"
  )

  output$save_aux_plot = downloadHandler(
    filename = "marginal.pdf",
    content = function(file) {
      pdf(file = file, width = 210/25.4, height = 148/25.4,
          title = "Marginal distribution", pointsize = 12)
      aux_plot()
      dev.off()
    },
    contentType = "application/pdf"
  )


  ##########################
  ## Data and computation ##
  ##########################

  ## Data
  data = reactive({

    input_file = input$file

    if (is.null(input_file))
      return(NULL)

    read.csv(input_file$datapath, header = input$header)

  })

  ## Plotting points
  xx  = reactive(seq(input$xlim[1], input$xlim[2], length.out = 100))

  ## Sample posterior
  posterior = reactive({

    ## Data
    x = data()[,input$x]
    y = data()[,input$y]

    ## Package data
    data = list(M = length(x), x = x, y = y,
                z = input$z, sigma_z = input$sigma_z,
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
    extract(buffer, c("alpha","beta","sigma","xstar","ystar"))

  })

  ## Sample posterior predictive
  predictive = reactive({

    ## Posterior predictive uncertainty
    fit = length(xx())
    lwr = length(xx())
    upr = length(xx())
    for (i in 1:length(xx())) {
      buffer = posterior()$alpha + posterior()$beta * xx()[i] +
        posterior()$sigma * rnorm(input$N)
      fit[i]  = mean(buffer)
      lwr[i]  = quantile(buffer,     as.numeric(input$alpha))
      upr[i]  = quantile(buffer, 1 - as.numeric(input$alpha))
    }

    ## Return samples
    list(fit = fit, lwr = lwr, upr = upr)

  })

  ## Compute discrepancy
  discrepancy = reactive({

    ## Posterior samples
    samples = posterior()

    ## Sample discrepancy
    xstar     = samples$xstar
    alphastar = samples$alpha + input$mu_delta_alpha +
      input$sigma_delta_alpha * rnorm(input$N)
    betastar  = samples$beta  + input$mu_delta_beta  +
      input$sigma_delta_beta  * rnorm(input$N)
    sigmastar = sqrt(samples$sigma^2 + input$sigma_sigma_star^2)

    ## Posterior predictive distribution
    ystar = alphastar + betastar * xstar + sigmastar * rnorm(input$N)

    ## Summarize discrepancy
    lwr = numeric(length(xx()))
    upr = numeric(length(xx()))
    for (i in 1:length(xx())) {
      buffer = alphastar + betastar * xx()[i] + sigmastar * rnorm(input$N)
      lwr[i] = quantile(buffer,     as.numeric(input$alpha))
      upr[i] = quantile(buffer, 1 - as.numeric(input$alpha))
    }

    ## Return predictions
    list(alphastar = alphastar, betastar = betastar, sigmastar = sigmastar,
         xstar = xstar, ystar = ystar, lwr = lwr, upr = upr)

  })

  ############
  ## Tables ##
  ############

  output$predictive_intervals = renderTable({

      ## Data
      xy = data()
      choices = names(xy)

      ## Skip table if no data is loaded
      if (is.null(xy) | ! input$x %in% choices |
          is.na(input$z) | is.na(input$sigma_z) |
          input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
          input$sigma_xstar <= 0 | input$sigma_sigma <= 0)
          return(NULL)

      pred_int = data.frame(numeric(2),numeric(2),numeric(2))
      colnames(pred_int) = c("Mean",paste0(100*as.numeric(input$alpha),"%"),
                             paste0(100*(1-as.numeric(input$alpha)),"%"))
      rownames(pred_int) = c("Exchangeable model","Coexchangeable model")

      pred_int[1,1] = mean(posterior()$ystar)
      pred_int[2,1] = mean(discrepancy()$ystar)
      pred_int[1,2:3] = quantile(posterior()$ystar,
                                 c(as.numeric(input$alpha),
                                   1 - as.numeric(input$alpha)))
      pred_int[2,2:3] = quantile(discrepancy()$ystar,
                                 c(as.numeric(input$alpha),
                                   1 - as.numeric(input$alpha)))

      return(pred_int)
  },
  rownames = TRUE
  )

  ##############
  ## Plotting ##
  ##############

  ## Data plot
  output$dataPlot <- renderPlot({

    ## Data
    xy = data()
    choices = names(xy)

    ## Skip plotting if no data is loaded
    if (is.null(xy) | ! input$x %in% choices)
      return(NULL)

    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3, ps = 12)

    # ## Create plot
    # p = ggplot() +
    #   geom_point(mapping = aes_string(x = input$x, y = input$y), data = xy) +
    #   labs(x = input$xlab, y = input$ylab)
    #
    # ## Add observations
    # if (is.numeric(input$z))
    #   p = p + geom_vline(mapping = aes_string(xintercept = input$z),
    #                      na.rm = TRUE, colour   = "blue", linetype = "dotdash")
    #
    # ## Plot data
    # p

    x = xy[,input$x]
    y = xy[,input$y]
    z = input$z

    ## Plot data
    plot(x, y, xlim = range(x, z, na.rm = TRUE), ylim = range(y),
         ann = FALSE, pch = 19)

    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)

    ## Add observations
    abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    # abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigma_z,
    #        col = "blue", lty = "dashed", lwd = 2)
    # abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigma_z,
    #        col = "blue", lty = "dashed", lwd = 2)

    ## Add legend
    legend("bottomright", legend = c("Models","Observation"),
           col = c("black","blue"), lty = c(NA,"dotdash"), lwd = c(2,2),
           pch = c(19,NA), bty = "n")

  })

  main_plot = function() {
    ## Data
    xy = data()
    choices = names(xy)

    ## Skip plotting if no data is loaded
    if (is.null(xy) | ! input$x %in% choices |
        is.na(input$z) | is.na(input$sigma_z) |
        input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
        input$sigma_xstar <= 0 | input$sigma_sigma <= 0)
      return(NULL)

    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")

    ## Data
    x = xy[,input$x]
    y = xy[,input$y]

    mask = sample.int(input$N, 1e3)

    plot(discrepancy()$xstar[mask], discrepancy()$ystar[mask],
         ann = FALSE, pch = 19, col = gray(0.75,0.25),
         xlim = input$xlim, ylim = input$ylim, xaxs = "i", yaxs = "i")

    points(x, y, pch = 19, col = "black")

    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    #title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    mtext("Emergent relationship fit", side = 3, adj = 0)

    ## Add linear regression
    lines(xx(), predictive()$fit, lty = "dotdash", lwd = 2)
    lines(xx(), predictive()$lwr, lty = "dashed" , lwd = 2)
    lines(xx(), predictive()$upr, lty = "dashed" , lwd = 2)

    ## Add discrepancy
    lines(xx(), discrepancy()$lwr, lty = "dashed" , col = "red", lwd = 2)
    lines(xx(), discrepancy()$upr, lty = "dashed" , col = "red", lwd = 2)

    ## Add observations
    abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$z + qnorm(    as.numeric(input$alpha)) * input$sigma_z,
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$z + qnorm(1 - as.numeric(input$alpha)) * input$sigma_z,
           col = "blue", lty = "dashed", lwd = 2)

    ## Add basic HPC CI
    dens = kde2d(posterior()$xstar, posterior()$ystar, n = 25)
    z = dens$z
    z = z/sum(z)
    o = order(z, decreasing = TRUE)
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]
    contour(dens$x, dens$y, z, levels = 1 - 2*as.numeric(input$alpha),
            drawlabels = FALSE, lwd = 2, lty = "dotted", add = TRUE)

    ## Add discrepancy HPC CI
    dens = kde2d(discrepancy()$xstar, discrepancy()$ystar, n = 25)
    z = dens$z
    z = z/sum(z)
    o = order(z, decreasing = TRUE)
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]
    contour(dens$x, dens$y, z, levels = 1 - 2*as.numeric(input$alpha),
            drawlabels = FALSE, col = "red", lwd = 2, lty = "dotted",
            add = TRUE)

    ## Add legend
    legend("bottomright",
           legend = c("Exchangeable model","Coexchangeable model",
                      "Observational constraint"),
           col = c("black","red","blue"),
           lty = c("dotdash","dotdash","dotdash"), lwd = c(2,2,2), bty = "n")

  }

  ## Main plot
  output$mainPlot = renderPlot(
    main_plot()
  )

  ## Auxilliary plot
  aux_plot = function() {
    ## Data
    xy = data()
    choices = names(xy)

    ## Skip plotting if no data is loaded
    if (is.null(xy) | ! input$x %in% choices |
        is.na(input$z) | is.na(input$sigma_z) |
        input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
        input$sigma_xstar <= 0 | input$sigma_sigma <= 0)
      return(NULL)

    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")

    ## Marginal distribution
    dens0 = density(xy[,input$y])

    ## Basic projection
    dens1 = density(posterior()$ystar)

    ## Discrepancy projection
    dens2 = density(discrepancy()$ystar)

    ymax = max(dens0$y,dens1$y,dens2$y)*1.04

    xlim = range(dens0$x,dens1$x,dens2$x)

    ## Plot data
    plot(dens0, xlim = xlim, ylim = c(0,ymax),
         col = "green", ann = FALSE, type = "l", lwd = 2,
         xaxs = "i", yaxs = "i")
    lines(dens1, lwd = 2, col = "black")
    lines(dens2, lwd = 2, col = "red")

    ## Add titles
    title(xlab = input$ylab)
    title(ylab = "Density")
    # title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    # mtext("Emergent relationship fit", side = 3, adj = 0)

    # ## Add quantiles
    # abline(v = mean(data()[,input$y]),
    #        col = "green", lwd = 2, lty = "dotdash")
    # abline(v = quantile(data()[,input$y],     as.numeric(input$alpha)),
    #        col = "green", lwd = 2, lty = "dashed")
    # abline(v = quantile(data()[,input$y], 1 - as.numeric(input$alpha)),
    #        col = "green", lwd = 2, lty = "dashed")
    #
    # ## Add quantiles
    # abline(v = mean(posterior()$ystar), col = "black", lwd = 2, lty = "dotdash")
    # abline(v = quantile(posterior()$ystar,     as.numeric(input$alpha)),
    #        col = "black", lwd = 2, lty = "dashed")
    # abline(v = quantile(posterior()$ystar, 1 - as.numeric(input$alpha)),
    #        col = "black", lwd = 2, lty = "dashed")
    #
    # ## Add quantiles
    # abline(v = mean(discrepancy()$ystar),
    #        col = "red", lwd = 2, lty = "dotdash")
    # abline(v = quantile(discrepancy()$ystar,     as.numeric(input$alpha)),
    #        col = "red", lwd = 2, lty = "dashed")
    # abline(v = quantile(discrepancy()$ystar, 1 - as.numeric(input$alpha)),
    #        col = "red", lwd = 2, lty = "dashed")

    ## Add legend
    legend("topright",
           legend = c("Model response","Exchangeable model",
                      "Coexchangeable model"),
           col = c("green","black","red"), lty = c("solid","solid","solid"),
           lwd = c(2,2,2), bty = "n")
  }

  ## Auxilliary plot
  output$auxPlot <- renderPlot(
    aux_plot()
  )

  ## Joint prior plot
  output$priorPlot <- renderPlot({

    if(is.na(input$mu_alpha) | is.na(input$sigma_alpha) | input$sigma_alpha < 0 |
       is.na(input$mu_beta ) | is.na(input$sigma_beta ) | input$sigma_beta  < 0 |
       is.na(input$mu_sigma) | is.na(input$sigma_sigma) | input$sigma_sigma < 0 |
       is.na(input$mu_xstar) | is.na(input$sigma_xstar) | input$sigma_xstar < 0)
      return(NULL)

    ## Simulate from prior
    alpha =     rnorm(input$N, input$mu_alpha, input$sigma_alpha)
    beta  =     rnorm(input$N, input$mu_beta , input$sigma_beta )
    sigma = abs(rnorm(input$N, input$mu_sigma, input$sigma_sigma))

    ## Simulate from prior predictive distribution
    fit = length(xx())
    lwr = length(xx())
    upr = length(xx())
    for (i in 1:length(xx())) {
      buffer = alpha + beta * xx()[i] + sigma * rnorm(input$N)
      fit[i] = mean(buffer)
      lwr[i] = quantile(buffer,     as.numeric(input$alpha))
      upr[i] = quantile(buffer, 1 - as.numeric(input$alpha))
    }

    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3)

    ## Plot data
    plot(xx(), fit, type = "l", ann = FALSE, lty = "dotdash", lwd = 2,
         ylim = range(lwr,upr), xaxs = "i", yaxs = "i")

    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    #title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    mtext("Joint prior distribution", side = 3, adj = 0)

    ## Plot prior predictive distribution
    lines(xx(), lwr, lty = "dashed" , lwd = 2)
    lines(xx(), upr, lty = "dashed" , lwd = 2)

  }) ## priorPlot

  ## Mean/intercept prior
  output$alphaPlot <- renderPlot({

    if (is.na(input$mu_alpha) | is.na(input$sigma_alpha) | input$sigma_alpha <= 0)
      return(NULL)

    ## Plotting parameters
    par(las = 1, mar = c(2.5,4.0,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")

    mu    = input$mu_alpha
    sigma = input$sigma_alpha
    xmin  = qnorm(pnorm(-4), mu, sigma)
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(xmin, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma)
    ymax  = 1.04*max(yy)

    ## Plot density
    plot(xx, yy, type = "l", xlim = c(xmin,xmax), ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")

    ## Add titles
    title(xlab = expression(paste("Intercept ", alpha)))
    title(ylab = "Density", line = 3.0)

    ## Add quantiles
    abline(v = mu, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = mu + qnorm(    as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = mu + qnorm(1 - as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)

  }) ## alphaPlot

  ## Slope prior
  output$betaPlot <- renderPlot({

    if (is.na(input$mu_beta) | is.na(input$sigma_beta) | input$sigma_beta <= 0)
      return(NULL)

    ## Plotting parameters
    par(las = 1, mar = c(2.5,4.0,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")

    mu    = input$mu_beta
    sigma = input$sigma_beta
    xmin  = qnorm(pnorm(-4), mu, sigma)
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(xmin, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma)
    ymax  = 1.04*max(yy)

    ## Plot density
    plot(xx, yy, type = "l", xlim = c(xmin,xmax), ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")

    ## Add titles
    title(xlab = expression(paste("Slope ", beta)))
    title(ylab = "Density", line = 3.0)

    ## Add quantiles
    abline(v = mu, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = mu + qnorm(    as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = mu + qnorm(1 - as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)

  }) ## betaPlot

  ## Sigma prior
  output$sigmaPlot <- renderPlot({

    if (is.na(input$mu_sigma) | is.na(input$sigma_sigma) | input$sigma_sigma <= 0)
      return(NULL)

    ## Plotting parameters
    par(las = 1, mar = c(2.5,4.0,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")

    mu    = input$mu_sigma
    sigma = input$sigma_sigma
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(0, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma) + dnorm(-xx, mu, sigma)
    ymax  = 1.04*max(yy)

    ## Plot density
    plot(xx, yy, type = "l", xlim = c(0,xmax), ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")

    ## Add titles
    title(xlab = expression(paste("Response spread ", sigma)))
    title(ylab = "Density", line = 3.0)

    ## Add quantiles
    abline(v = mu, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = mu + qnorm(    as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = mu + qnorm(1 - as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)

  }) ## sigmaPlot

  ## xstar prior
  output$xstarPlot <- renderPlot({

    if (is.na(input$mu_xstar) | is.na(input$sigma_xstar) | input$sigma_xstar <= 0)
      return(NULL)

    ## Plotting parameters
    par(las = 1, mar = c(2.5,4.0,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")

    mu    = input$mu_xstar
    sigma = input$sigma_xstar
    xmin  = qnorm(pnorm(-4), mu, sigma)
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(xmin, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma)
    ymax  = 1.04*max(yy)

    ## Plot density
    plot(xx, yy, type = "l", xlim = c(xmin,xmax), ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")

    ## Add titles
    title(xlab = expression(paste("Real world predictor ", X["*"])))
    title(ylab = "Density", line = 3.0)

    ## Add quantiles
    abline(v = mu, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = mu + qnorm(    as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = mu + qnorm(1 - as.numeric(input$alpha)) * sigma,
           col = "blue", lty = "dashed", lwd = 2)

  }) ## xstarPlot

}
