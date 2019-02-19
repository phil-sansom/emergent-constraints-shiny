server = function(input, output, session) {

  ###############
  ## Observers ##
  ###############

  ## Update predictor choices when data loaded
  observe({
    choices = names(data())
    updateSelectInput(session  = session,
                      inputId  = "x",
                      choices  = choices,
                      selected = choices[1]
    )
  }) ## observe

  ## Update response choices when data loaded
  observe({
    choices = names(data())
    updateSelectInput(session  = session,
                      inputId  = "y",
                      choices  = choices,
                      selected = choices[2]
    )
  }) ## observe

  ## Update predictor label when predictor changes
  observe(
    updateTextInput(session  = session,
                    inputId  = "xlab",
                    value    = input$x
    )
  ) ## observe

  ## Update response label when response changes
  observe(
    updateTextInput(session  = session,
                    inputId  = "ylab",
                    value    = input$y
    )
  ) ## observe

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
    ) ## updateSliderInput
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
    ) ## updateSliderInput
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
    ) ## updateSliderInput
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
    ) ## updateSliderInput
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
    ) ## updateSliderInput
  }) ## observe

  ## Update marginal plot limits
  observe ({

    limits = ylim_posterior()

    updateSliderInput(session = session,
                      inputId = "xlim_marginal",
                      value   = limits$value,
                      min     = limits$min,
                      max     = limits$max,
                      step    = limits$step
    ) ## updateSliderInput

  })

  ## Update joint plot limits
  observe ({

    limits = ylim_posterior()

    updateSliderInput(session = session,
                      inputId = "ylim_joint",
                      value   = limits$value,
                      min     = limits$min,
                      max     = limits$max,
                      step    = limits$step
    ) ## updateSliderInput

  })

  ## Update joint plot limits
  observe ({

    limits = xlim_posterior()

    updateSliderInput(session = session,
                      inputId = "xlim_joint",
                      value   = limits$value,
                      min     = limits$min,
                      max     = limits$max,
                      step    = limits$step
    ) ## updateSliderInput

  })

  ###############
  ## Downloads ##
  ###############

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
      if (is.null(data()) | ! input$x %in% names(data()) |
          is.na(input$z) | is.na(input$sigma_z) |
          input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
          input$sigma_xstar <= 0 | input$sigma_sigma <= 0) {
        write.csv(NULL, file = file, row.names = FALSE)
      } else {
        write.csv(cbind(data.frame(posterior  ()[c("alpha","beta","sigma")]),
                        data.frame(discrepancy())),
                  file = file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
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

  ## Update X limits
  xlim = reactive({

    xy      = data()
    choices = names(xy)
    if (is.null(xy) | ! input$x %in% choices)
      return(list(min = 0, max = 1, val = c(0,1), step = 0.1))
    v       = xy[,input$x]

    vrange  = range(v, input$z, na.rm = TRUE)
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

  })

  ## Update Y limits
  ylim = reactive({

    xy = data()
    choices = names(xy)
    if (is.null(xy) | ! input$y %in% choices)
      return(list(min = 0, max = 1, val = c(0,1), step = 0.1))
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
    vstep   = 10^floor(log10(vdiff)-1)

    return(list(value = c(vminval,vmaxval), min = vmin, max = vmax,
                step = vstep))

  })

  ## Update posterior X limits
  xlim_posterior = reactive({

    xy      = data()
    choices = names(xy)
    ## Return defaults if problem
    if (is.null(xy) | ! input$x %in% choices |
        is.na(input$z) | is.na(input$sigma_z) |
        input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
        input$sigma_xstar <= 0 | input$sigma_sigma <= 0) {

      vminval = 0
      vmaxval = 1
      vmin    = 0
      vmax    = 1
      vstep   = 0.1

    } else {

      vrange  = range(xy[,input$x],posterior()$xstar,discrepancy()$xstar)
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

    }

    return(list(value = c(vminval,vmaxval),
                min = vmin, max = vmax, step = vstep))

  })

  ## Update posterior Y limits
  ylim_posterior = reactive({

    xy      = data()
    choices = names(xy)
    ## Return defaults if problem
    if (is.null(xy) | ! input$x %in% choices |
        is.na(input$z) | is.na(input$sigma_z) |
        input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
        input$sigma_xstar <= 0 | input$sigma_sigma <= 0) {

      vminval = 0
      vmaxval = 1
      vmin    = 0
      vmax    = 1
      vstep   = 0.1

    } else {

      vrange  = range(xy[,input$y],posterior()$ystar,discrepancy()$ystar)
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

    }

    return(list(value = c(vminval,vmaxval),
                min = vmin, max = vmax, step = vstep))

  })

  ## Subset samples for plotting
  mask = reactive(sample.int(input$N, 1e3))

  ## Plotting points
  xx  = reactive(seq(xlim()$min, xlim()$max, length.out = 101))

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
      lwr[i]  = quantile(buffer, 0.5*(1 - as.numeric(input$gamma)))
      upr[i]  = quantile(buffer, 0.5*(1 + as.numeric(input$gamma)))
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

    ## Return predictions
    list(alphastar = alphastar, betastar = betastar, sigmastar = sigmastar,
         xstar = xstar, ystar = ystar)

  })

  ## Compute predictive discrepancy
  predictive_discrepancy = reactive({

    ## Summarize discrepancy
    fit = length(xx())
    lwr = length(xx())
    upr = length(xx())
    for (i in 1:length(xx())) {
      buffer = discrepancy()$alphastar + discrepancy()$betastar * xx()[i] +
        discrepancy()$sigmastar * rnorm(input$N)
      fit[i]  = mean(buffer)
      lwr[i]  = quantile(buffer, 0.5*(1 - as.numeric(input$gamma)))
      upr[i]  = quantile(buffer, 0.5*(1 + as.numeric(input$gamma)))
    }

    ## Return predictions
    list(fit = fit, lwr = lwr, upr = upr)

  })


  ############
  ## Tables ##
  ############

  output$predictive_intervals = renderTable(
      {   ## Data
          xy = data()
          choices = names(xy)

          ## Skip table if no data is loaded
          if (is.null(xy) | ! input$x %in% choices |
              is.na(input$z) | is.na(input$sigma_z) | input$sigma_z <= 0 |
              input$sigma_alpha <= 0 | input$sigma_beta  <= 0 |
              input$sigma_xstar <= 0 | input$sigma_sigma <= 0)
              return(NULL)

          pred_int = data.frame(numeric(2),numeric(2),numeric(2))
          colnames(pred_int) = c("Mean",
                                 paste0(100*0.5*(1-as.numeric(input$gamma)),"%"),
                                 paste0(100*0.5*(1+as.numeric(input$gamma)),"%"))
          rownames(pred_int) = c("Basic model",
                                 "Conditionally exchangeable model")

          pred_int[1,1  ] = mean(posterior()$ystar)
          pred_int[2,1  ] = mean(discrepancy()$ystar)
          pred_int[1,2:3] = quantile(posterior()$ystar,
                                     c(0.5*(1 - as.numeric(input$gamma)),
                                       0.5*(1 + as.numeric(input$gamma))))
          pred_int[2,2:3] = quantile(discrepancy()$ystar,
                                     c(0.5*(1 - as.numeric(input$gamma)),
                                       0.5*(1 + as.numeric(input$gamma))))

          return(pred_int)
      },
      rownames = TRUE
  )


  ##############
  ## Plotting ##
  ##############

  ## User defined colours
  alpha_black = rgb(0, 0, 0, 0.5)
  alpha_red   = rgb(1, 0, 0, 0.5)
  alpha_green = rgb(0, 1, 0, 0.5)
  alpha_blue  = rgb(0, 0, 1, 0.5)

  ## Graphical parameters
  graphical_parameters = function()
      par(ann = FALSE, las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0),
          ps = 12, tcl = -1/3, xaxs = "i", yaxs = "i")

  ## Normal prior plot
  normal_prior_plot = function(mu, sigma, xlab, ylab, ...) {

    ## Graphical parameters
    graphical_parameters()
    dots = list(...)
    if (exists("par", where = dots))
      par(dots$par)

    ## Limits
    xmin  = qnorm(pnorm(-4), mu, sigma)
    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(xmin, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma)
    ymax  = 1.04*max(yy)

    xp = seq(qnorm(0.5*(1 - as.numeric(input$gamma)), mu, sigma),
             qnorm(0.5*(1 + as.numeric(input$gamma)), mu, sigma), length.out = 101)
    yp = dnorm(xp, mu, sigma)

    ## Plot density
    plot (xx, yy, type = "n", xlim = c(xmin,xmax), ylim = c(0,ymax))
    polygon(x = c(xp[1],xp,xp[101]), y = c(0,yp,0),
            border = NA, col = alpha_red)
    lines(xx, yy, col = "red", lwd = 2)

    ## Add titles
    title(xlab = xlab)
    title(ylab = ylab, line = 3.0)

  } ## normal_prior_plot

  ## Folded normal prior plot
  folded_normal_prior_plot = function(mu, sigma, xlab, ylab, ...) {

    ## Graphical parameters
    graphical_parameters()
    dots = list(...)
    if (exists("par", where = dots))
      par(dots$par)

    xmax  = qnorm(pnorm(+4), mu, sigma)
    xx    = seq(0, xmax, length.out = 101)
    yy    = dnorm(xx, mu, sigma) + dnorm(xx, -mu, sigma)
    ymax  = 1.04*max(yy)

    cdf   = pnorm(xx, mu, sigma) + pnorm(xx, -mu, sigma) - 1
    xp    = seq(max(which(cdf < 0.5*(1 - as.numeric(input$gamma)))),
                min(which(cdf > 0.5*(1 + as.numeric(input$gamma)))), 1)

    ## Plot density
    plot (xx, yy, type = "n", xlim = c(0,xmax), ylim = c(0,ymax))
    polygon(x = c(xx[xp[1]],xx[xp],xx[xp[length(xp)]]), y = c(0,yy[xp],0),
            border = NA, col = alpha_red)
    lines(xx, yy, col = "red", lwd = 2)

    ## Add titles
    title(xlab = xlab)
    title(ylab = ylab, line = 3.0)

  } ## folded_normal_prior_plot

  ## Data plot
  output$data_plot = renderPlot({

    ## Data
    xy = data()
    choices = names(xy)

    ## Skip plotting if no data is loaded
    if (is.null(xy) | ! input$x %in% choices)
      return(NULL)

    ## Graphical parameters
    graphical_parameters()

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
    plot(x, y, xlim = xlim()$value, ylim = ylim()$value, pch = 19)

    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)

    ## Add observations
    abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)

    ## Add legend
    legend("bottomright", legend = c("Models","Observation"),
           col = c("black","blue"), lty = c(NA,"dotdash"), lwd = c(2,2),
           pch = c(19,NA), bty = "n")

  })

  ## Joint posterior predictive distribution
  joint_plot = function() {
    ## Data
    xy = data()
    choices = names(xy)

    ## Skip plotting if no data is loaded
    if (is.null(xy) | ! input$x %in% choices |
        is.na(input$z) | is.na(input$sigma_z) |
        input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
        input$sigma_xstar <= 0 | input$sigma_sigma <= 0)
      return(NULL)

    ## Graphical parameters
    graphical_parameters()

    ## Data
    x = xy[,input$x]
    y = xy[,input$y]

    ## Plot predictive point cloud
    plot(discrepancy()$xstar[mask()], discrepancy()$ystar[mask()],
         col = gray(0.75,0.25), pch = 19,
         xlim = input$xlim_joint, ylim = input$ylim_joint)

    ## Add data
    points(x, y, col = "black", pch = 19)

    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)

    ## Add linear regression
    lines(xx(), predictive()$fit, lty = "dotdash", lwd = 2)
    lines(xx(), predictive()$lwr, lty = "dashed" , lwd = 2)
    lines(xx(), predictive()$upr, lty = "dashed" , lwd = 2)

    ## Add discrepancy
    lines(xx(), predictive_discrepancy()$fit,
          col = "red", lty = "dotdash", lwd = 2)
    lines(xx(), predictive_discrepancy()$lwr,
          col = "red", lty = "dashed" , lwd = 2)
    lines(xx(), predictive_discrepancy()$upr,
          col = "red", lty = "dashed" , lwd = 2)

    ## Add observations
    abline(v = mean    (posterior()$xstar),
           col = "blue", lty = "dotdash", lwd = 2)
    abline(v = quantile(posterior()$xstar, 0.5*(1 - as.numeric(input$gamma))),
           col = "blue", lty = "dashed" , lwd = 2)
    abline(v = quantile(posterior()$xstar, 0.5*(1 + as.numeric(input$gamma))),
           col = "blue", lty = "dashed" , lwd = 2)

    ## Add posterior density
    joint_density = kde2d(posterior()$xstar, posterior()$ystar, n = 25)
    z = joint_density$z
    z = z/sum(z)
    o = order(z, decreasing = TRUE)
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]
    joint_density$z = z
    contour(joint_density$x, joint_density$y, joint_density$z,
            levels = as.numeric(input$gamma), drawlabels = FALSE,
            lwd = 2, col = "black", lty = "dotted", add = TRUE)

    ## Add discrepancy density
    joint_density = kde2d(discrepancy()$xstar, discrepancy()$ystar, n = 25)
    z = joint_density$z
    z = z/sum(z)
    o = order(z, decreasing = TRUE)
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]
    joint_density$z = z
    contour(joint_density$x, joint_density$y, joint_density$z,
            levels = as.numeric(input$gamma), drawlabels = FALSE,
            lwd = 2, col = "red", lty = "dotted", add = TRUE)

    ## Add legend
    legend("bottomright",
           legend = c("Basic model","Conditionally exchangeable model",
                      "Observational constraint"),
           col = c("black","red","blue"),
           lty = c("dotdash","dotdash","dotdash"), lwd = c(2,2,2), bty = "n")

  } ## Joint posterior predictive distribution

  ## Joint posterior preditive plot
  output$joint_plot = renderPlot(
    joint_plot()
  )

  ## Marginal posterior predictive distribution
  marginal_plot = function() {
    ## Data
    xy = data()
    choices = names(xy)

    ## Skip plotting if no data is loaded
    if (is.null(xy) | ! input$x %in% choices |
        is.na(input$z) | is.na(input$sigma_z) |
        input$sigma_z <= 0 | input$sigma_alpha <= 0 | input$sigma_beta <= 0 |
        input$sigma_xstar <= 0 | input$sigma_sigma <= 0)
      return(NULL)

    ## Graphical parameters
    graphical_parameters()

    ## Basic projection
    dens1 = density (posterior()$ystar)
    x1m   = quantile(posterior()$ystar,
                     0.5 + c(-0.5,+0.5)*as.numeric(input$gamma))
    x1m   = seq(max(which(dens1$x < x1m[1])), min(which(dens1$x > x1m[2])), 1)

    ## Discrepancy projection
    dens2 = density (discrepancy()$ystar)
    x2m   = quantile(discrepancy()$ystar,
                     0.5 + c(-0.5,+0.5)*as.numeric(input$gamma))
    x2m   = seq(max(which(dens2$x < x2m[1])), min(which(dens2$x > x2m[2])), 1)

    ymax = max(dens1$y,dens2$y)*1.04

    xlim = input$xlim_marginal
    ylim = c(0,ymax)

    ## Plot data
    plot (xy[,input$y], type = "n", xlim = xlim, ylim = ylim)
    rug(xy[,input$y], ticksize = 0.02, lwd = 2, col = "black")
    polygon(x = c(dens1$x[x1m[1]],dens1$x[x1m],dens1$x[x1m[length(x1m)]]),
            y = c(0,dens1$y[x1m],0), border = NA, col = alpha_black)
    polygon(x = c(dens2$x[x2m[1]],dens2$x[x2m],dens2$x[x2m[length(x2m)]]),
            y = c(0,dens2$y[x2m],0), border = NA, col = alpha_red)

    lines(dens1, col = "black", lwd = 2)
    lines(dens2, col = "red"  , lwd = 2)

    ## Add titles
    title(xlab = input$ylab)
    title(ylab = "Density")

    ## Add legend
    legend("topright",
           legend = c("Basic model","Conditionally exchangeable model"),
           col = c("black","red"), lty = c("solid","solid"), lwd = c(2,2),
           bty = "n")

  } ## Marginal posterior predictive distribution

  ## Marginal posterior predictive plot
  output$marginal_plot = renderPlot(
    marginal_plot()
  )

  ## Joint prior plot
  output$joint_prior_plot = renderPlot({

    xy = data()
    choices = names(data)

    if(is.null(xy) | ! input$x %in% names(xy) |
       is.na(input$mu_alpha) | is.na(input$sigma_alpha) | input$sigma_alpha < 0 |
       is.na(input$mu_beta ) | is.na(input$sigma_beta ) | input$sigma_beta  < 0 |
       is.na(input$mu_sigma) | is.na(input$sigma_sigma) | input$sigma_sigma < 0 |
       is.na(input$mu_xstar) | is.na(input$sigma_xstar) | input$sigma_xstar < 0)
      return(NULL)

    x = xy[,input$x]
    y = xy[,input$y]
    z = input$z

    ## X limits
    xrange  = range(x, z, na.rm = TRUE)
    xdiff   = diff(xrange)
    xstep   = 10^floor(log10(xdiff))
    xmin    = xrange[1] - 0.04 * xdiff
    xmax    = xrange[2] + 0.04 * xdiff
    xmin    = floor  (xmin/xstep)*xstep
    xmax    = ceiling(xmax/xstep)*xstep
    xx      = seq(xmin, xmax, length.out = 101)

    ## Simulate from prior
    alpha =     rnorm(input$N, input$mu_alpha, input$sigma_alpha)
    beta  =     rnorm(input$N, input$mu_beta , input$sigma_beta )
    sigma = abs(rnorm(input$N, input$mu_sigma, input$sigma_sigma))

    ## Simulate from prior predictive distribution
    fit = length(xx)
    lwr = length(xx)
    upr = length(xx)
    for (i in 1:length(xx)) {
      buffer = alpha + beta * xx[i] + sigma * rnorm(input$N)
      fit[i] = mean(buffer)
      lwr[i] = quantile(buffer, 0.5*(1 - as.numeric(input$gamma)))
      upr[i] = quantile(buffer, 0.5*(1 + as.numeric(input$gamma)))
    }

    ## Graphical parameters
    graphical_parameters()

    ## Plot data
    plot(xx, fit, type = "l", xlim = c(xmin,xmax), ylim = range(lwr,upr),
         lty = "dotdash", lwd = 2, yaxs = "r")

    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)

    ## Plot prior predictive distribution
    lines(xx, lwr, lty = "dashed" , lwd = 2)
    lines(xx, upr, lty = "dashed" , lwd = 2)

  }) ## joint_prior_plot

  ## Mean/intercept prior
  output$alpha_plot = renderPlot({

    if (is.na(input$mu_alpha) | is.na(input$sigma_alpha) | input$sigma_alpha <= 0)
      return(NULL)

    normal_prior_plot(mu    = input$mu_alpha,
                      sigma = input$sigma_alpha,
                      xlab  = expression(paste("Intercept ", alpha)),
                      ylab  = "Density",
                      par   = list(mar = c(2.5,4.0,1,1)+0.1))

  }) ## alpha_plot

  ## Slope prior
  output$beta_plot = renderPlot({

    if (is.na(input$mu_beta) | is.na(input$sigma_beta) | input$sigma_beta <= 0)
      return(NULL)

    normal_prior_plot(mu    = input$mu_beta,
                      sigma = input$sigma_beta,
                      xlab  = expression(paste("Slope ", beta)),
                      ylab  = "Density",
                      par   = list(mar = c(2.5,4.0,1,1)+0.1))

  }) ## beta_plot

  ## Sigma prior
  output$sigma_plot = renderPlot({

    if (is.na(input$mu_sigma) | is.na(input$sigma_sigma) | input$sigma_sigma <= 0)
      return(NULL)

    folded_normal_prior_plot(mu    = input$mu_sigma,
                             sigma = input$sigma_sigma,
                             xlab  = expression(paste("Response spread ", sigma)),
                             ylab  = "Density",
                             par   = list(mar = c(2.5,4.0,1,1)+0.1))

  }) ## sigma_plot

  ## xstar prior
  output$xstar_plot = renderPlot({

    ## Error checking
    if (is.na(input$mu_xstar) | is.na(input$sigma_xstar) | input$sigma_xstar <= 0)
      return(NULL)

    normal_prior_plot(mu    = input$mu_xstar,
                      sigma = input$sigma_xstar,
                      xlab  = expression(paste("Real world predictor ", X["*"])),
                      ylab  = "Density",
                      par   = list(mar = c(2.5,4.0,1,1)+0.1))

  }) ## xstar_plot

}
