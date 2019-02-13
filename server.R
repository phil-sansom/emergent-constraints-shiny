server = function(input, output) {
  
  ##########################
  ## Reactive UI elements ##
  ##########################

  output$predictor = renderUI({
    selectInput(inputId = "predictor", 
                label   = withMathJax("Predictor \\(X_m\\)"), 
                choices = colnames(data()))
  })

  output$response = renderUI({
    selectInput(inputId = "response", 
                label   = withMathJax("Response \\(Y_m\\)"), 
                choices = colnames(data()))
  })

  output$xlab = renderUI({
    textInput(inputId = "xlab", label = "Predictor label", 
              value = input$predictor)
  })

  output$ylab = renderUI({
    textInput(inputId = "ylab", label = "Response label", 
              value = input$response)
  })
  
  output$xmin = renderUI({
    v       = data()[,input$predictor]
    vmin    = min(v)
    vmax    = max(v)
    vrange  = vmax - vmin
    vstep   = 10^floor(log10(vrange))
    vmin    = vmin - 0.04 * vrange
    vmin    = floor  (vmin / vstep) * vstep

    numericInput(inputId = "xmin", 
                 label   = "X min",
                 value   = vmin,
                 step    = vstep)
  })

  output$xmax = renderUI({
    v       = data()[,input$predictor]
    vmin    = min(v)
    vmax    = max(v)
    vrange  = vmax - vmin
    vstep   = 10^floor(log10(vrange))
    vmax    = vmax + 0.04 * vrange
    vmax    = ceiling(vmax / vstep) * vstep
    
    numericInput(inputId = "xmax", 
                 label   = "X max",
                 value   = vmax,
                 step    = vstep)
  })
  
  output$ymin = renderUI({
    v       = data()[,input$response]
    vmin    = min(v)
    vmax    = max(v)
    vrange  = vmax - vmin
    vstep   = 10^floor(log10(vrange))
    vmin    = vmin - 0.04 * vrange
    vmin    = floor  (vmin / vstep) * vstep
    
    numericInput(inputId = "ymin", 
                 label   = "Y min",
                 value   = vmin,
                 step    = vstep)
  })
  
  output$ymax = renderUI({
    v       = data()[,input$response]
    vmin    = min(v)
    vmax    = max(v)
    vrange  = vmax - vmin
    vstep   = 10^floor(log10(vrange))
    vmax    = vmax + 0.04 * vrange
    vmax    = ceiling(vmax / vstep) * vstep
    
    numericInput(inputId = "ymax", 
                 label   = "Y max",
                 value   = vmax,
                 step    = vstep)
  })
  
  
  ##########################
  ## Data and computation ##
  ##########################
  
  ## Data
  data = reactive({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
    
  })
  
  ## Plotting points
  xx  = reactive(seq(input$xmin, input$xmax, length.out = 100))
  xxx = reactive(seq(input$xmin, input$xmax, length.out = 100))
  yyy = reactive(seq(input$ymin, input$ymax, length.out = 100))
  
  ## Sample posterior  
  posterior = reactive({
    
    ## Data
    x = data()[,input$predictor]
    y = data()[,input$response]
    
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
    extract(buffer, c("alpha","beta","sigma","xstar"))
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
    sigmastar = sqrt(samples$sigma^2 + input$sigmad^2)

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
  
  
  ##############
  ## Plotting ##
  ##############
  
  ## Data plot
  output$dataPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    ## Data
    x = data()[,input$predictor]
    y = data()[,input$response]
    
    ## Plot data
    plot(x, y, xlim = c(input$xmin,input$xmax), ylim = c(input$ymin,input$ymax),
         ann = FALSE, pch = 19, xaxs = "i", yaxs = "i")
    
    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    #title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    #mtext("Emergent relationship fit", side = 3, adj = 0)
    
    # ## Observation density    
    # p      = dnorm(xxx(), input$z, input$sigma_z)
    # # ymax = 10^(floor(log10(max(p)))-1)
    # # ymax = ymax*ceiling(max(p)/ymax)
    # pmax   = max(p)
    # yrange = diff(input$ylim)
    # pscale = 0.5*yrange/pmax
    # 
    # ## Plot obs density
    # lines(xxx(), input$ylim[1] + p * pscale, col = "blue", lwd = 2)
    
    ## Add observations
    abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigma_z,
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigma_z,
           col = "blue", lty = "dashed", lwd = 2)
    
    # ## Add legend
    # legend("bottomright",
    #        legend = c("Linear regression","Observational constraint"),
    #        col = c("black","blue"), lty = c("dotdash","dotdash"), lwd = c(2,2),
    #        bty = "n")
    
  })
  
  ## Main plot
  output$ecPlot <- renderPlot({
    
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    ## Data
    x = data()[,input$predictor]
    y = data()[,input$response]
    
    ## Plot data
    plot(x, y, xlim = c(input$xmin,input$xmax), ylim = c(input$ymin,input$ymax),
         ann = FALSE, pch = 19, xaxs = "i", yaxs = "i")
    
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
    abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigma_z, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigma_z,
           col = "blue", lty = "dashed", lwd = 2)
    
    ## Add HPC CI
    dens = kde2d(discrepancy()$xstar, discrepancy()$ystar, n = 25)
    z = dens$z
    z = z/sum(z)
    o = order(z, decreasing = TRUE)
    for (i in 2:length(z))
      z[o[i]] = z[o[i]] + z[o[i-1]]
    contour(dens$x, dens$y, z, levels = 1 - 2*as.numeric(input$alpha),
            drawlabels = FALSE, col = 2, lwd = 2, add = TRUE)
    
    ## Add legend
    legend("bottomright",
           legend = c("Linear regression","Regression with discrepancy",
                      "Observational constraint"),
           col = c("black","red","blue"),
           lty = c("dotdash","dotdash","dotdash"), lwd = c(2,2,2), bty = "n")
    
  })
  
  ## Joint prior plot
  output$priorPlot <- renderPlot({
    
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    ## Plot data
    plot(NULL, NULL, type = "n", xlim = c(input$xmin,input$xmax), 
         ylim = c(input$ymin,input$ymax), ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    #title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    mtext("Joint prior distribution", side = 3, adj = 0)
    
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

    ## Plot prior predictive distribution
    lines(xx(), fit, lty = "dotdash", lwd = 2)
    lines(xx(), lwr, lty = "dashed" , lwd = 2)
    lines(xx(), upr, lty = "dashed" , lwd = 2)

  }) ## priorPlot
  
  ## Mean/intercept prior
  output$alphaPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(yyy(), input$mu_alpha, input$sigma_alpha)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(yyy(), p, type = "l", xlim = c(input$ymin,input$ymax), 
         ylim = c(0,ymax), lwd = 2, col = "red", ann = FALSE, 
         xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$mu_alpha, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$mu_alpha + 
             qnorm(    as.numeric(input$alpha)) * input$sigma_alpha, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$mu_alpha + 
             qnorm(1 - as.numeric(input$alpha)) * input$sigma_alpha,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## alphaPlot

  ## Slope prior
  output$betaPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(yyy(), input$mu_beta, input$sigma_beta)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(yyy(), p, type = "l", xlim = c(input$xmin,input$xmax), 
         ylim = c(0,ymax), lwd = 2, col = "red", ann = FALSE, 
         xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$mu_beta, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$mu_beta + 
             qnorm(    as.numeric(input$alpha)) * input$sigma_beta, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$mu_beta + 
             qnorm(1 - as.numeric(input$alpha)) * input$sigma_beta,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## betaPlot
  
  ## Sigma prior
  output$sigmaPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(yyy(), input$mu_sigma, input$sigma_sigma)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(yyy(), p, type = "l", xlim = c(input$ymin,input$ymax), 
         ylim = c(0,ymax), lwd = 2, col = "red", ann = FALSE, 
         xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$mu_sigma, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$mu_sigma + 
             qnorm(    as.numeric(input$alpha)) * input$sigma_sigma, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$mu_sigma + 
             qnorm(1 - as.numeric(input$alpha)) * input$sigma_sigma,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## sigmaPlot

  ## xstar prior
  output$xstarPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(xxx(), input$mu_xstar, input$sigma_xstar)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(xxx(), p, type = "l", xlim = c(input$xmin,input$xmax), 
         ylim = c(0,ymax), lwd = 2, col = "red", ann = FALSE, 
         xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$mu_xstar, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$mu_xstar + 
             qnorm(    as.numeric(input$alpha)) * input$sigma_xstar, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$mu_xstar + 
             qnorm(1 - as.numeric(input$alpha)) * input$sigma_xstar,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## xstarPlot
  
}
