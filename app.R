## Load libraries
library(MASS)
library(shiny)
library(rstan)
options(mc.cores = 4)             ## Set the number of CPU cores to use
rstan_options(auto_write = TRUE)  ## Prevent STAN recompiling unnecessarily

## Compile STAN model
model = stan_model("app.stan")

###############
## Define UI ##
###############

ui <- fluidPage(
  title = "Uncertainty quantification for emergent constraints",
  titlePanel("Uncertainty quantification for emergent constraints"),
  withMathJax(),
  tabsetPanel(
    
    ## Overview tab
    tabPanel("Overview",
             mainPanel(
               h1("Statistical model:"),
               p("$$ \\begin{align} 
                       Y_m & = \\alpha + \\beta X_m + \\epsilon_m & 
                         \\epsilon_m & \\sim 
                           \\text{Normal} ( 0, \\sigma^2 ) \\quad
                         (m = 1,\\ldots,M) & 
                         \\text{Multi-model ensemble} \\\\
                       Y_\\star & = \\alpha_\\star + \\beta_\\star X_\\star + 
                           \\epsilon_\\star &
                         \\epsilon_\\star & \\sim 
                           \\text{Normal} ( 0, \\sigma_\\star^2 ) &
                         \\text{Real world} \\\\
                       Z & = X_\\star + \\omega &
                         \\omega & \\sim \\text{Normal} ( 0, \\sigma_Z^2 ) &
                         \\text{Observations}
                     \\end{align} $$"),
               h2("Priors"),
               p("$$ \\begin{align}
                       \\alpha  & \\sim \\text{Normal} 
                         (\\mu_\\alpha, \\sigma_\\alpha^2 ) &
                         \\text{Ensemble intercept} \\\\
                       \\beta   & \\sim \\text{Normal}
                         (\\mu_\\beta , \\sigma_\\beta^2  ) &
                         \\text{Ensemble slope} \\\\
                       \\sigma  & \\sim \\text{Half-Normal} 
                         (\\mu_\\sigma, \\sigma_\\sigma^2 ) &
                         \\text{Ensemble response uncertainty} \\\\
                       X_\\star & \\sim \\text{Normal}
                         (\\mu_{X_\\star} , \\sigma_{X_\\star}^2  ) &
                         \\text{Real world predictor}
                     \\end{align} $$"),
               h2("Discrepancies"),
               p("$$ \\begin{align}
                       \\alpha_\\star & = \\alpha + \\delta_\\alpha &
                          \\delta_\\alpha & \\sim \\text{Normal} 
                            (\\mu_{\\delta_\\alpha}, 
                              \\sigma_{\\delta_\\alpha}^2) &
                         \\text{Real world Intercept} \\\\
                       \\beta_\\star & = \\beta + \\delta_\\beta &
                          \\delta_\\beta & \\sim \\text{Normal} 
                            (\\mu_{\\delta_\\beta}, 
                              \\sigma_{\\delta_\\beta}^2) &
                         \\text{Real world slope} \\\\
                       \\sigma_\\star^2 & = 
                           \\sigma^2 + \\sigma_{\\sigma_\\star}^2 & & &
                         \\text{Real world response uncertainty} \\\\
                     \\end{align} $$")
             )
    ),
    
    ## Data tab
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 uiOutput("predictor"),
                 uiOutput("response"),
                 uiOutput("xlab"),
                 uiOutput("ylab"),
                 uiOutput("xlim"),
                 uiOutput("ylim")
               ),
               mainPanel(
                 plotOutput(outputId = "dataPlot")
               )
             )
    ),
    
    ## Observation tab
    tabPanel("Observations",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "z"     , 
                              label = "Observation 
                                       \\(Z\\):",
                              value = 0.13, step = 0.01),
                 numericInput(inputId = "sigma_z", 
                              label = "Observation uncertainty 
                                       \\(\\sigma_Z\\):",
                              min = 0, value = 0.016, step = 0.001)
               ),
               mainPanel(
                 plotOutput(outputId = "obsPlot")
               )
             )
    ),
    
    ## Prior panel
    tabPanel("Priors",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "mu_alpha", 
                              label = "Intercept mean
                                       \\(\\mu_\\alpha\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_alpha", 
                              label = "Intercept uncertainty
                                       \\(\\sigma_\\alpha\\):",
                              value = 1e3, step = 1),
                 numericInput(inputId = "mu_beta", 
                              label = "Slope mean
                                       \\(\\mu_\\beta\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_beta", 
                              label = "Slope uncertainty
                                       \\(\\sigma_\\beta\\):",
                              value = 1e3, step = 1),
                 numericInput(inputId = "mu_sigma", 
                              label = "Spread mean
                                       \\(\\mu_\\sigma\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_sigma", 
                              label = "Spread uncertainty
                                       \\(\\sigma_\\sigma\\):",
                              value = 1e3, step = 1),
                 numericInput(inputId = "mu_xstar", 
                              label = "Real world predictor mean
                                       \\(\\mu_{X_\\star}\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_xstar", 
                              label = "Real world predictor uncertainty
                                       \\(\\sigma_{X_\\star}\\):",
                              value = 1e3, step = 1)
               ),
               mainPanel(
                 fluidRow(
                   plotOutput(outputId = "priorPlot", height = 400)
                 ),
                 fluidRow(
                   column(6, plotOutput(outputId = "alphaPlot", height = 200)),
                   column(6, plotOutput(outputId = "betaPlot" , height = 200))
                 ),
                 fluidRow(
                   column(6, plotOutput(outputId = "sigmaPlot", height = 200)),
                   column(6, plotOutput(outputId = "xstarPlot", height = 200))
                 )
               )
             )
    ),
    
    ## Discrepancy tab
    tabPanel("Discrepancy",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "mu_delta_alpha", 
                             label = "Intercept bias 
                                      \\(\\mu_{\\delta_\\alpha}\\):",
                             min = 0, max =  3, value = 0, step = 0.1),
                 sliderInput(inputId = "sigma_delta_alpha", 
                             label = "Intercept uncertainty
                                      \\(\\sigma_{\\delta_\\alpha}\\):",
                             min = 0, max =  3, value = 0, step = 0.1),
                 sliderInput(inputId = "mu_delta_beta", 
                             label = "Slope bias 
                                      \\(\\mu_{\\delta_\\beta}\\):",
                             min = 0, max =  10, value = 0, step = 0.5),
                 sliderInput(inputId = "sigma_delta_beta", 
                             label = "Slope uncertainty
                                      \\(\\sigma_{\\delta_\\beta}\\):",
                             min = 0, max = 10, value = 0, step = 0.5),
                 sliderInput(inputId = "sigmad", 
                             label = "Spread uncertainty
                                      \\(\\sigma_{\\sigma_\\star}\\):",
                             min = 0, max =  3, value = 0, step = 0.1),
                 numericInput(inputId = "N", label = "Number of samples:",
                              value = 10000),
                 # numericInput(inputId = "mc.cores", label = "Number of cores:",
                 #              min = 1, max = 16, value = 4, step = 1),
                 selectInput(inputId = "alpha", label = "Credible interval:",
                             choices = list("68% (1 sd)" = 0.16,
                                            "90%"        = 0.05,
                                            "95% (2 sd)" = 0.025,
                                            "99%"        = 0.005),
                             selected = 0.05)
               ),
               mainPanel(
                 plotOutput(outputId = "ecPlot")
               )
             )
    )
    
  ) ## tabsetPanel
) ## ui


#########################
## Define server logic ##
#########################

server <- function(input, output) {
  
  ##########################
  ## Reactive UI elements ##
  ##########################

  output$predictor = renderUI({
    selectInput(inputId = "predictor", label = "Predictor", 
                choices = colnames(data()))
  })

  output$response = renderUI({
    selectInput(inputId = "response", label = "Response", 
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
  
  output$xlim = renderUI({
    x       = data()[,input$predictor]
    xrange  = range(x)
    xdiff   = diff(xrange)
    xmean   = mean(x)
    xstep   = 10^floor(log10(xdiff))
    xmin    = xrange[1] - xdiff
    xmax    = xrange[2] + xdiff
    xminval = xrange[1] - 0.04 * xdiff
    xmaxval = xrange[2] + 0.04 * xdiff
    xmin    = floor  (xmin   /xstep)*xstep
    xmax    = ceiling(xmax   /xstep)*xstep
    xminval = floor  (xminval/xstep)*xstep
    xmaxval = ceiling(xmaxval/xstep)*xstep
    
    sliderInput(inputId = "xlim", label = "X limits",
                min = xmin, max = xmax, value = c(xminval,xmaxval), 
                step = xstep)
  })

  output$ylim = renderUI({
    y       = data()[,input$response]
    yrange  = range(y)
    ydiff   = diff(yrange)
    ymean   = mean(y)
    ystep   = 10^floor(log10(ydiff))
    ymin    = yrange[1] - ydiff
    ymax    = yrange[2] + ydiff
    yminval = yrange[1] - 0.04 * ydiff
    ymaxval = yrange[2] + 0.04 * ydiff
    ymin    = floor  (ymin   /ystep)*ystep
    ymax    = ceiling(ymax   /ystep)*ystep
    yminval = floor  (yminval/ystep)*ystep
    ymaxval = ceiling(ymaxval/ystep)*ystep
    
    sliderInput(inputId = "ylim", label = "Y limits",
                min = ymin, max = ymax, value = c(yminval,ymaxval), 
                step = ystep)
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
  xx  = reactive(seq(input$xlim[1], input$xlim[2], length.out = 100))
  xxx = reactive(seq(input$xlim[1], input$xlim[2], length.out = 100))
  yyy = reactive(seq(input$ylim[1], input$ylim[2], length.out = 100))
  
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
    plot(x, y, xlim = input$xlim, ylim = input$ylim, ann = FALSE, pch = 16,
         xaxs = "i", yaxs = "i", cex = 2)
    
    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    #title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    #mtext("Emergent relationship fit", side = 3, adj = 0)
    
    # ## Add observations
    # abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    # abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigma_z, 
    #        col = "blue", lty = "dashed", lwd = 2)
    # abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigma)z,
    #        col = "blue", lty = "dashed", lwd = 2)
    # 
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
    plot(x, y, xlim = input$xlim, ylim = input$ylim, ann = FALSE, pch = 16,
         xaxs = "i", yaxs = "i", cex = 2)
    
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
  
  ## Observation uncertainty
  output$obsPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
   
    ## Data
    x = data()[,input$predictor]
    y = data()[,input$response]
    
    ## Plot data
    plot(x, y, xlim = input$xlim, ylim = input$ylim, ann = FALSE, pch = 16,
         xaxs = "i", yaxs = "i", cex = 2)
    
    ## Add titles
    title(xlab = input$xlab)
    title(ylab = input$ylab)
    
    p      = dnorm(xxx(), input$z, input$sigma_z)
    # ymax = 10^(floor(log10(max(p)))-1)
    # ymax = ymax*ceiling(max(p)/ymax)
    pmax   = max(p)
    yrange = diff(input$ylim)
    pscale = 0.5*yrange/pmax
    
    ## Plot obs density
    lines(xxx(), input$ylim[1] + p * pscale, col = "blue", lwd = 2)
    
    ## Add quantiles
    abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    # abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigma_z,
    #        col = "blue", lty = "dashed", lwd = 2)
    # abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigma_z,
    #        col = "blue", lty = "dashed", lwd = 2)

  }) ## obsPlot
  
  ## Joint prior plot
  output$priorPlot <- renderPlot({
    
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    ## Plot data
    plot(NULL, NULL, type = "n", xlim = input$xlim, ylim = input$ylim, 
         ann = FALSE, xaxs = "i", yaxs = "i")
    
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
    plot(yyy(), p, type = "l", xlim = input$ylim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
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
    plot(yyy(), p, type = "l", xlim = input$xlim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
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
    plot(yyy(), p, type = "l", xlim = input$ylim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
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
    plot(xxx(), p, type = "l", xlim = input$xlim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$mu_xstar, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$mu_xstar + 
             qnorm(    as.numeric(input$alpha)) * input$sigma_xstar, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$mu_xstar + 
             qnorm(1 - as.numeric(input$alpha)) * input$sigma_xstar,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## xstarPlot
  
} ## server

## Run the app
shinyApp(ui = ui, server = server, options = "launch.browser = TRUE")

