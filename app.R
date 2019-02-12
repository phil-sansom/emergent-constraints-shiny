## Load libraries
library(shiny)
library(rstan)
options(mc.cores = 4)             ## Set the number of CPU cores to use
rstan_options(auto_write = TRUE)  ## Prevent STAN recompiling unnecessarily

## Load data
source("data.R")
x      = data$psi[1:16]
y      = data$ecs[1:16]
z      = obs$psi.mean[1]
sigmaz = obs$psi.sd[1]
rm(data,obs,time.series)

## Compile STAN model
model = stan_model("app.stan")

## Define UI
ui <- fluidPage(
  titlePanel("Uncertainty quantification for emergent constraints"),
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Discrepancy",
                 sliderInput(inputId = "sigmam", label = 'Mean discrepancy:',
                             min = 0, max =  3, value = 0, step = 0.1),
                 sliderInput(inputId = "sigmab", label = "Slope discrepancy:",
                             min = 0, max = 10, value = 0, step = 0.5),
                 sliderInput(inputId = "sigmad", label = "Spread discrepancy:",
                             min = 0, max =  3, value = 0, step = 0.1)
        ),
        tabPanel("Priors",
                 # uiOutput("meanmu"),
                 # uiOutput("sdmu"),
                 # uiOutput("meanbeta"),
                 # uiOutput("sdbeta"),
                 # uiOutput("meansigma"),
                 # uiOutput("sdsigma"),
                 # uiOutput("meanxstar"),
                 # uiOutput("sdxstar")
                 numericInput(inputId = "meanmu"   , label = "Mean mu:",
                              value = 0, step = 1),
                 numericInput(inputId = "sdmu"     , label = "SD mu:",
                              value = 1e3, step = 1),
                 numericInput(inputId = "meanbeta" , label = "Mean beta:",
                              value = 0, step = 1),
                 numericInput(inputId = "sdbeta"   , label = "SD beta:",
                              value = 1e3, step = 1),
                 numericInput(inputId = "meansigma", label = "Mean sigma:",
                              value = 0, step = 1),
                 numericInput(inputId = "sdsigma"  , label = "SD sigma:",
                              value = 1e3, step = 1),
                 numericInput(inputId = "meanxstar", label = "Mean xstar:",
                              value = 0, step = 1),
                 numericInput(inputId = "sdxstar"  , label = "SD xstar:",
                              value = 1e3, step = 1)
                 
                 # sliderInput(inputId = "mu"   , label = 'Mean ECS:',
                 #             min = 0, max =  3, value = 0, step = 0.1),
                 # sliderInput(inputId = "beta" , label = "Slope:",
                 #             min = 0, max = 10, value = 0, step = 0.5),
                 # sliderInput(inputId = "sigma", label = "Spread:",
                 #             min = 0, max =  3, value = 0, step = 0.1),
                 # numericInput(inputId = "psi" , label = "Psi (K):",
                 #              min = 0, value = 0.15, step = 0.01)
        ),
        tabPanel("Observations",
                 numericInput(inputId = "z"     , label = "Observed Psi (K):",
                              min = 0, value = 0.13, step = 0.01),
                 numericInput(inputId = "sigmaz", label = "Observation SD (K):",
                              min = 0, value = 0.016, step = 0.001)
        ),
        tabPanel("Plotting",
                 numericInput(inputId = "N", label = "Number of samples:",
                              value = 10000),
                 # numericInput(inputId = "mc.cores", label = "Number of cores:",
                 #              min = 1, max = 16, value = 4, step = 1),
                 selectInput(inputId = "alpha", label = "Credible interval:",
                             choices = list("68% (1 sd)" = 0.16,
                                            "90%"        = 0.05,
                                            "95% (2 sd)" = 0.025,
                                            "99%"        = 0.005),
                             selected = 0.025),
                 sliderInput(inputId = "xlim", label = "X limits",
                             min = 0, max = 0.5, 
                             value = c(0.00,0.30), step = 0.01),
                 sliderInput(inputId = "ylim", label = "Y limits",
                             min = 0, max = 10,  value = c(0,6), step = 0.1)
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Discrepancy",
                 plotOutput(outputId = "ecPlot")
        ),
        tabPanel("Priors",
                fluidRow(
                  plotOutput(outputId = "priorPlot", height = 400)
                ),
                fluidRow(
                  column(6, plotOutput(outputId = "muPlot"  , height = 200)),
                  column(6, plotOutput(outputId = "betaPlot", height = 200))
                ),
                fluidRow(
                  column(6, plotOutput(outputId = "sigmaPlot", height = 200)),
                  column(6, plotOutput(outputId = "xstarPlot", height = 200))
                )
        ),
        tabPanel("Observations",
                 plotOutput(outputId = "obsPlot")
        )
        # tabPanel("Plotting",
        #          plotOutput(outputId = "ecPlot")
        # )
      ) ## tabsetPanel
    ) ## mainPanel
  ) ## sidebarLayout
) ## ui

## Define server logic
server <- function(input, output) {
  
  # ##########################
  # ## Reactive UI elements ##
  # ##########################
  # 
  # output$meanmu = renderUI({
  #   sliderInput(inputId = "meanmu"   , label = 'Mean mu:',
  #               min = input$ylim[1], max = input$ylim[2], 
  #               value = mean(input$ylim), step = 0.1)
  # })
  # 
  # output$sdmu = renderUI({
  #   sliderInput(inputId = "sdmu"   , label = 'SD mu:',
  #               min = 0, max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })
  # 
  # output$meanbeta = renderUI({
  #   sliderInput(inputId = "meanbeta"   , label = 'Mean beta:',
  #               min = input$xlim[1], max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })
  # 
  # output$sdbeta = renderUI({
  #   sliderInput(inputId = "meanmu"   , label = 'SD beta:',
  #               min = input$xlim[1], max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })
  # 
  # output$meansigma = renderUI({
  #   sliderInput(inputId = "meansigma"   , label = 'Mean sigma:',
  #               min = input$xlim[1], max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })
  # 
  # output$sdsigma = renderUI({
  #   sliderInput(inputId = "sdsigma"   , label = 'SD sigma:',
  #               min = input$xlim[1], max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })
  # 
  # output$meanxstar = renderUI({
  #   sliderInput(inputId = "meanxstar"   , label = 'Mean xstar:',
  #               min = input$xlim[1], max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })
  # 
  # output$sdxstar = renderUI({
  #   sliderInput(inputId = "sdxstar"   , label = 'SD xstar:',
  #               min = input$xlim[1], max = input$xlim[2], 
  #               value = mean(input$xlim), step = 0.01)
  # })

  ## Plotting points
  xx  = reactive(seq(input$xlim[1], input$xlim[2], length.out = 100))
  xxx = reactive(seq(input$xlim[1], input$xlim[2], length.out = 100))
  yyy = reactive(seq(input$ylim[1], input$ylim[2], length.out = 100))
  
  ## Sample posterior  
  posterior = reactive({
    
    ## Package data
    data = list(M = length(x), x = x, y = y, 
                z = input$z, sigmaz = input$sigmaz,
                meanmu    = input$meanmu   , sdmu    = input$sdmu,
                meanbeta  = input$meanbeta , sdbeta  = input$sdbeta,
                meansigma = input$meansigma, sdsigma = input$sdsigma,
                meanxstar = input$meanxstar, sdxstar = input$sdxstar)
    
    ## Fit STAN model
    buffer = sampling(model, data = data, chains = getOption("mc.cores"),
                 iter = 2*input$N/getOption("mc.cores"), 
                 warmup = input$N/getOption("mc.cores"), 
                 verbose = FALSE, show_messages = FALSE)
    
    ## Extract posterior samples
    extract(buffer, c("mu","beta","sigma","xstar"))
  })
  
  
  ## Sample posterior predictive 
  predictive = reactive({
    
    ## Posterior predictive uncertainty
    fit = length(xx())
    lwr = length(xx())
    upr = length(xx())
    for (i in 1:length(xx())) {
      buffer = posterior()$mu + posterior()$beta * xx()[i] + 
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
    
    ## Sample discrepancy
    mustar    = rnorm(input$N, posterior()$mu  , input$sigmam)
    betastar  = rnorm(input$N, posterior()$beta, input$sigmab)
    sigmastar = sqrt(posterior()$sigma^2 + input$sigmad^2)
    
    ## Summarize discrepancy
    lwr = numeric(length(xx()))
    upr = numeric(length(xx()))
    for (i in 1:length(xx())) {
      buffer = mustar + betastar * xx()[i] + sigmastar * rnorm(input$N)
      lwr[i] = quantile(buffer,     as.numeric(input$alpha))
      upr[i] = quantile(buffer, 1 - as.numeric(input$alpha))
    }
    
    ## Return predictions
    list(mustar = mustar, betastar = betastar, sigmastar = sigmastar,
         lwr = lwr, upr = upr)
    
  })
  
  ## Main plot
  output$ecPlot <- renderPlot({
    
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    ## Plot data
    plot(x, y, xlim = input$xlim, ylim = input$ylim, ann = FALSE, pch = 16,
         xaxs = "i", yaxs = "i")
    
    ## Add titles
    title(xlab = expression(paste(Psi, " (K)")))
    title(ylab = "ECS (K)")
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
    abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigmaz, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigmaz,
           col = "blue", lty = "dashed", lwd = 2)
    
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

    p    = dnorm(xxx(), input$z, input$sigmaz)
    ymax = 10^(floor(log10(max(p)))-1)
    ymax = ymax*ceiling(max(p)/ymax)
    
    ## Plot density
    plot(xxx(), p, type = "l", xlim = input$xlim, ylim = c(0,ymax),
         ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$z, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$z + qnorm(    as.numeric(input$alpha))*input$sigmaz, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$z + qnorm(1 - as.numeric(input$alpha))*input$sigmaz,
           col = "blue", lty = "dashed", lwd = 2)

  }) ## obsPlot
  
  ## Joint prior plot
  output$priorPlot <- renderPlot({
    
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    ## Plot data
    plot(x, y, type = "n", xlim = input$xlim, ylim = input$ylim, 
         ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add titles
    title(xlab = expression(paste(Psi, " (K)")))
    title(ylab = "ECS (K)")
    #title(main = "Emergent relationship fit", cex.main = 1, font.main = 1)
    mtext("Joint prior distribution", side = 3, adj = 0)
    
    ## Simulate from prior
    mu    =     rnorm(input$N, input$meanmu   , input$sdmu   )
    beta  =     rnorm(input$N, input$meanbeta , input$sdbeta )
    sigma = abs(rnorm(input$N, input$meansigma, input$sdsigma))
    
    ## Simulate from prior predictive distribution
    fit = length(xx())
    lwr = length(xx())
    upr = length(xx())
    for (i in 1:length(xx())) {
      buffer = mu + beta * xx()[i] + sigma * rnorm(input$N)
      fit[i]  = mean(buffer)
      lwr[i]  = quantile(buffer,     as.numeric(input$alpha))
      upr[i]  = quantile(buffer, 1 - as.numeric(input$alpha))
    }

    ## Plot prior predictive distribution
    lines(xx(), fit, lty = "dotdash", lwd = 2)
    lines(xx(), lwr, lty = "dashed" , lwd = 2)
    lines(xx(), upr, lty = "dashed" , lwd = 2)

  }) ## priorPlot
  
  ## Mean/intercept prior
  output$muPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(yyy(), input$meanmu, input$sdmu)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(yyy(), p, type = "l", xlim = input$ylim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$meanmu, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$meanmu + qnorm(    as.numeric(input$alpha))*input$sdmu, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$meanmu + qnorm(1 - as.numeric(input$alpha))*input$sdmu,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## muPlot

  ## Slope prior
  output$betaPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(yyy(), input$meanbeta, input$sdbeta)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(yyy(), p, type = "l", xlim = input$xlim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$meanbeta, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$meanbeta + qnorm(    as.numeric(input$alpha))*input$sdbeta, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$meanbeta + qnorm(1 - as.numeric(input$alpha))*input$sdbeta,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## betaPlot
  
  ## Sigma prior
  output$sigmaPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(yyy(), input$meansigma, input$sdsigma)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(yyy(), p, type = "l", xlim = input$ylim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$meansigma, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$meansigma + qnorm(    as.numeric(input$alpha))*input$sdsigma, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$meansigma + qnorm(1 - as.numeric(input$alpha))*input$sdsigma,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## sigmaPlot

  ## xstar prior
  output$xstarPlot <- renderPlot({
    
    ## Plotting parameters
    par(las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0), tcl = -1/3,
        xaxs = "r", yaxs = "r")
    
    p    = dnorm(xxx(), input$meanxstar, input$sdxstar)
    ymax = 10^(floor(log10(max(p))))
    ymax = ymax*ceiling(1.04*max(p)/ymax)
    
    ## Plot density
    plot(xxx(), p, type = "l", xlim = input$xlim, ylim = c(0,ymax),
         lwd = 2, col = "red", ann = FALSE, xaxs = "i", yaxs = "i")
    
    ## Add quantiles
    abline(v = input$meanxstar, col = "blue", lty = "dotdash", lwd = 2)
    abline(v = input$meanxstar + qnorm(    as.numeric(input$alpha))*input$sdxstar, 
           col = "blue", lty = "dashed", lwd = 2)
    abline(v = input$meanxstar + qnorm(1 - as.numeric(input$alpha))*input$sdxstar,
           col = "blue", lty = "dashed", lwd = 2)
    
  }) ## xstarPlot
  
} ## server

## Run the app
shinyApp(ui = ui, server = server)
