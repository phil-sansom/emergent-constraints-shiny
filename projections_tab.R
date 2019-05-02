#####################
## Projections tab ##
#####################

## Predictor distribution
output$predictor = renderUI({
  
  if (input$discrepancy != "guided")
    return(NULL)
     
  if (no_data()) {
    ymean = 0
    ysd   = 1
  } else {
    y      = data()$y
    yrange = diff(range(y))
    yorder = 10^round(log10(yrange) - 1)
    
    ymean = round(mean(y)/yorder)*yorder
    ysd   = round(sd(y)/yorder)*yorder
    
  }

  tagList(
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = "ymean",
          label   = "Current response mean",
          value   = ymean
        )
      ), ## column
      column(
        width = 6,
        numericInput(
          inputId = "ysd",
          label   = "Current response SD",
          value   = ysd
        )
      ) ## column
    ) ## fluidRow
  ) ## tagList

}) ## predictor


## Likelihood of emergent constraint
output$likelihood = renderUI(
  
  if (input$discrepancy == "guided") {
    
    tagList(
      selectInput(inputId  = "likelihood",
                  label    = "Likelihood of emergent relationship",
                  choices  = list("Virtually certain (99-100%)" = 0.99,
                                  "Very likely       (90-100%)" = 0.90,
                                  "Likely            (66-100%)" = 0.66,
                                  "Custom"            = "custom"),
                  selected = 0.99
      )
    )
    
  } else {
    
    NULL
    
  }
  
) ## likelihood


## Custom likelihood
output$likelihood_custom = renderUI({
  
  if (input$discrepancy != "guided" | is.null(input$likelihood))
    return(NULL)

  if (input$likelihood == "custom")
    tagList(
      numericInput(inputId = "likelihood_custom", 
                   label   = "Custom", 
                   value   = 0.666,
                   min     = 0.501, 
                   max     = 1, 
                   step    = 0.001) 
    ) ## tagList

}) ## custom_likelihood


## Update likelihood
likelihood = reactive({
  
  if (is.null(input$likelihood))
    return(0.99)
 
  if (input$likelihood != "custom")
    return(as.numeric(input$likelihood))
 
  if (is.null(input$likelihood_custom))
    return(0.99)
    
  if (is.na(input$likelihood_custom))
    return(0.99)

  return(input$likelihood_custom)

}) ## likelihood


## Print predictive intervals
output$discrepancies = renderTable({
  
  ## Skip table if no data is loaded
  if (input$discrepancy == "manual")
    return(NULL)
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)

  ## Initialise storage
  discs = data.frame("SD" = numeric(4))
  rownames(discs) = c("Intercept","Slope","Uncertainty","Correlation")
  
  ## Compute intervals
  discs[1,1] = sigma_alpha_star()
  discs[2,1] = sigma_beta_star()
  discs[3,1] = sigma_sigma_star()
  discs[4,1] = rho_star()

  ## Return intervals
  return(discs)
  
},
rownames = TRUE
) ## discrepancies


## Input selection
output$discrepancy_input_select = renderUI({
  
  if (input$discrepancy == "manual") {
    
    tagList(
      radioButtons(
        inputId  = "discrepancy_input_select",
        label    = "Input style",
        choices  = list(Sliders = "sliders", Numerical = "numerical"),
        selected = "sliders",
        inline   = FALSE
      )
    ) ## tagList
    
  } else {
    
    NULL
    
  }
  
}) ## discrepancy_input_select


## Discrepancy interface
output$discrepancy_interface = renderUI({
  
  if (input$discrepancy != "manual" | is.null(input$discrepancy_input_select))
    return(NULL)
  
  if (no_data()) {
    
    amax  = +1.0
    astep =  0.1
    bmin  = -1
    bmax  = +1
    bstep = 0.1
    smin  = -1
    smax  = +1
    sstep = 0.1
    
  } else {

    x     = c(xlim()$min,xlim()$max)
    y     = c(ylim()$min,ylim()$max)

    ## alpha_star
    astep = 10^(floor(log10(diff(x)))-1)
    amax  = + diff(x) / 2
    amax  = ceiling(amax / astep) * astep
 
    ## beta_star
    bstep = 10^(floor(log10(diff(y)/diff(x)))-1)
    bmax  = + diff(y)/diff(x) / 2
    bmax  = ceiling(bmax / bstep) * bstep
    
    ## sigma_star
    sstep = 10^(floor(log10(diff(y)))-1)
    smax  = + diff(y) / 2
    smax  = ceiling(smax / sstep) * sstep
    
  }
  
  numerical = input$discrepancy_input_select == "numerical"
  
  tagList(
    withMathJax(),
    h5("Intercept \\(\\alpha_\\star\\)"),
    if (numerical) {
      numericInput(inputId = "sigma_alpha_star",
                   label   = "Uncertainty \\(\\sigma_{\\alpha_\\star}\\)",
                   value   = 0,
                   min     = 0,
                   max     = NA,
                   step    = astep
      ) ## sigma_alpha_star
    } else {
      sliderInput(inputId = "sigma_alpha_star",
                  label   = "Uncertainty \\(\\sigma_{\\alpha_\\star}\\)",
                  value   = 0,
                  min     = 0,
                  max     = amax,
                  step    = astep,
                  ticks   = FALSE
      ) ## sigma_alpha_star
    },
    hr(),
    h5("Slope \\(\\beta_\\star\\)"),
    if (numerical) {
      numericInput(inputId = "sigma_beta_star",
                   label   = "Uncertainty \\(\\sigma_{\\beta_\\star}\\)",
                   value   = 0,
                   min     = 0,
                   max     = NA,
                   step    = bstep
      )
    } else {
      sliderInput(inputId = "sigma_beta_star",
                  label   = "Uncertainty \\(\\sigma_{\\beta_\\star}\\)",
                  value   = 0,
                  min     = 0,
                  max     = bmax,
                  step    = bstep,
                  ticks   = FALSE
      )
    },
    hr(),
    h5("Correlation \\(\\rho_\\star\\)"),
    if (numerical) {
      numericInput(inputId = "rho_star",
                   label   = "Corr(\\(\\alpha_\\star,\\beta_\\star\\))",
                   value   =  0,
                   min     = -1,
                   max     = +1,
                   step    = 0.01
      )
    } else {
      sliderInput(inputId = "rho_star",
                  label   = "Corr(\\(\\alpha_\\star,\\beta_\\star\\))",
                  value   =  0,
                  min     = -1,
                  max     = +1,
                  step    = 0.01,
                  ticks   = FALSE
      )
    },
    hr(),
    h5("Response spread \\(\\sigma_\\star\\)"),
    if (input$discrepancy_input_select == "numerical") {
      numericInput(inputId = "sigma_sigma_star",
                   label   = "Scale \\(\\sigma_{\\sigma_\\star}\\)",
                   value   = 0,
                   min     = 0,
                   max     = NA,
                   step    = sstep
      )
    } else {
      sliderInput(inputId = "sigma_sigma_star",
                  label   = "Scale \\(\\sigma_{\\sigma_\\star}\\)",
                  value   = 0,
                  min     = 0,
                  max     = smax,
                  step    = sstep,
                  ticks   = FALSE
      )
    }
  ) ## tagList
  
}) ## discrepancy_interface


## Custom interval width
output$gamma_custom = renderUI({
  tagList(
    if (input$gamma == "custom") {
      numericInput(inputId = "gamma_custom", 
                   label   = "Custom", 
                   value   = 0.66,
                   min     = 0, 
                   max     = 1, 
                   step    = 0.01) 
    } else {
      NULL
    }
  ) ## tagList
})


## Update gamma
gamma = reactive({
  if (input$gamma == "custom") {
    if (is.null(input$gamma_custom)) {
      0.90
    } else {
      if (is.na(input$gamma_custom)) {
        0.90
      } else {
        input$gamma_custom
      }
    }
  } else {
    as.numeric(input$gamma)
  }
})


## Plot intercept discrepancy
output$alpha_discrepancy_plot = renderPlot({
  
  ## Skip plotting if no data is loaded
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  ## Skip plotting if discrepancy not defined
  if (is.null(sigma_alpha_star()))
    return(NULL)
  if (is.na  (sigma_alpha_star()))
    return(NULL)
  if (sigma_alpha_star() < 0)
    return(NULL)

  ## Posterior and predictive
  alpha     = as.numeric(posterior()[,,"alpha"])
  alphastar = as.numeric(discrepancy()[,,"alphastar"])

  ## Compute densities
  da  = density(alpha)
  das = density(alphastar)
  
  ## Compute limits for credible intervals
  qa  = quantile(alpha, 0.5 + c(-0.5,+0.5)*gamma())
  xa  = seq(max(which(da$x < qa[1])), min(which(da$x > qa[2])), 1)
  qas = quantile(alphastar, 0.5 + c(-0.5,+0.5)*gamma())
  xas = seq(max(which(das$x < qas[1])), min(which(das$x > qas[2])), 1)
  
  ## Plotting limits
  xlim = range(alpha,alphastar)
  ylim = c(0, 1.04*max(da$y))
    
  ## Graphical parameters
  graphical_parameters()
  
  ## Plot posterior and predictive densities
  plot (NA, type = "n", xlim = xlim, ylim = ylim)
  polygon(x = c(da$x[xa[1]],da$x[xa],da$x[xa[length(xa)]]),
          y = c(0,da$y[xa],0), border = NA, col = alpha_col(input$ref_col))
  lines(da, col = input$ref_col, lwd = 2)
  polygon(x = c(das$x[xas[1]],das$x[xas],das$x[xas[length(xas)]]),
          y = c(0,das$y[xas],0), border = NA, col = alpha_col(input$inf_col))
  lines(das, col = input$inf_col, lwd = 2)
  
  ## Add labels
  title(xlab = parameter_labels["alphastar"])
  title(ylab = "Density")
  
  ## Add legend
  legend(input$legend_position, 
         legend = c(parameter_labels["alpha"],parameter_labels["alphastar"]),
         col = c(input$ref_col,input$inf_col), lty = c("solid","solid"), 
         lwd = c(2,2), bty = "n", horiz = input$legend_orientation)
  
}) ## alpha_prior_plot


## Plot slope discrepancy
output$beta_discrepancy_plot = renderPlot({
  
  ## Skip plotting if no data is loaded
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  ## Skip plotting if discrepancy not defined
  if (is.null(sigma_beta_star()))
    return(NULL)
  if (is.na  (sigma_beta_star()))
    return(NULL)
  if (sigma_beta_star() < 0)
    return(NULL)
  
  ## Posterior and predictive
  beta     = as.numeric(posterior()[,,"beta"])
  betastar = as.numeric(discrepancy()[,,"betastar"])
  
  ## Compute densities
  da  = density(beta)
  das = density(betastar)
  
  ## Compute limits for credible intervals
  qa  = quantile(beta, 0.5 + c(-0.5,+0.5)*gamma())
  xa  = seq(max(which(da$x < qa[1])), min(which(da$x > qa[2])), 1)
  qas = quantile(betastar, 0.5 + c(-0.5,+0.5)*gamma())
  xas = seq(max(which(das$x < qas[1])), min(which(das$x > qas[2])), 1)
  
  ## Plotting limits
  xlim = range(beta,betastar)
  ylim = c(0, 1.04*max(da$y))
  
  ## Graphical parameters
  graphical_parameters()
  
  ## Plot posterior and predictive densities
  plot (NA, type = "n", xlim = xlim, ylim = ylim)
  polygon(x = c(da$x[xa[1]],da$x[xa],da$x[xa[length(xa)]]),
          y = c(0,da$y[xa],0), border = NA, col = alpha_col(input$ref_col))
  lines(da, col = input$ref_col, lwd = 2)
  polygon(x = c(das$x[xas[1]],das$x[xas],das$x[xas[length(xas)]]),
          y = c(0,das$y[xas],0), border = NA, col = alpha_col(input$inf_col))
  lines(das, col = input$inf_col, lwd = 2)
  
  ## Add labels
  title(xlab = parameter_labels["betastar"])
  title(ylab = "Density")
  
  ## Add legend
  legend(input$legend_position, 
         legend = c(parameter_labels["beta"],parameter_labels["betastar"]),
         col = c(input$ref_col,input$inf_col), lty = c("solid","solid"), 
         lwd = c(2,2), bty = "n", horiz = input$legend_orientation)
  
}) ## beta_prior_plot


## Plot spread discrepancy
output$sigma_discrepancy_plot = renderPlot({
  
  ## Skip plotting if no data is loaded
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  ## Skip plotting if discrepancy not defined
  if (is.null(sigma_sigma_star()))
    return(NULL)
  if (is.na  (sigma_sigma_star()))
    return(NULL)
  if (sigma_sigma_star() < 0)
    return(NULL)
  
  ## Posterior and predictive
  sigma     = as.numeric(posterior()[,,"sigma"])
  sigmastar = as.numeric(discrepancy()[,,"sigmastar"])
  
  ## Fitted approximation
  theta    = fit_folded_normal(sigma)
  sigmahat = abs(rnorm(input$N, theta[1], theta[2]))
  
  ## Compute densities
  bw  = bw.nrd0(sigma)
  bws = bw.nrd0(sigmastar)
  bwh = bw.nrd0(sigmahat)
  da  = density(sigma    , bw = bw , from = max(0,min(sigma    )-3*bw ))
  das = density(sigmastar, bw = bws, from = max(0,min(sigmastar)-3*bws))
  dah = density(sigmahat , bw = bwh, from = max(0,min(sigmahat )-3*bwh))
  dah = list(x = dah$x)
  dah$y = dfnorm(dah$x, theta[1], theta[2])
   
  ## Compute limits for credible intervals
  qa  = quantile(sigma, 0.5 + c(-0.5,+0.5)*gamma())
  xa  = seq(max(which(da$x < qa[1])), min(which(da$x > qa[2])), 1)
  qas = quantile(sigmastar, 0.5 + c(-0.5,+0.5)*gamma())
  xas = seq(max(which(das$x < qas[1])), min(which(das$x > qas[2])), 1)
  qah = quantile(sigmahat, 0.5 + c(-0.5,+0.5)*gamma())
  xah = seq(max(which(dah$x < qah[1])), min(which(dah$x > qah[2])), 1)
  
  ## Plotting limits
  xlim = range(da$x,das$x,dah$x)
  ylim = c(0, 1.04*max(da$y))
  
  ## Graphical parameters
  graphical_parameters()
  
  ## Plot posterior and predictive densities
  plot (NA, type = "n", xlim = xlim, ylim = ylim)
  polygon(x = c(dah$x[xah[1]],dah$x[xah],dah$x[xah[length(xah)]]),
          y = c(0,dah$y[xah],0), border = NA, col = alpha_col("blue"))
  lines(dah, col = "blue", lwd = 2)
  polygon(x = c(da$x[xa[1]],da$x[xa],da$x[xa[length(xa)]]),
          y = c(0,da$y[xa],0), border = NA, col = alpha_col(input$ref_col))
  lines(da, col = input$ref_col, lwd = 2)
  polygon(x = c(das$x[xas[1]],das$x[xas],das$x[xas[length(xas)]]),
          y = c(0,das$y[xas],0), border = NA, col = alpha_col(input$inf_col))
  lines(das, col = input$inf_col, lwd = 2)
  
  ## Add labels
  title(xlab = parameter_labels["sigmastar"])
  title(ylab = "Density")
  
  ## Add legend
  legend(input$legend_position, 
         legend = c(parameter_labels["sigma"],
                    expression(paste("Folded-normal approximation ", hat(sigma))),
                    parameter_labels["sigmastar"]),
         col = c(input$ref_col,"blue",input$inf_col), 
         lty = c("solid","solid","solid"), lwd = c(2,2), bty = "n", 
         horiz = input$legend_orientation)
  
}) ## sigma_discrepancy_plot


## Plot prior discrepancy
output$prior_discrepancy_plot = renderPlot({
  
  ## Skip plotting if error condition
  if(no_data() | input$model_priors == "reference" | bad_model_prior() |
                 input$real_priors  == "reference" | bad_real_prior())
    return(NULL)
  
  ## Simulate from parameter priors
  mu    = c(input$mu_alpha,input$mu_beta)
  Sigma = matrix(c(input$sigma_alpha^2,
                   input$rho*input$sigma_alpha*input$sigma_beta,
                   input$rho*input$sigma_alpha*input$sigma_beta,
                   input$sigma_beta^2),
                 2, 2)
  theta = mvrnorm(input$N, mu, Sigma)
  alpha = theta[,1]
  beta  = theta[,2]
  sigma = abs(rnorm(input$N, input$mu_sigma, input$sigma_sigma))

  ## Simulate from discrepancies
  Sigma = matrix(c(sigma_alpha_star()^2,
                   rho_star()*sigma_alpha_star()*sigma_beta_star(),
                   rho_star()*sigma_alpha_star()*sigma_beta_star(),
                   sigma_beta_star()^2),
                 2, 2)
  delta = mvrnorm(input$N, c(0,0), Sigma)
  alphastar = alpha + delta[,1]
  betastar  = beta  + delta[,2] 
  sigmastar = abs(rnorm(input$N, sigma, sigma_sigma_star()))

  ## Simulate from real world predictor
  xstar = input$mu_xstar + input$sigma_xstar * rnorm(input$N)

  ## Plotting points
  range = range(xstar)
  diff  = diff(range)
  step  = 10^floor(log10(diff))
  min   = floor  (range[1] / step)*step
  max   = ceiling(range[2] / step)*step
  xx    = seq(min, max, length.out = 101)
  nn    = length(xx)
  
  ## Simulate from prior predictive distribution
  pp = matrix(NA, nn, 3, dimnames = list(NULL, c("fit","lwr","upr")))
  dp = matrix(NA, nn, 3, dimnames = list(NULL, c("fit","lwr","upr")))
  for (i in 1:nn) {
    buffer = alpha + beta * xx[i] + sigma * rnorm(input$N)
    pp[i,"fit"]          = mean(buffer)
    pp[i,c("lwr","upr")] = quantile(buffer, 0.5*(1 + c(-1,+1)*gamma()))
    buffer = alphastar + betastar * xx[i] + sigmastar * rnorm(input$N)
    dp[i,"fit"]          = mean(buffer)
    dp[i,c("lwr","upr")] = quantile(buffer, 0.5*(1 + c(-1,+1)*gamma()))
  }
  
  ## Labels
  xlab = ifelse (nchar(input$xlab) == 0, input$x, input$xlab)
  ylab = ifelse (nchar(input$ylab) == 0, input$y, input$ylab)
  
  ## Graphical parameters
  graphical_parameters()
  
  ## Plot prior predictive mean
  plot(xx, pp[,"fit"], type = "l", 
       col = input$ref_col, lty = "dotdash", lwd = 2,
       xlim = range(xx), ylim = range(pp,dp), yaxs = "r")
  
  ## Add prior predictive interval
  lines(xx, pp[,"lwr"], col = input$ref_col, lty = "dashed", lwd = 2)
  lines(xx, pp[,"upr"], col = input$ref_col, lty = "dashed", lwd = 2)
  
  ## Add discrepancy predictive distribution
  lines(xx, dp[,"fit"], col = input$inf_col, lty = "dotdash" , lwd = 2)
  lines(xx, dp[,"lwr"], col = input$inf_col, lty = "dashed", lwd = 2)
  lines(xx, dp[,"upr"], col = input$inf_col, lty = "dashed", lwd = 2)

  ## Add labels
  title(xlab = xlab)
  title(ylab = ylab)
  
  ## Add legend
  legend(input$legend_position, legend = c("Models","Real world"), 
         col = c(input$ref_col, input$inf_col), lty = c("solid","solid"), 
         lwd = c(2,2), bty = "n", horiz = input$legend_orientation)
  
}) ## prior_discrepancy_plot


## Marginal posterior predictive plot
marginal_plot = function() {

  ## Skip plotting if no data is loaded
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)

  ## Extract data
  y      = data()$y
  ystar1 = ystar_reference()
  ystar2 = ystar()
  probs  = 0.5*(1 + c(-1,+1)*gamma())

  ## Reference predictive density interval
  dens1 = density (ystar1)
  x1m   = quantile(ystar1, probs = probs)
  x1m   = seq(max(which(dens1$x < x1m[1])), min(which(dens1$x > x1m[2])), 1)

  ## Discrepancy predictive density interval
  dens2 = density (ystar2)
  x2m   = quantile(ystar2, probs = probs)
  x2m   = seq(max(which(dens2$x < x2m[1])), min(which(dens2$x > x2m[2])), 1)

  ## Plotting limits
  xlim    = numeric(2)
  ylim    = numeric(2)
  xlim[1] = if (is.na(input$ymin)) min(dens1$x,dens2$x) else input$ymin
  xlim[2] = if (is.na(input$ymax)) max(dens1$x,dens2$x) else input$ymax
  ylim[1] = 0
  ylim[2] = max(dens1$y,dens2$y)*1.04

  ## Labels
  xlab = ifelse (nchar(input$ylab) == 0, input$y, input$ylab)
  ylab = "Density"
  
  ## Graphical parameters
  graphical_parameters()

  ## Plot data as rug
  plot(y, type = "n", xlim = xlim, ylim = ylim)
  rug (y, ticksize = 0.02, side = 1, lwd = 2, col = "black", quiet = TRUE)

  ## Add predictive densities
  polygon(x = c(dens1$x[x1m[1]],dens1$x[x1m],dens1$x[x1m[length(x1m)]]),
          y = c(0,dens1$y[x1m],0), border = NA, col = alpha_col(input$ref_col))
  polygon(x = c(dens2$x[x2m[1]],dens2$x[x2m],dens2$x[x2m[length(x2m)]]),
          y = c(0,dens2$y[x2m],0), border = NA, col = alpha_col(input$inf_col))
  lines(dens1, col = input$ref_col, lwd = 2)
  lines(dens2, col = input$inf_col, lwd = 2)

  ## Add labels
  title(xlab = xlab)
  title(ylab = ylab)

  ## Add legend
  legend(input$legend_position,
         legend = c("Reference model","Conditionally exchangeable model"),
         col = c(input$ref_col,input$inf_col), lty = c("solid","solid"), 
         lwd = c(2,2), bty = "n", horiz = input$legend_orientation)

} ## marginal_plot
output$marginal_plot = renderPlot(marginal_plot())


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
) ## save_marginal_plot


## Download samples
output$save_samples = downloadHandler(
  filename = "samples.csv",
  content = function(file) {
    ## Skip plotting if no data is loaded
    if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior()) {
      write.csv(NULL, file = file, row.names = FALSE)
    } else {
      write.csv(data.frame(alpha     = as.numeric(posterior()[,,"alpha"]),
                           beta      = as.numeric(posterior()[,,"beta" ]),
                           sigma     = as.numeric(posterior()[,,"sigma"]),
                           alphastar = as.numeric(discrepancy()[,,"alphastar"]),
                           betastar  = as.numeric(discrepancy()[,,"betastar"]),
                           sigmastar = as.numeric(discrepancy()[,,"sigmastar"]),
                           xstar     = as.numeric(xstar()),
                           ystar     = as.numeric(ystar())
      ),
      file = file, row.names = FALSE)
    }
  },
  contentType = "text/csv"
) ## save_samples


## Print predictive intervals
output$predictive_intervals = renderTable({

  ## Skip table if no data is loaded
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)

  ## Extract data
  ystar1 = ystar_reference()
  ystar2 = ystar()

  ## Interval width
  probs = 0.5*(1 + c(-1,+1)*gamma())

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
) ## predictive_intervals


## Joint posterior preditive plot
joint_plot = function() {

  ## Skip plotting if error condition
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)

  ## Extract data
  x         = data()$x
  y         = data()$y
  xstar     = as.numeric(xstar())
  ystar     = as.numeric(ystar())
  xstar_ref = as.numeric(xstar_reference())
  ystar_ref = as.numeric(ystar_reference())

  ## Extract predictive intervals
  reference   = reference_predictive()
  discrepancy = discrepancy_predictive()

  ## Points to sample predictive distribution
  xx = xx()

  ## Mask to limit number of points plotted
  mask = mask()

  ## Interval width
  probs = 0.5*(1 + c(-1,+1)*gamma())

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
  xlim = numeric(2)
  ylim = numeric(2)
  xlim[1] = if (is.na(input$xmin)) min(x,xstar,xstar_ref) else input$xmin
  xlim[2] = if (is.na(input$xmax)) max(x,xstar,xstar_ref) else input$xmax
  ylim[1] = if (is.na(input$ymin)) min(y,ystar,ystar_ref) else input$ymin
  ylim[2] = if (is.na(input$ymax)) max(y,ystar,ystar_ref) else input$ymax

  ## Labels
  xlab = ifelse (nchar(input$xlab) == 0, input$x, input$xlab)
  ylab = ifelse (nchar(input$ylab) == 0, input$y, input$ylab)
  
  ## Graphical parameters
  graphical_parameters()

  ## Plot predictive point cloud
  plot(xstar[mask], ystar[mask], col = gray(0.75, alpha = 0.25), pch = 19,
       xlim = xlim, ylim = ylim)

  ## Add data
  points(x, y, col = "black", pch = 19)

  ## Add reference predictions
  lines(xx, reference[,"fit"], col = input$ref_col, lty = "dotdash", lwd = 2)
  lines(xx, reference[,"lwr"], col = input$ref_col, lty = "dashed" , lwd = 2)
  lines(xx, reference[,"upr"], col = input$ref_col, lty = "dashed" , lwd = 2)

  ## Add discrepancy predictions
  lines(xx, discrepancy[,"fit"], col = input$inf_col, lty = "dotdash", lwd = 2)
  lines(xx, discrepancy[,"lwr"], col = input$inf_col, lty = "dashed" , lwd = 2)
  lines(xx, discrepancy[,"upr"], col = input$inf_col, lty = "dashed" , lwd = 2)

  ## Add observations
  abline(v = mean    (xstar)       , col = input$obs_col, lty = "dotdash", lwd = 2)
  abline(v = quantile(xstar, probs), col = input$obs_col, lty = "dashed" , lwd = 2)

  ## Add reference density
  contour(reference_density$x, reference_density$y, reference_density$z,
          levels = gamma(), drawlabels = FALSE,
          lwd = 2, col = input$ref_col, lty = "dotted", add = TRUE)

  ## Add discrepancy density
  contour(discrepancy_density$x, discrepancy_density$y, discrepancy_density$z,
          levels = gamma(), drawlabels = FALSE,
          lwd = 2, col = input$inf_col, lty = "dotted", add = TRUE)

  ## Add labels
  title(xlab = xlab)
  title(ylab = ylab)

  ## Add legend
  legend(input$legend_position,
         legend = c("Reference model","Conditionally exchangeable model",
                    "Observational constraint"),
         col = c(input$ref_col,input$inf_col,input$obs_col),
         lty = c("solid","solid","solid"), lwd = c(2,2,2), bty = "n", 
         horiz = input$legend_orientation)

} ## joint_plot
output$joint_plot = renderPlot(joint_plot())

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
) ## save_joint_plot
