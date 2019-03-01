#####################
## Diagnostics tab ##
#####################

## Print summary of MCMC output
output$summary = renderTable({   
  
  ## Skip table if error condition
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  sims = posterior()
  dimnames(sims)$parameters = c("alpha","beta","sigma","xstar")
  sims[,,"xstar"] = xstar()
  
  monitor(sims, warmup = 0, print = FALSE)[c("alpha","beta","sigma","xstar"),]
  
},
rownames = TRUE
) ## summary

## Plot time series of model parameter samples
output$sample_plot = renderPlot({
  
  ## Skip plotting if error condition
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
 
  if (input$diag_var == "xstar") {
    samples = xstar()
  } else {
    samples = posterior()[,,input$diag_var]
  }
  label   = parameter_labels[input$diag_var]
  
  plot_samples(samples, label)
  
}) ## sample_plot

## Plot density of model parameter samples
output$density_plot = renderPlot({
  
  ## Skip plotting if error condition
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  if (input$diag_var == "xstar") {
    samples = xstar()
  } else {
    samples = posterior()[,,input$diag_var]
  }
  label   = parameter_labels[input$diag_var]
  
  plot_density(samples, label)
  
}) ## density_plot

## Plot samples vs log posterior
output$log_posterior_plot = renderPlot({
  
  ## Skip plotting if error condition
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  ## Extract data    
  if (input$diag_var == "xstar") {
    x = xstar()
  } else {
    x = posterior()[,,input$diag_var]
  }
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
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
  
  ## Extract data
  if (input$diag_var == "xstar") {
    x = xstar()
  } else {
    x = posterior()[,,input$diag_var]
  }
  
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
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)

  samples = posterior()[,,"lp__"]
  label   = parameter_labels["lp__"]

  plot_samples(samples, label)
  
}) ## log_posterior_samples

## Plot density of log posterior
output$log_posterior_density = renderPlot({
  
  ## Skip plotting if error condition
  if (no_data() | bad_obs() | bad_model_prior() | bad_real_prior())
    return(NULL)
 
  samples = posterior()[,,"lp__"]
  label   = parameter_labels["lp__"]
  
  plot_density(samples, label)
  
}) ## log_posterior_density
