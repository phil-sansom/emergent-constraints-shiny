###############
## Functions ##
###############

## Graphical parameters
graphical_parameters = function() {
  
  par(ann = FALSE, las = 1, mar = c(2.5,2.5,1,1)+0.1, mgp = c(1.5,0.5,0),
      ps = 12, tcl = -1/3, xaxs = "i", yaxs = "i")
  
}

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
normal_plot = function(mu, sigma, gamma, xlab, ylab, ...) {
  
  ## Compute plotting limits and probability density
  xmin  = qnorm(pnorm(-4), mu, sigma)
  xmax  = qnorm(pnorm(+4), mu, sigma)
  xx    = seq(from = xmin, to = xmax, length.out = 101)
  yy    = dnorm(xx, mu, sigma)
  ymax  = 1.04*max(yy)
  
  ## Compute limits of prior interval
  xp = seq(from       = qnorm(0.5*(1 - gamma), mu, sigma),
           to         = qnorm(0.5*(1 + gamma), mu, sigma), 
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
folded_normal_plot = function(mu, sigma, gamma, xlab, ylab, ...) {
  
  ## Compute plotting limits and probability density
  xmax  = quantile(abs(rnorm(1e6, mu, sigma)), pnorm(+4))
  xx    = seq(0, xmax, length.out = 101)
  yy    = dnorm(xx, mu, sigma) + dnorm(xx, -mu, sigma)
  ymax  = 1.04*max(yy)
  
  ## Compute limits of prior interval
  cdf   = pnorm(xx, mu, sigma) + pnorm(xx, -mu, sigma) - 1
  xp    = seq(max(which(cdf < 0.5*(1 - gamma))),
              min(which(cdf > 0.5*(1 + gamma))), 1)
  
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
    qq = quantile(x[,i], 0.5 + c(-0.5,+0.5)*gamma())
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
