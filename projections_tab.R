#####################
## Projections tab ##
#####################

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
  )
})

## Update gamma
gamma = reactive({
  if (input$gamma == "custom") {
    if (is.null(input$gamma_custom)) {
      0.90
    } else {
      input$gamma_custom
    }
  } else {
    as.numeric(input$gamma)
  }
})

## Marginal posterior predictive plot
marginal_plot = function() {

  ## Skip plotting if no data is loaded
  if (no_data() | bad_obs() | bad_prior())
    return(NULL)

  ## Extract data
  y      = data()[,input$y]
  ystar1 = reference_posterior()[,,"ystar"]
  ystar2 = discrepancy()[,,"ystar"]
  probs  = 0.5*(1 + c(-1,+1)*gamma())

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

}
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
  ystar1 = reference_posterior()[,,"ystar"]
  ystar2 = discrepancy()[,,"ystar"]

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
)

## Joint posterior preditive plot
joint_plot = function() {

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
          levels = gamma(), drawlabels = FALSE,
          lwd = 2, col = "black", lty = "dotted", add = TRUE)

  ## Add discrepancy density
  contour(discrepancy_density$x, discrepancy_density$y, discrepancy_density$z,
          levels = gamma(), drawlabels = FALSE,
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

}
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
)
