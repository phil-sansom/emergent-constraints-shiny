##############
## Data tab ##
##############

## Visualise data
output$data_plot = renderPlot({
  # print("Data tab 1: data_plot")

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
  title(xlab = input$x)
  title(ylab = input$y)

  ## Add legend
  legend("bottomright", legend = c("Models","Observation"),
         col = c("black","blue"), lty = c(NA,"dotdash"), lwd = c(2,2),
         pch = c(19,NA), bty = "n")

  # ## Create plot
  # p = ggplot() +
  #   geom_point(mapping = aes_string(x = input$x, y = input$y),
  #              data = data()) +
  #   labs(x = input$x, y = input$y)
  #
  # ## Add observations
  # if (is.numeric(input$z))
  #   p = p + geom_vline(mapping = aes_string(xintercept = input$z),
  #                      na.rm = TRUE, colour   = "blue", linetype = "dotdash")
  #
  # ## Plot data
  # p

})
