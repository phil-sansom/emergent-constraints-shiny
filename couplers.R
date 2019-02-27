#######################
## Reactive couplers ##
#######################

## Update X limits
xlim = reactive({
  # print("Couplers 1: xlim")

  if (no_data())
    return(list(value = c(0,1), min = 0, max = 1, step = 0.1))

  limits(data()[,input$x], input$z + c(-2,+2)*input$sigma_z)

})

## Update Y limits
ylim = reactive({
  # print("Couplers 2: ylim")

  if (no_data())
    return(list(val = c(0,1), min = 0, max = 1, step = 0.1))

  limits(data()[,input$y])

})
