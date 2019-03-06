###############
## Observers ##
###############

## Update variable choices when data loaded
observe({
  # print("Observers 1: Update x,y")

  choices = names(data())

  updateSelectInput(session  = session,
                    inputId  = "x",
                    choices  = choices,
                    selected = choices[1]
  ) ## x

  updateSelectInput(session  = session,
                    inputId  = "y",
                    choices  = choices,
                    selected = choices[2]
  ) ## y

}) ## observe

## Update observation input controls
observe({
  # print("Observers 2: Update z and sigma_z scale")

  if (no_data()) {

    vstep = 1

  } else {

    vstep  = floor(log10(diff(range(data()[,input$x]))))

  }

  updateNumericInput(session = session,
                     inputId = "z",
                     step    = 10^(vstep-1)
  ) ## z

  updateNumericInput(session = session,
                     inputId = "sigma_z",
                     step    = 10^(vstep-2)
  ) ## sigma_z

}) ## observe

## Set sensible values for discrepancy reference values
observe({
  
  if (no_data())
    return(NULL)
  
  y      = data()[,input$y]
  yrange = diff(range(y))
  yorder = 10^round(log10(yrange) - 1)
  
  updateNumericInput(
    session = session,
    inputId = "ymean",
    value   = round(mean(y)/yorder)*yorder
  ) ## ymean

  updateNumericInput(
    session = session,
    inputId = "ysd",
    value   = round(sd(y)/yorder)*yorder
  ) ## ysd
  
}) ## observer

## Check sample number
observe ({
  
  if (input$N %% getOption("mc.cores") != 0)
    updateNumericInput(session = session, 
                       inputId = "N",
                       value   = input$N %/% getOption("mc.cores") *
                         getOption("mc.cores")
    )
    
})
