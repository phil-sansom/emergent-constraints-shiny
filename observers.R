###############
## Observers ##
###############

## Update variable choices when data loaded
observe({
  # print("Observers 1: Update x,y")

  if (!is.null(data())) {
  
    ## Extract classes
    classes = sapply(data(), is.numeric)

    if (sum(classes) >= 2) {
      
      ## Extract column names
      choices = names(data())[classes]
      
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
      
    }
    
  }

}) ## Update variable choices when data loaded

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

}) ## Update observation input controls

## Check sample number
observe ({
  
  if (input$N %% getOption("mc.cores") != 0)
    updateNumericInput(session = session, 
                       inputId = "N",
                       value   = input$N %/% getOption("mc.cores") *
                         getOption("mc.cores")
    )
    
}) ## Check sample number
