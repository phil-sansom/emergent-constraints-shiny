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

## Update predictor label when predictor changes
observe({
  # print("Observers 2: Update xlab")
  updateTextInput(session = session,
                  inputId = "xlab",
                  value   = input$x
  )
}) ## observe

## Update response label when response changes
observe({
  # print("Observers 3: Update ylab")
  updateTextInput(session = session,
                  inputId = "ylab",
                  value   = input$y
  )
}) ## observe

## Update observation input controls
observe({
  # print("Observers 4: Update z and sigma_z scale")

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

## Check sample number
observe ({
  
  if (input$N %% getOption("mc.cores") != 0)
    updateNumericInput(session = session, 
                       inputId = "N",
                       value   = input$N %/% getOption("mc.cores") *
                         getOption("mc.cores")
    )
    
})
