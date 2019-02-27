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

## Update posterior plot limits
observe ({
  # print("Observers 11: xlim_marginal,xlim_joint,ylim_joint")

  if (no_data() | bad_obs() | bad_prior()) {

    xlims = list(value = c(0,1), min = 0, max = 1, step = 0.1)
    ylims = xlims

  } else {

    xlims = posterior_limits(data()[,input$x], posterior()[,,"xstar"],
                             discrepancy()[,,"xstar"])
    ylims = posterior_limits(data()[,input$y], posterior()[,,"ystar"],
                             discrepancy()[,,"ystar"])

  }

  updateSliderInput(session = session,
                    inputId = "xlim_marginal",
                    value   = ylims$value,
                    min     = ylims$min,
                    max     = ylims$max,
                    step    = ylims$step
  ) ## xlim_marginal

  updateSliderInput(session = session,
                    inputId = "xlim_joint",
                    value   = xlims$value,
                    min     = xlims$min,
                    max     = xlims$max,
                    step    = xlims$step
  ) ## xlim_joint

  updateSliderInput(session = session,
                    inputId = "ylim_joint",
                    value   = ylims$value,
                    min     = ylims$min,
                    max     = ylims$max,
                    step    = ylims$step
  ) ## ylim_joint

})

## Check sample number
observe ({
  
  if (input$N %% getOption("mc.cores") != 0)
    updateNumericInput(session = session, 
                       inputId = "N",
                       value   = input$N %/% getOption("mc.cores") *
                         getOption("mc.cores")
    )
    
})

## Disable informative priors
observe({
  # print("Observers 12: Toggle reference")
  condition = input$priors != "reference"
  toggleState(id = "mu_alpha"   , condition = condition)
  toggleState(id = "sigma_alpha", condition = condition)
  toggleState(id = "mu_beta"    , condition = condition)
  toggleState(id = "sigma_beta" , condition = condition)
  toggleState(id = "rho"        , condition = condition)
  toggleState(id = "mu_sigma"   , condition = condition)
  toggleState(id = "sigma_sigma", condition = condition)
  toggleState(id = "mu_xstar"   , condition = condition)
  toggleState(id = "sigma_xstar", condition = condition)
})
