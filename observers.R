###############
## Observers ##
###############

## Update variable choices when data loaded
observe({
  
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
observe(
  updateTextInput(session = session,
                  inputId = "xlab",
                  value   = input$x
  )
) ## observe

## Update response label when response changes
observe(
  updateTextInput(session = session,
                  inputId = "ylab",
                  value   = input$y
  )
) ## observe

## Update observation input controls
observe({
  
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

## Update intercept bias range
observe({
  
  v     = c(ylim()$min,ylim()$max)
  vdiff = diff(v)
  vstep = 10^(floor(log10(vdiff))-1)
  vmin  = - vdiff / 2
  vmax  = + vdiff / 2
  vmin  = floor  (vmin / vstep) * vstep
  vmax  = ceiling(vmax / vstep) * vstep
  
  updateSliderInput(session = session,
                    inputId = "mu_delta_alpha",
                    value   = 0,
                    min     = vmin,
                    max     = vmax,
                    step    = vstep
  ) ## mu_delta_alpha
  
}) ## observe

## Update intercept uncertainty range
observe({
  
  v     = c(ylim()$min,ylim()$max)
  vdiff = diff(v)
  vstep = 10^(floor(log10(vdiff))-1)
  vmax  = vdiff / 2
  vmax  = ceiling(vmax / vstep) * vstep
  
  updateSliderInput(session = session,
                    inputId = "sigma_delta_alpha",
                    value   = 0,
                    min     = 0,
                    max     = vmax,
                    step    = vstep
  ) ## sigma_delta_alpha
  
}) ## observe

## Update slope bias range
observe({
  
  x     = c(xlim()$min,xlim()$max)
  y     = c(ylim()$min,ylim()$max)
  xdiff = diff(x)
  ydiff = diff(y)
  
  vdiff = ydiff/xdiff
  vstep = 10^(floor(log10(vdiff))-1)
  vmin  = - vdiff / 2
  vmax  = + vdiff / 2
  vmin  = floor  (vmin / vstep) * vstep
  vmax  = ceiling(vmax / vstep) * vstep
  
  updateSliderInput(session = session,
                    inputId = "mu_delta_beta",
                    value   = 0,
                    min     = vmin,
                    max     = vmax,
                    step    = vstep
  ) ## mu_delta_beta
  
}) ## observe

## Update slope uncertainty range
observe({
  
  x     = c(xlim()$min,xlim()$max)
  y     = c(ylim()$min,ylim()$max)
  xdiff = diff(x)
  ydiff = diff(y)
  
  vdiff = ydiff/xdiff
  vstep = 10^(floor(log10(vdiff))-1)
  vmax  = vdiff / 2
  vmax  = ceiling(vmax / vstep) * vstep
  
  updateSliderInput(session = session,
                    inputId = "sigma_delta_beta",
                    value   = 0,
                    min     = 0,
                    max     = vmax,
                    step    = vstep
  ) ## sigma_delta_beta
  
}) ## observe

## Update response uncertainty range
observe({
  
  v     = c(ylim()$min,ylim()$max)
  vdiff = diff(v)
  vstep = 10^(floor(log10(vdiff))-1)
  vmax  = vdiff / 2
  vmax  = ceiling(vmax / vstep) * vstep
  
  updateSliderInput(session = session,
                    inputId = "sigma_sigma_star",
                    value   = 0,
                    min     = 0,
                    max     = vmax,
                    step    = vstep
  ) ## sigma_sigma_star
  
}) ## observe

## Update posterior plot limits
observe ({
  
  if (no_data() | bad_obs() | bad_prior()) {
    
    xlims = list(value = c(0,1), min = 0, max = 1, step = 0.1)
    ylims = xlims
    
  } else {
    
    xlims = posterior_limits(data()[,input$x], discrepancy()[,,"xstar"])
    ylims = posterior_limits(data()[,input$y], discrepancy()[,,"ystar"])
    
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

## Disable informative priors
observeEvent(input$reference, {
  toggleState(id = "mu_alpha")
  toggleState(id = "sigma_alpha")
  toggleState(id = "mu_beta")
  toggleState(id = "sigma_beta")
  toggleState(id = "mu_sigma")
  toggleState(id = "sigma_sigma")
  toggleState(id = "mu_xstar")
  toggleState(id = "sigma_xstar")
})
