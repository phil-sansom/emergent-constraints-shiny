########################
## Condition handling ##
########################

## Check for data
no_data = reactive({
  # print("Handlers: no_data")
  any(is.null(data()),
      ! input$x %in% names(data()),
      ! input$y %in% names(data())
  )
})

## Check observation parameters
bad_obs = reactive({
  # print("Handlers: bad_obs")
  any(is.na(input$z), is.na(input$sigma_z), input$sigma_z <= 0)
})

## Check prior parameters
bad_model_prior = reactive({
  # print("Handlers: bad_model_prior")
  if (input$model_priors == "reference")
    return(FALSE)
  if (is.null(input$model_input_select))
    return(TRUE)
  if (is.null(input$mu_alpha) | is.null(input$sigma_alpha) |
      is.null(input$mu_beta ) | is.null(input$sigma_beta ) |
      is.null(input$mu_sigma) | is.null(input$sigma_sigma) |
      is.null(input$rho))
    return(TRUE)
  if (is.na(input$mu_alpha) | is.na(input$sigma_alpha) |
      is.na(input$mu_beta ) | is.na(input$sigma_beta ) |
      is.na(input$mu_sigma) | is.na(input$sigma_sigma) |
      is.na(input$rho))
    return(TRUE)
  if (input$sigma_alpha <=  0 | input$sigma_beta  <=  0 |   
      input$sigma_sigma <=  0 | 
      input$rho         <  -1 | input$rho          > +1 ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
})

bad_real_prior = reactive({
  # print("Handlers: bad_real_prior")
  if (input$real_priors == "reference")
    return(FALSE)
  if (is.null(input$real_input_select))
    return(TRUE)
  if (is.null(input$mu_xstar) | is.null(input$sigma_xstar))
    return(TRUE)
  if (is.na(input$mu_xstar) | is.na(input$sigma_xstar))
    return(TRUE)
  if (input$sigma_xstar <=  0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
})
