########################
## Condition handling ##
########################

## Check for data
no_data = reactive(
  any(is.null(data()), 
      ! input$x %in% names(data()), 
      ! input$y %in% names(data())
  )
)

## Check observation parameters
bad_obs = reactive(
  any(is.na(input$z), is.na(input$sigma_z), input$sigma_z <= 0)
)

## Check prior parameters
bad_prior = reactive(
  if (input$reference) {
    FALSE
  } else {
    any(
      is.na(input$mu_alpha), is.na(input$sigma_alpha), input$sigma_alpha <= 0,
      is.na(input$mu_beta ), is.na(input$sigma_beta ), input$sigma_beta  <= 0,
      is.na(input$mu_sigma), is.na(input$sigma_sigma), input$sigma_sigma <= 0,
      is.na(input$mu_xstar), is.na(input$sigma_xstar), input$sigma_xstar <= 0
    )
  }
)
