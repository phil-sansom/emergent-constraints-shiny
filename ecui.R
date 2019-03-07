ui = navbarPage(
  title = "Uncertainty quantification for emergent constraints",
  id    = "page",

  ## Overview tab
  tabPanel(title = "Overview",
           value = "overview",
           withMathJax(),
           mainPanel(
             p("$$ \\begin{align}

                       \\text{Statistical model} \\\\
                       Y_m & = \\alpha + \\beta X_m + \\epsilon_m &
                         \\epsilon_m & \\sim
                           \\text{Normal} \\left( 0, \\sigma^2 \\right) \\quad
                         (m = 1,\\ldots,M) &
                         \\text{Multi-model ensemble} \\\\
                       Y_\\star & = \\alpha_\\star + \\beta_\\star X_\\star +
                           \\epsilon_\\star &
                         \\epsilon_\\star & \\sim
                           \\text{Normal} \\left( 0, \\sigma_\\star^2 \\right) &
                         \\text{Real world} \\\\
                       Z & = X_\\star + \\omega &
                         \\omega & \\sim 
                           \\text{Normal} \\left( 0, \\sigma_Z^2 \\right) &
                         \\text{Observations} \\\\ ~ \\\\

                       \\text{Discrepancies} \\\\
                       & &
                       \\left[ \\begin{array}{c}
                         \\alpha_\\star \\\\ \\beta_\\star
                       \\end{array} \\right] 
                       \\bigg\\vert
                       \\left[ \\begin{array}{c}
                         \\alpha \\\\ \\beta
                       \\end{array} \\right] &
                       \\sim \\text{Normal} \\left(
                         \\left[ \\begin{array}{c} 
                           \\alpha \\\\ \\beta
                         \\end{array} \\right],
                         \\left[ \\begin{array}{cc} 
                           \\sigma_{\\alpha_\\star}^2 &
                             \\rho_\\star \\sigma_{\\alpha_\\star}
                               \\sigma_{\\beta_\\star} \\\\
                           \\rho_\\star \\sigma_{\\alpha_\\star}
                             \\sigma_{\\beta_\\star} &
                               \\sigma_{\\beta_\\star}^2
                         \\end{array} \\right]
                       \\right) &
                       \\begin{array}{r}
                         \\text{Real world intercept} \\\\
                         \\text{Real world slope}
                       \\end{array} \\\\
                        & & 
                       \\sigma_\\star \\vert \\sigma & \\sim
                       \\text{Folded-Normal} \\left( 
                           \\sigma, \\sigma_{\\sigma_\\star}^2
                       \\right) &
                       \\text{Real world response uncertainty} \\\\ ~ \\\\

                       \\text{Reference priors} \\\\
                       & &
                       \\alpha & \\sim \\text{Uniform}
                         \\left( -\\infty, +\\infty \\right) &
                         \\text{Ensemble intercept} \\\\
                       & &
                       \\beta  & \\sim \\text{Uniform}
                         \\left( -\\infty, +\\infty \\right) &
                         \\text{Ensemble slope} \\\\
                       & &
                       \\log \\sigma & \\sim \\text{Uniform}
                         \\left( -\\infty, +\\infty \\right) &
                         \\text{Ensemble response uncertainty} \\\\
                       & &
                       X_\\star & \\sim \\text{Uniform}
                         \\left( -\\infty, +\\infty \\right) &
                         \\text{Real world predictor} \\\\ ~ \\\\

                       \\text{Informative priors} \\\\
                       & &
                       \\left[ \\begin{array}{c} 
                         \\alpha \\\\ \\beta 
                       \\end{array} \\right] &
                       \\sim \\text{Normal} \\left(
                         \\left[ \\begin{array}{c}
                           \\mu_\\alpha \\\\ \\mu_\\beta
                         \\end{array} \\right],
                         \\left[ \\begin{array}{cc} 
                           \\sigma_\\alpha^2 &
                             \\rho \\sigma_\\alpha \\sigma_\\beta \\\\
                           \\rho \\sigma_\\alpha \\sigma_\\beta &
                             \\sigma_\\beta^2
                         \\end{array} \\right]
                       \\right) &
                       \\begin{array}{r}
                         \\text{Ensemble intercept} \\\\
                         \\text{Ensemble slope}
                       \\end{array} \\\\
                       & &
                       \\sigma  & \\sim \\text{Folded-Normal}
                         \\left( \\mu_\\sigma, \\sigma_\\sigma^2 \\right) &
                         \\text{Ensemble response uncertainty} \\\\
                       & &
                       X_\\star & \\sim \\text{Normal}
                         \\left( \\mu_{X_\\star} , \\sigma_{X_\\star}^2 \\right) &
                         \\text{Real world predictor}

                     \\end{align} $$")
           ) ## mainPanel
  ), ## tabPanel

  ## Data tab
  tabPanel(title = "Data",
           value = "data",
           sidebarLayout(
             sidebarPanel(
               title = "Data and observations",
               h4("Data"),
               fileInput(inputId = "file",
                         label   = "Choose CSV File",
                         accept  = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
               ), ## fileInput
               checkboxInput(inputId = "header",
                             label   = "Header",
                             value   = TRUE
               ), ## checkboxInput
               fluidRow(
                 column(width = 6,
                 radioButtons("sep", "Separator",
                              choices = c(Comma     = "," ,
                                          Semicolon = ";" ,
                                          Tab       = "\t"),
                              selected = ","
                 ) ## radioButtons
                 ), ## column
                 column(width = 6,
                   radioButtons("quote", "Quote",
                                choices = c(None           = "" ,
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"'
                   ) ## radioButtons
                 ) ## column
               ), ## fluidRow
               hr(),
               fluidRow(
                 column(width = 6,
                        selectInput(inputId  = "x",
                                    label    = "Predictor \\(X_m\\)",
                                    choices  = NULL,
                                    selected = NULL)
                 ), ## column
                 column(width = 6,
                        selectInput(inputId  = "y",
                                    label    = "Response \\(Y_m\\)",
                                    choices  = NULL,
                                    selected = NULL)
                 ) ## column
               ), ## fluidRow
               hr(),
               h4("Observations"),
               fluidRow(
                 column(width = 6,
                        numericInput(inputId = "z",
                                     label   = "Observation \\(Z\\)",
                                     value   = NULL
                        ) ## numericInput
                 ), ## column
                 column(width = 6,
                        numericInput(inputId = "sigma_z",
                                     label   = "Observation uncertainty
                                           \\(\\sigma_Z\\)",
                                     value   = NULL,
                                     min     = 0.0
                        ) ## numericInput
                 ) ## column
               ) ## fluidRow
             ), ## sidePanel
             mainPanel(
               plotOutput(outputId = "data_plot")
             ) ## mainPanel
           ) ## sidebarLayout
  ), ## tabPanel

  ## Prior panel
  tabPanel("Priors",
           value = "priors",
           sidebarLayout(
             sidebarPanel(
               # h4("Priors"),
               fluidRow(
                 column(width = 6,
                        radioButtons(
                          inputId  = "model_priors",
                          label    = "Ensemble priors",
                          choices  = list(Reference   = "reference", 
                                          Informative = "informative"),
                          selected = "reference",
                          inline   = FALSE
                        ) ## priors
                 ),
                 column(width = 6,
                        uiOutput(outputId = "model_input_select")
                 )
               ),
               uiOutput(outputId = "model_prior_interface"),
               hr(),
               fluidRow(
                 column(width = 6,
                        radioButtons(
                          inputId  = "real_priors",
                          label    = "Real world priors",
                          choices  = list(Reference   = "reference", 
                                          Informative = "informative"),
                          selected = "reference",
                          inline   = FALSE
                        ) ## priors
                 ),
                 column(width = 6,
                        uiOutput(outputId = "real_input_select")
                 )
               ),
               uiOutput(outputId = "real_prior_interface")
             ), ## sidePanel

             mainPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Marginal priors",
                   fluidRow(
                     column(width = 6,
                            plotOutput(outputId = "alpha_prior_plot",
                                       height   = 200
                            ) ## plotOutput
                     ), ## column
                     column(width = 6,
                            plotOutput(outputId = "beta_prior_plot",
                                       height   = 200
                            ) ## plotOutput
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     column(width = 6,
                            plotOutput(outputId = "sigma_prior_plot",
                                       height   = 200
                            ) ## plotOutput
                     ), ## column
                     column(width = 6,
                            plotOutput(outputId = "xstar_prior_plot",
                                       height   = 200
                            ) ## plotOutput
                     ) ## column
                   ) ## fluidRow
                 ), ## tabPanel
                 tabPanel(
                   title = "Ensemble prior predictive",
                   plotOutput(outputId = "prior_predictive_plot")
                 ) ## tabPanel
               ) ## tabsetPanel
             ) ## mainPanel
           ) ## sidepanelLayout
  ), ## tabPanel

  ## Projection tab
  tabPanel("Projections",
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Discrepancy parameters",
                   fluidRow(
                     column(width = 6,
                            radioButtons(
                              inputId  = "discrepancy",
                              label    = "Discrepancy specification",
                              choices  = list(Guided = "guided", 
                                              Manual = "manual"),
                              selected = "guided",
                              inline   = FALSE
                            )
                     ), ## column
                     column(width = 6,
                            uiOutput(outputId = "discrepancy_input_select")
                     ) ## column
                   ), ## fluidRow
                   hr(),
                   uiOutput(outputId = "predictor"),
                   fluidRow(
                     column(width = 8,
                            uiOutput(outputId = "likelihood")
                     ), ## column
                     column(width = 4,
                            uiOutput(outputId = "likelihood_custom")
                     ) ## column
                   ), ## fluidRow
                   uiOutput(outputId = "alpha_discrepancy"),
                   uiOutput(outputId = "beta_discrepancy" ),
                   uiOutput(outputId = "rho_discrepancy"  ),
                   uiOutput(outputId = "sigma_discrepancy")
                 ), ## tabPanel
                 tabPanel(
                   title = "Plotting options",
                   h5("Labels"),
                   fluidRow(
                     column(width = 6,
                            textInput(inputId = "xlab",
                                      label   = "Predictor"
                            ) ## xlab
                     ), ## column
                     column(width = 6,
                            textInput(inputId = "ylab",
                                      label   = "Response"
                            ) ## ylab
                     ) ## column
                   ), ## fluidRow
                   hr(),
                   h5("Predictor limits"),
                   fluidRow(
                     column(width = 6,
                            numericInput(inputId = "xmin",
                                         label   = "Min",
                                         value   = NA
                            ) ## xmin
                     ), ## column
                     column(width = 6,
                            numericInput(inputId = "xmax",
                                          label   = "Max",
                                          value   = NA
                            ) ## xmax
                     ) ## column
                   ), ## fluidRow
                   h5("Response limits"),
                   fluidRow(
                     column(width = 6,
                            numericInput(inputId = "ymin",
                                         label   = "Min",
                                         value   = NA
                            ) ## ymin
                     ), ## column
                     column(width = 6,
                            numericInput(inputId = "ymax",
                                         label   = "Max",
                                         value   = NA
                            ) ## ymax
                     ) ## column
                   ), ## fluidRow
                   h5("Plotting colours"),
                   fluidRow(
                     column(width = 4,
                            selectInput(inputId = "ref_col",
                                        label   = "Reference",
                                        choices = list(Black   = "black",
                                                       Red     = "red",
                                                       Blue    = "blue",
                                                       Green   = "green",
                                                       Yellow  = "yellow",
                                                       Orange  = "orange",
                                                       Brown   = "brown",
                                                       Cyan    = "cyan",
                                                       Gold    = "gold",
                                                       Gray    = "gray",
                                                       Magenta = "magenta",
                                                       Pink    = "pink",
                                                       Purple  = "purple"),
                                        selected = "black"
                            ) ## ref_col
                     ), ## column
                     column(width = 4,
                            selectInput(inputId = "inf_col",
                                        label   = "Discrepancy",
                                        choices = list(Black   = "black",
                                                       Red     = "red",
                                                       Blue    = "blue",
                                                       Green   = "green",
                                                       Yellow  = "yellow",
                                                       Orange  = "orange",
                                                       Brown   = "brown",
                                                       Cyan    = "cyan",
                                                       Gold    = "gold",
                                                       Gray    = "gray",
                                                       Magenta = "magenta",
                                                       Pink    = "pink",
                                                       Purple  = "purple"),
                                        selected = "red"
                            ) ## inf_col
                     ), ## column
                     column(width = 4,
                            selectInput(inputId = "obs_col",
                                        label   = "Observations",
                                        choices = list(Black   = "black",
                                                       Red     = "red",
                                                       Blue    = "blue",
                                                       Green   = "green",
                                                       Yellow  = "yellow",
                                                       Orange  = "orange",
                                                       Brown   = "brown",
                                                       Cyan    = "cyan",
                                                       Gold    = "gold",
                                                       Gray    = "gray",
                                                       Magenta = "magenta",
                                                       Pink    = "pink",
                                                       Purple  = "purple"),
                                        selected = "blue"
                            ) ## obs_col
                     ) ## column
                   ), ## fluidRow
                   hr(),
                   h5("Legend"),
                   fluidRow(
                     column(width = 8,
                            selectInput(inputId = "legend_position",
                                        label   = "Position",
                                        choices = list("Bottom right" = "bottomright",
                                                        Bottom        = "botttom",
                                                       "Bottom left"  = "bottomleft",
                                                        Left          = "left",
                                                       "Top left"     = "topleft",
                                                        Top           = "top",
                                                       "Top right"    = "topright",
                                                        Right         = "right"),
                                        selected = "topright"
                            ) ## legend_position
                     ), ## column
                     column(width = 4,
                            checkboxInput(inputId = "legend_orientation",
                                          label   = "Horizontal",
                                          value   = FALSE
                            ) ## legend_orientation
                     ) ## column
                   ), ## fluidRow
                   hr(),
                   numericInput(inputId = "N",
                                label   = "Number of samples",
                                value   = 10000,
                                min     = 1000,
                                step    = 1000
                   ), ## numericInput
                   fluidRow(
                     column(width = 8,
                            selectInput(inputId  = "gamma",
                                        label    = "Interval width",
                                        choices  = list("68% (1 sd)" = 0.68,
                                                        "90%"        = 0.90,
                                                        "95% (2 sd)" = 0.95,
                                                        "99%"        = 0.99,
                                                        "Custom"     = "custom"),
                                        selected = 0.90
                            ) ## selectInput
                     ), ## column
                     column(width = 4,
                            uiOutput(outputId = "gamma_custom")
                     ) ## column
                   ) ## fluidRow
                 ) ## tabPanel
               ) ## tabsetPanel
             ), ## sidebarPanel
             
             mainPanel(
               tabsetPanel(
                 selected = "Marginal projections",
                 tabPanel(
                   title = "Discrepancies",
                   fluidRow(
                     column(
                       width = 6,
                       plotOutput(outputId = "alpha_discrepancy_plot")
                     ), ## column
                     column(
                       width = 6,
                       plotOutput(outputId =  "beta_discrepancy_plot")
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     column(
                       width = 6,
                       plotOutput(outputId = "sigma_discrepancy_plot")
                     ), ## column
                     column(
                       width = 6,
                       tableOutput(outputId = "discrepancies")
                     ) ## column
                   ) ## fluidRow
                  ),
                 tabPanel(
                   title = "Prior predictive distribution",
                   plotOutput(outputId = "prior_discrepancy_plot")
                 ),
                 tabPanel(
                   title = "Marginal projections",
                   plotOutput(outputId = "marginal_plot"),
                   fluidRow(
                     column(width = 4,
                            downloadButton(outputId = "save_marginal_plot",
                                           label    = "Download plot"
                            )
                     ), ## column
                     column(width = 4),
                     column(width = 4,
                            downloadButton(outputId = "save_samples",
                                           label    = "Download samples"
                            )
                     ) ## column
                   ), ## fluidRow
                   tableOutput(outputId = "predictive_intervals")
                 ), ## tabPanel
                 tabPanel(
                   title = "Joint projections",
                   plotOutput(outputId = "joint_plot"),
                   downloadButton(outputId = "save_joint_plot",
                                  label    = "Download plot"
                   )
                 ) ## tabPanel
               ) ## tabsetPanel
             ) ## mainPanel
           ) ## sidebarLayout
  ), ## tabPanel

  ## Diagnostics tab
  tabPanel(
    title = "Diagnostics",
    value = "diagnostics",
    navlistPanel(
      tabPanel(
        title = "Posterior summary statistics",
        tableOutput(outputId = "summary")
      ),
      tabPanel(
        title = "Model parameters",
        selectInput(inputId = "diag_var",
                    label   = "Select variable",
                    choices = list("Intercept"            = "alpha",
                                   "Slope"                = "beta",
                                   "Response spread"      = "sigma",
                                   "Real world predictor" = "xstar"),
                    selected = "alpha"
        ), ## diag_var
        hr(),
        plotOutput(outputId = "sample_plot"),
        fluidRow(
          column(width = 6,
                 plotOutput(outputId = "density_plot")
          ),
          column(width = 6,
                 plotOutput(outputId = "log_posterior_plot")
          )
        ), ## fluidRow
        plotOutput(outputId = "autocorrelation_plot")
      ), ## tabPanel
      tabPanel(
        title = "Sample information",
        plotOutput(outputId = "log_posterior_samples"),
        plotOutput(outputId = "log_posterior_density")
      )
    ) ## navlistPanel
  ) ## tabPanel

) ## ui

