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
                       \\left[ \\begin{array}{c}
                         \\alpha_\\star \\\\ \\beta_\\star
                       \\end{array} \\right]
                         & = \\left[ \\begin{array}{c} 
                               \\alpha \\\\ \\beta 
                             \\end{array} \\right]
                           + \\left[ \\begin{array}{c} 
                               \\delta_\\alpha \\\\ \\delta_\\beta
                             \\end{array} \\right] &
                       \\left[ \\begin{array}{c} 
                         \\delta_\\alpha \\\\ \\delta_\\beta
                       \\end{array} \\right] & 
                       \\sim \\text{Normal} \\left(
                         \\left[ \\begin{array}{c} 
                           \\mu_{\\delta_\\alpha} \\\\ \\mu_{\\delta_\\beta}
                         \\end{array} \\right],
                         \\left[ \\begin{array}{cc} 
                           \\sigma_{\\delta_\\alpha}^2 &
                             \\rho_\\delta \\sigma_{\\delta_\\alpha}
                               \\sigma_{\\delta_\\beta} \\\\
                           \\rho_\\delta \\sigma_{\\delta_\\alpha}
                             \\sigma_{\\delta_\\beta} &
                               \\sigma_{\\delta_\\beta}^2
                         \\end{array} \\right]
                       \\right) &
                       \\begin{array}{r}
                         \\text{Real world intercept} \\\\
                         \\text{Real world slope}
                       \\end{array} \\\\
                       \\sigma_\\star^2 & =
                           \\sigma^2 + \\delta_\\sigma^2 & 
                       \\delta_\\sigma & \\sim
                       \\text{Half-Normal} \\left( 
                           \\mu_{\\delta_\\sigma}, \\sigma_{\\delta_\\sigma}^2
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
                       \\sigma  & \\sim \\text{Half-Normal}
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
               fluidRow(
                 column(width = 6,
                        textInput(inputId = "xlab",
                                  label   = "Predictor label",
                                  value   = NULL)
                 ), ## column
                 column(width = 6,
                        textInput(inputId = "ylab",
                                  label   = "Response label",
                                  value   = NULL
                        )
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
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(
               h4("Priors"),
               radioButtons(
                 inputId  = "priors",
                 label    = "Select priors",
                 choices  = list(Reference   = "reference", 
                                 Informative = "informative"),
                 selected = "reference",
                 inline   = TRUE
               ),
               hr(),
               h4("Informative priors"),
               h5("Intercept \\(\\alpha\\)"),
               fluidRow(
                 column(width = 6,
                        numericInput(inputId = "mu_alpha",
                                     label   = "Mean \\(\\mu_\\alpha\\)",
                                     value   = 0
                        ) ## numericInput
                 ), ## column
                 column(width = 6,
                        numericInput(inputId = "sigma_alpha",
                                     label   = "Standard deviation
                                                \\(\\sigma_\\alpha\\)",
                                     value   = 1e3,
                                     min     = 0
                        ) ## numericInput
                 ) ## column
               ), ## fluidRow
               h5("Slope \\(\\beta\\)"),
               fluidRow(
                 column(width = 6,
                        numericInput(inputId = "mu_beta",
                                     label   = "Mean \\(\\mu_\\beta\\)",
                                     value   = 0
                        ) ## numericInput
                 ), ## column
                 column(width = 6,
                        numericInput(inputId = "sigma_beta",
                                     label   = "Standard deviation
                                                \\(\\sigma_\\beta\\)",
                                     value   = 1e3,
                                     min     = 0
                        ) ## numericInput
                 ) ## column
               ), ## fluidRow
               h5("Correlation \\(\\rho\\)"),
               sliderInput(
                 inputId = "rho",
                 label   = "Corr(\\(\\alpha,\\beta\\))",
                 value   =  0,
                 min     = -1,
                 max     = +1,
                 step    = 0.01,
                 ticks   = FALSE
               ),
               h5("Response spread \\(\\sigma\\)"),
               fluidRow(
                 column(width = 6,
                        numericInput(inputId = "mu_sigma",
                                     label   = "Location \\(\\mu_\\sigma\\)",
                                     value   = 0,
                                     min     = 0
                        ) ## numericInput
                 ), ## column
                 column(width = 6,
                        numericInput(inputId = "sigma_sigma",
                                     label   = "Scale \\(\\sigma_\\sigma\\)",
                                     value   = 1e3,
                                     min     = 0
                        ) ## numericInput
                 ) ## column
               ), ## fluidRow
               # hr(),
               # h4("Real world"),
               h5("Real world predictor \\(X_\\star\\)"),
               fluidRow(
                 column(width = 6,
                        numericInput(inputId = "mu_xstar",
                                     label   = "Mean \\(\\mu_{X_\\star}\\)",
                                     value   = 0
                        ) ## numericInput
                 ), ## column
                 column(width = 6,
                        numericInput(inputId = "sigma_xstar",
                                     label   = "Standard deviation
                                                \\(\\sigma_{X_\\star}\\)",
                                     value   = 1e3,
                                     min     = 0
                        ) ## numericInput
                 ) ## column
               ) ## fluidRow
             ), ## sidePanel

             mainPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Marginal priors",
                   fluidRow(
                     column(width = 6,
                            plotOutput(outputId = "alpha_prior",
                                       height   = 200
                            ) ## plotOutput
                     ), ## column
                     column(width = 6,
                            plotOutput(outputId = "beta_prior",
                                       height   = 200
                            ) ## plotOutput
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     column(width = 6,
                            plotOutput(outputId = "sigma_prior",
                                       height   = 200
                            ) ## plotOutput
                     ), ## column
                     column(width = 6,
                            plotOutput(outputId = "xstar_prior",
                                       height   = 200
                            ) ## plotOutput
                     ) ## column
                   ) ## fluidRow
                 ), ## tabPanel
                 tabPanel(
                   title = "Ensemble prior predictive",
                   plotOutput(outputId = "prior_predictive")
                 ) ## tabPanel
               ) ## tabsetPanel
             ) ## mainPanel
           ) ## sidepanelLayout
  ), ## tabPanel

  ## Projection tab
  tabPanel("Projections",
           sidebarLayout(
             sidebarPanel(
               title = "Discrepancy parameters",
               radioButtons(
                 inputId  = "discrepancy_inputs",
                 label    = "Select inputs",
                 choices  = list(Sliders = "sliders", Numerical = "numerical"),
                 selected = "sliders",
                 inline   = TRUE
               ),
               hr(),
               h5("Intercept \\(\\alpha_\\star\\)"),
               uiOutput(outputId = "mu_delta_alpha"),
               uiOutput(outputId = "sigma_delta_alpha"),
               hr(),
               h5("Slope \\(\\beta_\\star\\)"),
               uiOutput(outputId = "mu_delta_beta"),
               uiOutput(outputId = "sigma_delta_beta"),
               hr(),
               h5("Correlation \\(\\rho_\\delta\\)"),
               uiOutput(outputId = "rho_delta"),
               hr(),
               h5("Spread \\(\\sigma_\\star\\)"),
               uiOutput(outputId = "mu_delta_sigma"),
               uiOutput(outputId = "sigma_delta_sigma"),
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
             ), ## sidebarPanel
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Marginal projections",
                   plotOutput(outputId = "marginal_plot"),
                   fluidRow(
                     column(width = 6,
                            sliderInput(inputId = "xlim_marginal",
                                        label   = "X limits",
                                        value   = c(0,1),
                                        min     = 0,
                                        max     = 1,
                                        step    = 0.1,
                                        ticks   = FALSE
                            ) ## xlim_marginal
                     ) ## column
                   ), ## fluidRow
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
                   fluidRow(
                     column(width = 6,
                            sliderInput(inputId = "xlim_joint",
                                        label   = "X limits",
                                        value   = c(0,1),
                                        min     = 0,
                                        max     = 1,
                                        step    = 0.1,
                                        ticks   = FALSE
                            ) ## xlim_joint
                     ), ## column
                     column(width = 6,
                            sliderInput(inputId = "ylim_joint",
                                        label   = "Y limits",
                                        value   = c(0,1),
                                        min     = 0,
                                        max     = 1,
                                        step    = 0.1,
                                        ticks   = FALSE
                            ) ## ylim_joint
                     ) ## column
                   ), ## fluidRow
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

