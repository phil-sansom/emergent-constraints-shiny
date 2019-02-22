ui = navbarPage(
  title = "Uncertainty quantification for emergent constraints",

  ## Overview tab
  tabPanel(title = "Overview",
           withMathJax(),
           mainPanel(
             p("$$ \\begin{align}

                       \\text{Statistical model} \\\\
                       Y_m & = \\alpha + \\beta X_m + \\epsilon_m &
                         \\epsilon_m & \\sim
                           \\text{Normal} ( 0, \\sigma^2 ) \\quad
                         (m = 1,\\ldots,M) &
                         \\text{Multi-model ensemble} \\\\
                       Y_\\star & = \\alpha_\\star + \\beta_\\star X_\\star +
                           \\epsilon_\\star &
                         \\epsilon_\\star & \\sim
                           \\text{Normal} ( 0, \\sigma_\\star^2 ) &
                         \\text{Real world} \\\\
                       Z & = X_\\star + \\omega &
                         \\omega & \\sim \\text{Normal} ( 0, \\sigma_Z^2 ) &
                         \\text{Observations} \\\\ ~ \\\\

                       \\text{Discrepancies} \\\\
                       \\alpha_\\star & = \\alpha + \\delta_\\alpha &
                          \\delta_\\alpha & \\sim \\text{Normal}
                            (\\mu_{\\delta_\\alpha},
                              \\sigma_{\\delta_\\alpha}^2) &
                         \\text{Real world Intercept} \\\\
                       \\beta_\\star & = \\beta + \\delta_\\beta &
                          \\delta_\\beta & \\sim \\text{Normal}
                            (\\mu_{\\delta_\\beta},
                              \\sigma_{\\delta_\\beta}^2) &
                         \\text{Real world slope} \\\\
                       \\sigma_\\star^2 & =
                           \\sigma^2 + \\sigma_{\\sigma_\\star}^2 & & &
                         \\text{Real world response uncertainty} \\\\ ~ \\\\

                       \\text{Priors} \\\\
                       & &
                       \\alpha  & \\sim \\text{Normal}
                         (\\mu_\\alpha, \\sigma_\\alpha^2 ) &
                         \\text{Ensemble intercept} \\\\
                       & &
                       \\beta   & \\sim \\text{Normal}
                         (\\mu_\\beta , \\sigma_\\beta^2  ) &
                         \\text{Ensemble slope} \\\\
                       & &
                       \\sigma  & \\sim \\text{Half-Normal}
                         (\\mu_\\sigma, \\sigma_\\sigma^2 ) &
                         \\text{Ensemble response uncertainty} \\\\
                       & &
                       X_\\star & \\sim \\text{Normal}
                         (\\mu_{X_\\star} , \\sigma_{X_\\star}^2  ) &
                         \\text{Real world predictor}

                     \\end{align} $$")
           ) ## mainPanel
  ), ## tabPanel

  ## Data tab
  tabPanel(title = "Data",
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
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(
               h4("Reference priors"),
               checkboxInput(inputId = "reference",
                             label   = "Use reference priors",
                             value   = TRUE
               ), ## checkboxInput
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
                            plotOutput(outputId = "alpha_plot",
                                       height   = 200
                            ) ## plotOutput
                     ), ## column
                     column(width = 6,
                            plotOutput(outputId = "beta_plot",
                                       height   = 200
                            ) ## plotOutput
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     column(width = 6,
                            plotOutput(outputId = "sigma_plot",
                                       height   = 200
                            ) ## plotOutput
                     ), ## column
                     column(width = 6,
                            plotOutput(outputId = "xstar_plot",
                                       height   = 200
                            ) ## plotOutput
                     ) ## column
                   ) ## fluidRow
                 ), ## tabPanel
                 tabPanel(
                   title = "Ensemble prior predictive",
                   plotOutput(outputId = "joint_prior_plot")
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
               h5("Intercept \\(\\alpha_\\star\\)"),
               sliderInput(inputId = "mu_delta_alpha",
                           label   = "Bias \\(\\mu_{\\delta_\\alpha}\\)",
                           min     = -1,
                           max     = +1,
                           value   = 0,
                           step    = 0.1,
                           ticks   = FALSE
               ), ## sliderInput
               sliderInput(inputId = "sigma_delta_alpha",
                           label   = "Uncertainty
                                         \\(\\sigma_{\\delta_\\alpha}\\)",
                           min     = 0,
                           max     = 1,
                           value   = 0,
                           step    = 0.1,
                           ticks   = FALSE
               ), ## sliderInput
               hr(),
               h5("Slope \\(\\beta_\\star\\)"),
               sliderInput(inputId = "mu_delta_beta",
                           label   = "Bias
                                         \\(\\mu_{\\delta_\\beta}\\)",
                           min     = -1,
                           max     = +1,
                           value   = 0,
                           step    = 0.1,
                           ticks   = FALSE
               ), ## sliderInput
               sliderInput(inputId = "sigma_delta_beta",
                           label   = "Uncertainty
                                         \\(\\sigma_{\\delta_\\beta}\\)",
                           min     = 0,
                           max     = 1,
                           value   = 0,
                           step    = 0.1,
                           ticks   = FALSE
               ), ## sliderInput
               hr(),
               h5("Spread \\(\\sigma_\\star\\)"),
               sliderInput(inputId = "sigma_sigma_star",
                           label   = "Uncertainty
                                         \\(\\sigma_{\\sigma_\\star}\\)",
                           min     = 0,
                           max     = 1,
                           value   = 0,
                           step    = 0.1,
                           ticks   = FALSE
               ), ## sliderInput
               hr(),
               numericInput(inputId = "N",
                            label   = "Number of samples",
                            value   = 10000,
                            min     = 1000,
                            step    = 1000
               ), ## numericInput
               selectInput(inputId  = "gamma",
                           label    = "Interval width",
                           choices  = list("68% (1 sd)" = 0.68,
                                           "90%"        = 0.90,
                                           "95% (2 sd)" = 0.95,
                                           "99%"        = 0.99),
                           selected = 0.90
               ) ## selectInput
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
