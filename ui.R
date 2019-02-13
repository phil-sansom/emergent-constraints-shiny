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
               tabsetPanel(
                 tabPanel(
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
                   hr(),
                   fluidRow(
                     column(width = 6,
                            uiOutput("predictor")
                     ), ## column
                     column(width = 6,
                            uiOutput("response")
                     ) ## column
                   ), ## fluidRow
                   hr(),
                   h4("Observations"),
                   fluidRow(
                     column(width = 6, 
                            numericInput(inputId = "z", 
                                         label   = "Observation \\(Z\\)",
                                         value   = 0
                            ) ## numericInput
                     ), ## column
                     column(width = 6,
                            numericInput(inputId = "sigma_z", 
                                         label   = "Observation uncertainty 
                                           \\(\\sigma_Z\\)",
                                         value   = 1, 
                                         min     = 0
                            ) ## numericInput
                     ) ## column
                   ) ## fluidRow
                 ), ## tabPanel
                 tabPanel(
                   title = "Plotting options",
                   fluidRow(
                     h4("Axis labels"),
                     column(width = 6,
                            uiOutput("xlab")
                     ), ## column
                     column(width = 6,
                            uiOutput("ylab")
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     h4("Predictor limits"),
                     column(width = 6,
                            uiOutput("xmin")
                     ), ## column
                     column(width = 6,
                            uiOutput("xmax")
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     h4("Response limits"),
                     column(width = 6,
                            uiOutput("ymin")
                     ), ## column
                     column(width = 6,
                            uiOutput("ymax")
                     ) ## column
                   ) ## fluidRow
                 ) ## tabPanel
               ) ## tabsetPanel
             ), ## sidePanel
             mainPanel(
               plotOutput(outputId = "dataPlot")
             ) ## mainPanel
           ) ## sidebarLayout
  ), ## tabPanel
  
  ## Prior panel
  tabPanel("Priors",
           sidebarLayout(
             sidebarPanel(
               h4("Multi-model ensemble"),
               fluidRow(
                 h5("Intercept \\(\\alpha\\)"),
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
               fluidRow(
                 h5("Slope \\(\\beta\\)"),
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
               fluidRow(
                 h5("Response spread \\(\\sigma\\)"),
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
               hr(),
               h4("Real world"),
               fluidRow(
                 h5("Predictor \\(X_\\star\\)"),
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
               fluidRow(
                 plotOutput(outputId = "priorPlot", 
                            height   = 400
                 ) ## plotOutput
               ), ## fluidRow
               fluidRow(
                 column(width = 6, 
                        plotOutput(outputId = "alphaPlot", 
                                   height   = 200
                        ) ## plotOutput
                 ), ## column
                 column(width = 6, 
                        plotOutput(outputId = "betaPlot", 
                                   height   = 200
                        ) ## plotOutput
                 ) ## column
               ), ## fluidRow
               fluidRow(
                 column(width = 6, 
                        plotOutput(outputId = "sigmaPlot", 
                                   height   = 200
                        ) ## plotOutput
                 ), ## column
                 column(width = 6, 
                        plotOutput(outputId = "xstarPlot", 
                                   height   = 200
                        ) ## plotOutput
                 ) ## column
               ) ## fluidRow
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
                     h5("Intercept \\(\\alpha_\\star\\)"),
                     column(width = 6,
                            numericInput(inputId = "mu_delta_alpha", 
                                         label   = "Bias 
                                         \\(\\mu_{\\delta_\\alpha}\\)",
                                         value   = 0
                            ) ## numericInput
                     ), ## column
                     column(width = 6,
                            numericInput(inputId = "sigma_delta_alpha", 
                                         label   = "Uncertainty 
                                         \\(\\sigma_{\\delta_\\alpha}\\)",
                                         value   = 0, 
                                         min     = 0
                            ) ## numericInput
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     h5("Slope \\(\\beta_\\star\\)"),
                     column(width = 6,
                            numericInput(inputId = "mu_delta_beta", 
                                         label   = "Bias 
                                         \\(\\mu_{\\delta_\\beta}\\)",
                                         value   = 0
                            ) ## numericInput
                     ), ## cloumn
                     column(width = 6,
                            numericInput(inputId = "sigma_delta_beta", 
                                         label   = "Uncertainty
                                         \\(\\sigma_{\\delta_\\beta}\\)",
                                         value   = 0,
                                         min     = 0
                            ) ## numericInput
                     ) ## column
                   ), ## fluidRow
                   fluidRow(
                     h5("Spread \\(\\sigma_\\star\\)"),
                     column(width = 6
                     ), ## cloumn
                     column(width = 6,
                            numericInput(inputId = "sigmad", 
                                         label   = "Uncertainty
                                         \\(\\sigma_{\\sigma_\\star}\\)",
                                         value   = 0,
                                         min     = 0
                            ) ## numericInput
                     ) ## column
                   ) ## fluidRow
                 ), ## tabPanel
                 
                 tabPanel(
                   title = "Plotting options",
                   numericInput(inputId = "N", 
                                label   = "Number of samples",
                                value   = 10000, 
                                min     = 1000, 
                                step    = 1000
                   ), ## numericInput
                   # numericInput(inputId = "mc.cores", 
                   #              label   = "Number of cores",
                   #              value   = 4,
                   #              min     = 1, 
                   #              max     = 16, 
                   #              step    = 1
                   # ), ## numericInput
                   selectInput(inputId  = "alpha", 
                               label    = "Credible interval",
                               choices  = list("68% (1 sd)" = 0.160,
                                               "90%"        = 0.050,
                                               "95% (2 sd)" = 0.025,
                                               "99%"        = 0.005),
                               selected = 0.05
                   ) ## selectInput
                 ) ## tabPanel
               ) ## tabsetPanel
             ), ## sidebarPanel
             mainPanel(
               plotOutput(outputId = "mainPlot"),
               plotOutput(outputId = "auxPlot")
             ) ## mainPanel
           ) ## sidebarLayout
  ) ## tabPanel
           
) ## ui
