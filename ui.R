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
           )
  ),
  
  ## Data tab
  tabPanel(title = "Data",
           sidebarLayout(
             
             sidebarPanel(
               
               tabsetPanel(
                 
                 tabPanel(
                   title = "Data and observations",
                   h4("Data"),
                   fileInput("file", "Choose CSV File",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                   ),
                   checkboxInput("header", "Header", TRUE),

                   hr(),
                   fluidRow(
                     column(6,
                       uiOutput("predictor")
                     ), ## column
                     column(6,
                       uiOutput("response")
                     ) ## column
                   ), ## fluidRow

                   hr(),
                   h4("Observations"),
                   fluidRow(
                     column(6, 
                            numericInput(inputId = "z"     , 
                                         label   = "Observation 
                                           \\(Z\\)",
                                         value   = 0)
                     ), ## column
                     column(6,
                            numericInput(inputId = "sigma_z", 
                                         label   = "Observation uncertainty 
                                           \\(\\sigma_Z\\)",
                                         value   = 1, 
                                         min     = 0)
                     ) ## column
                   ) ## fluidRow
                   
                 ), ## tabPanel
                 
                 tabPanel(
                   
                   title = "Plotting options",
                   uiOutput("xlab"),
                   uiOutput("ylab"),
                   uiOutput("xlim"),
                   uiOutput("ylim")
                   
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
                 column(6,
                        numericInput(inputId = "mu_alpha", 
                                     label   = "Intercept mean 
                                                \\(\\mu_\\alpha\\):",
                                     value   = 0)
                 ), ## column
                 column(6,
                        numericInput(inputId = "sigma_alpha", 
                                     label   = "Intercept uncertainty
                                                \\(\\sigma_\\alpha\\):",
                                     value   = 1e3, 
                                     min     = 0)
                 ) ## column
               ), ## fluidRow
               fluidRow(
                 column(6,
                        numericInput(inputId = "mu_beta", 
                                     label   = "Slope mean \\(\\mu_\\beta\\):",
                                     value   = 0)
                 ), ## column
                 column(6,
                        numericInput(inputId = "sigma_beta", 
                                     label   = "Slope uncertainty 
                                           \\(\\sigma_\\beta\\):",
                                     value   = 1e3, 
                                     min     = 0)
                 ) ## column
               ), ## fluidRow
               fluidRow(
                 column(6,
                        numericInput(inputId = "mu_sigma", 
                            label   = "Spread mean \\(\\mu_\\sigma\\):",
                            value   = 0, 
                            min     = 0)
                 ), ## column
                 column(6,
                        numericInput(inputId = "sigma_sigma", 
                                     label   = "Spread uncertainty 
                                       \\(\\sigma_\\sigma\\):",
                                     value   = 1e3, 
                                     min     = 0)
                 ) ## column
               ), ## fluidRow
               hr(),
               h4("Real world"),
               fluidRow(
                 column(6,
                        numericInput(inputId = "mu_xstar", 
                            label   = "Predictor mean
                                       \\(\\mu_{X_\\star}\\):",
                            value   = 0)
                 ),
                 column(6,
                        numericInput(inputId = "sigma_xstar", 
                                     label   = "Predictor uncertainty
                                                \\(\\sigma_{X_\\star}\\):",
                                     value   = 1e3, 
                                     min     = 0)
                 ) ## column
               ) ## fluidRow
             ), ## sidePanel
             
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "priorPlot", height = 400)
               ),
               fluidRow(
                 column(6, plotOutput(outputId = "alphaPlot", height = 200)),
                 column(6, plotOutput(outputId = "betaPlot" , height = 200))
               ),
               fluidRow(
                 column(6, plotOutput(outputId = "sigmaPlot", height = 200)),
                 column(6, plotOutput(outputId = "xstarPlot", height = 200))
               )
             )
           )
  ),
  
  ## Discrepancy tab
  tabPanel("Discrepancy",
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Discrepancy parameters",
                   sliderInput(inputId = "mu_delta_alpha", 
                               label = "Intercept bias 
                                      \\(\\mu_{\\delta_\\alpha}\\):",
                               min = 0, max =  3, value = 0, step = 0.1),
                   sliderInput(inputId = "sigma_delta_alpha", 
                               label = "Intercept uncertainty
                                      \\(\\sigma_{\\delta_\\alpha}\\):",
                               min = 0, max =  3, value = 0, step = 0.1),
                   sliderInput(inputId = "mu_delta_beta", 
                               label = "Slope bias 
                                      \\(\\mu_{\\delta_\\beta}\\):",
                               min = 0, max =  10, value = 0, step = 0.5),
                   sliderInput(inputId = "sigma_delta_beta", 
                               label = "Slope uncertainty
                                      \\(\\sigma_{\\delta_\\beta}\\):",
                               min = 0, max = 10, value = 0, step = 0.5),
                   sliderInput(inputId = "sigmad", 
                               label = "Spread uncertainty
                                      \\(\\sigma_{\\sigma_\\star}\\):",
                               min = 0, max =  3, value = 0, step = 0.1)
                 ), ## tabPanel
                 tabPanel(
                   title = "Plotting options",
                   numericInput(inputId = "N", label = "Number of samples:",
                                value = 10000, min = 1000, step = 1000),
                   # numericInput(inputId = "mc.cores", label = "Number of cores:",
                   #              min = 1, max = 16, value = 4, step = 1),
                   selectInput(inputId = "alpha", label = "Credible interval:",
                               choices = list("68% (1 sd)" = 0.16,
                                              "90%"        = 0.05,
                                              "95% (2 sd)" = 0.025,
                                              "99%"        = 0.005),
                               selected = 0.05)
                 ) ## tabPanel
               ) ## tabsetPanel
             ), ## sidebarPanel
             mainPanel(
               plotOutput(outputId = "ecPlot")
             ) ## mainPanel
           ) ## sidebarLayout
  ) ## tabPanel
           
) ## ui
