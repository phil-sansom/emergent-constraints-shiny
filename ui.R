ui = fluidPage(
  title = "Uncertainty quantification for emergent constraints",
  titlePanel("Uncertainty quantification for emergent constraints"),
  withMathJax(),
  tabsetPanel(
    
    ## Overview tab
    tabPanel("Overview",
             mainPanel(
               h1("Statistical model:"),
               p("$$ \\begin{align} 
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
                         \\text{Observations}
                     \\end{align} $$"),
               h2("Priors"),
               p("$$ \\begin{align}
                       \\alpha  & \\sim \\text{Normal} 
                         (\\mu_\\alpha, \\sigma_\\alpha^2 ) &
                         \\text{Ensemble intercept} \\\\
                       \\beta   & \\sim \\text{Normal}
                         (\\mu_\\beta , \\sigma_\\beta^2  ) &
                         \\text{Ensemble slope} \\\\
                       \\sigma  & \\sim \\text{Half-Normal} 
                         (\\mu_\\sigma, \\sigma_\\sigma^2 ) &
                         \\text{Ensemble response uncertainty} \\\\
                       X_\\star & \\sim \\text{Normal}
                         (\\mu_{X_\\star} , \\sigma_{X_\\star}^2  ) &
                         \\text{Real world predictor}
                     \\end{align} $$"),
               h2("Discrepancies"),
               p("$$ \\begin{align}
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
                         \\text{Real world response uncertainty} \\\\
                     \\end{align} $$")
             )
    ),
    
    ## Data tab
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 uiOutput("predictor"),
                 uiOutput("response"),
                 uiOutput("xlab"),
                 uiOutput("ylab"),
                 uiOutput("xlim"),
                 uiOutput("ylim")
               ),
               mainPanel(
                 plotOutput(outputId = "dataPlot")
               )
             )
    ),
    
    ## Observation tab
    tabPanel("Observations",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "z"     , 
                              label = "Observation 
                                       \\(Z\\):",
                              value = 0.13, step = 0.01),
                 numericInput(inputId = "sigma_z", 
                              label = "Observation uncertainty 
                                       \\(\\sigma_Z\\):",
                              min = 0, value = 0.016, step = 0.001)
               ),
               mainPanel(
                 plotOutput(outputId = "obsPlot")
               )
             )
    ),
    
    ## Prior panel
    tabPanel("Priors",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "mu_alpha", 
                              label = "Intercept mean
                                       \\(\\mu_\\alpha\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_alpha", 
                              label = "Intercept uncertainty
                                       \\(\\sigma_\\alpha\\):",
                              value = 1e3, step = 1),
                 numericInput(inputId = "mu_beta", 
                              label = "Slope mean
                                       \\(\\mu_\\beta\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_beta", 
                              label = "Slope uncertainty
                                       \\(\\sigma_\\beta\\):",
                              value = 1e3, step = 1),
                 numericInput(inputId = "mu_sigma", 
                              label = "Spread mean
                                       \\(\\mu_\\sigma\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_sigma", 
                              label = "Spread uncertainty
                                       \\(\\sigma_\\sigma\\):",
                              value = 1e3, step = 1),
                 numericInput(inputId = "mu_xstar", 
                              label = "Real world predictor mean
                                       \\(\\mu_{X_\\star}\\):",
                              value = 0, step = 1),
                 numericInput(inputId = "sigma_xstar", 
                              label = "Real world predictor uncertainty
                                       \\(\\sigma_{X_\\star}\\):",
                              value = 1e3, step = 1)
               ),
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
                             min = 0, max =  3, value = 0, step = 0.1),
                 numericInput(inputId = "N", label = "Number of samples:",
                              value = 10000),
                 # numericInput(inputId = "mc.cores", label = "Number of cores:",
                 #              min = 1, max = 16, value = 4, step = 1),
                 selectInput(inputId = "alpha", label = "Credible interval:",
                             choices = list("68% (1 sd)" = 0.16,
                                            "90%"        = 0.05,
                                            "95% (2 sd)" = 0.025,
                                            "99%"        = 0.005),
                             selected = 0.05)
               ),
               mainPanel(
                 plotOutput(outputId = "ecPlot")
               )
             )
    )
    
  ) ## tabsetPanel
  
)
