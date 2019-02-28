###############
## Constants ##
###############

## Parameter labels
parameter_labels = c(expression(paste("Intercept ", alpha)),
                     expression(paste("Slope ", beta)),
                     expression(paste("Response spread ", sigma)),
                     expression(paste("Real world predictor ", X["*"])),
                     expression(paste("Intercept discrepancy ", alpha["*"])),
                     expression(paste("Slope discrepancy ", beta)["*"]),
                     expression(paste("Response spread discrepancy ", sigma["*"])),
                     "Log Posterior")
names(parameter_labels) = c("alpha","beta","sigma","xstar",
                            "alphastar","betastar","sigmastar","lp__")

## User defined colours
alpha_black = rgb(0, 0, 0, 0.5)
alpha_red   = rgb(1, 0, 0, 0.5)
alpha_green = rgb(0, 1, 0, 0.5)
alpha_blue  = rgb(0, 0, 1, 0.5)
