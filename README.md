library(rpact)
# Example: beta-spending function approach with O'Brien & Fleming alpha-spending
# function and Pocock beta-spending function
design1 <- getDesignGroupSequential(kMax = 5,
    sided = 2, alpha = 0.05, beta = 0.2,
    informationRates = c(0.2, 0.4, 0.6,0.8,1),
    typeOfDesign = "asOF",
    typeBetaSpending = "bsP",bindingFutility = FALSE
)
# Sample size calculation assuming event probabilities of in control and treatment
# (pi2 controal) vs (pi1 treatment) in intervention
sampleSizeResultGS1 <- getSampleSizeRates(design1, pi2 = reten_1_30, pi1 = reten_1_40,
                                         sided = 2, alpha = 0.05, beta = 0.2)
# Standard rpact output (sample size object only, not design object)
kable(sampleSizeResultGS1)
kable(summary(sampleSizeResultGS1))

designChar <- getDesignCharacteristics(design1)
kable(designChar)

plot(design,type = 1, showSource = TRUE)
plot(design,type = 4, showSource = TRUE)
