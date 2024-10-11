```{r}
# loading libraries
library(tidyverse)
library(knitr)
library(MASS)
library(patchwork)
library(broom)
library(rpact)
library(effsize)
library(pwr)
```

```{r}
data <- read.csv("data/cookiecats.csv") # import dataset or subset 
data$retention_1 <- as.logical(data$retention_1)
data$retention_7 <- as.logical(data$retention_7)
data$retention_1 <- as.numeric(data$retention_1)
data$retention_7 <- as.numeric(data$retention_7)
# calculate the sum users and users per group
nall <- nrow(data)
groupnumber <- table(data$version)
groupnumber
# the average game rounds per group and whole
agrall <- mean(data$sum_gamerounds)
agr <- tapply(data$sum_gamerounds, data$version, mean)
# the retention rate per group and whole
# day1
ret.1 <- mean(data$retention_1)
ret34.1 <- tapply(data$retention_1,data$version, mean)
Count.1 <- table(data$retention_1, data$version)
# day7
ret.7 <- mean(data$retention_7)
ret34.7 <- tapply(data$retention_7,data$version, mean)
Count.7 <- table(data$retention_7, data$version)
# SRM checking
n30 <- 44700
n40 <- 45489
total_number <- n40 + n30
observed <- c(n30, n40)
expected <- c(total_number/2, total_number/2)
srm_test <- chisq.test(observed, p = expected / sum(expected))
print(srm_test)
```

```{r}
data <- read.csv("data/cookiecats_subset.csv")
data$retention_1 <- as.logical(data$retention_1)
data$retention_7 <- as.logical(data$retention_7)
data$retention_1 <- as.numeric(data$retention_1)
data$retention_7 <- as.numeric(data$retention_7)
ret.1 <- mean(data$retention_1)
total_number <- n40 + n30
ret.7 <- mean(data$retention_7)
agrall <- mean(data$sum_gamerounds)
```

```{r}
# plot pie and bar 
grouped_data <- group_by(data, version)
average_gamerounds <- summarise(grouped_data, mean_gamerounds = 
                                  mean(sum_gamerounds))
ggplot(average_gamerounds, aes(x = version, y = mean_gamerounds, 
                               fill = version)) +
  geom_bar(stat = "identity") +
  labs(x = "Version",
       y = "Average Game Rounds") +
  geom_text(aes(label = round(mean_gamerounds, 1)) ) +
  theme_minimal()
```

```{r}
nall <- nrow(data)
n30 <- 44000
n40 <- 44000
gate_30_data <- data[data$version == "gate_30", ] # version gate_30
gate_40_data <- data[data$version == "gate_40", ] # version gate_40
avgr30 <- mean(gate_30_data$sum_gamerounds)
avgr40 <- mean(gate_40_data$sum_gamerounds)
reten_1_30 <- mean(gate_30_data$retention_1)
reten_1_40 <- mean(gate_40_data$retention_1)
reten_1_all <- mean(data$retention_1)
reten_7_30 <- mean(gate_30_data$retention_7)
reten_7_40 <- mean(gate_40_data$retention_7)
reten_7_all <- mean(data$retention_7)
leave_1_30 <- 1 - reten_1_30
leave_1_40 <- 1 - reten_1_40
leave_1_all <- 1 - reten_1_all
leave_7_30 <- 1 - reten_7_30
leave_7_40 <- 1 - reten_7_40
leave_7_all <- 1 - reten_7_all

pie <- function(reten_value, leave_value, title) {
  df <- data.frame(
    category = c("Retention", "Leave"),
    value = c(reten_value, leave_value)
  )
  
  ggplot(df, aes(x = "", y = value, fill = category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = scales::percent(value/sum(value))),
    position = position_stack(vjust = 0.5))+
    labs(title = title, fill = "Status") +
    theme_void()
}
p1 <- pie(reten_1_30, leave_1_30, "Gate30 Retention day1")
p2 <- pie(reten_1_40, leave_1_40, "Gate40 Retention day1")
p3 <- pie(reten_7_30, leave_7_30, "Gate30 Retention day7")
p4 <- pie(reten_7_40, leave_7_40, "Gate40 Retention day7")

(p1 | p2) / (p3 | p4)
```

```{r}
library(pwr)
library(effsize)
# using equation 5.11
var30 <- var(gate_30_data$sum_gamerounds)
var40 <- var(gate_40_data$sum_gamerounds)
se30gr <-sd(gate_30_data$sum_gamerounds) 
se40gr <- sd(gate_40_data$sum_gamerounds)
std_pooled_gr <- sqrt((((n30 - 1)*var30)+ ((n40 - 1)*var40))
                      /(n30 + n40 - 2))
d.gr <- abs(avgr30 - avgr40)
r <- 1
n_gr <- ((1 + r)*((qnorm(0.975) + qnorm(0.8))^2) * (std_pooled_gr^2))/
  (1 *(d.gr^2))
#using package by cohend's d
cohend <- cohen.d(gate_30_data$sum_gamerounds,gate_40_data$sum_gamerounds,
                  pooled = TRUE)
 cohen.d <- cohend$estimate
n_gr1 <- pwr.t.test(d = cohen.d, sig.level = 0.05, power = 0.8, 
                   type = "two.sample",
                    alternative = "two.sided")
n.gr <- n_gr1$n

power_value<- pwr.t.test(d = cohen.d, n = 44000, sig.level = 0.05, 
                         type = "two.sample", alternative = "two.sided")
# 13.1% power 
# using equation 5.12
# For day1 retention rate 
std_pooled_1 <- sqrt((reten_1_30*(1 - reten_1_30)) + 
                       (reten_1_40*(1 - reten_1_40)*r))
# std_pooled_1^2 is the variance day1
d.ren1 <- abs(reten_1_30 - reten_1_40)
N_ren1 <- ((qnorm(0.975) + (qnorm(0.8)))^2 * (std_pooled_1^2))/
  (d.ren1^2)
## std_pooled_7^2 is the variance day7
std_pooled_7 <- sqrt((reten_7_30*(1 - reten_7_30)) + 
                       (reten_7_40*(1 - reten_7_40)*r))
d.ren7 <- abs(reten_7_30 - reten_7_40)
N_ren7 <- ((qnorm(0.975) + (qnorm(0.8)))^2 * (std_pooled_7^2))/
  (d.ren7^2)
# using cohen'h
cohen.h1 <- ES.h(reten_1_30,reten_1_40)
N.reten1 <- pwr.2p.test(h = cohen.h1, sig.level = 0.05, power = 0.8, 
                      alternative = "two.sided")
N.ren1 <- N.reten1$n
cohen.h7 <- ES.h(reten_7_30,reten_7_40)
N.reten7 <- pwr.2p.test(h = cohen.h7, sig.level = 0.05, power = 0.8, 
                      alternative = "two.sided")
N.ren7 <- N.reten7$n
power_value1 <- pwr.2p.test(h = cohen.h1, n = 44000, sig.level = 0.05,
                          alternative = "two.sided")
```

```{r}
#build welch T statistic
grgate30 <- gate_30_data$sum_gamerounds
grgate40 <- gate_40_data$sum_gamerounds
welch.t <- t.test(grgate30, grgate40, alternative = "two.sided", 
                  var.equal = FALSE, conf.level = 0.95)
# build Z statistic
se.1 <- sqrt(reten_1_all*(1 - reten_1_all)*(1/n30+1/n40))
z.value <- (reten_1_30 - reten_1_40)/se.1
p.value <- 2*(1 - pnorm(abs(z.value)))
print(p.value)
se.7 <- sqrt(reten_7_all*(1 - reten_7_all)*(1/n30+1/n40))
z.value <- (reten_7_30 - reten_7_40)/se.7
p.value <- 2*(1 - pnorm(abs(z.value)))
print(p.value)

# buile chi-squre statistic
n30true1 <- sum(gate_30_data$retention_1 ==1)
n40true1 <- sum(gate_40_data$retention_1 ==1)
n30true7 <- sum(gate_30_data$retention_7 ==1)
n40true7 <- sum(gate_40_data$retention_7 ==1)
stay1 <- c(n30true1,n40true1)
stay7 <- c(n30true7,n40true7)
n <- c(n30,n40)
ztest <- prop.test(stay7,n,alternative = "two.sided")# change stay1 or stay7
print(ztest)
```

```{r}
# established logistical model 
data$version <- factor(data$version, levels = c("gate_40", "gate_30"))
logmodel <- glm(retention_7 ~ version, family = binomial(link = "logit"),
                data = data) # retention_7 or retention_1
summary(logmodel)
RAhat <- predict(logmodel, type = "response")
mean(RAhat)
odds <- exp(-1.49904 + 0.04874 * 1)
```

```{r}
# loading new data with time variable
data1 <- read.csv("data/cookiecats_subset1.csv")
data1$retention_1_t <- as.logical(data1$retention_1_t)
data1$retention_7_t <- as.logical(data1$retention_7_t)
data1$retention_1_t <- as.numeric(data1$retention_1_t)
data1$retention_7_t <- as.numeric(data1$retention_7_t)
data1$retention_1_c <- as.logical(data1$retention_1_c)
data1$retention_7_c <- as.logical(data1$retention_7_c)
data1$retention_1_c <- as.numeric(data1$retention_1_c)
data1$retention_7_c <- as.numeric(data1$retention_7_c)
```

```{r}
# choosing pocock boundary 
library(rpact)
# Example: beta-spending function approach with Pocock spending function
# function and Pocock beta-spending function
designgr <- getDesignGroupSequential(
    sided = 2, alpha = 0.05, beta = 0.2, kMax = 10,
    informationRates = c(0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
    typeOfDesign = "asP",
    typeBetaSpending = "bsP",bindingFutility = FALSE,twoSidedPower = FALSE
)
plot(designgr, type = 1)
summary(designgr)

# ASE and maximum sample size
sampleSizeResultgr <- getSampleSizeMeans(designgr, groups = 2,
                     normalApproximation = FALSE, meanRatio = FALSE,
                     thetaH0 = 0, alternative = d.gr,stDev = std_pooled_gr,
                     allocationRatioPlanned = 1 )
summary(sampleSizeResultgr)
designChargr <- getDesignCharacteristics(designgr)
summary(designChargr)
sampleSizeResultgr[["earlyStop"]]
sampleSizeResultgr[["numberOfSubjects"]]
sampleSizeResultgr[["maxNumberOfSubjects"]]
ASNgr <- sampleSizeResultgr[["expectedNumberOfSubjectsH01"]]
fixed_ngr <- 2*n_gr
saverate.gr <-  abs((ASNgr - fixed_ngr))/fixed_ngr
(1009039-712317)/1009039

```

```{r}
# the total sample size for RA1
fixed_nr1 <- 2*N_ren1
# beta-spending function Pocock; O'Brien & Fleming alpha-spending function
# k = 5 keeping 
design1 <- getDesignGroupSequential( kMax = 5,
    sided = 2, alpha = 0.05, beta = 0.2,
    informationRates = c(0.2, 0.4, 0.6,0.8,1),
    typeOfDesign = "asOF",
    typeBetaSpending = "bsP",bindingFutility = FALSE,twoSidedPower = FALSE
)
plot(design1, 1)
summary(design1)
# (pi2 control) vs (pi1 treatment) in intervention
sampleSizeResultGS1 <- getSampleSizeRates(design1, pi1 = reten_1_30, 
                                         pi2 = reten_1_40,
                                    allocationRatioPlanned = 1,
                                    groups = 2,normalApproximation = TRUE)
# Standard rpact output 
summary(sampleSizeResultGS1)
designCharr1 <- getDesignCharacteristics(design1)
summary(designCharr1)
sampleSizeResultGS1[["numberOfSubjects"]]
sampleSizeResultGS1[["earlyStop"]]
sampleSizeResultGS1[["maxNumberOfSubjects"]]
sampleSizeResultGS1[["expectedNumberOfSubjectsH1"]]
sampleSizeResultGS1[["expectedNumberOfSubjectsH01"]]
sampleSizeResultGS1[["expectedNumberOfSubjectsH0"]]
ASNgr <- sampleSizeResultGS1[["expectedNumberOfSubjectsH01"]]
#stage 1 for retention rate diff testing on day1
data_stage1d1 <- data1[1:29218, ]
meand1stage1 <-apply(data_stage1d1[,c("retention_1_t","retention_1_c")],2,mean)
rate1.t <- meand1stage1[1]
rate1.c <- meand1stage1[2]
staystage1_t <- sum(data_stage1d1$retention_1_t)
staystage1_c <- sum(data_stage1d1$retention_1_c)
sizestage1 <- nrow(data_stage1d1)
d1ra.stage1 <- (staystage1_c+staystage1_t)/(2*sizestage1)
se.11 <- sqrt(d1ra.stage1*(1 - d1ra.stage1)*(1/sizestage1+1/sizestage1))
z.value.11 <- (rate1.t - rate1.c)/se.11
unname(z.value.11)
#p.value.11 <- 2*(1 - pnorm(abs(z.value.11)))
#print(p.value.11)
#saving rates and inflation rate
(242153-172572)/242153
(292177-242153)/242153
```

```{r}
# the total sample size for RA
fixed_nr7 <- 2*N_ren7
# beta-spending function Pocock; O'Brien & Fleming alpha-spending function
# k = 4 
design7 <- getDesignGroupSequential( kMax = 4,
    sided = 2, alpha = 0.05, beta = 0.2,
    informationRates = c(0.25, 0.5, 0.75,1),
    typeOfDesign = "asOF",
    typeBetaSpending = "bsP",bindingFutility = FALSE,twoSidedPower = FALSE
)
summary(design7)
plot(design7, type =1 )
# (pi2 control) vs (pi1 treatment) in intervention
sampleSizeResultGS7 <- getSampleSizeRates(design7, pi1 = reten_7_30, 
                                         pi2 = reten_7_40,
                                    allocationRatioPlanned = 1,
                                    groups = 2,normalApproximation = TRUE)
# Standard rpact output 
summary(sampleSizeResultGS7)
designCharr1 <- getDesignCharacteristics(design1)
summary(designCharr1)
sampleSizeResultGS7[["numberOfSubjects"]]
```

```{r}
# stage 1 for RA7
sampleSizeResultGS7[["numberOfSubjects1"]]
data_stage1d7 <- data1[1:14186, ]
meand7stage1 <-apply(data_stage1d7[,c("retention_7_t","retention_7_c")],2,mean)
rate7_1.t <- meand7stage1[1]
rate7_1.c <- meand7stage1[2]
staystage1_t7 <- sum(data_stage1d7$retention_7_t)
staystage1_c7 <- sum(data_stage1d7$retention_7_c)
sizestage1.7 <- nrow(data_stage1d7)
d7ra.stage1 <- (staystage1_c7+staystage1_t7)/(2*sizestage1.7)
se.71 <- sqrt(d7ra.stage1*(1 - d7ra.stage1)*(1/sizestage1.7+1/sizestage1.7))
z.value.71 <- (rate7_1.t - rate7_1.c)/se.71
unname(z.value.71)
#stage 2 for RA7
data_stage1d7 <- data1[1:28372, ]
meand7stage1 <-apply(data_stage1d7[,c("retention_7_t","retention_7_c")],2,mean)
rate7_1.t <- meand7stage1[1]
rate7_1.c <- meand7stage1[2]
staystage1_t7 <- sum(data_stage1d7$retention_7_t)
staystage1_c7 <- sum(data_stage1d7$retention_7_c)
sizestage1.7 <- nrow(data_stage1d7)
d7ra.stage1 <- (staystage1_c7+staystage1_t7)/(2*sizestage1.7)
se.71 <- sqrt(d7ra.stage1*(1 - d7ra.stage1)*(1/sizestage1.7+1/sizestage1.7))
z.value.71 <- (rate7_1.t - rate7_1.c)/se.71
unname(z.value.71)
#stage 3 for RA7
data_stage1d7 <- data1[1:42558, ]
meand7stage1 <-apply(data_stage1d7[,c("retention_7_t","retention_7_c")],2,mean)
rate7_1.t <- meand7stage1[1]
rate7_1.c <- meand7stage1[2]
staystage1_t7 <- sum(data_stage1d7$retention_7_t)
staystage1_c7 <- sum(data_stage1d7$retention_7_c)
sizestage1.7 <- nrow(data_stage1d7)
d7ra.stage1 <- (staystage1_c7+staystage1_t7)/(2*sizestage1.7)
se.71 <- sqrt(d7ra.stage1*(1 - d7ra.stage1)*(1/sizestage1.7+1/sizestage1.7))
z.value.71 <- (rate7_1.t - rate7_1.c)/se.71
unname(z.value.71)
(87212-85116)/87212
# finding a eariler stopping
design7a <- getDesignGroupSequential( kMax = 5,
    sided = 2, alpha = 0.05, beta = 0.2,
    informationRates = c(0.2, 0.4,0.6,0.8,1),
    typeOfDesign = "asOF",
    typeBetaSpending = "bsP",bindingFutility = FALSE,twoSidedPower = FALSE
)
# (pi2 control) vs (pi1 treatment) in intervention
sampleSizeResultGS7 <- getSampleSizeRates(design7a, pi1 = reten_7_30, 
                                 pi2 = reten_7_40,
                                allocationRatioPlanned = 1,
                                groups = 2,normalApproximation = TRUE)
summary(sampleSizeResultGS7)# 31569.6 is about 31570
data_stage1d7 <- data1[1:31570, ]
meand7stage1 <-apply(data_stage1d7[,c("retention_7_t","retention_7_c")],2,mean)
rate7_1.t <- meand7stage1[1]
rate7_1.c <- meand7stage1[2]
staystage1_t7 <- sum(data_stage1d7$retention_7_t)
staystage1_c7 <- sum(data_stage1d7$retention_7_c)
sizestage1.7 <- nrow(data_stage1d7)
d7ra.stage1 <- (staystage1_c7+staystage1_t7)/(2*sizestage1.7)
se.71 <- sqrt(d7ra.stage1*(1 - d7ra.stage1)*(1/sizestage1.7+1/sizestage1.7))
z.value.71 <- (rate7_1.t - rate7_1.c)/se.71
unname(z.value.71)
63140/2
(87212-63140)/87212
sampleSizeResultGS7[["expectedNumberOfSubjectsH1"]]# compare ASN under H1
sampleSizeResultGS7[["maxNumberOfSubjects"]]
(105232-87212)/87212
```





