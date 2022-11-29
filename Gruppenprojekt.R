library(mosaic)
library(lsr)
library(tidyverse)

path <- "C:/Users/crazy/Desktop/FOM/Wissenschaftliche Methodik/Gruppenprojekt"
setwd(path)

# chd <- read_csv("CHD_preprocessed.csv")
chd2 <- read_csv2("ext.csv")

chd <- read_csv(
  "CHD_preprocessed.csv",
  col_types = list(
    male = col_logical(),
    age = col_integer(),
    education = col_factor(ordered = TRUE),
    currentSmoker = col_logical(),
    cigsPerDay = col_number(),
    BPMeds = col_number(),
    prevalentStroke = col_logical(),
    prevalentHyp = col_logical(),
    diabetes = col_logical(),
    totChol = col_number(),
    sysBP = col_number(),
    diaBP = col_number(),
    BMI = col_number(),
    heartRate = col_number(),
    glucose = col_number(),
    TenYearCHD = col_logical()
  )
)%>% mutate (BPMeds = if_else(BPMeds == 1, TRUE, FALSE))

# Hypothese 1

lr3 <- lm(SYSBP ~ BMI, data=chd2)

set.seed(69420)

BS1 <- do(10000)* lm(SYSBP~BMI, data=resample(chd2))

summary(BS1)

gf_histogram(~BMI, data=BS1)

sd(~BMI, data=BS1)

qdata(~BMI, data=BS1, p=c(0.001, 0.999))

set.seed(69420)

NV1 <- do(10000)* lm(SYSBP~shuffle(BMI), data=resample(chd2))

gf_histogram(~BMI, data=NV1)

qdata(~BMI, data=NV1, p=c(0.001, 0.999))

summary(NV1)

summary(lr3)

plotModel(lr3)

cor(SYSBP~BMI, data=chd2)

coef(lr3)

# Hypothese 2

chd2$CVD <- factor(chd2$CVD, levels = c("0", "1"))

lr6 <- glm(CVD~GLUCOSE, data=chd2, family=binomial("logit"))

summary(lr6)

plotModel(lr6)

set.seed(69420)

BS2 <- do(10000)* glm(CVD~GLUCOSE, data=resample(chd2), family=binomial("logit"))

summary(BS2)

gf_histogram(~GLUCOSE, data=BS2)

set.seed(69420)

NV2 <-do(10000)* glm(CVD~shuffle(GLUCOSE), data=resample(chd2), family=binomial("logit"))

summary(NV2)

gl_histogram(~GLUCOSE, data=NV2)

prop(~abs(GLUCOSE) >= abs(coef(BS2) [2]), data=NV2)

mosaicplot(CVD~DIABETES, data=chd2)

prop(~CVD, data=chd2)
