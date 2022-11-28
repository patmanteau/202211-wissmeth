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

str(chd)

gf_histogram(~age, data=chd)

gf_bar(~age|TenYearCHD, data=chd)

lr1 <- lm(TenYearCHD~age, data=chd)

summary(lr1)

plotModel(lr1)

gf_boxplot(age~TenYearCHD, data=chd)

gf_point(TenYearCHD~age, data=chd)

gf_point(sysBP ~ heartRate, data=chd)

lr2 <- lm(heartRate ~ diaBP, data=chd)

summary(lr2)

plotModel(lr2)

lr3 <- lm(sysBP ~ BMI, data=chd2) # Hyp 1

summary(lr3)

plotModel(lr3)

gf_boxplot(heartRate~currentSmoker, data=chd)

lr4 <- lm(diabetes ~ BMI, data=chd)

summary(lr4)

plotModel(lr4)

gf_boxplot(diabetes~BMI, data=chd)

lr5 <- lm(currentSmoker~male, data=chd)

summary(lr5)

plotModel(lr5)

gf_bar(~BMI, data=chd)

fav_stats(~BMI, data=chd)

max(~BMI, data=chd)

chd[chd$BMI == 56.8,]

lr6 <- lm(DEATH~DIABETES, data=chd2) # Hyp 2

summary(lr6)

plotModel(lr6)

mosaicplot(DEATH~DIABETES, data=chd2)

prop(~DEATH, data=chd2)

gf_boxplot(AGE~PREVCHD, data=chd2)

count(~PREVCHD, data=chd2)
