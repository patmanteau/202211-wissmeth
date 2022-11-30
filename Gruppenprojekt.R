library(mosaic)
library(lsr)
library(tidyverse)

#Path einfügen
path <- "C:/Users/crazy/Desktop/FOM/Wissenschaftliche Methodik/Gruppenprojekt" 
setwd(path) # Set workdirectory to path


Data <- read_csv2("ext.csv") #Daten einlesen

#Hypothese 1

LinReg <- lm(SYSBP ~ BMI, data=Data) # Lineare Regression

summary(LinReg) 

plotModel(LinReg)

set.seed(69420) # Reproduzierbarkeit

Boostr_1 <- do(10000)* lm(SYSBP ~ BMI, data=resample(Data)) # Bootstrapping

summary(Bootstr_1)

gf_histogram(~ BMI, data=Bootstr_1)

# Standardabweichung
sd(~ BMI, data=Bootstr_1) 

# Konfidenzintervall alpha = 1 Prozent
qdata(~ BMI, data=Bootstr_1, p=c(0.005, 0.995))  

set.seed(69420) # Reproduzierbarkeit

# Nullverteilung
Nullver_1 <- do(10000) * lm(SYSBP ~ shuffle(BMI), data=resample(Data)) 

gf_histogram(~ BMI, data=Nullver_1)

# Konfidenzintervall alpha = 1 Prozent
qdata(~ BMI, data=Nullver_1, p=c(0.005, 0.995)) 

summary(Nullver_1)

cor( SYSBP ~ BMI, data=Data) # Korrelation aus Datensatz

coef(LinReg) # Koeffizient aus Linearer Regression aus Datensatz

# Hypothese 2

Data$CVD <- factor(Data$CVD, levels = c("0", "1")) # Setting 0=FALSE 1=TRUE

# Logarithmische Rergession
LogReg <- glm(CVD ~ GLUCOSE, data=Data, family=binomial("logit")) 

summary(LogReg)

plotModel(LogReg)

set.seed(69420)

Bootstr_2 <- do(10000) * glm(CVD ~ GLUCOSE, data=resample(Data), family=binomial("logit"))

summary(Bootstr_2)

gf_histogram(~ GLUCOSE, data=Bootstr_2)

set.seed(69420)

Nullver_2 <-do(10000) * glm(CVD ~ shuffle(GLUCOSE), data=resample(Data), family=binomial("logit"))

summary(Nullver_2)

gl_histogram(~ GLUCOSE, data=Nullver_2)


# Anteil von Koeffizeinten aus der Nullverteilung der sich mit der Range der
# Koeffizienten aus dem Bootsrapping überschneidet oder größer ist
prop(~ abs(GLUCOSE) >= abs(coef(Bootstr_2) [2]), data=Nullver_2) 

mosaicplot(CVD ~ DIABETES, data=Data)

# Wahrscheinlichkeit eine CVD zu erleiden im Verlauf der Studie
prop(~ CVD, data=Data) 
