#Loaded packages 
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)

# Loaded data and View
dataA =  read.csv("/Users/tanjiasarker/Downloads/selectedvariables_pulsesurveydata.csv")
View(dataA)

#Structure of data
str(dataA)
variable.names(dataA)

dataA$Age <- 2021 - dataA$TBIRTH_YEAR
variable.names(dataA)
dataA$ANXIOUS[dataA$ANXIOUS== 2] <- 0
dataA$ANXIOUS[dataA$ANXIOUS== 3] <- 0
dataA$ANXIOUS[dataA$ANXIOUS== 4] <- 0

dataA$RHISPANIC <- as.factor(dataA$RHISPANIC)
dataA$RRACE <- as.factor(dataA$RRACE)
dataA$EEDUC <- as.factor(dataA$EEDUC)
dataA$MS <- as.factor(dataA$MS)
dataA$EGENID_BIRTH <- as.factor(dataA$EGENID_BIRTH)
dataA$WRKLOSSRV <- as.factor(dataA$WRKLOSSRV)
dataA$EXPNS_DIF <- as.factor(dataA$EXPNS_DIF)
dataA$DOWN <- as.factor(dataA$DOWN)
dataA$PRESCRIPT <- as.factor(dataA$PRESCRIPT)
dataA$INCOME <- as.factor(dataA$INCOME)
dataA$ANXIOUS <- as.factor(dataA$ANXIOUS)

str(dataA)
summary(dataA)

#logistic regression glm of ANXIOUS
View(dataA)

ModelH <- glm(ANXIOUS ~ RHISPANIC, data=dataA, family="binomial")
summary(ModelH)

ModelR <- glm(ANXIOUS ~ RRACE, data=dataA, family="binomial")
summary(ModelR)

ModelE <- glm(ANXIOUS ~ EEDUC, data=dataA, family="binomial")
summary(ModelE)

ModelM <- glm(ANXIOUS ~ MS, data=dataA, family="binomial")
summary(ModelM)

Modelg <- glm(ANXIOUS ~ EGENID_BIRTH, data=dataA, family="binomial")
summary(Modelg)

ModelW <- glm(ANXIOUS ~ WRKLOSSRV, data=dataA, family="binomial")
summary(ModelW)

ModelE <- glm(ANXIOUS ~ EXPNS_DIF, data=dataA, family="binomial")
summary(ModelE)

ModelD <- glm(ANXIOUS ~ DOWN, data=dataA, family="binomial")
summary(ModelD)

ModelP <- glm(ANXIOUS ~ PRESCRIPT, data=dataA, family="binomial")
summary(ModelP)

ModelI <- glm(ANXIOUS ~ INCOME, data=dataA, family="binomial")
summary(ModelI)

ModelAge <- glm(ANXIOUS ~  Age, data=dataA, family="binomial")
summary(ModelAge)





Model1 <- glm(ANXIOUS ~ INCOME+RRACE+RHISPANIC+EEDUC+MS+EGENID_BIRTH+Age, data=dataA, family="binomial")
summary(Model1)

Model2 <- glm(ANXIOUS ~ EXPNS_DIF+INCOME+RRACE+RHISPANIC+EEDUC+MS+EGENID_BIRTH+Age, data=dataA, family="binomial")
summary(Model2)

Model3 <- glm(ANXIOUS ~ WRKLOSSRV+INCOME+RRACE+RHISPANIC+EEDUC+MS+EGENID_BIRTH+Age, data=dataA, family="binomial")
summary(Model3)

Model4 <- glm(ANXIOUS ~ WRKLOSSRV*Age, data=dataA, family="binomial")
summary(Model4)

Model5 <- glm(ANXIOUS ~ EXPNS_DIF*Age, data=dataA, family="binomial")
summary(Model5)

#another visual- geom bar for ANXIOUS and INCOME
glimpse(dataA)

ggplot(data=dataA, aes(x=INCOME, y=ANXIOUS)) +
  geom_bar(stat='identity', fill="forest green")+
  ylab("Anxious ")+ 
  facet_wrap(~ANXIOUS)

#histogram
library(ggplot2)

hist(dataA$TBIRTH_YEAR)

ggplot(data = dataA, mapping = aes(x = age)) +
  geom_histogram(bins=40,color="white")

ggplot(data = dataA, mapping = aes(x = age,)) +
  geom_histogram(binwidth = 20, color = "white") +
  facet_wrap(~ MS)

install.packages('jmv')
library(jmv)

descriptives(dataA, vars = vars(TBIRTH_YEAR, RHISPANIC 
                                ,RRACE, EEDUC, MS, EGENID_BIRTH, WRKLOSSRV,
                                EXPNS_DIF, ANXIOUS, DOWN, PRESCRIPT, INCOME), freq = TRUE)

install.packages("funModeling")
install.packages("Hmisc")
library(funModeling)
library(Hmisc)
freq(dataA)
freq(dataA$ANXIOUS)
freq(dataA$INCOME)
freq(dataA$WRKLOSSRV)
freq(dataA$EXPNS_DIF)
cross_plot(dataA,input="WRKLOSSRV", target = "ANXIOUS")
plot_num(dataA)
plot_num(dataA, path_out = ".")

describe(dataA)

categ_analysis(dataA, target="ANXIOUS")
