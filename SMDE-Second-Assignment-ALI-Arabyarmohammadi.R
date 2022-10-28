
# SMDE SECOND ASSIGNMENT (60% OF THE FINAL MARK, INDIVIDUAL) 
# FIB-UPC June, 2021
# Ali Arabyarmohammadi
#_____________________________________________________________


# import the necessary libraries
library("car")
library("ggplot2")
library("Hmisc")
library("lattice")
library ("psych")
library("FactoMineR")
 


########################
# Generating dataset
######################## 

# number of individuals (rows in the dataset)
n=2000

# Normal distribution
factor1 <- rnorm(n, mean=0, sd=1)

# Exponential Distribution
factor2 <- rexp(n, rate=.5)
 
# Uniform distribution
factor3 <- runif(n, min=5, max=10)
 
# Beta distribution
factor4 <- rbeta (n , 1 , 2) 
 
# Normal distribution
factor5 <- rnorm(n, mean=1, sd=2)

 
# define function F6 to F10 that use the variables F1 to F5
factor6 <-  factor3 +(3*factor1)
factor7 <-  factor1 + factor2 
factor8 <-  (4*factor5) + factor3 + (2*factor4)   
factor9 <-  factor5 +(2*factor1)
factor10 <- (3*factor3) + factor1 + (3*factor2) 



err = rnorm(n, mean=0, sd=1)

# chosen answer variable 
answer <-  (2*factor8) + (3*factor3) + factor9  + factor5 + err



# creating a data frame (Generating the dataset)
dset <- data.frame(factor1, factor2, factor3, factor4, factor5, factor6,
                   factor7, factor8, factor9, factor10, answer)

View (dset)
dim (dset)
str (dset)
describe (dset)



#visualization dset
boxplot (dset, col="blue", border="brown")
 

pairs.panels (dset, 
              method= "pearson",#correlation method
              hist.col= "#00AFBB",
              density= TRUE, #show density plot 
              ellipses=TRUE, #show correlation ellipses
              pch=19)




#############################################
# Obtaining an expression to generate new data
############################################# 


#PCA analysis (Answer's column is removed)
pca1 = PCA(dset[,-11], scale.unit=TRUE, ncp =5,  graph=T)
pca1$eig
plot (pca1$eig[,1], type= "o" , main= "ScreePlot") 





# Generating the linear model of the answer distribution  
regmodel1 <- lm( answer ~ ., data=dset)
summary (regmodel1)
 


# Generating the linear model of factor6 distribution
regmodel2 <- lm(formula = factor6 ~ factor1 + factor2 + factor3 + factor4 + factor5,
                data = dset)
summary (regmodel2)
 


# Generating the linear model of factor7 distribution
regmodel3 <- lm( formula = factor7 ~ factor1 + factor2 + factor3 + factor4 + factor5,
                 data = dset)
summary (regmodel3)
 


# Generating the linear model of factor8 distribution
regmodel4 <- lm( formula = factor8 ~ factor1 + factor2 + factor3 + factor4 + factor5,
                 data = dset)
summary (regmodel4)
 


# Generating the linear model of factor9 distribution
regmodel5 <- lm( formula = factor9 ~ factor1 + factor2 + factor3 + factor4 + factor5,
                 data = dset)
summary (regmodel5)
 


# Generating the linear model of factor10 distribution
regmodel6<- lm( formula = factor10 ~ factor1 + factor2 + factor3 + factor4 + factor5,
                data = dset)
summary (regmodel6)
 


# Generating the linear model without discarded factors
regmodel7 <- lm(formula = answer ~ factor1 + factor3 + factor4 + factor5   , data = dset)
summary (regmodel7)



#############################################
# Testing the assumptions for the new model
#############################################


# Normality test
qqnorm(residuals(regmodel7))
hist(residuals(regmodel7))

# Shapiro-Wilk normality test
shapiro.test(residuals(regmodel7))
# The calculated p-value is higher than the significance level 0.05, so there is no indication that normality
# distribution is violated


# Homogeneity of variance
# Breusch Pagan test
lmtest::bptest(regmodel7)
# The calculated p-value is higher than the significance level 0.05, so there is no indication of heteroscedasticity
# on the calculated model. 


# Durbin Watson test
lmtest::dwtest(regmodel7)
# The calculated p-value is higher than the significance level 0, so the alternative hypothesis can be accepted.
# It means that the variables are not independent and it exits a correlation between them.



###################
#  prediction
###################



# Normal distribution
factor1 <- rnorm(50, mean=0, sd=1)

# Exponential Distribution
factor2 <- rexp(50, rate=.5)

# Uniform distribution
factor3 <- runif(50, min=5, max=10)

# Beta distribution
factor4 <- rbeta (50 , 1 , 2) 

# Normal distribution
factor5 <- rnorm(50, mean=1, sd=2)



# define function F6 to F10 that use the variables F1 to F5
factor6 <-  factor3 +(3*factor1)
factor7 <-  factor1 + factor2 
factor8 <-  (4*factor5) + factor3 + (2*factor4)   
factor9 <-  factor5 +(2*factor1)
factor10 <- (3*factor3) + factor1 + (3*factor2) 


# new answer variable 
answer <-  0.13070 + (10.00118 * factor5) + (4.99923 * factor3) + (4.00015 * factor4) + (2.01580 * factor1)

dset2 <- data.frame(factor1, factor2, factor3, factor4, factor5, factor6,
                    factor7, factor8, factor9, factor10, answer)




#Predict with prediction interval
pred <- predict(regmodel7, newdata=dset2, interval="prediction")
pred

compData <- c()
predictedDataFrame <- data.frame(x1=pred)
answerDataFrame <- data.frame(x1=answer)
for(x in 1:nrow(predictedDataFrame)) {
  if( answerDataFrame$x1[x] >= predictedDataFrame$x1.lwr[x] &&
      answerDataFrame$x1[x] <= predictedDataFrame$x1.upr[x]){
    compData[x] <- TRUE
  }else{
    compData[x] <- FALSE
  }
}
predictedDataFrame["prediction"] <- compData
predictedDataFrame


 
################################  
# Design of experiments (DOE)
################################   




#################################
# 10 replications using a loop
#################################

#------------------------------------------------------------------
for (i in 1:10) {
  
# Define factor 2 to 5 
factor1 <- rnorm(2000, mean=0, sd=1)
# Exponential Distribution
factor2 <- rexp(2000, rate=.5)
# Uniform distribution
factor3 <- runif(2000, min=5, max=10)
# Beta distribution
factor4 <- rbeta (2000 , 1 , 2) 
# Normal distribution
factor5 <- rnorm(2000, mean=1, sd=2)
# Define the answer based on LRM
answer <-  0.13070 + (10.00118 * factor5) + (4.99923 * factor3) + (4.00015 * factor4) + (2.01580 * factor1)
# creating a data frame
data1Rep <- data.frame(factor1, factor3, factor4, factor5, answer)
# print the summary (To extract min and max of each factor)
print(summary(data1Rep))

min_Factor1 = min(abs(factor1))   
max_Factor1 = max(abs(factor1))
min_Factor3 = min(factor3)
max_Factor3 = max(factor3)
min_Factor4 = min(factor4)
max_Factor4 = max(factor4)
min_Factor5 = min(abs(factor5))  
max_Factor5 = max(abs(factor5))  

cat("min_Factor1 =\t", min_Factor1,"\n")
cat("max_Factor1 =\t", max_Factor1,"\n")
cat("min_Factor3 =\t", min_Factor3,"\n")
cat("max_Factor3 =\t", max_Factor3,"\n")
cat("min_Factor4 =\t", min_Factor4,"\n")
cat("max_Factor4 =\t", max_Factor4,"\n")
cat("min_Factor5 =\t", min_Factor5,"\n")
cat("max_Factor5 =\t", max_Factor5,"\n")

cat("---------------------------------- ", "\n")
cat("---------------------------------- ", "\n")
cat("---------------------------------- ", "\n")
}
#------------------------------------------------------------------




# print the combinations (+ and - signs)

signs <- c("-", "+")

ExperimentDesignSigns <- expand.grid(Factor1 = gl(2, 1, labels =signs),
                                Factor3 = gl(2, 1, labels =signs),
                                Factor4 = gl(2, 1, labels = signs),
                                Factor5 = gl(2, 1, labels = signs))

ExperimentDesignSigns




#################################
# Yates 
#################################

library("readxl")
library("dae")

# Select an Excel file manually
yatesTable <- read_excel(file.choose(),1)


#Yates algorithm
anovaModel <- aov(Values~ Factor1*Factor3*Factor4*Factor5, data=yatesTable)
anovaModel
yatesModel <- yates.effects(anovaModel, data = yatesTable)
yatesModel
 
meanData  <- (sum(yatesTable$Values)/16)




######################################
# Check experimental assumptions.
######################################


#------------------------------------------------------------------
for (i in 1:10) {
  
  # Define factor 2 to 5 
  factor1 <- rnorm(2000, mean=0, sd=1)
  # Exponential Distribution
  factor2 <- rexp(2000, rate=.5)
  # Uniform distribution
  factor3 <- runif(2000, min=5, max=10)
  # Beta distribution
  factor4 <- rbeta (2000 , 1 , 2) 
  # Normal distribution
  factor5 <- rnorm(2000, mean=1, sd=2)
  # Define the answer based on LRM
  answer <-  0.13070 + (10.00118 * factor5) + (4.99923 * factor3) + (4.00015 * factor4) + (2.01580 * factor1)
  # creating a data frame
  data1Rep <- data.frame(factor1, factor3, factor4, factor5, answer)
  # print the summary (To extract min and max of each factor)
  
  
  #ANOVA test for each replication
  anovaModel <- aov(answer~ factor1*factor3*factor4*factor5 , data=data1Rep)
  
  #Shapiro-Wilk normality test
  print(shapiro.test(residuals(anovaModel)))
 
  
  #Breusch Pagan test
 print(lmtest::bptest(anovaModel))
 
  
  #Durbin Watson test
  print(lmtest::dwtest(anovaModel))
 
}
#------------------------------------------------------------------

