unlink("/home/me/src/Rlibs/00LOCK-Rcpp", recursive = TRUE)

install.packages("dvmisc",dependencies=TRUE)
library(dvmisc)
get_mse(model, var.estimate = FALSE)
mean((data1$y - predict(model))^2)

#Installing Required packages
install.packages("questionr",dependencies=TRUE)
install.packages("dplyr",dependencies=TRUE)
install.packages("car",dependencies=TRUE)
install.packages("boot",dependencies=TRUE)
install.packages("QuantPsyc",dependencies=TRUE)
install.packages("lmtest",dependencies=TRUE)
install.packages("sandwich",dependencies=TRUE)
install.packages("vars",dependencies=TRUE)
install.packages("MASS",dependencies=TRUE)
install.packages("nortest",dependencies=TRUE)
install.packages("ggplot2",dependencies=TRUE)
install.packages("imputeTS",dependencies=TRUE)
install.packages("perturb",dependencies=TRUE)
install.packages("leaps",dependencies=TRUE)
install.packages("olsrr",dependencies=TRUE)
install.packages("e1071",dependencies=TRUE)
install.packages("reshape2",dependencies=TRUE)


# Calling the liabraries

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(ggplot2)
library(imputeTS)
library(perturb)
library(leaps)
library(olsrr)
library(e1071)
library(reshape2)
library(questionr)

#load data

setwd("C:\\Users\\user\\Desktop\\GD project")
data<- read.csv("Life_Expectancy_Data.csv")
str(data)


# Dropping year and country column
data <- data[ -c(1,2) ]
data <- data[,c(2,1,3:19,20)]
data$Status<-as.factor(data$Status)

# structure and summary of dataset
str(data)
summary(data)

#Renaming the columns
colnames(data)
data<-rename.variable(data,"Life.expectancy","Life_expectancy")
data<-rename.variable(data,"Adult.Mortality","Adult_Mortality")
data<-rename.variable(data,"infant.deaths","infant_deaths")
data<-rename.variable(data,"percentage.expenditure","percentage_expenditure")
data<-rename.variable(data,"Hepatitis.B","Hepatitis_B")
data<-rename.variable(data,"under.five.deaths","under_five_deaths")
data<-rename.variable(data,"Total.expenditure","Total_expenditure")
data<-rename.variable(data,"HIV.AIDS","HIV_AIDS")
data<-rename.variable(data,"thinness.5.9.years","thinness_5_9_years")
data<-rename.variable(data,"thinness..1.19.years","thinness_10_19_years")
data<-rename.variable(data,"Income.composition.of.resources","Income_composition_of_resources")


# Checking for missing values and data imputation by interpolation
sapply(data, function(x) sum(is.na(x))/length(x)*100)

data$Adult_Mortality <- na_interpolation(data$Adult_Mortality )
data$Alcohol<-na_interpolation(data$Alcohol)
data$Hepatitis_B<-na_interpolation(data$Hepatitis_B)
data$BMI<-na_interpolation(data$BMI)
data$Polio<-na_interpolation(data$Polio)
data$Total_expenditure<-na_interpolation(data$Total_expenditure)
data$Diphtheria<-na_interpolation(data$Diphtheria)
data$GDP<-na_interpolation(data$GDP)
data$Population<-na_interpolation(data$Population)
data$thinness_10_19_years<-na_interpolation(data$thinness_10_19_years)
data$thinness_5_9_years<-na_interpolation(data$thinness_5_9_years)
data$Income_composition_of_resources<-na_interpolation(data$Income_composition_of_resources)
data$Schooling<-na_interpolation(data$Schooling)

# omitting Null rows
data <- na.omit(data)
str(data)
summary(data)

#Checking Skewness
skewness(data$Life_expectancy)
skewness(data$thinness_5_9_years)
skewness(data$thinness_10_19_years)
skewness(data$Population)
skewness(data$Income_composition_of_resources)
skewness(data$GDP)
skewness(data$HIV_AIDS)
skewness(data$Diphtheria)
skewness(data$Total_expenditure)
skewness(data$Polio)
skewness(data$under_five_deaths)
skewness(data$BMI)
skewness(data$Measles)
skewness(data$Hepatitis_B)
skewness(data$percentage_expenditure)
skewness(data$Alcohol)
skewness(data$infant_deaths)
skewness(data$Adult_Mortality)
skewness(data$Schooling)


#Response Variable
y <- data$Life_expectancy

#Regressor Variables
x1 <- data$Status
x2 <- data$Adult_Mortality
x3 <- data$infant_deaths
x4 <- data$Alcohol
x5 <- data$percentage_expenditure
x6 <- data$Hepatitis_B
x7 <- data$Measles
x8 <- data$BMI
x9 <- data$under_five_deaths
x10 <- data$Polio
x11 <- data$Total_expenditure
x12 <- data$Diphtheria
x13 <- data$HIV_AIDS
x14 <- data$GDP
x15 <- data$Population
x16 <- data$thinness_10_19_years
x17 <- data$thinness_5_9_years
x18 <- data$Income_composition_of_resources
x19 <- data$Schooling


# Data Normalization/Standardization
y <- data$Life_expectancy
x1 <- data$Status
x2 <- log(data$Adult_Mortality)
x3 <- (data$infant_deaths)^(1/3)
x4 <- data$Alcohol^(1/3)
x5 <- (data$percentage_expenditure)^(1/3)
x6 <- log(data$Hepatitis_B)
x7 <- (data$Measles)^(1/3)
x8 <- data$BMI
x9 <- (data$under_five_deaths)^(1/3)
x10 <- log(data$Polio)
x11 <- data$Total_expenditure
x12 <- log(data$Diphtheria)
x13 <- log(data$HIV_AIDS)
x14 <- log(data$GDP)
x15 <- log(data$Population)
x16 <- log(data$thinness_10_19_years)
x17 <- log(data$thinness_5_9_years)
x18 <- data$Income_composition_of_resources
x19 <- data$Schooling

# Creating new dataframe 
data1 <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
x11,x12,x13,x14,x15,x16,x17,x18,x19)


head(data1)
nrow(data1)
str(data1)
summary(data1)

#########################################
#Heatmap & correlation Matrix to visualize Multicolinearity

df2 <- data1[ -c(1,2) ]
cormat <- round(cor(df2),2)

melted_cormat <- melt(cormat)
head(melted_cormat)
# Get upper triangle of the correlation matrix
 get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Pearson\nCorrelation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 8, hjust = 1))+
 coord_fixed()


ggheatmap + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(0.6, 0.7),
  legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))

############################################################
#removing unnecessery variables
data1 <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x10,
x11,x12,x13,x14,x15,x17,x18)

#Outlier Detection (Cook's Distance)
sapply(data1, function(x) sum(is.na(x))/length(x)*100)
model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x10 +
x11 + x12 + x13 + x14 + x15 + x17 + x18,data=data1)

cooksd <- cooks.distance(model)

# Plot the Cook's Distance using the traditional 4/(n=sample zize) criterion
sample_size <- nrow(data1)
# plot cook's distance
plot(cooksd, pch="*", cex=1, main="Influential Obs by Cooks distance") 
# add cutoff line 
abline(h = (4/(sample_size)), col="red")  

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/(sample_size-19-1)))])

data <- data1[-influential, ]

# Training and Test Data
index <- sample(1:nrow(data),round(0.8*nrow(data)))
mlr_train <- data [index,]
mlr_test <- data [-index,]

head(mlr_train)
nrow(mlr_train)

# Fitting Multiple Linear Regression Model on training data
fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x10 +
            x11 + x12 + x13 + x14 + x15 + x17 + x18,data=mlr_train)

fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x10 +
            x11 + x12 + x13 + x14 + x15 + x17 + x18,data=mlr_train)

fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x7 + x10 +
            x11 + x12 + x13 + x14 + x15 + x17 + x18,data=mlr_train)

fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x7 + x10 +
            x11 + x12 + x13 + x14 + x17 + x18,data=mlr_train)

fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x7 + x10 +
            x11 + x12 + x13 + x17 + x18,data=mlr_train)

fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x7 + x10 +
            x11 + x12 + x13 + x17 + x18,data=mlr_train)


best_subset<-ols_step_best_subset(fit)
best_subset

# Summary of Model
#P value of independent variable (p value should be less than .05)
summary(fit)
#Again Checking Multicolinearity vif>2 means presence of multicollinearity
vif(fit)

fit <- lm(y ~x1 + x2 + x3 + x4 + x5 + x7 +
            x11 + x12 + x13 + x17 + x18,data=mlr_train)


## Get the predicted or fitted values on training data
train_pred<-fitted(fit)

#Validation on train set
attach(mlr_train)

train_Mape<-(sum((abs(mlr_train$y-train_pred))/mlr_train$y))/nrow(mlr_train)*100
MSE_train <- sum((train_pred - mlr_train$y)^2)/nrow(mlr_train)
deviation_train=((mlr_train$y-train_pred)/mlr_train$y)
accuracy_train=(1-abs(mean(deviation_train)))*100

## Get the predicted or fitted values on training data
test_pred <- predict(fit,mlr_test)

#Validation on test set
attach(mlr_test)

test_Mape<-(sum((abs(mlr_test$y-test_pred))/mlr_test$y))/nrow(mlr_test)*100
MSE_test <- sum((test_pred - mlr_test$y)^2)/nrow(mlr_test)
deviation_test=((mlr_test$y-test_pred)/mlr_test$y)
accuracy_test=(1-abs(mean(deviation_test)))*100


MSE_train
MSE_test
train_Mape
test_Mape
accuracy_train
accuracy_test



#Stepwise regression
model <- lm(y ~x1 + x2 + x4 + x5 + x7 + x10 +
            x11 + x12 + x13 + x17 + x18,data=mlr_train)

summary(model)
step_model = stepAIC(model,direction="both",trace=1)
step_model
vif(model)

best_subset <-ols_step_best_subset(step_model)
best_subset


model <- lm(y ~x1 + x2 + x4 + x5 + x7 + x10 +
            x11 + x12 + x13 + x17 + x18,data=mlr_train)

summary(model)
step_model1 = stepAIC(model,direction="forward",trace=1)
step_model1
vif(model1)

best_subset <-ols_step_best_subset(model1)
best_subset

# Test for Normality
qqnorm(data$y)
qqline(data$y,col="red")

shapiro.test(data$y)
ad.test(data$y)

 


