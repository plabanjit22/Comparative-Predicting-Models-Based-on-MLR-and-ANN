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
#load data

setwd("C:\\Users\\user\\Desktop\\GD project")
data<- read.csv("Life_Expectancy_Data.csv")
str(data)

summary(data)

data <- data[ -c(1,2) ]
data <- data[,c(2,1,3:19,20)]
data$Status<-as.factor(data$Status)
data$Status <- ifelse(data$Status == "Developing",0,1)


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

sapply(data, function(x) sum(is.na(x))/length(x)*100)

str(data)
summary(data)


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


 
data <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
x11,x12,x13,x14,x15,x16,x17,x18,x19)

#Scaling dataset

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
data <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

summary(data)
str(data)

 
data <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x10,
x11,x13,x15,x17,x18)



#########################################

#Heatmap & correlation Matrix

df2 <- data[ -c(1,2) ]
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

#########################################


head(data)
nrow(data)
str(data)
summary(data)
sapply(data, function(x) sum(is.na(x))/length(x)*100)


# Training and Test Data
index <- sample(1:nrow(data),round(0.8*nrow(data)))
ann_train <- data [index,]
ann_test <- data [-index,]


str(ann_train)
nrow(ann_test)
head(ann_test)
install.packages("neuralnet",dependencies=T)
library(neuralnet)

n <- names(ann_train)
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))

## model1
set.seed(123)

nn1 <- neuralnet(f,data=ann_train,hidden=8,linear.output=T)

plot(nn1)
nn1$result.matrix
summary(nn1)
nrow(ann_train)
b <- ann_train[,2:15]
head(b)
pr.nn1_test <- compute(nn1,ann_test[,2:15])
pr.nn1_test <- pr.nn1_test$net.result*(max(data$y)-min(data$y))+min(data$y)
test.r <- (ann_test$y)*(max(data$y)-min(data$y))+min(data$y)
MSE.nn1_test <- sum((test.r - pr.nn1_test)^2)/nrow(ann_test)
test_Mape<-(sum((abs(test.r-pr.nn1_test))/test.r))/nrow(ann_test)*100


pr.nn1_train <- compute(nn1,b)
pr.nn1_train <- pr.nn1_train$net.result*(max(data$y)-min(data$y))+min(data$y)
train.r <- (ann_train$y)*(max(data$y)-min(data$y))+min(data$y)
MSE.nn1_train <- sum((train.r - pr.nn1_train)^2)/nrow(ann_train)
train_Mape<-(sum((abs(train.r-pr.nn1_train))/train.r))/nrow(ann_train)*100





#### model2

nn1 <- neuralnet(f,data=ann_train,hidden=c(8,4),linear.output=T)
plot(nn1)
nn1$result.matrix


pr.nn1_test <- compute(nn1,ann_test[,2:15])
pr.nn1_test <- pr.nn1$net.result*(max(data$y)-min(data$y))+min(data$y)
test.r_test <- (ann_test$y)*(max(data$y)-min(data$y))+min(data$y)
MSE.nn1_test <- sum((test.r - pr.nn1_ann)^2)/nrow(ann_test)

pr.nn1_train <- compute(nn1,ann_train[,2:15])
pr.nn1_train <- pr.nn1$net.result*(max(data$y)-min(data$y))+min(data$y)
test.r_train <- (ann_test$y)*(max(data$y)-min(data$y))+min(data$y)
MSE.nn1_train <- sum((test.r - pr.nn1_ann)^2)/nrow(ann_test)


###### model3

nn1 <- neuralnet(f,data=ann_train,hidden=c(9,5),linear.output=T)
plot(nn1)
nn1$result.matrix

summary(pr.nn1_ann)

pr.nn1 <- compute(nn1,ann_test[,2:16])
pr.nn1_ann <- pr.nn1$net.result*(max(data$y)-min(data$y))+min(data$y)
test.r <- (ann_test$y)*(max(data$y)-min(data$y))+min(data$y)
MSE.nn1 <- sum((test.r - pr.nn1_ann)^2)/nrow(ann_test)

##

results_train <- data.frame(actual = train.r, prediction = pr.nn1_train)
results_train

## Accuracy

predicted_train=results_train$prediction 
actual_train=results_train$actual 
#comparison_train=data.frame(predicted_train,actual_train)
deviation_train=((actual_train-predicted_train)/actual_train)
comparison_train=data.frame(predicted_train,actual_train,deviation_train)
accuracy_train=1-abs(mean(deviation_train))
accuracy_train


results_test <- data.frame(actual = test.r, prediction = pr.nn1_test)
results_test


predicted_test=results_test$prediction 
actual_test=results_test$actual 
#comparison_test=data.frame(predicted_test,actual_test)
deviation_test=((actual_test-predicted_test)/actual_test)
comparison_test=data.frame(predicted_test,actual_test,deviation_test)
accuracy_test=1-abs(mean(deviation_test))
accuracy_test
