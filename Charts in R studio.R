setwd("C:\\Users\\user\\Desktop\\GD project")
data<- read.csv("Life_Expectancy_Data.csv")

data <- data[ c(3,4) ]
data$Status <- as.factor(data$Status)

str(data)

data1 <- data[data$Status == "Developed", ]
data2 <- data[data$Status == "Developing",]
nrow(data1)
nrow(data2)

a<-nrow(data1)/(nrow(data1)+nrow(data2))*100
b<-nrow(data2)/(nrow(data1)+nrow(data2))*100

Developed_LE <- data1[ 2 ]
Developing_LE <- data2[ 2 ]

head(Developed_LE)
head(Developing_LE)
# Create data for the graph.
x <- c(a, b)
labels <- c("Developed", "Developing")


# Plot the chart with title and rainbow color pallet.
par(mfrow=c(1,2))
pie(x, labels, main = "Status wise data Share",
 col = rainbow(length(x)))
boxplot(data$Life_expectancy ~ data$Status, data = data, main = "Comparison of Boxplots")




install.packages("gridExtra",dependencies=T)
library(gridExtra)
library(ggplot2)
g1<- ggplot(df, aes(x = df$ Adult_Mortality, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) +  theme(legend.position="none") 


g2 <- ggplot(df, aes(x = df$ infant_deaths, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g3 <-ggplot(df, aes(x = df$Alcohol, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g4 <- ggplot(df, aes(x = df$percentage_expenditure, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g5 <- ggplot(df, aes(x = df$Hepatitis_B, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g6 <- ggplot(df, aes(x = df$Measles, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g7 <- ggplot(df, aes(x = df$ BMI, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g8<-ggplot(df, aes(x = df$ under.five_deaths, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g9<-ggplot(df, aes(x = df$ Polio, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g10<-ggplot(df, aes(x = df$ Total_expenditure, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g11<-ggplot(df, aes(x = df$ Diphtheria, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g12<-ggplot(df, aes(x = df$ HIV_AIDS, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g13<-ggplot(df, aes(x = df$ GDP, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g14<-ggplot(df, aes(x = df$ Population, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g15<-ggplot(df, aes(x = df$ thinness_.1.19_years, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g16<-ggplot(df, aes(x = df$ thinness_5.9_years, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g17<-ggplot(df, aes(x = df$ Income_composition_of_resources, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")


g18<-ggplot(df, aes(x = df$ Schooling, y = df$Life_expectancy)) +
    geom_point(aes(color = factor(df$ Status ))) + theme(legend.position="none")

grid.arrange(g1, g2,g3,g4,g5,g6,g7,g8,g9, nrow = 3)
grid.arrange(g10,g11,g12,g13,g14,g15,g16,g17,g18, nrow = 3)



