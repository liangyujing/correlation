
rm(list=ls())

# Set your working dir as the current dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
my_data <- read.csv(file ="R code/regression-assignments 1+ 2/ExperienceSampling_Group7.csv", head=T,sep=";") 

## Use the function cor.test(x,y) to analyze the correlation coefficient between two variables and to get significance level of the correlation.
## Three possible correlation methods using the function cor.test(x,y): pearson, kendall, spearman


## 1.相关系数
##cor() computes the correlation coefficient
##cor.test() test for association/correlation between paired samples. 
##It returns both the correlation coefficient and the significance level(or p-value) of the correlation .

library(gplots)
cor(x=beepnum, y=PA, method = c("pearson", "kendall", "spearman"))
cor.test(x=beepnum, y=PA, method=c("pearson", "kendall", "spearman"))

##If your data contain missing values, use the following R code to handle missing values by case-wise deletion.
cor(x, y,  method = "pearson", use = "complete.obs")


## 画相关图Visualize your data using scatter plots
library("ggpubr")
ggscatter(my_data, x = "beepnum", y = "PA", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


## Preleminary test to check the test assumptions
##1.线性：Is the covariation linear? Yes, form the plot above, the relationship is linear. In the situation where the scatter plots show curved patterns, we are dealing with nonlinear association between the two variables.
##2.正态：Are the data from each of the 2 variables (x, y) follow a normal distribution? 
  #Use Shapiro-Wilk normality test –> R function: shapiro.test()
  #and look at the normality plot —> R function: ggpubr::ggqqplot()
##Shapiro-Wilk test can be performed as follow: 
  #Null hypothesis: the data are normally distributed
  #Alternative hypothesis: the data are not normally distributed

# 是否正态：Shapiro-Wilk normality test for PA
shapiro.test(my_data$PA) # => p = 0.1229
# Shapiro-Wilk normality test for wt
shapiro.test(my_data$wt) # => p = 0.09
##From the output, the two p-values are greater than the significance level 0.05 
##implying that the distribution of the data are not significantly different from normal distribution. 
##In other words, we can assume the normality.

## 画图：正态：Q-Q plot draws the correlation between a given sample and the normal distribution.
library("ggpubr")
# PA
ggqqplot(my_data$PA, ylab = "PA")
# wt
ggqqplot(my_data$wt, ylab = "WT")
##From the normality plots, we conclude that both populations may come from normal distributions.
##Note that, if the data are not normally distributed, it’s recommended to use the non-parametric correlation, 
##including Spearman and Kendall rank-based correlation tests.


## 1. Pearson correlation test
#Correlation test between mpg and wt variables:
res <- cor.test(my_data$beepnum, my_data$PA, 
                  method = "pearson")
res

##sample estimates is the correlation coefficient (Cor.coeff = -0.87).
##The p-value of the test is 1.29410^{-10}, which is less than the significance level alpha = 0.05. 
##We can conclude that wt and mpg are significantly correlated with a correlation coefficient of -0.87 and p-value of 1.29410^{-10} .

#Extract the p.value
res$p.value
#Extract the correlation coefficient
res$estimate

## 2.Kendall rank correlation test
#used to estimate a rank-based measure of association. 
res2 <- cor.test(my_data$wt, my_data$mpg,  method="kendall")
res2
##The correlation coefficient between x and y are “sample estimates” and the p-value is xx.

## 3.Spearman rank correlation coefficient
#used to estimate a rank-based measure of association. 
#f非正态This test may be used if the data do not come from a bivariate normal distribution.
res2 <-cor.test(my_data$wt, my_data$mpg,  method = "spearman")
res2
