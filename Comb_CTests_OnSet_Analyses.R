#install packages
install.packages("dplyr")
install.packages("psych")
install.packages("sjPlot")
library(sjPlot)
library(dplyr)
library(psych)

#read in csv file
#use read.csv2 for data that uses commas as a decimal point
tests_Data_comb <- read.csv2("comb_Set3C-Test_OnSet.csv",TRUE,",")
#check if dataframe
class(tests_Data_comb)
#check if headers are correct
head(tests_Data_comb)

#manually look for means of columns OnSet and total C-Test Scores
mean(tests_Data_comb$onSET_TOTAL)
mean(tests_Data_comb$C.Test_Total)
#visual aid to check for distribution
boxplot(tests_Data_comb)

#returns summary of dataframe including mean, min, max, median and Quarters
summary(tests_Data_comb)
#shows sd, psych package needed


#create subsets
#only superitem subset
Only_SuperItems_comb <-(subset(tests_Data_comb, select = c(Item1, Item2, Item3, Item4, Item5)))
#only data of test and superitem scores
DataToExport_comb <- (subset(tests_Data_comb, select = c(Item1, Item2, Item3, Item4, Item5, C.Test_Total, onSET_TOTAL)))
#make summary of only data Test info
summaryDataToExport_comb <-summary(DataToExport_comb)
print(summaryDataToExport_comb)
write.table(summaryDataToExport_comb, file="DataSummary_comb.csv", row.names= F, sep=",")

#perform kolmogorov-simrnov-test for normal distribution for C-TESTS data points, according the Björn Walther https://bjoernwalther.com/normalverteilung-in-r-pruefen/ this is a somewhat dated test, as esp. with smaller dataset it does not have enough power
ks.test(tests_Data_comb$C.Test_Total, "pnorm", mean=mean(tests_Data_comb$C.Test_Total), sd=sd(tests_Data_comb$C.Test_Total))
#perform kolmogorov-simrnov-test for normal distribution for OnSet data points
ks.test(tests_Data_comb$onSET_TOTAL, "pnorm", mean=mean(tests_Data_comb$onSET_TOTAL), sd=sd(tests_Data_comb$onSET_TOTAL))

#the above indicates a normal distribution for the OnSet data points and for the C-Test data,
#there is high p-value, so we can accept the null hypothesis that there is no difference between this data and a normal distribution; with a significant p value we would reject the null hypothesis

#perform the Shapiro-Wilk normality test, significant p-value should be over Alpha 0.05, null-hypothesis: data is normally distributed, can reject the hypothesis with a sig p-value
#for C-TESTS data points
shapiro.test(tests_Data_comb$C.Test_Total)
#for OnSet data points
shapiro.test(tests_Data_comb$onSET_TOTAL)


#Q-Q-Plot_Quantile-Quantile-Diagram compares the actual distribution with the ideal Normal distribution
#it is recommended to perform a z-standardization of the variable
#with CTests data points:
tests_Data_comb$C.Test_Total_z <- scale(tests_Data_comb$C.Test_Total) 
qqnorm(tests_Data_comb$C.Test_Total_z)
qqline(tests_Data_comb$C.Test_Total_z)
#with OnSet data points:
tests_Data_comb$onSET_TOTAL_z <- scale(tests_Data_comb$onSET_TOTAL) 
qqnorm(tests_Data_comb$onSET_TOTAL_z)
qqline(tests_Data_comb$onSET_TOTAL_z)
#check for normality with a histogram and z-standardization
#for CTest data points
hist(tests_Data_comb$C.Test_Total_z)
#for OnSet data points
hist(tests_Data_comb$onSET_TOTAL_z)
#check for normality with a histogram without z-standardization
#for CTest data points
hist(tests_Data_comb$C.Test_Total)
#for OnSet data points
hist(tests_Data_comb$onSET_TOTAL)
#install and load package to run pearsons correlation 
install.packages("ggpubr")
library("ggpubr")

#pearson correlation coefficient # for normally distributed data
cor(tests_Data_comb$C.Test_Total, tests_Data_comb$onSET_TOTAL, method = c("pearson"))
cor.test(tests_Data_comb$C.Test_Total, tests_Data_comb$onSET_TOTAL, method = c("pearson")) 

#kendall correlation coefficient #The Kendall rank correlation coefficient or Kendall’s tau statistic is used to estimate a rank-based measure of association. This test may be used if the data do not necessarily come from a bivariate normal distribution.
cor(tests_Data_comb$C.Test_Total, tests_Data_comb$onSET_TOTAL, method = c("kendall"))
cor.test(tests_Data_comb$C.Test_Total, tests_Data_comb$onSET_TOTAL, method = c("kendall"))

#spearman correlation coefficient rho # for data that is not from a bivariate normal distribution 
cor(tests_Data_comb$C.Test_Total, tests_Data_comb$onSET_TOTAL, method = c("spearman"))
cor.test(tests_Data_comb$C.Test_Total, tests_Data_comb$onSET_TOTAL, method = c("spearman"))

#create scatterplot to look for linear covariation
ggscatter(tests_Data_comb, x = "C.Test_Total", y = "onSET_TOTAL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "C.Test_Total Scores", ylab = "onSET_TOTAL Scores")

#students t-test, to compare the means between two groups, The t test tells you how significant the differences between group means are

#t.test(Set_Score ~ m.0.f.1, data = tests_Data)
#t.test(OnSet_Score ~ m.0.f.1, data = tests_Data)

#Trennschärfe berechen,Itemtrennschärfe genannt, darunter versteht man in der klassischen Testtheorie die Korrelation eines Items i mit dem Gesamtergebnis eines Tests.

#check correlation between items and complete C-Test

Only_C_test_comb<-(subset(tests_Data_comb, select=c(Item1, Item2, Item3, Item4, Item5, C.Test_Total)))

round(cor(Only_C_test_comb),2)

#check correlation between SuperItems

#round(cor(Only_SuperItems),2)

#Cronbachs alpha berechnen...Test reliability
#psych::alpha ---- include psych:: otherwise default for alpha is to use the scales package

#psych::alpha(Only_SuperItems)

#Die Item-Trennschärfe macht eine Aussage darüber, wie stark das Abschneiden in einem Item mit dem Gesamt-Testscore zusammenhängt.
#round(cor(Only_SuperItems),2)

#taken from Markus Burkhardt youtube tutorial
#use std.alpha data if the answer formats are different, e.g., 5- vs 6-point Likert scale
correlation_with_rdrop <- psych::alpha(Only_C_test_comb)
#r.drop  correlation of each item with the total score,NOT including this item;  Items that are particularly good at discriminating between individuals at the extreme ends of the scale will have strong positive
#correlations with the total score, and so this correlation is often cited as a measure of the “discriminatory
#power” of an item
correlation_with_rdrop 

#Item Trennschaerfe Superitemcorrelation with total test
#The point-biserial correlation is the Pearson correlation between responses to a particular item and scores on the total test
#(with or without that item). The Biserial Correlation models the responses to the item to represent 
#stratification of a normal distribution and computes the correlation accordingly. 
#Like the Discrimination Index the range is -1.0 to 1.0. Generally 0.2 and above is considered to have high correlation 
#and positive association with overall performance on the assessment

######
#visualize information between clusters (see other script) of C-Test and CEFR levels of onSET
# read in CSV file with clusters and levels

clusters_levels <- read.csv("levels_clusters_orig.csv", header = TRUE)

#look at data
head(clusters_levels)
summary(clusters_levels) #data shows many NA's in columns that are shorter

#rearrange data so clusters are next to corresponding levels

in_order <- select(clusters_levels, c(1,5,2,6,3,7,4,8))
head(in_order)
summary <- summary(in_order)
#export summary

write.csv(summary, "summary_cluster_levels_ordered.csv")

#same with modified data

clusters_levels_mod <- read.csv("levels_clusters_mod.csv", header = TRUE)

#look at data
head(clusters_levels_mod)
summary(clusters_levels_mod) #data shows many NA's in columns that are shorter

#rearrange data so clusters are next to corresponding levels

in_order_mod <- select(clusters_levels_mod, c(1,5,2,6,3,7,4,8))
head(in_order_mod)
summary_mod <- summary(in_order_mod)
#export summary

write.csv(summary_mod, "summary_cluster_levels_ordered_mod.csv")

#look at data with boxplot


boxplot(in_order)
#"uA2_onSET", "Cluster_1", "A2_onSET", "Cluster_2", "B1_onSET", "Cluster_3", "B2_onSET", "B2_onSET", "Cluster_4"
ggplot(in_order, aes(x =  ,y = )) +
        geom_boxplot()



