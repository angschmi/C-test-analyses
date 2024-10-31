#To create a graph of the clusters generated with the kmeans() 
install.packages("factoextra")
library(factoextra)


library(tidyverse)
#https://stackoverflow.com/questions/13706188/importing-csv-file-into-r-numeric-values-read-as-characters
#the above was consulted to resolve issues with commas as decimals in csv file
delim = ","  # or is it "\t" ?
dec = ","    # or is it "," ?
testScoresData <- read.csv("comb_Set3C-Test_OnSet.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=TRUE)
head(testScoresData)
summary(testScoresData)
#make sure its a data.frame
class(testScoresData)

#create subsets of main data frame
#remove onSET column
no_onSET = subset(testScoresData, select = -c(onSET_TOTAL))
head(no_onSET)
#create subset with only the super items
only_SuperItems <- select(testScoresData, c("Item1", "Item2", "Item3", "Item4", "Item5")) 
head(only_SuperItems)


# centroid based clusterting #https://uc-r.github.io/kmeans_clustering
# kmeans() arguments
# k stands for the number of clusters, k can be manually determined, or the best amount of clusters for the data computed
# kmeans(x, centers, iter.max = 10, nstart = 1)
# x: numeric matrix, numeric data frame or a numeric vector
# centers: Possible values are the number of clusters (k) or a set of initial (distinct) cluster centers. If a number, a random set of (distinct) rows in x is chosen as the initial centers.
# iter.max: The maximum number of iterations allowed. Default value is 10.
# nstart: The number of random starting partitions when centers is a number. Trying nstart > 1 is often recommended. 
#However!: As the final result of k-means clustering result is sensitive to the random starting assignments, we specify nstart = 25 (recommended e.g. https://uc-r.github.io/kmeans_clustering). 
#This means that R will try 25 different random starting assignments and then select the best results corresponding to the one with the lowest within cluster variation. 
#The default value of nstart in R is one. But, it’s strongly recommended to compute k-means clustering with a large value of nstart such as 25 or 50, in order to have a more stable result.

#data needs to be standardized (mean of zero, standard deviation of one)
#use scale()
#standarize Super Items
standard_df_SI <- scale(only_SuperItems)
head(standard_df_SI)

#standarize Ctest Scores
standard_CTest <- scale(testScoresData$C.Test_Total)
standard_CTest

#add standardized CTest Scores to dataframe
no_onSET$standard_CTest_Score <-standard_CTest
head(no_onSET)

#perform kmeans cluster analysis on standarized C.Test_Total
#kmeans(onlyCTests_TotalScores,centers = 4,iter.max = 10, nstart = 1) #this belongs to unnecessary step
k4_CTest_Total_Standard <- kmeans(no_onSET$standard_CTest_Score,centers = 4,iter.max = 10, nstart = 25) 
k4_CTest_Total_Standard

#perform kmeans cluster analysis for non standarized C.Test_Total
k4_CTest_Total<- kmeans(no_onSET$C.Test_Total, centers = 4,iter.max = 10, nstart = 25) 
k4_CTest_Total


#curious: what does clustering look like for onSET scores?
k4_Onset_Score <- kmeans(testScoresData$onSET_TOTAL,centers = 4,iter.max = 10, nstart = 25) 
k4_Onset_Score

#kmeans(only_SuperItems,centers = 4,iter.max = 10, nstart = 25) #this is completely unnecessary, as it is creating four clusters per each superitem 
#however,Sireci, S. G., Robin, F., & Patelis, T. (1999). Using cluster analysis to facilitate standard setting. Applied Measurement in Education, 12(3), 301-325.
###say that one uses "all individual items comprising the test" (page308)
k4_onlySI <- kmeans(only_SuperItems, centers = 4, iter.max = 10, nstart = 25)
k4_onlySI

k4_onlySI_standardized <- kmeans(standard_df_SI, centers = 4, iter.max = 10, nstart = 25)
k4_onlySI_standardized
#kmeans(): understanding the output... 

#visualizising kmeans() output with fviz_cluster()

fviz_cluster(k4_onlySI, data = only_SuperItems)
#using standardized Super Items
fviz_cluster(k4_onlySI_standardized, data = standard_df_SI)
#extract the clusters and add to our initial data 

testScoresData %>%
  mutate(Cluster = k4_CTest_Total$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


testScoresData %>%
  mutate(Cluster = k4_onlySI_standardized$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
#add clusters to dataframe
add_clusters <- cbind(testScoresData, cluster = k4_onlySI_standardized$cluster)
head(add_clusters)
#export dataframe with clusters
write.csv(add_clusters, "add_clusters.csv")




