
#load libraries
library(tidyverse)
library(cluster)
library(fpc)
library(factoextra)
library(janitor)

#set working directory
setwd("C:/Users/steph/OneDrive/Documents/DTSC560")

#read in datafile
df2<-read.csv("Employees.csv")
View(df2)

#Create a data frame with only binary variables - Gender, 
#Real.Estate.Purchases, Graduate.Degree, Have.Children - 
#by removing columns 1, 4, 5, and 7
quantdf2<-df2[c(1,16,20,25)]
View(quantdf2)

#normalize each variable
quantdfn2<-scale(quantdf2)
View(quantdfn2)

#set random number seed in order to replicate the analysis
set.seed(42)

#calculate distance between each pair of observations using the dist function 
#and manhattan distance
match_dist<-dist(quantdfn2, method="euclidean")

#run hierarchical clustering with the hclust function and group average linkage
cl_match_avg<-hclust(match_dist, method="ward.D")

#plot the dendrogram
plot(cl_match_avg)

#Create 4 clusters using the cutree function
cl_match_avg_4<-cutree(cl_match_avg, k=4)

#display vector of cluster assignments for each observation
cl_match_avg_4

#visualize clusters on the dendrogram
rect.hclust(cl_match_avg, k=4, border=2:4)

#link cluster assignments to original categorical data frame
hcl4df<-cbind(quantdf2, clusterID=cl_match_avg_4)

#write data frame to CSV file to analyze in Excel
write.csv(hcl4df, "assignment2-2.csv")

#display number of observations in each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize(n())

#calculate variable averages for all non-normalized observations
summarize_all(quantdf2, mean)

#Calculate variable averages for each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize_all(mean)

#Create frequency tables for each variable overall
tabyl(hcl4df$Age)
tabyl(hcl4df$MonthlyIncome)
tabyl(hcl4df$PercentSalaryHike)
tabyl(hcl4df$YearsAtCompany)

#Create frequency tables for each variable by cluster
tabyl(hcl4df,Age,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df,MonthlyIncome,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df,PercentSalaryHike,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df,YearsAtCompany,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()



