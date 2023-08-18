#Arissa Noordina Bahari R Mini Project 

# In this R mini Project, I will be using the College.csv dataset and perform analyses on it
# Information on this dataset and columns can be found here: https://www.kaggle.com/datasets/faressayah/college-data

#########################################

# Step 1: import the data
# I have loaded the csv file through the Environment. The code below starts after that step

attach(College)   # attach the college dataset to the R script
df = College      # name the table df
df
View(df)

########################################

# Step 2: Data Exploration


str(df)           # review the structure of the data
#from the structure, we can observe that besides the Private column, all other columns are numeric

summary(df)       # view the summary of the data
# from the summary, we can see that there are no null values in the datasetfurther.
# this means that we do  not have to account for any missing values

names(df)   # column names

nrow(df)    # number of rows
ncol(df)    # number of columns

head(df)    # head
tail(df)    # tail

colSums((is.na(df)))  # sum na values for all columns

# Set Private as factors
#Since the Private column is a categorical data with two values, we can change it to a factor class
df$Private = as.factor(df$Private) #change df$Private as a factor
df$Private

str(df)           # check that the data type is factor. we can see that it is.


#########################################

#Step 3: perform any preprocessing
#We will now perform preprocessing on the data

#First, we load the tidyverse and dplyr packages

library(dplyr)
library(tidyverse)

# Next, we change the convert the values of Private to 0 and 1
#df <- df %>% mutate(Private = ifelse(Private == "Yes", 1, 0))

#From the ISLRv2 pg 56
# We create a new column Elite, which takes 1 for Colleges with more then 50% of students from the top10perc

df$Elite = rep("No", nrow(df))         # Create a new col Elite with No's in every row
df$Elite[df$Top10perc > 50 ] = "Yes"    # In Elite, we filter for colleges with more than 50% of students in the top 10 percent group, take as yes
df$Elite = as.factor(df$Elite)      # Convert to factors

#We have now created two factor columns with 0,1 as values

#Now, we create  to better scale our data, we create two new percentage columns

df$Acc.Rate = df$Accept/df$Apps*100       #Acceptance rate visualises the actual pct of accepted applications
df$Enroll.Rate = df$Enroll/df$Apps*100    #Enroll.Rate visualises the actual pct of enrolled vs applied

####

summary(df) #Take a look at the summary before we proceed

# based on the link above, we know that Top10perc, Top25perc, PhD, Terminal, perc.alumni and Grad.Rate are percentages
# which means that no value can be above 100%
# since Grad.Rate and PhD have values more than 100, we remove those rows

####

#Remove row from Grad.Rate with pct > 100

sum(df$Grad.Rate >100)  # check how many values are more than 100 pct
which.max(df$Grad.Rate) # find which row contains the value
df = df[-96, ]          # remove the row

#Remove row from Grad.Rate with pct > 100

sum(df$PhD >100)  # check how many values are more than 100 pct
which.max(df$PhD) # find which row contains the value
df = df[-582, ]          # remove the row

summary(df)             #check that the max value of Grad.Rate and PhD is =< 100


# We have now completed the data preprocessing



############################################

# Step 4: Data visualisation
# In this step, we are going to create some graphs to answer questions based on the data


library(ggplot2)  
library(gridExtra)

###########
#Before we investigate our dataset, let us visualise the numeric data
par(mfrow = c(2,3))

hist(df$Apps, xlab = "Applications", main = "Histogram of Applications", col = "red")
hist(df$Top10perc, xlab = "Top 10 Percent", main = "Histogram of Top 10%", col = "blue")
hist(df$F.Undergrad, xlab = "FullTime Undergrads", main = "Histogram of Full Time Undergrads", col = "yellow")
hist(df$Outstate,xlab = "Outstate Tuition", main = "Histogram of Outstate Tuition", col = "green")
hist(df$Grad.Rate, xlab = "Graduation Rate", main = "Histogram of Grad Rate", col = "pink")
hist(df$Expend, xlab = "Expenditure", main = "Histogram of Expenditure", col = "cyan")

# From the histograms, we can see that Apps, Top10perc, Ft.Undergrad and Expend are very right skewed
# Meanwhile, Outstate is slightly right skewed
# Grad.Rate is slightly left skewed


#Private and Public colleges, and Elite relationship
e1 = ggplot(df, aes(Private)) + geom_bar(aes(fill = Private)) + ggtitle("Private vs Public Colleges")
e2 = ggplot(df, aes(Elite)) + geom_bar(aes(fill = Private)) + ggtitle("Elite vs Normal Colleges")

grid.arrange(e1, e2, ncol = 2)

# From the Private bar chart, we can observe there are more private colleges than public. Meanwhile, There are lesser Elite colleges.
# For Elite colleges, most are private

############

# Question 1: What is the relationship between expenditure, SFRatio, PhD % and Outstate tuition and graduation rates for the college types?

q11 = ggplot(df, aes(x = Expend, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Expenditure vs Grad Rate")
q12 = ggplot(df, aes(x = S.F.Ratio, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Student-Faculty Ratio vs Grad Rate")
q13 = ggplot(df, aes(x = PhD, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Faculty PhD Pct vs Grad Rate")
q14 = ggplot(df, aes(x = Outstate, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Outstate Tuition vs Grad Rate")

grid.arrange(q11,q12,q13,q14, ncol = 2)

# Analysis on the charts
# Expend vs Grad Rate: There are no discernible correlations between expenditure and grad rate
# SFRatio vs Grad Rate:  The is no trend between SFRatio and grad rate
# PhD vs Grad Rate: For both private and public colleges, there is a positive trend between PhD and graduation rate
# Outstate vs Grad Rate: For both college types, the higher the Outstate, the higher the graduation rate

# Question 2: What is the relationship between acceptance rate, enrolled rate and graduation rate?

q21 = ggplot(df, aes(x = Acc.Rate, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Acceptance Rate vs Grad Rate")
q22 = ggplot(df, aes(x = Enroll.Rate, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Enrolment Rate vs Grad Rate")

grid.arrange(q21,q22, ncol = 2)

# Analysis on the plots

# Acceptance rate vs Grad Rate: There is no trend
# Enrolment rate vs Grad rate: There is no trend

# Question 3: How does Top10perc and Top25perc affect graduation rate?

q31 = ggplot(df, aes(x = Top10perc, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Top10perc vs Grad Rate")
q32 = ggplot(df, aes(x = Top25perc, y = Grad.Rate, col = Private, shape = Private)) + geom_point() + ggtitle("Top25perc vs Grad Rate")

grid.arrange(q31,q32, ncol = 2)

# Analysis
# For both Top10perc and Top25perc, there is a weak positive trend between the percentile and graduation rate


# Question 4: How does the Private status affect Outstate Tuition and Student Faculty Ratio

q41 = ggplot(df, aes(Private, Outstate)) + geom_boxplot(aes(fill=Private)) + ggtitle("Boxplot of Outstate Tuition")
q42 = ggplot(df, aes(Private, S.F.Ratio)) + geom_boxplot(aes(fill=Private)) + ggtitle("Boxplot of Student Faculty Ratio")

grid.arrange(q41,q42, ncol = 2)

# Analysis
# In Outstate Tuition, we can see that there are 6 outliers for the public colleges, and one outlier for the Private college
# Meanwhile, in the SFRatio, there are many outliers for private colleges, while only 4 outliers for public colleges

# Question 5: How does college Eliteness affect Graduation rate and Outstate Tuition for both private and public colleges

q51 = ggplot(df, aes(x = Elite, y = Grad.Rate, col = Private, shape = Private)) + geom_boxplot() + ggtitle("Elite vs Grad Rate")
q52 = ggplot(df, aes(x = Elite, y = Outstate, col = Private, shape = Private)) + geom_boxplot() + ggtitle("Elite vs Outstate Tuition")

grid.arrange(q51,q52, ncol = 2)

# In Elite vs Grad Rate, we can observe that both private and public elite colleges have overall higher medians, shorter tails, and smaller IQR
# Meanwhile, non-elite private and public colleges have longer tails, lower medians and larger IQR than their Elite counterparts

# In Elite vs Outstation, while both private and public non-elite colleges have lower outstate tuition fees than their private counterparts, 
# Elite school have a higher range of fees.
# Private elite colleges are generally more expensive that non-elite private colleges; however, non-elite private colleges have longer tails
# and cover a wider range 

#Question 6: What is the density plot for the graduation rate

ggplot(df, aes(Grad.Rate, fill = Private)) + geom_density(alpha = 0.6) + ggtitle("Density Plot of Graduation Rate")

# Public colleges follow a somewhat normal distribution around mean 53
# Private colleges follow a left skewed distribution, with a lower maximum density than public colleges

###########################################

#Step 5: Perform any Machine Learning algorithm

# In this step, I will perform K-means clustering on my dataset
# In K means clustering, I will investigate how the columns affect the clusters and choose the optimal cluster value based on a scree plot

############

# K-means clustering
# Before performing clustering, change the Private and Elite columns to binary variables as clustering only takes numeric values

df1 = df %>% mutate(Private = ifelse(Private == "Yes", 1, 0), Elite = ifelse(Elite == "Yes", 1, 0))

library(factoextra)
library(stats)

# Perform K-means clustering for K = 2:5

dfclust2 = kmeans(scale(df1), centers = 2, nstart = 100)
dfclust3 = kmeans(scale(df1), centers = 3, nstart = 100)
dfclust4 = kmeans(scale(df1), centers = 4, nstart = 100)
dfclust5 = kmeans(scale(df1), centers = 5, nstart = 100)

# Plot the K-means cluster charts in a grod

kmn2 = fviz_cluster(dfclust2, data = df1, ellipse = TRUE, ellipse.type = "norm", main = "2 Cluster Plot of College")
kmn3 = fviz_cluster(dfclust3, data = df1, ellipse = TRUE, ellipse.type = "norm", main = "3 Cluster Plot of College")
kmn4 = fviz_cluster(dfclust4, data = df1, ellipse = TRUE, ellipse.type = "norm", main = "4 Cluster Plot of College")
kmn5 = fviz_cluster(dfclust5, data = df1, ellipse = TRUE, ellipse.type = "norm", main = "5 Cluster Plot of College")

grid.arrange(kmn2, kmn3, kmn4, kmn5, ncol = 2)

# We can observe distinct groups for each value of k. Hence, to make the best decision, we choose the optimal K with a scree plot

#scree plot to determine the optimal number of clusters

fviz_nbclust(scale(df1), kmeans, method = "wss", k.max = 10)

# From the scree plot, we can observe that after 5 clusters, the gradient becomes shallow, which indicates the best cutoff for cluster amounts
# Hence, we choose 5 clusters as our optimal clustering



names(df)
