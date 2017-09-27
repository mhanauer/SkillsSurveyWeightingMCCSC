---
  title: "Needs Asssessment SEL Quantiative"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Steps: 
  1. Get rid of extra data for each 
2. Combine the relevant data
3. Grab demographics gender, degree, type of teacher, race
4. CCR grab all the and transform to numbers
5. Get an overall average value
6. Export the data 

Probably should get general demographics first.  Need to combine them and then 

Get the variables I want for SEL  

SELQuant1 = SEL first survey quantitative question 
SELQualRes = SEL resources question
SELQualBarr = SEL barriers question

Now school SEL
```{r}
#setwd("~/Google Drive/Skills/SurveyWeighting")
#school = as.data.frame(read.csv("MCCSCStaffSurvey.csv", header = TRUE, na.strings = ("")))
head(school)
school = school[-c(1:11),]; head(school, 20)
school1 = school[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6")]
eth = school[c("Q30")]
school = as.data.frame(cbind(school1,eth))
school = na.omit(school)
# Now I need grab the average but need to seperate from eth.  This gets a list to a data frame.
schoolSelAverage = as.matrix(school[,1:6]); head(schoolSelAverage)
schoolSelAverage = as.integer(schoolSelAverage); head(schoolSelAverage)
schoolSelAverage = matrix(schoolSelAverage, length(schoolSelAverage), 6); schoolSelAverage
schoolSelAverage = as.data.frame(schoolSelAverage); head(schoolSelAverage)
colnames(schoolSelAverage) = c("SELQuant1", "SELQuant2", "SELQuant3", "SELQuant4", "SELQuant5", "SELQuant6") 
schoolSelAverage = as.data.frame(schoolSelAverage); head(schoolSelAverage)
schoolSelAverage = as.data.frame(rowMeans(schoolSelAverage)); schoolSelAverage
colnames(schoolSelAverage) = c("selAverage")
school = cbind(schoolSelAverage, eth)
colnames(school) = c("selAverage", "eth")
school
# Quantiative, qual questions, and demographics
```
Now we need to 
1. Figure out what the factors for each eth are
2. Combine them to the following: American Indian, Black (3), Asian, Hispanic (2), White (1), Multiracial, Native Hawaiian or other Pacific Islander

Some other race or ethnicitiy (12)

Drop everything besides, white (1), black (2), and hispanic (2) not enough to justify weighting.
```{r}
count(school, 'eth')
school = subset(school,eth == 1 | eth == 2 | eth == 3, select = c(selAverage, eth)); head(school)
write.csv(school, "school.csv", row.names = FALSE)
school = read.csv("school.csv", header = TRUE)
count(school, 'eth')
```
Now we need to get do the actual weighting
White = 66,273; 93.4
Hispanic = 944; 1.3
Black  = 3,106; 4.3

Here I create the survey data set that has no weights and assumes a simple random sample (ids =~ 1) using the school data set.
```{r}
library(survey)
school.svy.unweighted <- svydesign(ids=~1, data=school)
```
Next, we have to get the marginal probabilities for the variables that we want to weight the data by. With the school data we know that the population values for white (1), Hispanic (2), and African American (3) and male (2) are as follows 93.4, 1.3, and 4.3.
```{r}
eth.dist <- data.frame(eth = c("1", "2", "3"),
                       Freq = nrow(school) * c(0.934, 0.013, .043))
```
Here we use the rake function in the survey package to weight the current data by the population values for each of the ethnicities included in the dataset.
```{r}
school.svy.rake <- rake(design = school.svy.unweighted,
                   sample.margins = list(~eth),
                   population.margins = list(eth.dist))
```
Finally, because the weights can become either too large or too small, we can put limits on the weights using the trimWeights function. In this example, we limit the weights to .3 to 3 (i.e. 3 means a value being counted three times as much as its original value).

Then we can get the weighted means by using the svymean function. We can then compare these means with the original means to evaluate the changes the weights played.  Although, in this case there is not much difference, in some cases weighting can play a larger impact providing a more representative view of your data.
```{r}
school.svy.rake.trim <- trimWeights(school.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE)

svymean(school$selAverage, school.svy.rake.trim)
mean(school$selAverage)
```

