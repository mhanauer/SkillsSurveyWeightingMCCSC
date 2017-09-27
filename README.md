---
title: "Survey Weights with School Dataset"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, message=FALSE, warning=FALSE, echo=FALSE}
setwd("~/Google Drive/Skills/SurveyWeighting")
schoolOriginal = as.data.frame(read.csv("MCCSCStaffSurvey.csv", header = TRUE, na.strings = ("")))
school = schoolOriginal
school = school[-c(1:11),]
school = as.data.frame(school[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q30")])
school = na.omit(school)
eth = school$Q30
# Now I need grab the average but need to seperate from eth.  This gets a list to a data frame.
schoolSelAverage = as.matrix(school[,1:6])
schoolSelAverage = as.integer(schoolSelAverage)
schoolSelAverage = matrix(schoolSelAverage, length(schoolSelAverage), 6)
schoolSelAverage = as.data.frame(schoolSelAverage)
colnames(schoolSelAverage) = c("SELQuant1", "SELQuant2", "SELQuant3", "SELQuant4", "SELQuant5", "SELQuant6") 
schoolSelAverage = as.data.frame(schoolSelAverage)
schoolSelAverage = as.data.frame(rowMeans(schoolSelAverage))
colnames(schoolSelAverage) = c("selAverage")
school = cbind(schoolSelAverage, eth)
colnames(school) = c("selAverage", "eth")
```

```{r, message=FALSE, warning=FALSE,echo=FALSE}
school = subset(school,eth == 1 | eth == 2 | eth == 3, select = c(selAverage, eth))
write.csv(school, "school.csv", row.names = FALSE)
school = read.csv("school.csv", header = TRUE)
```
Here is an example of weighting data from a project I completed with a local school district.  In this example, I have a selAverage score, which is the average score on six Likert scale questions regarding the district's staff's satisfaction with their social and emotional learning programs.  I also have an ethnicity variable with three categories white (1), Hispanic, (2), and African American (3).  Because only approximately 30% of respondents answered the questions, it is unclear if those who did respond are representative of the whole school district.  For example, it could be the case that few minorities answered the survey minimizing their representation in the survey.  One way to account for the missing responses is to weigh up the responses to their population values.  Below is a table of the percentages of three ethnicities for this school district.  We can help ensure that we are not over or under-representing different ethnicities by weighting the responses to the selAverage variable by actual population percentages for these three ethnicities.  For example, if in the data there are only 2.15% African Americans, then weighting data would count their responses twice as much since 2.15% is only half (4.3%) the population percentage of African Americans.
```{r, message=FALSE, warning=FALSE, echo = FALSE}
ethTable = matrix(c(.934, .013, .043), ncol = 1, byrow = TRUE)
colnames(ethTable)= c("Percentage")
rownames(ethTable) = c("White", "Hispanic", "African American")
ethTable
```
The first step is to create the survey data set that has no weights and assumes a simple random sample (ids =~ 1).  This how we sampled the data in the school data set therefore is a fair assumption.
```{r, message=FALSE, warning=FALSE}
library(survey)
school.svy.unweighted <- svydesign(ids=~1, data=school)
```
Next, we have to input the marginal probabilities (i.e., percentages for each ethnicity in the population) for the variables that we want to weight the data by, which is located in the table above into a new variable eth.dist.
```{r, message=FALSE, warning=FALSE}
eth.dist <- data.frame(eth = c("1", "2", "3"),
                       Freq = nrow(school) * c(0.934, 0.013, .043))
```
Next, we can use the rake function in the survey package to weight the current data by the population values for each of the ethnicities included in the table above.
```{r, message=FALSE, warning=FALSE}
school.svy.rake <- rake(design = school.svy.unweighted,
                   sample.margins = list(~eth),
                   population.margins = list(eth.dist))
```
Finally, because the weights can become either too large or too small, we can put limits on the weights using the trimWeights function. In this example, we restrict the weights to .3 to 3 (i.e., three means a value being counted three times as much as its original value).

Then we can get the weighted means by using the svymean function. We can then compare these means with the original means to evaluate the changes the weights played.  Although, in this case, there is not much difference, in some cases weighting can play a larger impact providing a more representative view of your data.
```{r, message=FALSE, warning=FALSE}
school.svy.rake.trim <- trimWeights(school.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE)

svymean(school$selAverage, school.svy.rake.trim)
mean(school$selAverage)
```

