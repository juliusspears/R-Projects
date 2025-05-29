LoadLibraries <- function() {
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ISLR2)
  library(DescTools)
  print("Libraries have been loaded")
}

LoadLibraries()

#GETTING DATA
studhabts <- read.csv("data/student_habits_performance.csv")
View(studhabts)
attach(studhabts)

str(studhabts)

##Numerical Traits

##Study Hours and Exam Scores
summary(studhabts$study_hours_per_day)
sd(studhabts$study_hours_per_day)
var(studhabts$study_hours_per_day)

plot(density(studhabts$study_hours_per_day), lwd = 2, col = 'blue', xlab =
       'Study Hours per Day', 
       main = 'Probability Density for Study Hours per Day of Students')

#Split study hours into three groups
studhabts$studyamount <- ifelse(studhabts$study_hours_per_day < 2, "Below Average",
"Above Average")
studhabts$studyamount[studhabts$study_hours_per_day >= 2 & 
                      studhabts$study_hours_per_day <= 5] <- "Average"
studhabts$studyamount <- factor(studhabts$studyamount, levels = c("Below Average",
"Average", "Above Average"))

Colors <- c('Below Average' = 'Red',
            'Average' = 'Green',
            'Above Average' = 'Blue')

pointCol <- Colors[studhabts$studyamount]

table(pointCol)
table(studhabts$studyamount)

#Descriptive statistics, mean and median of exam scores based on study amount
tapply(studhabts$exam_score, studhabts$studyamount, mean)
tapply(studhabts$exam_score, studhabts$studyamount, median)
tapply(studhabts$exam_score, studhabts$studyamount, var)

#Boxplots of exam score based on study hours
boxplot(exam_score ~ studyamount, data = studhabts, xlab = 'Study Amount',
        ylab = 'Exam Score', main = 'Boxplots of Exam Score Based on Study Amount'
        , col = Colors)

#Split exam score into groups by study amount
scrbystudy <- split(studhabts$exam_score, studhabts$studyamount)

#Probability densities of exam scores based on study amount
plot(density(scrbystudy$`Above Average`), col = 'blue'
     , xlim = c(10,100), ylim = c(0, 0.06), lwd = 2, xlab = 'Exam Scores', 
     main = 'Probability Densities of Exam Scores Based on Study Amount')
lines(density(scrbystudy$Average), col = 'green', lwd = 2)
lines(density(scrbystudy$`Below Average`), col = 'red', lwd = 2)
legend('topleft', legend = levels(studhabts$studyamount), lty = c(1,1,1), lwd = 2, 
       col = Colors)

#Anova test
summary(aov(studhabts$exam_score ~ studhabts$study_hours_per_day))



##Sleep Hours and Exam Scores
summary(studhabts$sleep_hours)
sd(studhabts$sleep_hours)
var(studhabts$sleep_hours)

plot(density(studhabts$sleep_hours), lwd = 2, col = 'blue', 
     xlab = 'Sleep Hours per Day', 
     main = 'Probability Density for Sleep Hours per Day of Students')

#Split sleep hours into groups based on amount
studhabts$sleep_quality <- ifelse(studhabts$sleep_hours < 5, "Poor", "Great")
studhabts$sleep_quality[studhabts$sleep_hours >= 5 & 
                          studhabts$sleep_hours <= 8] <- "Fair"
studhabts$sleep_quality <- factor(studhabts$sleep_quality, levels = 
                                    c("Poor", "Fair", "Great"))

Colors2 <- c("Poor" = "Red",
             "Fair" = "Green",
             "Great" = "Blue")

pointCol2 <- Colors2[studhabts$sleep_quality]

#Descriptive statistics for each sleep quality group, mean and median of exam scores
tapply(studhabts$exam_score, studhabts$sleep_quality, mean)
tapply(studhabts$exam_score, studhabts$sleep_quality, median)
tapply(studhabts$exam_score, studhabts$sleep_quality, sd)

#Boxplot of exam score based on sleep quality 
boxplot(exam_score ~ sleep_quality, data = studhabts, col = Colors2
        ,xlab = 'Sleep Quality', ylab = 'Exam Score', 
        main = 'Boxplots of Effect of Sleep Quality on Exam Scores for Students')

stdybyslp <- split(studhabts$exam_score, studhabts$sleep_quality)

#Probability densities of exam scores based on sleep amount
plot(density(stdybyslp$Fair), col = 'green', lwd = 2, xlab = 'Exam Scores',
     main = 'Probability Densities of Exam Scores Based on Sleep Hours')
lines(density(stdybyslp$Great), lwd = 2,  col = 'blue')
lines(density(stdybyslp$Poor), lwd = 2, col = 'red')
legend('topleft', legend = levels(studhabts$sleep_quality), 
       lty = c(1,1,1), lwd = 2, col = c('red', 'green', 'blue'))


#Anova test of different means of sleep quality groups
summary(aov(exam_score ~ sleep_quality, data = studhabts))



##Social Media Hours and Exam Scores
summary(studhabts$social_media_hours)
sd(studhabts$social_media_hours)
var(studhabts$social_media_hours)

plot(density(studhabts$social_media_hours), lwd = 2, col = 'blue',
     xlab = 'Social Media Hours per Day', 
     main = 'Probability Density of Student Social Media Hours')

#Organize social media hours into smaller groups
studhabts$social_media_activity <- ifelse(social_media_hours < 1.5, "Low",
                                          "High")
studhabts$social_media_activity[studhabts$social_media_hours >= 1.5 &
                                  studhabts$social_media_hours <= 3.5] <- "Moderate"
studhabts$social_media_activity <- factor(studhabts$social_media_activity, levels =
                                    c("Low", "Moderate", "High"))

pointCol4 <- Colors[studhabts$social_media_activity]

#MEANS AND MEDIANS
tapply(studhabts$exam_score, studhabts$social_media_activity, mean)
tapply(studhabts$exam_score, studhabts$social_media_activity, median)
tapply(studhabts$exam_score, studhabts$social_media_activity, var)

#Boxplot of Exam Scores Based on Social Media Activity
boxplot(exam_score ~ social_media_activity, data = studhabts, col = Colors,
        xlab = 'Social Media Activity', ylab = 'Exam Score', 
        main = 'Social Media Activity of Students and Exam Scores')

scrbysocact <- split(studhabts$exam_score, studhabts$social_media_activity)

#Probability Densities of Exam Scores Based on Social Media Activity
plot(density(scrbysocact$High), col = 'blue', xlab = 'Exam Scores',
     main = 'Probability Densities of Exam Scores Based on Social Media Activity',
     lwd = 2, xlim = c(10, 100))
lines(density(scrbysocact$Moderate), lwd = 2,  col = 'green')
lines(density(scrbysocact$Low), lwd = 2, col = 'red')
legend('topleft', legend = levels(studhabts$social_media_activity), 
       lty = c(1,1,1), lwd = 2, col = Colors)

#ANOVA TEST
summary(aov(exam_score ~ social_media_activity, data = studhabts))


###Categorical Traits


##Part Time Job Analysis on Exam Scores
tapply(exam_score, part_time_job, summary)
tapply(exam_score, part_time_job, sd)
tapply(exam_score, part_time_job, var)

scrbyjob <- split(exam_score, part_time_job)

#Boxplot of exam scores based on part time job status
boxplot(exam_score ~ part_time_job, data = studhabts, col = c('red', 'blue')
        , xlab = 'Part Time Job Status', ylab = 'Exam Scores'
        , main = 'Boxplots of Exam Scores Based on Part Time Job Status')
summary(scrbyjob$No) 
summary(scrbyjob$Yes)

#Probability densities of exam scores based on part time job status
plot(density(scrbyjob$Yes), 
  main = "Probability Densities of Exam Scores Based on Part-Time Job Status", 
  xlab = 'Exam Scores', lwd = 2, col = 'blue', xlim = c(10, 100))
lines(density(scrbyjob$No), col = 'red', lwd = 2)
legend('topleft', legend  = levels(as.factor(part_time_job)), lty = c(1,1,1),
       lwd = 2, col = c('red', 'blue'), title = 'Part Time Job Status')

#T test conducted
t.test(exam_score ~ part_time_job, data = studhabts, var.equal = T)



##Mental Health and Exam Scores
summary(studhabts$mental_health_rating)
sd(studhabts$mental_health_rating)
var(studhabts$mental_health_rating)

table(studhabts$mental_health_rating)
barplot(table(studhabts$mental_health_rating), ylim = c(0, max(table(studhabts$mental_health_rating)) + 10),
        xlab = "Mental Health Rating", ylab = "Frequency", 
        main = "Frequency of Mental Health Ratings", col = 'Salmon')

#Organizing mental health rating into groups known as the mental health quality
studhabts$MHQuality <- ifelse(mental_health_rating < 4, 'Poor', 'Great')
studhabts$MHQuality[studhabts$mental_health_rating >= 4 & 
                      studhabts$mental_health_rating <= 6] <- "Normal"
studhabts$MHQuality <- factor(studhabts$MHQuality, 
                              levels = c('Poor', 'Normal', 'Great'))

Colors3 <- c('Poor' = 'Red',
             'Normal' = 'Green',
             'Great' = 'Blue')

pointCol3 <- Colors3[studhabts$MHQuality]

#Take the mean of each group's exam scores
tapply(studhabts$exam_score, studhabts$MHQuality, mean)
tapply(studhabts$exam_score, studhabts$MHQuality, median)
tapply(studhabts$exam_score, studhabts$MHQuality, sd)
tapply(studhabts$exam_score, studhabts$MHQuality, var)

#Boxplot of Exam scores based on mental health quality
boxplot(exam_score ~ MHQuality, data = studhabts, 
        xlab = "Mental Health Rating", ylab = "Exam Score"
        , main = "Exam Score Based on Mental Health Rating for Students"
        , col = Colors3)

scrbymhq <- split(studhabts$exam_score, studhabts$MHQuality)

#Probability densities of exam scores based on mental health quality
plot(density(scrbymhq$Great), col = 'blue', lwd = 2, xlim = c(10, 100), 
     xlab = 'Exam Scores', 
     main = 'Probability Densities of Exam Scores based on Mental Health Quality')
lines(density(scrbymhq$Normal), col = 'green', lwd = 2)
lines(density(scrbymhq$Poor), col = 'red', lwd = 2)
legend('topleft', legend = levels(studhabts$MHQuality), 
       lty = c(1,1,1), lwd = 2, col = c('red', 'green', 'blue'))

#Anova test
summary(aov(studhabts$exam_score ~ studhabts$MHQuality))



##Parental Education Level and Exam Scores
table(studhabts$parental_education_level)

studhabts$parental_education_level <- factor(studhabts$parental_education_level,
                        levels = c('None', 'High School', 'Bachelor', 'Master'))

pelexmscr <- split(studhabts$exam_score, studhabts$parental_education_level)

#Descriptive statistics, exam scores based on parental education level
tapply(studhabts$exam_score, studhabts$parental_education_level, summary)
tapply(studhabts$exam_score, studhabts$parental_education_level, sd)
tapply(studhabts$exam_score, studhabts$parental_education_level, var)

#Boxplot of Exam Scores based on parental education level
boxplot(studhabts$exam_score ~ studhabts$parental_education_level,
        xlab = 'Parental Education Level', ylab = 'Exam Scores',
        main = 'Exam Scores Based on Parental Education Level of Students',
        col = c('red', 'orange', 'blue', 'purple'))

#Probability densities of exam scores based on parental education level 
plot(density(pelexmscr$Master), col = 'purple', lwd = 2, xlim = c(10, 100),
     xlab = 'Exam Scores', 
     main = 'Probability Densities of Exam Scores Based on Parental Education Level')
lines(density(pelexmscr$Bachelor), col = 'blue', lwd = 2)
lines(density(pelexmscr$`High School`), col = 'orange', lwd = 2)
lines(density(pelexmscr$None), col = 'red', lwd = 2)
legend('topleft', legend = levels(studhabts$parental_education_level),
       lty = c(1, 1, 1), lwd = 2, col = c('red', 'orange', 'blue', 'purple'))

#Anova Test
summary(aov(exam_score ~ parental_education_level, data = studhabts))


