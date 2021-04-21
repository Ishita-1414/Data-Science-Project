library(ggplot2)
print(getwd())
setwd("C:/Users/ishit/OneDrive/DEsktop/ds")
student <- read.csv("studentInfo.csv")
print("DATASET")
print(student)
print("Type of dataset")
print(class(student))
print(mode(student))
print(typeof(student))

obs = nrow(student)
var = ncol(student)
print("no of observations")
print(obs)
print("no of variables")
print(var)
attach(student)
print("Info about studied credit")
print(summary(student$studied_credits))

#Average credit of students by gender
agg_gender = aggregate(studied_credits~(gender),student,mean)
print(agg_gender)
barplot(agg_gender$studied_credits, width = 1,names.arg = agg_gender$gender, 
        horiz = FALSE, xlab = "gender", ylab = "studied credit", axes = TRUE, 
        col = c("pink","blue"), ylim = c(0,85))


diff_age = unique(student$age_band)
print("Different range of ages of student")
print(diff_age)
n = c()
print("No. of students of range 55<=")
n[1]=length(age_band[age_band=="55<="])
print(n[1])
print("No. of students of range 35-55")
n[2]=length(age_band[age_band=="35-55"])
print(n[2])
print("No. of students of range 0-35")
n[3]=length(age_band[age_band=="0-35"])
print(n[3])
pie(n,diff_age,main = "No of people of each age range",col = c("red","blue","green"))


print("Aggregate of studied credit for diff. age gruop")
agg_age=aggregate(studied_credits~(age_band),student,mean)
print(agg_age)
barplot(agg_age$studied_credits, width = 1,names.arg = agg_age$age_band, 
        horiz = FALSE, xlab = "gender", ylab = "studied credit", axes = TRUE, 
        col = c("pink","blue","green"), main = "Mean Credit of Age Groups")


print("Aggregate of studied credit for different genders and age gruop")
print("Age group 55<=")
people_55 = student[which(age_band=="55<=" ),]
agg_55 = aggregate(studied_credits~(gender),people_55,mean)
print(agg_55)
print("Age group 35-55")
people_35_55 = student[which(age_band=="35-55" ),]
agg_35_55 = aggregate(studied_credits~(gender),people_35_55,mean)
print(agg_35_55)
print("Age group 0-35")
people_35 = student[which(age_band=="0-35" ),]
agg_35 = aggregate(studied_credits~(gender),people_35,mean)
print(agg_35)
age_gen = data.frame(Gender = c('M','M','M','F','F','F'),
                     Category = c("age_0to35", "age_35to55", "agg_above55",
                                  "age_0to35", "age_35to55", "agg_above55"),
                     Credit_Mean = c(agg_35$studied_credits[2],agg_35_55$studied_credits[2],
                                     agg_55$studied_credits[2],agg_35$studied_credits[1],
                                     agg_35_55$studied_credits[1],agg_55$studied_credits[1])
                     )
ggplot(age_gen, aes(factor(Category), Credit_Mean, fill = Gender)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")



diff_region = unique(student$region)
print("Different Regions")
print(diff_region)
diff_edu = unique(student$highest_education)
print("Different highest education")
print(diff_edu)
print("Aggregate of studied credit for students with diff. highest education")
high_edu = aggregate(studied_credits~(highest_education),student,mean)
print(high_edu)
barplot(high_edu$studied_credits, width = 1,names.arg = c("A Level","HE Qual","Lower A","No Formal","PG"), 
        horiz = FALSE, xlab = "Education", ylab = "studied credit", axes = TRUE, 
        col = c("red","cyan","blue","green","yellow"), main = "Mean Credit of Diff. Education Qualification")
