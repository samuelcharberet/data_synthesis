### How heterogeneous is a sample of dried horse 
### dung in terms of C and N content ?
### Does C and N content vary with drying temperature ?
### 

setwd("C:/Users/Samuel/Documents/7. Doctorat/3. Data/0. Chemical composition/0. Samples/0_appendix/0_test_protocole/1_test_protocole_CHN")
table= read.csv("20210211_CHN_test_heterogeneity_1.csv", sep=";")
colnames(table) = c("sample",	"drying_temperature",	
                    "drying_duration",	"autoclaving",	
                    "%N",	"%C",	"%15N",	"%13C")


##### Generalities ##### 
str(table)
hist(table$`%N`, breaks=10)
hist(table$`%C`, breaks=10)
hist(table$`%15N`, breaks=10)
hist(table$`%13C`, breaks=10)

##### Normality and homoscedasticity ##### 

shapiro.test(table$`%N`) #We can assume normality
shapiro.test(table$`%C`) #We can assume normality
shapiro.test(table$`%15N`) #We can assume normality
shapiro.test(table$`%13C`) #We can assume normality

var.test(table$`%N`~table$drying_temperature) #We can assume homoscedasticity
var.test(table$`%C`~table$drying_temperature) #We can assume homoscedasticity
var.test(table$`%15N`~table$drying_temperature) #We can NOT assume homoscedasticity
var.test(table$`%13C`~table$drying_temperature) #We can assume homoscedasticity

##### Comparison tests ##### 

t.test(table$`%N`~table$drying_temperature) # There is a significant difference
t.test(table$`%C`~table$drying_temperature) # There is NO significant difference
wilcox.test(table$`%15N`~table$drying_temperature) # There is a significant difference
t.test(table$`%13C`~table$drying_temperature) # There is NO significant difference

##### Visual exploration ##### 

boxplot(table$`%N`~table$drying_temperature) #We can assume homoscedasticity
boxplot(table$`%C`~table$drying_temperature) #We can assume homoscedasticity
boxplot(table$`%15N`~table$drying_temperature) #We can NOT assume homoscedasticity
boxplot(table$`%13C`~table$drying_temperature) #We can assume homoscedasticity








