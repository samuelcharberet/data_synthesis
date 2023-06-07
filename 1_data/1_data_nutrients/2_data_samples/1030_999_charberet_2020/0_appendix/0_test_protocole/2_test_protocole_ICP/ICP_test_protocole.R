### Does drying conditions and autocleavage has an effect on P content in a feces sample ? ###
library(plyr)
library(goeveg)

setwd("C:/Users/Samuel/Documents/7. Doctorat/3. Data/0. Chemical composition/0. Samples/0_appendix/0_test_protocole/2_test_protocole_ICP")
table= read.csv("ICP_test_protocole.csv", sep=";")
colnames(table) = c("bombe"	,"session",	"echantillon",	
                    "autoclavage",	"temperature_C",	"duree_sechage_h",	
                    "P_g_g")


table$autoclavage = factor(table$autoclavage)
table$bombe = factor(table$bombe)
table$echantillon = factor(table$echantillon)


##### Generalities ##### 
str(table)
hist(table$P_g_g, breaks=30)
shapiro.test(table$P_g_g) #We can assume normality

# All replicates
plot(jitter(table$P_g_g, 1)~table$echantillon, pch = 15)
cvs = tapply(table$P_g_g, table$echantillon,  goeveg::cv)
mean(cvs)

#Only averages
averages<-ddply(table, .(echantillon), summarize, mean=mean(P_g_g))
plot(averages$mean~averages$echantillon, pch = 15)



##### Finding the best model   ##### 
lm_temp=lm(table$P_g_g~table$temperature_C) 
summary(lm_temp) #drying temperature does not have an effect

lm_autoclaving=lm(table$P_g_g~table$autoclavage)
summary(lm_autoclaving) #autoclaving does not have an effect

lm_duree=lm(table$P_g_g~table$duree_sechage_h)
summary(lm_duree) #drying duration does not have an effect

#####  Effet autoclavage ?   #####
plot(jitter(table$P_g_g, 1)~table$autoclavage, pch = 15, xlab="Autoclavage",
     ylab="Phosphorus content (g/g)")
shapiro.test(table[table$autoclavage==0, ]$P_g_g) #normal
shapiro.test(table[table$autoclavage==1, ]$P_g_g) #normal
var.test(table$P_g_g~table$autoclavage) # no significant differences between the two variances
t.test(table$P_g_g~table$autoclavage, var.equal=TRUE) # no significant difference between the two means
# Autoclaving does not have an effect




##### Effet température ?   ##### 
plot(jitter(table$P_g_g, 1)~table$temperature_C, pch = 15, xlab="Temperature (°C)",
     ylab="Phosphorus content (g/g)")




#####  Effet temps de séchage ?   #####
# Only 24 and 48 h
table_2=table[-(which(table$duree_sechage_h==1320, arr.ind = FALSE, useNames = TRUE)), ]
plot(jitter(table_2$P_g_g, 1)~table_2$duree_sechage_h, pch = 15, xlab="Temperature (°C)",
     ylab="Phosphorus content (g/g)")

shapiro.test(table_2[table_2$duree_sechage_h==24, ]$P_g_g) # not normal
shapiro.test(table_2[table_2$duree_sechage_h==48, ]$P_g_g) #normal

var.test(table_2$P_g_g~table_2$duree_sechage_h) # no significant differences between the two variances
wilcox.test(table_2$P_g_g~table_2$duree_sechage_h) # significant difference between 24 and 48
t.test(table_2$P_g_g~table_2$duree_sechage_h, var.equal=TRUE) # significant difference between 24 and 48
# Drying longer seems to increase P content

# Including 1320 h
kruskal.test(P_g_g ~ duree_sechage_h, data = table) # significant differences
pairwise.wilcox.test(table$P_g_g, table$duree_sechage_h)
#No difference between 24 and 1320 hours
