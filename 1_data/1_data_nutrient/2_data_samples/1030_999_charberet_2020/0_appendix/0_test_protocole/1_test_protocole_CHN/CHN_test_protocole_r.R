### Does drying conditions and autocleavage has an effect on C, N  content in a feces sample ? ###

setwd("C:/Users/Samuel/Documents/7. Doctorat/2. Experiments/1. CHN/0_test_protocole")
table= read.csv("20210104_testprotocole_r.csv", sep=";")
colnames(table) = c("numero_tube"	,"traitement",	"autoclavage",	
                    "temperature_C",	"temps_sechage_h",	"teneur_N",	
                    "teneur_C",	"15N",	"13C", 'C/N',	'13C/15N')


table$autoclavage = factor(table$autoclavage)
table$numero_tube = factor(table$numero_tube)
table$temperature_C = numeric(table$temperature_C)
table$temps_sechage_h = numeric(table$temps_sechage_h)


str(table)

#Effet autoclavage ?
plot(jitter(table$teneur_N, 1)~table$autoclavage, pch = 15)
plot(jitter(table$teneur_C, 1)~table$autoclavage, pch = 15, xlab="Autoclavage",
     ylab="Teneur en carbone")
plot(jitter(table$`15N`, 1)~table$autoclavage, pch = 15)
plot(jitter(table$`13C`, 1)~table$autoclavage, pch = 15)
plot(jitter(table$`C/N`, 1)~table$autoclavage, pch = 15)
plot(jitter(table$`13C/15N`, 1)~table$autoclavage, pch = 15)


# Effet température ?


plot(table[table$temps_sechage_h==24, ]$teneur_N~table[table$temps_sechage_h==24, ]$temperature_C)
plot(table[table$temps_sechage_h==48, ]$teneur_N~table[table$temps_sechage_h==48, ]$temperature_C)

plot(jitter(table$teneur_N, 1)~table$temperature_C, pch = 15)
plot(jitter(table$teneur_C, 1)~table$temperature_C, pch = 15)
plot(jitter(table$`15N`, 1)~table$temperature_C, pch = 15, xlab="Température de séchage (C°)",
     ylab="15N")
plot(jitter(table$`13C`, 1)~table$temperature_C, pch = 15)
plot(jitter(table$`C/N`, 1)~table$temperature_C, pch = 15)
plot(jitter(table$`13C/15N`, 1)~table$temperature_C, pch = 15)


# Effet temps de séchage ?
plot(table[table$temperature_C==60, ]$teneur_N~table[table$temperature_C==60, ]$temps_sechage_h)
plot(table[table$temperature_C==80, ]$teneur_N~table[table$temperature_C==80, ]$temps_sechage_h)
plot(table[table$temperature_C==100, ]$teneur_N~table[table$temperature_C==100, ]$temps_sechage_h)


boxplot(table$teneur_N ~ table$temps_sechage_h, data = table, show.point=T)
boxplot(table$teneur_C ~ table$temps_sechage_h, data = table)
boxplot(table$`15N` ~ table$temps_sechage_h, data = table)
boxplot(table$`13C` ~ table$temps_sechage_h, data = table)

