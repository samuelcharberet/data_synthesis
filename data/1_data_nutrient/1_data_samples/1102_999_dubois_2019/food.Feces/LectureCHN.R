# CHN
setwd("~/Dropbox/dejections/Analyse CHN/data/food.Feces")
setwd("D:\\Dropbox\\en_cours\\recherche\\projets\\dejections\\Analyse CHN\\data\\brute")

CHN_Food_Feces <- list.files(pattern = "feces")
CHN_Food_Food <- list.files(pattern = "food")
CHN_Food_Feces <- lapply(CHN_Food_Feces,read.csv,sep=";",dec=".")
CHN_Food_Food <- lapply(CHN_Food_Food,read.csv,sep=";",dec=".")

# CHN_Food_Feces_df  <- rbind.fill(CHN_Food_Feces) etc

CHN_Feces_Food <- merge(CHN_Food_Feces,CHN_Food_Food,by="Organism")


lapply(CHN_Food_Feces,head)
summary(CHN_Food_Feces)