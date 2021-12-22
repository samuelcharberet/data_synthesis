# Lucie Serre 
# mis a jour le 08.04.2020

# rearrangement en prevision du merge avec elton

library(readr)
library(rvest)
library(dplyr)

setwd("~/Dropbox/dejections/database/traits/TOCHECK/adw/fichiers_prep")
adw_ready_to_merge <- read_csv("~/Dropbox/dejections/database/traits/TOCHECK/adw/fichiers_prep/complete_adw.csv")

# REGIMES TROPHIQUES

  # carnivore_vertebrate
    adw_ready_to_merge$carnivore_vertebrate <- rep(0,5735)
    adw_ready_to_merge$carnivore_vertebrate <- rowSums(adw_ready_to_merge[,c("carnivore","carnivore_piscivore","carnivore_sanguivore")])
    adw_ready_to_merge$carnivore_vertebrate <- replace(adw_ready_to_merge$carnivore_vertebrate,adw_ready_to_merge$carnivore_vertebrate>1,1)

    adw_ready_to_merge[,c("carnivore","carnivore_piscivore","carnivore_sanguivore")]<-NULL

  # carnivore_invertebrate
    adw_ready_to_merge$carnivore_invertebrate <- rep(0,5735)
    adw_ready_to_merge$carnivore_invertebrate <- rowSums(adw_ready_to_merge[,c("carnivore_insectivore","carnivore_molluscivore","carnivore_eats_other_marine_invertebrates","planktivore")])
    adw_ready_to_merge$carnivore_invertebrate<-replace(adw_ready_to_merge$carnivore_invertebrate,adw_ready_to_merge$carnivore_invertebrate>1,1)

    adw_ready_to_merge[,c("carnivore_insectivore","carnivore_molluscivore","carnivore_eats_other_marine_invertebrates","planktivore")] <- NULL

  # herbivore_plantseed
    adw_ready_to_merge$herbivore_plantseed <- rep(0,5735)
    adw_ready_to_merge$herbivore_plantseed <- rowSums(adw_ready_to_merge[,c("herbivore","herbivore_lignivore","herbivore_algivore")])
    adw_ready_to_merge$herbivore_plantseed <- replace(adw_ready_to_merge$herbivore_plantseed,adw_ready_to_merge$herbivore_plantseed>1,1)

    adw_ready_to_merge[,c("herbivore","herbivore_lignivore","herbivore_algivore")] <- NULL

  # herbivore_fruitnect
    adw_ready_to_merge$herbivore_fruitnect <- adw_ready_to_merge$herbivore_frugivore

    adw_ready_to_merge$herbivore_frugivore <- NULL

  # mycophage, detritivore, coprophage: pas de changement

# REGIME ALIMENTAIRE

# diet_invertebrate
  adw_ready_to_merge$diet_invertebrate <- rep(0,5735)
  adw_ready_to_merge$diet_invertebrate <- rowSums(adw_ready_to_merge[,c("insects","terrestrial_non_insect_arthropods",
                                                                        "mollusks","terrestrial_worms","aquatic_or_marine_worms",
                                                                        "aquatic_crustaceans","echinoderms","cnidarians",
                                                                        "other_marine_invertebrates","zooplankton")])
  adw_ready_to_merge$diet_invertebrate<-replace(adw_ready_to_merge$diet_invertebrate,adw_ready_to_merge$diet_invertebrate>1,1)

  adw_ready_to_merge[,c("insects","terrestrial_non_insect_arthropods","mollusks","terrestrial_worms","aquatic_or_marine_worms",
                        "aquatic_crustaceans","echinoderms","cnidarians","other_marine_invertebrates","zooplankton")] <- NULL

  # diet_vertebrate_endo
    adw_ready_to_merge$diet_vertebrate_endo <- rep(0,5735)
    adw_ready_to_merge$diet_vertebrate_endo <- rowSums(adw_ready_to_merge[,c("birds","mammals")])
    adw_ready_to_merge$diet_vertebrate_endo <- replace(adw_ready_to_merge$diet_vertebrate_endo,adw_ready_to_merge$diet_vertebrate_endo>1,1)

    adw_ready_to_merge[,c("birds","mammals")] <- NULL

  # diet_vertebrate_ecto
    adw_ready_to_merge$diet_vertebrate_ecto <- rep(0,5735)
    adw_ready_to_merge$diet_vertebrate_ecto <- rowSums(adw_ready_to_merge[,c("amphibians","reptiles")])
    adw_ready_to_merge$diet_vertebrate_ecto <- replace(adw_ready_to_merge$diet_vertebrate_ecto,adw_ready_to_merge$diet_vertebrate_ecto>1,1)

    adw_ready_to_merge[,c("amphibians","reptiles")] <- NULL

  # diet_vertebrate_fish
    adw_ready_to_merge$diet_vertebrate_fish <- adw_ready_to_merge$fish

    adw_ready_to_merge$fish <- NULL

  # diet_vertebrate_unk
    adw_ready_to_merge$diet_vertebrate_unk <- rep(0,5735)

  #diet_eggs
    adw_ready_to_merge$diet_eggs <- adw_ready_to_merge$eggs

    adw_ready_to_merge$eggs <- NULL

  # diet_scav
    adw_ready_to_merge$diet_scav <- rep(0,5735)
    adw_ready_to_merge$diet_scav <- rowSums(adw_ready_to_merge[,c("blood","body_fluids","carrion")])
    adw_ready_to_merge$diet_scav <- replace(adw_ready_to_merge$diet_scav,adw_ready_to_merge$diet_scav>1,1)

    adw_ready_to_merge[,c("blood","body_fluids","carrion")] <- NULL


  # diet_plant
    adw_ready_to_merge$diet_plant <- rep(0,5735)
    adw_ready_to_merge$diet_plant <- rowSums(adw_ready_to_merge[,c("leaves","roots_and_tubers","wood_bark_or_stems",
                                                                   "sap_or_other_plant_fluids","bryophytes","lichens",
                                                                   "algae","macroalgae","phytoplankton","fungus")])
    adw_ready_to_merge$diet_plant <- replace(adw_ready_to_merge$diet_plant,adw_ready_to_merge$diet_plant>1,1)

    adw_ready_to_merge[,c("leaves","roots_and_tubers","wood_bark_or_stems","sap_or_other_plant_fluids","bryophytes",
                          "lichens","algae","macroalgae","phytoplankton","fungus")] <- NULL

  # diet_seed
    adw_ready_to_merge$diet_seed <- adw_ready_to_merge$seeds_grains_and_nuts

    adw_ready_to_merge$seeds_grains_and_nuts <- NULL

  # diet_fruit
    adw_ready_to_merge$diet_fruit <- adw_ready_to_merge$fruit
    adw_ready_to_merge$fruit <- NULL

  # diet_nect
    adw_ready_to_merge$diet_nect <- rep(0,5735)
    adw_ready_to_merge$diet_nect <- rowSums(adw_ready_to_merge[,c("nectar","pollen","flowers")])
    adw_ready_to_merge$diet_nect <- replace(adw_ready_to_merge$diet_nect,adw_ready_to_merge$diet_nect>1,1)

    adw_ready_to_merge[,c("nectar","pollen","flowers")] <- NULL

  # diet_detritus
    adw_ready_to_merge$diet_detritus <- adw_ready_to_merge$detritus

    adw_ready_to_merge$detritus <- NULL

  # diet_dung
    adw_ready_to_merge$diet_dung <- adw_ready_to_merge$dung

    adw_ready_to_merge$dung <- NULL

  # diet_microbes
    adw_ready_to_merge$diet_microbes <- adw_ready_to_merge$microbes

    adw_ready_to_merge$microbes <- NULL

# PHYSICAL FEATURES

  colnames(adw_ready_to_merge) <- gsub("mass_average_kg", "bodymass_kg", colnames(adw_ready_to_merge))
  colnames(adw_ready_to_merge) <- gsub("length_average_m", "bodylength_m", colnames(adw_ready_to_merge))

#changement de l'odre des colonnes
  adw_ready_to_merge <- adw_ready_to_merge[c(1,5,4,2,3,6,46:63,7:45)]

# I/O --
  write.table(adw_ready_to_merge, "adw_ready_to_merge.csv",sep=",",row.names=FALSE)

