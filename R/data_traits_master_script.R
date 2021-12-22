################################ NETSTO PROJECT ################################

# This script imports various trait, physiology, and
# body chemical composition databases and combines them for each species.

#### Libraries and user defined function sources
library(traitdataform)
library(dplyr)
library(taxize)

source(
  "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/2_data_structuration/functions/data_traits_functions.r"
)


#################### I. TRAITS ####################


########## 1. Mammals ##########

# Primates: We use the Ecological traits of the worlds primates (ETWP) database
data_etwp = read_etwp(path = "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/galan-acedo_primates/data")

# Other mammals: We use the COMBINE database (COalesced Mammal dataBase of INtrinsic and Extrinsic traits)
data_combine = read_combine(path = "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/coalesced_mammal_database_of_intrinsic_and_extrinsic_traits/COMBINE_archives")

# Combine both by including primates from etwp and the other mammals from combine

data_mammals = combining_mammals(data_etwp, data_combine)

########## 2. Birds ##########

# We use the EltonTraits database
pulldata("eltontraits")
data_eltontraits = read_eltontraits(eltontraits = eltontraits)
rm(eltontraits)

pull()
########## 3. Reptiles ##########
# We use the Meiri_2018 database
data_meiri = read_meiri(path= "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/meiri_lizards")


########## 4. Amphibians ##########

# European amphibians: We use the European Amphibians Database (EAD, Trochet 2014)
data_ead = read_ead(path = "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/trochet_european_amphibians")

# Other amphibians: We use the Amphibio database
pulldata("amphibio")
data_amphibio = read_amphibio(amphibio = amphibio)
rm(amphibio)

# Combine both by including european amphibians from ead and the other amphibians from amphibio

########## 5. Arthropods ##########

# We use the Functional Arthropod Traits (FAT) database from Gossner 2016
# pulldata("arthropodtraits") # Sometimes does not work
data_fat = read_fat(path="C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/gossner_arthropods")

# We use the Global Ant database (includes on row per species and per caste)
data_globalant = read_globalant(path="C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/global_ant")

########## 6. Other amniotes ##########
pulldata("amniota")
data_amniota = read_amniota(amniota = amniota)
rm(amniota)

#### Own data
# Additional digestive physiology, diet collected from the literature

#################### II. PHYSIOLOGICAL DATA ####################

# We use the add my pet (AmP) database
data_amp =
  
#################### III. BODY CHEMICAL COMPOSITION ####################

#### Animal body chemical composition
# We use Andrieux 2020 data

data_andrieux = read_andrieux(path="C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/andrieux_body_composition/data")
 

#################### IV. POPULATION DENSITY ####################

# We use TetraDENSITY database
# Warning some of the data are already present in other databases up in the script

data_tetradensity = read_tetradensity(path = "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/tetradensity/data")
