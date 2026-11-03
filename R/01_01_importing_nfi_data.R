# ---------------------------------------------------------------------------- #
############## *-------------------------------------------------* #############
################### Import and preparation of the NFI data #####################
############## *-------------------------------------------------* #############
# ---------------------------------------------------------------------------- #

# This script contains the code to import and clean the National Forest Inventory (NFI) data
# used in this study.





################# ******************************************** #################
# --------------------------------- #
##### 1. Importing raw NFI data #####
# --------------------------------- #
##### __________________________ #####
# ---------------------------------------------------------------------------- #
##### .. 1.1. Plot dataset ---------------------------------------------------
# ---------------------------------------------------------------------------- #

# nfi_plots <- readr::read_csv2(file = here::here(... = "data/ifn_placettes.csv"), 
#                               col_names = TRUE, na = "NA",
#                               col_types = readr::cols(CAMPAGNE = readr::col_factor(),
#                                                 IDP = readr::col_factor(),
#                                                 DATEPOINT = readr::col_factor(),
#                                                 VISITE = readr::col_factor(),
#                                                 RAD13 = readr::col_factor(),
#                                                 DEP = readr::col_factor(),
#                                                 GRECO = readr::col_factor(),
#                                                 SER = readr::col_factor(),
#                                                 NINCID = readr::col_factor(),
#                                                 INCID = readr::col_factor(
#                                                   ordered = TRUE,
#                                                   levels = c("0", "1", "2", "3" , "4"),
#                                                   include_na = TRUE),
#                                                 GEST = readr::col_factor()))
# summary(nfi_plots)

# FAIRE METHODE DE PATRICK§§§§§
# FAIRE METHODE DE PATRICK§§§§§
# FAIRE METHODE DE PATRICK§§§§§
# FAIRE METHODE DE PATRICK§§§§§
# FAIRE METHODE DE PATRICK§§§§§





##### __________________________ #####
# ---------------------------------------------------------------------------- #
##### .. 1.2. Floristic dataset ----------------------------------------------
# ---------------------------------------------------------------------------- #




###### TODOLIST§§§ ####
###### TODOLIST§§§ ####
###### TODOLIST§§§ ####
# (1) On garde d'abord les PLACETTE des années 2020-2025, et critères de sélection déjà validés.
# (2) On charge les FLORE et on ne garde que les relevés correspondant aux placettes de (1).
# (3) On associe les noms validés TAXREF à chaque taxon de sous-bois.
# (4) On associe les noms validés EURO+MED PLANTBASE à chaque taxon de sous-bois.
# (5) On extrait les EIVE pour chaque taxon.
# (6) On extrait les valeurs CLIMPLANT pour chaque taxon. 
# (XXX) On voit plus tard pour :
#     (a) Les colonnes à garder et autres critères de sélection.
#     (b) Les autres tables.
# (XXX) On répète les étapes précédentes pour toutes les espèces (sous-bois + arbres) pour
#       créer une BDD de bioindication pour TOUTES les espèces de l'IFN (quelle que soit 
#       l'année).