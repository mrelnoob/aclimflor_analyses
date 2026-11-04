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
##### .. 1.1. General import -------------------------------------------------
# ---------------------------------------------------------------------------- #

source(here::here("R/imput_metadata_2025.R")) # Function to easily read NFI metadata. 
get.metadata_2025("DC") # Test of the function.

zip_file <- here::here("data/export_dataifn_2005_2025.zip")

read_NFI <- function (filename, zipfile) { # Custom function to import NFI files from the ZIP file!
  dt <- tibble::as.tibble(read.table(unz(zipfile, paste0(filename, ".csv")), 
                                             header = TRUE, 
                                             sep = ";", dec = ".", encoding = "UTF-8"))
  dt |> dplyr::select(!X) |> # Remove the X variable (error in the importation).
    dplyr::rename_with(tolower) |> # Change column names to lowercases. 
    dplyr::mutate(dplyr::across(where(is.character), factor)) |> 
    dplyr::mutate(campagne = as.factor(campagne), # These two variables are always present in the
                  # files, so they can be converted here.
                  idp = as.factor(idp)) -> dt
  return(dt)
}

nfi_plots <- read_NFI("PLACETTE", zip_file)
nfi_flora <- read_NFI("FLORE", zip_file)
# nfi_trees <- read_NFI("ARBRE", zip_file)
# nfi_ecology <- read_NFI("ECOLOGIE", zip_file)
# nfi_covers <- read_NFI("COUVERT", zip_file)





##### __________________________ #####
# ---------------------------------------------------------------------------- #
##### .. 1.2. Initial formatting and selections ------------------------------
# ---------------------------------------------------------------------------- #
# ____________________________
### .... 1.1.1. Plot data ----
# ____________________________

### To handle the variables involved in the initial exclusion criteria__________
nfi_plots |> dplyr::mutate(
  datepoint = as.factor(datepoint),
  visite = as.factor(visite),
  datepoint = as.factor(datepoint),
  rad13 = as.factor(rad13),
  nincid = factor(nincid, ordered = TRUE, levels = c("0", "1", "2", "3" , "4", "5")),
  incid = factor(incid, ordered = TRUE, levels = c("0", "1", "2", "3" , "4")),
  gest = factor(gest, ordered = TRUE, levels = c("0", "1", "2")),
  def5 = factor(def5, ordered = TRUE, levels = c("0", "1", "2", "3" , "4", "5"))) |> 
    dplyr::select(!(dist:orniere)) -> nfi_plots 


### Selecting the 2020-2025 campaign only_______________________________________
nfi_plots |> dplyr::filter(campagne %in% c("2021", "2022", "2023", "2024", "2025") &
                             visite == 1) -> nfi_plots2
summary(nfi_plots2) # It yields 31,357 plots. Some IDP (i.e. IDs) start with numbers that do
# not match the given campaign year (y). Those are plots that were not wooded 5 years ago but now
# are wooded, and so they have now been sampled (in year y) but are given the IDP for the first
# part of the campaign (y-5).



# _________________________________
### .... 1.1.2. Floristic data ----
# _________________________________

### To handle the variables involved in the initial exclusion criteria__________
nfi_flora |> dplyr::mutate(
  cd_ref = as.factor(cd_ref),
  abond = factor(abond, ordered = TRUE, levels = c("0", "1", "2", "3" , "4", "5")),
  inco_flor = factor(inco_flor, ordered = TRUE, levels = c("0", "1", "2", "3" , "4", "5"))
  ) -> nfi_flora


### Selecting the 2020-2025 campaign only_______________________________________
nfi_flora |> dplyr::filter(campagne %in% 
                             c("2021", "2022", "2023", "2024", "2025")) -> nfi_flora2
summary(nfi_flora2) # It yields ca. 743,000 plant observations.
length(unique(nfi_flora2$idp)) # Belonging to 31,354 unique plots.





########### *-----------------------------------------------------* ############
############### *********************************************** ################
# ---------------------------------------------------------- #
##### 2. Computing CTI (Commununity Temperature Indices) #####
# ---------------------------------------------------------- #
##### __________________________ #####
# ---------------------------------------------------------------------------- #
##### .. 2.1. Assigning species name -----------------------------------------
# ---------------------------------------------------------------------------- #



# DANS IMPUT METADATA (modif pour enluver le / car j'utilise here::here())



###### TODOLIST§§§ ####
# (1) On garde d'abord les PLACETTE des années 2020-2025, et critères de sélection déjà validés. CHECK
# (2) On charge les FLORE et on ne garde que les relevés correspondant aux placettes de (1). CHECK
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