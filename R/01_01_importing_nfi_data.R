# ---------------------------------------------------------------------------- #
############## *-------------------------------------------------* #############
################### Import and preparation of the NFI data #####################
############## *-------------------------------------------------* #############
# ---------------------------------------------------------------------------- #

# This script contains the code to import and clean the National Forest Inventory (NFI) 
# data used in this study.





################# ******************************************** #################
# --------------------------------- #
##### 1. Importing raw NFI data #####
# --------------------------------- #
##### __________________________ #####
# ---------------------------------------------------------------------------- #
##### .. 1.1. General import -------------------------------------------------
# ---------------------------------------------------------------------------- #

### To easily access NFI metadata_______________________________________________
source(here::here("R/imput_metadata_2025.R"))
get.NFImetadata_2025("def5") # Function to query NFI metadata (it has several arguments, 
# if needed).


### To load NFI datasets________________________________________________________
zip_file <- here::here("data/export_dataifn_2005_2025.zip")

read.NFIdataset <- function (filename, zipfile) { # Custom function to import NFI files 
  # from the ZIP file!
  dt <- tibble::as.tibble(read.table(unz(zipfile, paste0(filename, ".csv")), 
                                             header = TRUE, 
                                             sep = ";", dec = ".", encoding = "UTF-8"))
  dt |> dplyr::select(!X) |> # Remove the X variable (error in the importation).
    dplyr::rename_with(tolower) |> # Change column names to lowercases. 
    dplyr::mutate(dplyr::across(where(is.character), factor)) |> 
    dplyr::mutate(campagne = as.factor(campagne), # These two variables are always present 
                  # in the files, so they can be converted here.
                  idp = as.factor(idp)) -> dt
  return(dt)
}

nfi_plots <- read.NFIdataset("PLACETTE", zip_file)
nfi_flora <- read.NFIdataset("FLORE", zip_file)
# nfi_trees <- read.NFIdataset("ARBRE", zip_file)
# nfi_ecology <- read.NFIdataset("ECOLOGIE", zip_file)
# nfi_covers <- read.NFIdataset("COUVERT", zip_file)





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
summary(nfi_plots2) # It yields 31,357 plots. Some IDP (i.e. IDs) start with numbers that 
# do not match the given campaign year (y). Those are plots that were not wooded 5 years 
# ago but now are wooded, and so they have now been sampled (in year y) but are given the 
# IDP for the first part of the campaign (y-5).



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
##### .. 2.1. Extracting the species thermal niche values --------------------
# ---------------------------------------------------------------------------- #
# ____________________________________________________
### .... 2.1.1. Assigning species names to plants ----
# ____________________________________________________

### ...... 2.1.1.1. Importing and filtering TAXREF ---------------------------
# ============================================================================ #

taxref <- tibble::as.tibble(readr::read_delim(unz(
  description = here::here("data/TAXREFv13.zip"), filename = "TAXREFv13.txt")))
# TaxRef is the official French taxonomic reference database. As NFI data are based on the
# the 13th version, we also use TaxRef v13.
taxref |> dplyr::filter(REGNE == "Plantae" | REGNE == "Fungi") |> 
  dplyr::select(!c(LB_AUTEUR, NOM_COMPLET_HTML, NOM_VERN:URL)) |> 
  dplyr::mutate(dplyr::across(dplyr::everything(), factor)) -> taxref 
summary(taxref)



### ...... 2.1.1.2. Joining species name to floristic observations -----------
# ============================================================================ #

nfi_flora2$valid_name <- taxref$LB_NOM[match(nfi_flora2$cd_ref, taxref$CD_NOM)] # For each 
# species observed in the NFI data (coded with 'cd_ref'), we assign the official short name 
# ('LB_NOM') associated with the unique TaxRef code ('CD_NOM').
nfi_flora2$inpn_group <- taxref$GROUP2_INPN[match(nfi_flora2$cd_ref, taxref$CD_NOM)]
summary(nfi_flora2) # There are ~64000 bryophytes and 91 lichens (Cladonia sp., the reindeer 
# lichen; 'cd_ref == 190873'). 



# _________________________________________________
### .... 2.1.2. Pairing with the EIVE database ----
# _________________________________________________

### ...... 2.1.2.1. Importing and cleaning EIVE data -------------------------
# ============================================================================ #

eive <- readr::read_csv2(file = here::here("data/EIVE_dengler2023.csv"), 
                         col_types = readr::cols(
                           TaxonConcept = readr::col_factor(),
                           UUID = readr::col_factor(),
                           TaxonRank = readr::col_factor(),
                           AccordingTo = readr::col_factor()))
summary(eive)

# The EIVE database doesn't share the same nomenclature than TaxRef for hybrids, so we 
# have to change it a little. More precisely, EIVE uses the multiplication symbol ('×',
# short-cut is alt+0215) with usually no space after (but there are many exceptions), 
# while TaxRef uses the regular 'x' symbol with always a space after. 
eive |> dplyr::filter(grepl(' ×', x = TaxonConcept, ignore.case = T)) -> ttt
nfi_flora2 |> dplyr::filter(grepl(' x', x = valid_name, ignore.case = T)) |> 
  droplevels() -> ttt
as.data.frame(levels(ttt$valid_name)) -> ttt

# So we have to harmonise that:
eive |> dplyr::mutate(TaxonConcept = 
                      stringr::str_replace_all(TaxonConcept, " ×", " x ")) -> eive
rm(ttt)



### ...... 2.1.2.2. Joining datasets NFI and EIVE datasets -------------------
# ============================================================================ #

ttjoin <- dplyr::left_join(x = nfi_flora2, y = eive, 
                              by = dplyr::join_by(valid_name == TaxonConcept), 
                              keep = T,
                              relationship = "many-to-one") # To tell {dplyr} that each 
# row in x matches at most 1 row in y (but x is allowed to have many duplicates).
ttjoin |> dplyr::select(!c("UUID", "EIVEres-M":"EIVEres-L.n")) -> ttjoin # Multiple NAs
# detected.


## Diagnostic 1: how many NAs (missing names and EIVE values):
ttjoin |> dplyr::filter(is.na(TaxonRank)) |> droplevels() -> ttjoin_NA
as.data.frame(levels(ttjoin_NA$valid_name)) -> ttjoin_NAuniq
# # So ~23% of NFI plant records have no match (NA), because of problems involving 837 taxa.
# # The 5 most current taxa involved are Rubus fruticosus, Quercus robur var. robur, 
# # Polytrichum formosum, Eurhynchium striatum, and Thuidium tamariscinum (3 bryophytes).
# # These 5 species represent ~50000 obs. so 29% of the NA.
# 
# ## Diagnostic 2: how many NFI observation with only a family/genus name (monotypes):
# ttjoin_NA |> dplyr::filter(!(grepl(" ", x = valid_name, ignore.case = T))) |> 
#   droplevels() -> ttjoin_MONO
# as.data.frame(levels(ttjoin_MONO$valid_name)) -> ttjoin_MONOuniq
# # So ~28% of the mismatches are linked to mono-taxon naming (e.g. only genus or family).
# # There are 432 single taxa names. Most of these (94%) have low abundance values (1 or 2).
# 
# ## Diagnostic 2: how many NAs remained linked to other reasons (hybrids, synonyms, aggr.,
# #  subspecies, varieties):
# ttjoin_NA |> dplyr::filter(grepl(" ", x = valid_name, ignore.case = T)) |> 
#   droplevels() -> ttjoin_POLY
# as.data.frame(levels(ttjoin_POLY$valid_name)) -> ttjoin_POLYuniq
# 
# rm(ttjoin, ttjoin_MONO, ttjoin_MONOuniq, ttjoin_NA, ttjoin_NAuniq, ttjoin_POLY, 
#    ttjoin_POLYuniq, tt)

# #### Donc faire fonction pour IF EIVE == NA, go look dans JOIN (LB_NOM == EIVEname, i.e.
# # avec jointure entre TOUS les LB_NOM (pas juste le validé)).




###### TEST WITH {taxify}§§§§§§ #####
###### TEST WITH {taxify}§§§§§§ #####


taxify::install_backbones(backbones = c("col", "gbif", "euromed", "fungorum")) # To install 
# the {taxify} backbones: Catalogue of Life, GBIF, Euro+Med PlantBase, Species Fungorum Plus.

nfi_flora2$valid_name |> droplevels() |> levels() -> taxref_nfi_names # 2692 taxa.

nfi_taxify <- taxify::taxify(x = taxref_nfi_names, 
                             backbone = c("euromed", "col", "gbif", "fungorum"), # To force 
                             # {taxify} to prioritise the matching with Euro+Med PlantBase.
                             region = "Europe", range = "present")
summary(nfi_taxify)
nfi_taxify_anomalies <- taxify::inspect(nfi_taxify) # Gives 191 anomalies, out of which 25 
# that need reviewing.

## Wrong suggestion: Noccaea montana subsp. montana ==> "Noccaea montana aggr." (EIVE == KEEP).
## Wrong suggestion: Potentilla erecta subsp. erecta ==> "Potentilla erecta aggr." (EIVE == KEEP).
## Wrong suggestion: Ranunculus flammula var. reptans ==> "Ranunculus flammula aggr." (EIVE == KEEP).
## Wrong suggestion: Scrophularia oblongifolia ==> "Scrophularia umbrosa ???" (EIVE unclear).
# Even in the notes, there are some problems: Rumex acetosella subsp. pyrenaicus.
# =======> CHECK ALL!!!!! *********************** STOP au 57 AFINIR§§§§§§
# =======> CHECK ALL!!!!! *********************** STOP au 57 AFINIR§§§§§§
# =======> CHECK ALL!!!!! *********************** STOP au 57 AFINIR§§§§§§
# =======> CHECK ALL!!!!! *********************** STOP au 57 AFINIR§§§§§§
## Wrong suggestion: Astragalus sempervirens subsp. sempervirens ==> "Astragalus sempervirens subsp. sempervirens" (EIVE == KEEP).

# Puis CORRIGER pour faire la VALID_NAME

# 1) Create a VALID_NAME with all unique NFI_flora2 with TAXIFY.
# 2) Add EIVE + ZANNE on the VALID_NAME
# 3) Join with NFI flora.



##### SEARCH CODES #####
eive |> dplyr::filter(grepl("berberis", x = TaxonConcept, ignore.case = T)) |> 
  dplyr::select(TaxonConcept, TaxonRank, AccordingTo, 
                'EIVEres-T', 'EIVEres-T.nw3', 'EIVEres-T.n')

taxref |> dplyr::filter(grepl("Cistus creticus var. eriocephalus", x = LB_NOM, ignore.case = T)) |> 
  dplyr::select(CD_NOM, CD_TAXSUP, CD_SUP, CD_REF, RANG, LB_NOM)
taxref |> dplyr::filter(CD_REF == "145597") |> 
  dplyr::select(CD_NOM, CD_TAXSUP, CD_SUP, CD_REF, RANG, LB_NOM)
  
# taxref |> dplyr::filter( # To search for a string in a given column.
#   grepl('Betula pendula', LB_NOM)) -> tt
# taxref |> dplyr::filter( # To search for a string in any column.
#   dplyr::if_any(.cols = dplyr::everything(), ~ grepl('Bryophytes', .))) -> tt




##### __________________________ #####
# ---------------------------------------------------------------------------- #
##### .. 2.2. COMPUTING STI -------------------------
# ---------------------------------------------------------------------------- #







########### *-----------------------------------------------------* ############
############### *********************************************** ################
###### TODOLIST§§§ ####
# (1) On garde d'abord les PLACETTE des années 2020-2025, et critères de sélection déjà 
#     validés. CHECK
# (2) On charge les FLORE et on ne garde que les relevés correspondant aux placettes 
#     de (1). CHECK
# (3) On associe les noms validés TAXREF à chaque taxon de sous-bois. CHECK
# (4) On associe les noms validés EURO+MED PLANTBASE à chaque taxon de sous-bois.
# (5) On extrait les EIVE pour chaque taxon.
# (6) On extrait les valeurs CLIMPLANT pour chaque taxon. 
# (XXX) On voit plus tard pour :
#     (a) Les colonnes à garder et autres critères de sélection.
#     (b) Les autres tables.
# (XXX) On répète les étapes précédentes pour toutes les espèces (sous-bois + arbres) pour
#       créer une BDD de bioindication pour TOUTES les espèces de l'IFN (quelle que soit 
#       l'année).