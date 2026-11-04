##### Functions to load the NFI 2025 metadata #####

load.metadata_2025 <- function(input.directory, zip.file){ # Loads the metadata (stored in "zip.file" 
  # in the "input.directory").
  
  definition_data <- data.table::as.data.table(readr::read_delim(
    unz(paste0(input.directory, "/", zip.file), "metadonnees.csv"), 
    delim = ";", 
    escape_double = FALSE, # No double quotation marks.
    trim_ws = TRUE, # Leading and trailing white spaces (or tabs) are removed.
    skip = 17, # Skips the 17 first lines (= comments).
    n_max = 180-18)) # Because n_max already accounts for the 'skip' argument.
  names(definition_data) <- c("code", "libelle", "definition")

  definition_units <- data.table::as.data.table(readr::read_delim(
    unz(paste0(input.directory, "/", zip.file), "metadonnees.csv"), 
    delim = ";", escape_double = FALSE, trim_ws = TRUE, 
    skip = 183, n_max = 423-184))
  names(definition_units)<-c("donnee","unite","campagnes","type","libelle","definition")

  definition_modalities <- data.table::as.data.table(readr::read_delim(
    unz(paste0(input.directory, "/", zip.file), "metadonnees.csv"), 
    delim = ";", escape_double = FALSE, trim_ws = TRUE, 
    skip = 426)) # No more false header problem (cf. older versions?).
  names(definition_modalities)<-c("unite","code","libelle","definition")

  return(list(
    variable = definition_data,
    units = definition_units,
    modalities = definition_modalities
  ))
  
}


get.metadata_2025 <- function(name, 
                              data = metadata_2025, # This object is created by the 1st function (cf.
                              # below).
                              variable=T, units=T, modalities=T, exact.name=T){ # To explore metadata.
  name <- toupper(name) # To convert names to UPPERCASES.
  
  output <- vector(mode = "list", length = 3)
  names(output) <- c("variable", "units", "modalities") # Creates a vector of 3 lists.
  
  if(exact.name==F){
    output[["variable"]] <- data$variable[grep(pattern = name, x = code)]
  }else if(exact.name==T){
    output[["variable"]] <- data$variable[name == code]
  } # If the given name does not match exactly a NFI variable name (code), then nothing is returned
  # unless if 'exact.name == FALSE'. In the latter case, all partial matches in code are returned.
  
  output[["units"]] <- data$units[donnee %in% output[["variable"]]$code,] # Returns the matching units.
  output[["modalities"]] <- data$modalities[unite %in% output[["units"]]$unite,
                                            .(unite,code,libelle, definition)] # Returns the matching
  # modalities.
  
  # Print:
  if(variable==T){
    print("variables")
    print(output$variable)
  }
  if(units==T){
    print("units")
    print(output$units)
  }
  if(modalities==T){
    print("modalities")
    print(output$modalities)
  }
  
  # Return:
  return(output)
}


metadata_2025 <- load.metadata_2025(here::here("data/"), "export_dataifn_2005_2025.zip")
# metadata_2025 <- load.metadata_2025("C:/Users/pvallet/Documents/MyDrive/Generique/DonneesIFN/Donnees2005_2024/", "export_dataifn_2005_2024.zip")

rm(load.metadata_2025)

# TEST
# get.metadata_2025("idp")
# get.metadata_2025("VEGET")
# get.metadata_2025("ACCI")

