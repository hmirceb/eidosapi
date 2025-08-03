#' Retrieve one of the tables related to the Spanish checklist
#'
#' @param table Name of the table to query
#'
#' @returns The desired table from the EIDOS API
#' @export
#'
#' @examples
#' eidos_tables(eidos_table = "listapatronespecie")
eidos_tables <- function(eidos_table = "listapatronespecie"){

  # Clean names just in case:
  eidos_table = gsub(pattern = " ",
                     replacement = "_",
                     x = tolower(
                       x = eidos_table
                       )
                     )

  if(!eidos_table %in% c("comunidades_autonomas",
                         "listapatronespecie_codigos",
                         "listapatronespecie",
                         "componente_tema",
                         "regbiogeograf_termar",
                         "listapatronespecie_sinonimos",
                         "pais",
                         "norma",
                         "provincias",
                         "listapatronespecie_normas")
  ){
    stop('The desired table is not available. Choose one of c("comunidades_autonomas",
                         "listapatronespecie_codigos",
                         "listapatronespecie",
                         "componente_tema",
                         "regbiogeograf_termar",
                         "listapatronespecie_sinonimos",
                         "pais",
                         "norma",
                         "provincias",
                         "listapatronespecie_normas")')
  }

  # Set base URL
  base_url = "https://iepnb.gob.es/api/catalogo/"

  # Create URL to desired table:
  table_url = paste0(base_url,
                     "v_", # for some reason all of them have this
                     eidos_table)

  ## Query the API ##
  api_table = jsonlite::fromJSON(table_url)

  # Return the table
  return(api_table)
}


#' Fuzzy matching with the Spanish checklist
#'
#' @param taxa_list A data frame or vector with taxonomic information
#' @param maxdist A number. Maximum distance between taxa names to match
#' @param method A string. Method to calculate the distance between names inherited from fuzzyjoin::stringdist_join. One of "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw" or "soundex"
#' @param mode A string. Type of join, one of "inner", "left", "right", "full", "semi" or "anti"
#' @param distance_col A string. Name of the column to display the dissimilarity distance between matches strings. Set NULL to omit the column
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' eidos_fuzzy_names(taxa_list = c("Bordere chouardii", "Alts cisternasii"))
eidos_fuzzy_names <- function(taxa_list,
                              checklist,
                              maxdist = 2,
                              method = "osa",
                              mode = "inner",
                              distance_col = "dist"){

  ## Check if checklist is in environment and download it if not
  if(missing(checklist)){
    stop("Checklist missing. Please run eidos_clean_checklist()")
  }

### Fuzzy match the provided name ###

  ## Prepare inoput data:
  ## If supplied list is a vector, generate appropiate data frame
  if(is.vector(taxa_list)){
    # Remove any possible "subsp."
    taxa_list = gsub(pattern = " subsp.", replacement = "", x = taxa_list)

    # Remove underscores if any
    taxa_list = gsub(pattern = "_", replacement = " ", x = taxa_list)

    # Split vector and extract genus, species and subspecies:
    taxa_split = strsplit(x = taxa_list, split = " ")
    genera = sapply(taxa_split, FUN = function(x){x[1]})
    species = sapply(taxa_split, FUN = function(x){x[2]})
    subspecies = sapply(taxa_split, FUN = function(x){x[3]})

    # Generate data frame
    taxa_list = data.frame(genus = genera,
                            species = species,
                            subspecies = subspecies)

    # Generate clean name to match
    taxa_list$taxon = gsub(pattern = " NA", replacement = "",
                                  x = paste(sep = " ",
                                            taxa_list$genus,
                                            taxa_list$species,
                                            taxa_list$subspecies))
    rm(genera, species, subspecies)
  }

  # Change column names to avoid conflicts in join
  names(taxa_list) = paste0("supplied_", names(taxa_list))

  ## Check if genus data is ok: ##
  if(sum(is.na(taxa_list$genus)) > 0){
    stop("Missing genus data")
  }

  ## Check if species data is ok: ##
  if(sum(is.na(taxa_list$species)) > 0){
    stop("Missing species data")
  }

  # Join
  eidos_checklist_join = fuzzyjoin::stringdist_join(x = taxa_list,
                        y = eidos_checklist,
                        by = c("supplied_taxon" = "taxon_clean"),
                        max_dist = maxdist,
                        method = method,
                        mode = mode,
                        distance_col = distance_col)

  # Return
  return(eidos_checklist_join)
}

aa <- eidos_fuzzy_names(taxa_list = "Borde chouardi")
