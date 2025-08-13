
#' Get the Spanish checklist with synonyms
#'
#' @returns A data frame with the Spanish checklist with synonyms in a long format ready to use in other functions in the *eidos_api* package.
#' @export
#'
eidos_clean_checklist <- function(){

  ## Message:
  cat("Downloading checklist and formatting, please wait...")

  ## Get checklist with synonyms ##
  checklist = eidos_tables(eidos_table = "listapatronespecie_sinonimos")

  # Remove unnecessary columns
  checklist = checklist[,-which(startsWith(x = names(checklist),
                                           prefix = "LP "))]

  # Create new column with the taxon name
  checklist$name = ifelse(is.na(checklist$Sinónimo),
                           checklist$ScientificName,
                           checklist$Sinónimo)

  # Remove "subsp." and authorities and any Unicode whitespaces
  checklist$name_clean = sapply(checklist$name, eidos_clean_names)

  # With above information, generate full name excluding any possible leftovers
  checklist$name_clean = ifelse(checklist$taxonRank == "Subspecies" | grepl("subsp.", checklist$name),
                                 sapply(strsplit(checklist$name_clean, split = " "), function(x){paste(x[1], x[2], x[3], sep = " ")}),
                                 sapply(strsplit(checklist$name_clean, split = " "), function(x){paste(x[1], x[2], sep = " ")}))

  # Remove any leftover "NA"
  checklist$name_clean = gsub(
    pattern = "NA",
    replacement = "",
    x = checklist$name_clean)

  # Remove any traces of leading and trailing whitespaces
  checklist$name_clean = trimws(checklist$name_clean)

  # Substitute "" for NA
  checklist[checklist == ""] <- NA

  # The default checklist is kind of a mess in terms of column names
  # and the does not match the other tables in EIDOS
  # Fix that

  # Remove camelCase and capital letters in general in column names
  colnames(checklist) = tolower(colnames(checklist))

  # Remove whitespace in column names
  colnames(checklist) = gsub(pattern = " ",
                             replacement = "_",
                             colnames(checklist))

  # Add 'nametype' column
  checklist$nametype <- ifelse(checklist$name == checklist$scientificname,
                               "Aceptado/válido",
                               "Sinónimo")

  # Rename columns
  names(checklist)[names(checklist) == "scientificname"] <- "acceptedname"
  names(checklist)[names(checklist) == "idnombre"] <- "nameid"
  names(checklist)[names(checklist) == "grupo_taxonómico"] <- "taxonomicgroup"
  names(checklist)[names(checklist) == "origen"] <- "origin"
  names(checklist)[names(checklist) == "taxonremarks"] <- "remarks"

  # Add 'acceptednameid' column
  checklist$acceptednameid <- checklist$idtaxon

  # Add 'acceptedname_clean' column
  checklist$acceptedname_clean <- sapply(checklist$acceptedname, eidos_clean_names)

  # If name si accepted, set idtaxon with that id. If not, use nameid.
  checklist$idtaxon <- ifelse(checklist$nametype == "Aceptado/válido",
                              checklist$idtaxon,
                              checklist$nameid)

  # Drop unnecessary columns
  checklist <- checklist[ , !(names(checklist) %in%
                                c("idnombre_sinónimo",
                                  "sinónimo",
                                  "tipo_sinonimia")) ]

  # Reorder columns
  checklist <- checklist[ , c("idtaxon", "name", "nametype", "taxonomicgroup", "name_clean",
                              "acceptednameid", "nameid", "acceptedname", "acceptedname_clean",
                              "taxonconceptid", "scientificnameid",
                              "nameaccordingto", "namepublishedin", "namepublishedinyear",
                              "kingdom", "family","genus","subgenus",
                              "specificepithet","infraspecificepithet","taxonrank",
                              "withoutautorship","scientificnameauthorship","remarks",
                              "vernacular_name","origin","environment")]

  return(checklist)
}
