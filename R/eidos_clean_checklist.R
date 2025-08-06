
#' Get the Spanish checklist with synonyms
#'
#' @returns A data frame with the Spanish checklist with synonyms in a long format ready to use in other functions in the *eidos_api* package.
#' @export
#'
#' @examples
#' eidos_clean_checklist()
eidos_clean_checklist <- function(){

  ## Message:
  cat("Downloading checklist and formatting, please wait...")

  ## Get checklist with synonyms ##
  checklist = eidos_tables(eidos_table = "listapatronespecie_sinonimos")

  # Remove unnecessary columns
  checklist = checklist[,-which(startsWith(x = names(checklist), prefix = "LP "))]

  # Substitute "" for NA
  checklist[checklist == ""] <- NA

  # Create new column with the taxon
  checklist$taxon = ifelse(is.na(checklist$Sinónimo),
                           checklist$ScientificName,
                           checklist$Sinónimo)

  # Remove "subsp." and authorities and any Unicode whitespaces
  checklist$taxon_clean = sapply(checklist$taxon, clean_names)

  # With above information, generate full name excluding any possible leftovers
  checklist$taxon_clean = ifelse(checklist$taxonRank == "Subspecies" | grepl("subsp.", checklist$taxon),
                                 sapply(strsplit(checklist$taxon_clean, split = " "), function(x){paste(x[1], x[2], x[3], sep = " ")}),
                                 sapply(strsplit(checklist$taxon_clean, split = " "), function(x){paste(x[1], x[2], sep = " ")}))

  # Remove any leftover "NA"
  checklist$taxon_clean = gsub(
    pattern = "NA",
    replacement = "",
    x = checklist$taxon_clean)

# Remove any traces of leading and trailing whitespaces
  checklist$taxon_clean = trimws(checklist$taxon_clean)

  return(checklist)
}
