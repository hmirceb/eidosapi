
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

  # Create new column with the taxon
  checklist$taxon = ifelse(checklist$Sinónimo == "",
                           checklist$ScientificName,
                           checklist$Sinónimo)

  # Remove "subsp." and authorities
  checklist$taxon_clean = sapply(checklist$taxon, clean_names)

  # With above information, generate full name excluding any possible leftovers
  checklist$taxon_clean = ifelse(checklist$taxonRank == "Species",
                                 sapply(strsplit(checklist$taxon_clean, split = " "), function(x){paste(x[1], x[2], sep = " ")}),
                                 sapply(strsplit(checklist$taxon_clean, split = " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))

  # Remove any traces of NA and leading and trailing whitespaces
  checklist$taxon_clean = trimws(
    x = gsub(
      pattern = "NA",
      replacement = "",
      x = checklist$taxon_clean
      )
    )

  return(checklist)
}
