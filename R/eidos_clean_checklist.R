#' Get the Spanish checklist with synonyms
#'
#' @returns A data frame with the Spanish checklist with synonyms in a long format ready to use in other functions
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

  # Split taxon by space " "
  checklist_split = strsplit(checklist$taxon, split = " ")

  # Remove taxonomic authority and other extra words such as "subsp." or "var."
  checklist$taxon_clean = sapply(checklist_split, function(x){
    indices = which(grepl("^[[:upper:]]+|[^a-zA-Z-]", x)) # Check which words in the full name start with capital letter or have non-letter characters other than "-"

    # Collapse into name:
    # The [-1] excludes the genus because they always start with capital letter
    if(length(indices[-1]) == 0){ # If the name did not meet any of the above criteria it returns 0 and leads to errors
      paste(x,
            collapse = " ")
    }else{
      paste(x[-indices[-1]],
            collapse = " ")
    }
  })

  # With above information, generate full name excluding any possible leftovers
  checklist$taxon_clean = ifelse(checklist$taxonRank == "Species",
                                 sapply(strsplit(checklist$taxon_clean, split = " "), function(x){paste(x[1], x[2], sep = " ")}),
                                 sapply(strsplit(checklist$taxon_clean, split = " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))

  return(checklist)
}
