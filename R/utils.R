#' Clean a vector or species names
#'
#' Remove any "subsp." or similar from name
#'
#' @param taxa_names A vector of species names.
#'
#' @returns A vector of clean species names.
#'
clean_names = function(taxa_names){

  # Remove underscores if any
  taxa_names = gsub(pattern = "_", replacement = " ", x = taxa_names)

  # Substitute Unicode whitespaces with normal whitespaces
  taxa_names = gsub("\xc2\xa0", " ", taxa_names, fixed = TRUE)

  # Remove anything between parentheses
  taxa_names = gsub("\\(.*?\\)", "", taxa_names)

  # Split by whitespaces
  taxa_split = unlist(strsplit(taxa_names, split = " "))

  # Detect "subsp.", "var.", words starting in capital letter and
  # non letter characters associated with
  # authorities except "-"
  indice1 = which(grepl("^[[:upper:]]+|&|[^a-zA-Z-]]", taxa_split))

  # Look for letters with umlaut "ö"
  indice2 = which(grepl("^[[\U00E0-\U017F]]", taxa_split))

  # Look for letters with dot
  indice3 = which(grepl("\\.", taxa_split))

  # Detect common words in names and authorities that
  # might go undetected in previous checks
  indice4 = which(taxa_split %in% c("ex", "et", "in",
                                    "y", "zur", "sensu",
                                    "pro", "parte", "de",
                                    "la", "non", "da", "nud",
                                    "del", "van", "der", "auct",
                                    "den", "and", "-", "degli",
                                    "en", "prensa", "subst",
                                    "var", "species", "unrecognised", "x",
                                    "subsp.", "var.", "subsp"))

  # Join indices
  indices = unique(c(indice1, indice2, indice3, indice4))

  # Collapse into full name:
  # The [-1] excludes the genus because they always start with capital letter
  if(length(indices[-1]) == 0){ # If the name did not meet any of the above criteria it returns 0 and leads to errors
    taxa_names_clean = paste(taxa_split,
          collapse = " ")
  }else{
    taxa_names_clean = paste(taxa_split[-indices[-1]],
          collapse = " ")
  }

  # Remove any extra whitespace just in case:
  taxa_names_clean = gsub(pattern = "\\s+", replacement = " ", x = taxa_names_clean)
  taxa_names_clean = trimws(taxa_names_clean)

  return(taxa_names_clean)

}

