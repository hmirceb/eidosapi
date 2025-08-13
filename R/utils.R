#' Remove whitespaces
#'
#' @param x A vector of strings.
#'
#' @returns A vector of strings with only " " whitespaces
#'
eidos_clean_whitespaces = function(x) {
  # Substitute Unicode whitespaces with normal whitespaces
  x = gsub("\\p{Zs}+", " ", x, perl = TRUE)

  # Remove zero-width spaces
  x = gsub("\u200B", "", x)

  # Remove double whitespaces
  x = gsub("\\s+", " ", x, fixed = TRUE)

  return(x)
}

#' Clean a single species names
#'
#' Removes any "subsp.", "var.", "f.", years and authorship similar from a species name.
#'
#' @param taxa_names A single species names.
#'
#' @returns A single clean species names.
#'
eidos_clean_names = function(taxa_names){

  # Remove underscores if any
  taxa_names = gsub(pattern = "_", replacement = " ", x = taxa_names)

  # Remove UNICODE whitespaces, doble whitespaces and zero-width spaces
  taxa_names = eidos_clean_whitespaces(taxa_names)

  # Remove anything between parentheses
  taxa_names = gsub("\\(.*?\\)", "", taxa_names)

  # Remove anything between brackets
  taxa_names = gsub("\\[.*?\\]", "", taxa_names)

  # Split by whitespaces
  taxa_split = unlist(strsplit(taxa_names, split = " "))

  # Detect "subsp.", "var.", words starting in capital letter and
  # non letter characters associated with
  # authorities except "-"
  indice1 = which(grepl("(^[[:upper:]])|&|[^A-Za-z-]", taxa_split))

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
                                    "del", "von", "van", "der", "auct",
                                    "den", "and", "-", "degli",
                                    "en", "prensa", "subst",
                                    "var", "species", "unrecognised", "x",
                                    "subsp.", "var.", "subsp"))

  # Join indices
  indices = unique(c(indice1, indice2, indice3, indice4))[-1]

  # Collapse into full name:
  # The [-1] excludes the genus because they always start with capital letter
  if(length(indices) == 0){ # If the name did not meet any of the above criteria it returns 0 and leads to errors
    taxa_names_clean = paste(taxa_split,
          collapse = " ")
  }else{
    taxa_names_clean = paste(taxa_split[-indices],
          collapse = " ")
  }

  # Remove any extra whitespace just in case:
  taxa_names_clean = gsub(pattern = "\\s+", replacement = " ", x = taxa_names_clean)
  taxa_names_clean = trimws(taxa_names_clean)

  return(taxa_names_clean)

}
