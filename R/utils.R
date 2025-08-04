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

  # Split by whitespaces
  taxa_split = unlist(strsplit(taxa_names, split = " "))

  # Detect "subsp.", "var." and non letter characters associated with
  # authorities except "-"
  indices = which(grepl("^[[:upper:]]+|[^a-zA-Z-]", taxa_split)) # Check which words in the full name start with capital letter or have non-letter characters other than "-"

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
  taxa_names_clean = gsub(pattern = "  ", replacement = "", x = taxa_names_clean)
  taxa_names_clean = trimws(taxa_names_clean)

  return(taxa_names_clean)

}

