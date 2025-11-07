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
                                    "subsp.", "var.", "subsp",
                                    "e", "du", "di", "des",
                                    "nec", "f", "illeg",
                                    "delle", ""))

  # Join indices
  indices = unique(c(indice1, indice2, indice3, indice4))[-1]

  # Collapse into full name:
  # The [-1] excludes the genus because they always start with capital letter
  if(length(indices) == 0){ # If the name did not meet any of the above criteria it returns 0 and leads to errors
    taxa_names_clean = paste(taxa_split,
          collapse = " ")
  }else{
    taxa_names_clean = paste(taxa_split[-indices][1:3], # The [1:3] ensures that only the first three words corresponding to genus, species, subspecies are used
          collapse = " ")
  }

  # Remove any NAs in strings derived from last step:
  taxa_names_clean = gsub(
    pattern = "NA",
    replacement = "",
    x = taxa_names_clean)

  # Remove any extra whitespace just in case:
  taxa_names_clean = gsub(pattern = "\\s+", replacement = " ", x = taxa_names_clean)
  taxa_names_clean = trimws(taxa_names_clean)

  return(taxa_names_clean)

}

#' Get scientific author from EIDOS
#'
#' @param taxa_auth a vector of species names
#'
#' @returns DF with supplied name and authorship in eidos. Only works for vectors of species names
#'
get_authorities <- function(taxa_auth){

  ## Set API URL ##
  api_url_base = "https://iepnb.gob.es:443/api/especie/rpc/obtenertaxonespornombre?_nombretaxon="

  # Create URLs
  auth_urls <- lapply(taxa_auth,
                      FUN = function(X) {
                        # genus
                        g <- unlist(strsplit(X, " "))[1]
                        # species
                        s <- unlist(strsplit(X, " "))[2]

                        # Create URL from API base url, genus and species. Use %20 as separator
                        url <- paste0(
                          api_url_base,
                          g, "%20",
                          s
                        )

                        # Dataframe with supplied data and URL for each taxon
                        # Suppress harmless warning
                        df <- suppressWarnings(
                          data.frame(genus = g,
                                     species = s,
                                     url = url)
                        )
                      }
  )
  auth_urls_df <- do.call("rbind", auth_urls)

  ## Query the EIDOS API ##
  auth_result_temp <- apply(auth_urls_df,
                            1,
                            function(X){
                              # DF with supplied information
                              supplied_data <- data.frame(supplied_genus = X[1],
                                                          supplied_species = X[2])

                              # Query EIDOS API
                              eidos_data <- jsonlite::fromJSON(txt = X[3])

                              # If the URL is not correct, EIDOS returns an empty DF
                              # Add error message
                              if (is.null(dim(eidos_data))) {
                                eidos_data <- data.frame(error = "No matches found")
                              }
                              # Join supplied data and EIDOS data
                              suppressWarnings(cbind(supplied_data, eidos_data))

                            })

  # Get taxa with no matches in EIDOS (DFs with only 5 columns):
  no_matches <- which(sapply(auth_result_temp, ncol) == 5)

  # Remove those from list
  if (length(no_matches) == 0) {
    auth_result <- do.call("rbind", auth_result_temp)
  }else{
    auth_result <- do.call("rbind", auth_result_temp[-no_matches])
  }

  # Keep only species-level records
  auth_result <- auth_result[auth_result$taxonrank == "Species",]

  # Keep only exact matches
  auth_result <- auth_result[auth_result$supplied_genus == auth_result$genus &
                               auth_result$supplied_species == auth_result$specificepithet,]

  # Select columns
  auth_result <- auth_result[c("supplied_genus", "supplied_species",
                               "nameid", "scientificnameauthorship")]

  # Create final name
  auth_result$name <- paste(auth_result$supplied_genus,
                            auth_result$supplied_species,
                            sep = " ")

  # Final df
  auth_result <- auth_result[c("name",
                               "nameid", "scientificnameauthorship")]
  return(auth_result)
}
