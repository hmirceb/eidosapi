#' Fuzzy matching with the Spanish checklist of Wildlife Species
#'
#' Matches a data frame or vector with species names to any names in the Spanish checklist of Wildlife Species (*Lista Patrón de Especies Silvestres*). Uses fuzzy matching to allow spelling errors. In addition, you can filter by higher taxonomic levels to refine the match.
#'
#' @param taxa_list A data frame or vector with taxonomic information to match with the Checklist. Must include at least the genus
#'  and species columns. Subspecies is optional. Additional taxonomic levels from
#'  kingdom to family can be supplied to filter any possible conflicts. These can be provided as columns with
#'  the corresponding name in the data frame or as function arguments if the supplied
#'  data was a vector of names.
#'  Taxonomic authorities are not supported yet.
#' @param checklist A data frame. The result of running the eidos_clean_checklist() function.
#' @param maxdist A number. Maximum dissimilarity distance between taxa names to match.
#' @param method A string. Method to calculate the distance between names inherited from fuzzyjoin::stringdist_join. One of "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw" or "soundex".
#' @param mode A string. Type of join, one of "inner", "left", "right", "full", "semi" or "anti" inherited from fuzzyjoin::stringdist_join.
#' @param distance_col A string. Name of the column to display the dissimilarity distance between matched names. Set NULL to omit the column.
#' @param kingdom A vector of strings of length = taxa_list with the kingdom of each supplied taxon.
#' @param phylum A vector of strings of length = taxa_list with the phylum of each supplied taxon.
#' @param class A vector of strings of length = taxa_list with the class of each supplied taxon.
#' @param order A vector of strings of length = taxa_list with the order of each supplied taxon.
#' @param family A vector of strings of length = taxa_list with the family of each supplied taxon.
#' @returns A data frame
#' @export
#'
#' @examples
#' checklist = eidos_clean_checklist()
#' taxa_vector = c("Bordere chouardii", "Alts cisternasii")
#' matched_names = eidos_fuzzy_names(taxa_list = taxa_vector, checklist = checklist)
#'
#' # Some names have conflicts when using fuzzy matching.
#' # This returns two matches for two different genera,
#' # Lanius (our species of interest, a bird) and Lasius (an ant).
#' taxa_df = data.frame(genus = "Lanius", species = "meridionalis")
#' matched_names = eidos_fuzzy_names(taxa_list = taxa_df, checklist = checklist)
#'
#' # We can refine the search by including higher taxonomic levels:
#' taxa_df = data.frame(class = "Aves", genus = "Lanius", species = "meridionalis")
#' refined_matched_names = eidos_fuzzy_names(taxa_list = taxa_df, checklist = checklist)
#'
#' # Or using a vector instead
#' taxa_vector = c("Lanius meridionalis")
#' class_vector = c("Aves")
#' refined_matched_names = eidos_fuzzy_names(taxa_list = taxa_vector, checklist = checklist, class = class_vector)
#'
eidos_fuzzy_names <- function(taxa_list,
                              checklist,
                              maxdist = 2,
                              method = "osa",
                              mode = "inner",
                              distance_col = "dist",
                              kingdom = NULL,
                              phylum = NULL,
                              class = NULL,
                              order = NULL,
                              family = NULL){

  ## Check if checklist is in environment
  if(missing(checklist)){
    stop("Checklist missing.
         Please run eidos_clean_checklist() and
         include result in argument checklist")
  }

### Fuzzy match the provided name ###

  ## Prepare input data:
  ## If a vector was supplied, generate appropiate data frame
  if(is.vector(taxa_list)){

    # Clean names (remove "subsp." and authorities)
    taxa_list = sapply(taxa_list, eidosapi:::eidos_clean_names)

    # Split vector and extract genus, species and subspecies:
    taxa_split = strsplit(x = taxa_list, split = " ")
    genera = sapply(taxa_split, FUN = function(x){x[1]})
    species = sapply(taxa_split, FUN = function(x){x[2]})
    subspecies = sapply(taxa_split, FUN = function(x){x[3]})

    # Tabulate higher taxonomic levels if supplied
    # If not supplied returns NULL
    higher_taxo = cbind(kingdom, phylum, class, order, family)

    # Generate data frame
    taxa_list = data.frame(genus = genera,
                            species = species,
                            subspecies = subspecies)

    # Join table of higher levels with supplied data.
    # If higher_taxo is NULL, returns taxa_list
    if(!is.null(higher_taxo)){
      taxa_list = cbind(higher_taxo, taxa_list)
    }

    # Generate clean name to match
    taxa_list$taxon = gsub(pattern = " NA", replacement = "",
                                  x = paste(sep = " ",
                                            taxa_list$genus,
                                            taxa_list$species,
                                            taxa_list$subspecies))
    rm(genera, species, subspecies)
  }

  ## Check if genus data is ok: ##
  if(sum(is.na(taxa_list$genus)) > 0){
    stop("Missing genus data")
  }

  ## Check if species data is ok: ##
  if(sum(is.na(taxa_list$species)) > 0){
    stop("Missing species data")
  }

  # Create supplied_taxon column:
  taxa_list$taxon = gsub(pattern = " NA", replacement = "",
                         x = paste(sep = " ",
                                   taxa_list$genus,
                                   taxa_list$species,
                                   taxa_list$subspecies))

  # Change column names to avoid conflicts in join
  names(taxa_list) = paste0("supplied_", names(taxa_list))

  # Join
  eidos_checklist_join = fuzzyjoin::stringdist_join(x = taxa_list,
                        y = checklist,
                        by = c("supplied_taxon" = "name_clean"),
                        max_dist = maxdist,
                        method = method,
                        mode = mode,
                        distance_col = distance_col)

  ## If any higher taxonomic information is supplied (family, order...) ##
  ## Filter the data frame to keep those supplied taxa that match the
  ## higher taxonomic levels in the accepted name of the Checklist

  ### NOTE: This only works for data frames, not vector lists ###
  # Initialize logical vector to keep all rows by default
  keep <- rep(TRUE, nrow(eidos_checklist_join))

  # Apply condition only if a particular taxonomic level was supplied for
  # at least one taxa of interest

  # The "keep &" part of the conditions update the existing logical vector
  # to keep only the rows that continue to meet all previous conditions
  # as well as the current condition to avoid overwriting

  # Kingdom
  if("supplied_kingdom" %in% names(eidos_checklist_join)) {
    keep <- keep &
      (is.na(eidos_checklist_join$supplied_kingdom) |
         eidos_checklist_join$supplied_kingdom == eidos_checklist_join$kingdom)
  }

  # Phylum
  if("supplied_phylum" %in% names(eidos_checklist_join)) {
    keep <- keep & (is.na(eidos_checklist_join$supplied_phylum) |
                      eidos_checklist_join$supplied_phylum == eidos_checklist_join$phylum)
  }

  # Class
  if("supplied_class" %in% names(eidos_checklist_join)) {
    keep <- keep & (is.na(eidos_checklist_join$supplied_class) |
                      eidos_checklist_join$supplied_class == eidos_checklist_join$class)
  }

  # Order
  if("supplied_order" %in% names(eidos_checklist_join)) {
    keep <- keep & (is.na(eidos_checklist_join$supplied_order) |
                      eidos_checklist_join$supplied_order == eidos_checklist_join$order)
  }

  # Family
  if("supplied_family" %in% names(eidos_checklist_join)) {
    keep <- keep & (is.na(eidos_checklist_join$supplied_family) |
                      eidos_checklist_join$supplied_family == eidos_checklist_join$family)
  }

  # Filter and keep only those rows that match any of the supplied taxonomic
  # information
  filtered_eidos_checklist_join <- eidos_checklist_join[keep, ]

  # Substitute "" for NA
  filtered_eidos_checklist_join[filtered_eidos_checklist_join == ""] <- NA

  # Remove duplicates:
  filtered_eidos_checklist_join[!duplicated(filtered_eidos_checklist_join), ]

  # Remove any wierd whitespaces from table
  filtered_eidos_checklist_join = as.data.frame(
    lapply(filtered_eidos_checklist_join, eidosapi:::eidos_clean_whitespaces),
    check.names = FALSE
  )

  # Return
  return(filtered_eidos_checklist_join)
}
