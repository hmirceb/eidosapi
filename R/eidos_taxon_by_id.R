
#' Get taxonomic information from EIDOS
#'
#' Retrieves the taxonomic information for a given taxon identifier from the EIDOS database.
#' Identifiers can be retrieved using the
#' eidos_fuzzy_names() or eidos_taxon_by_name() functions.
#'
#' @param taxon_id An integer. A valid taxon identifier from EIDOS
#'
#' @returns A data frame.
#' @export
#'
#' @examples
#'eidos_taxon_by_id(taxon_id = 1)
eidos_taxon_by_id <- function(taxon_id){

  ## Make sure ID is numeric ##
  taxon_id = as.numeric(taxon_id)

  ## Check if ID is correct (only numbers allowed) ##
  if(sum(is.na(taxon_id) != 0)){
    stop("At least one ID includes letters")
  }

  ## Set base URL ##
  base_url = "https://iepnb.gob.es:443/api/especie/rpc/obtenertaxonporid?_idtaxon="

  ## Create URL for API ##
  eidos_url = paste0(base_url, taxon_id)

  ## Query API ##
  eidos_query = lapply(eidos_url,
         jsonlite::fromJSON)

  ## Merge results ##
  eidos_result = do.call("rbind", eidos_query)

  # Rename "taxonid" to "idtaxon" for consistency
  names(eidos_result)[names(eidos_result)=="taxonid"] <- "idtaxon"
  return(eidos_result)
}
