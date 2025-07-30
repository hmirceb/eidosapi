#' get_taxon_by_id
#'
#' @param taxon_id A valid ID number from EIDOS
#'
#' @returns A data frame with the taxonomic information for all taxa related to that ID
#' @export
#'
#' @examples
#'
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
  do.call("rbind", eidos_query)
}
