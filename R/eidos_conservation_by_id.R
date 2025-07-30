#' Title
#'
#' @param taxon_id
#'
#' @returns
#' @export
#'
#' @examples
#'
eidos_conservation_by_id <- function(taxon_id, taxo_info = F){
  ## Make sure ID is numeric ##
  taxon_id = as.numeric(taxon_id)

  ## Check if ID is correct (only numbers allowed) ##
  if(is.na(taxon_id)){
    stop("At least one ID includes letters")
  }

  ## Set base URL ##
  base_url = "https://iepnb.gob.es:443/api/especie/rpc/obtenerestadosconservacionportaxonid?_idtaxon="

  ## Create URL for API ##
  eidos_url = paste0(base_url, taxon_id)

  ## Query API ##
  eidos_query = lapply(eidos_url,
                       jsonlite::fromJSON)

  ## Get taxonomic information if appropiate ##
  if(isTRUE(taxo_info)){
    taxonomic_information = eidos_taxon_by_id(taxon_id = taxon_id)
  }

  ## Merge results ##
  do.call("rbind", eidos_query)

}

aa = eidos_conservation_by_id(1)
