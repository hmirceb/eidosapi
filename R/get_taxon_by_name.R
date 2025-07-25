# Retrieve taxon information by name from the EIDOS Taxonomy Web Service
# of the Spanish Inventory of Natural Patrimony and Biodiversity (IEPNB)
#
# The function 'get_taxon_by_name'
# connects to the EIDOS API' using the base URL:
# https://iepnb.gob.es:443/api/especie/rpc/obtenertaxonespornombre?_nombretaxon=
#' @param taxon_list A data.frame with three columns: genus, species, subspecies
#'
#' @returns
#' @export
#'
#' @examples
get_taxon_by_name <- function(taxon_list) {
  require(jsonlite)
  if(sum(is.na(taxon_list)) > 0){
    stop()
  }

  api_url_base <- "https://iepnb.gob.es:443/api/especie/rpc/obtenertaxonespornombre?_nombretaxon="
  taxon_list$eidos_url <- ifelse(is.na(taxon_list$subspecies),
                                 paste0(api_url_base,
                                        taxon_list$genus,
                                        "%20",
                                        taxon_list$species),
                                 paste0(api_url_base,
                                        taxon_list$genus,
                                        "%20",
                                        taxon_list$species,
                                        "%20",
                                        taxon_list$subspecies))

  eidos_result <- do.call("rbind", lapply(taxon_list$eidos_url, fromJSON))
  return(eidos_result)
}
