
#' Get conservation information from EIDOS
#'
#' Get the conservation information from IEPNB (*Inventario Español del Patrimonio Natural y de la Biodiversidad*) for a given taxon using its unique identifier
#' from the IEPNB database. Identifiers can be retrieved using the
#' eidos_fuzzy_names() or eidos_taxon_by_name() functions. By default de API does not return taxonomic information.
#' If needed, the function retrieves that information using eidos_taxon_by_id().
#'
#' @param taxon_id An integer. A unique taxon identifier from IEPNB
#' @param taxo_info Boolean. Return the taxonomic information of the taxon.
#' @param latest Boolean. Return the information from only the latest assessment.
#'
#' @returns A data frame with the conservation information of each taxa.
#' @export
#'
#' @examples
#' eidos_conservation_by_id(taxon_id = 1, taxon_info = F, latest = F)
#' eidos_conservation_by_id(taxon_id = 1, taxon_info = T, latest = F)
eidos_conservation_by_id <- function(taxon_id,
                                     taxo_info = F,
                                     latest = F){
  ## Make sure ID is numeric ##
  taxon_id = as.numeric(taxon_id)

  ## Check if ID is correct (only numbers allowed) ##
  if(sum(is.na(taxon_id) != 0)){
    stop("At least one ID includes letters")
  }

  ## Set base URL ##
  base_url = "https://iepnb.gob.es:443/api/especie/rpc/obtenerestadosconservacionportaxonid?_idtaxon="

  ## Create URL for API ##
  eidos_url = paste0(base_url, taxon_id)

  ## Query API ##
  eidos_query_list = lapply(eidos_url,
                       jsonlite::fromJSON)

  # If required, return only the latest conservation assessment
  if(isTRUE(latest)){
    eidos_query_list = lapply(eidos_query_list, function(x){
      latest_year = max(x$anio, na.rm = T)
      x[x$anio == latest_year,]
    })
  }

  ## Get taxonomic information if needed ##
  if(isTRUE(taxo_info)){
    # Run the eidos_taxon_by_id() function:
    taxonomic_information = eidos_taxon_by_id(taxon_id = taxon_id)
    taxonomic_information = taxonomic_information[taxonomic_information$nameid == taxonomic_information$acceptednameid,]

    ## Merge results ##
    eidos_query_temp = do.call("rbind", eidos_query_list)
    eidos_query = merge(x = eidos_query_temp,
          y = taxonomic_information,
          by.x = "idtaxon",
          by.y = "nameid")
  }else{

    ## Merge results ##
    eidos_query = do.call("rbind", eidos_query_list)
  }

  ## Return results ##
  return(eidos_query)

}
