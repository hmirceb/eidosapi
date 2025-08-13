
#' Get taxonomic information from EIDOS
#'
#' Retrieves the taxonomic information for a given taxon identifier from the EIDOS database.
#' Identifiers can be retrieved using the
#' eidos_fuzzy_names() or eidos_taxon_by_name() functions.
#'
#' @param taxon_id An integer or vector of integers. Valid taxon identifiers from EIDOS.
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
                            function(x){
                              # Query EIDOS API
                              a = jsonlite::fromJSON(txt = x)

                              # If the URL is valid but there is no info, return NULL
                              if(is.null(unlist(a))){
                                return(NULL)
                              }else{
                                return(a)
                              }
                            }
  )

  # Remove NULLs
  eidos_query = eidos_query[!sapply(eidos_query, is.null)]

  ## Merge results ##
  eidos_result = do.call("rbind", eidos_query)

  ## Check if at least one id was valid ##
  if(is.null(eidos_result)){
    stop("All the supplied IDs are invalid.
         Please use accepted IDs from eidos_taxon_by_name() or eidos_fuzzy_names()")
  }

  # Rename "taxonid" to "idtaxon" for consistency
  names(eidos_result)[names(eidos_result)=="taxonid"] <- "idtaxon"

  # Substitute "" for NA
  eidos_result[eidos_result == ""] <- NA

  # Remove duplicates:
  eidos_result[!duplicated(eidos_result), ]

  # Remove any wierd whitespaces from table
  eidos_result = as.data.frame(
    lapply(eidos_result, eidosapi:::eidos_clean_whitespaces),
    check.names = FALSE
  )

  # For accepted names, the EIDOS API returns the wrong ID in the
  # "nameid" and "acceptednameid" columns.
  # If name is not accepted, nameid should be the ID for the invalid name
  # NOT for the accepted name because it leas to confussion.
  eidos_result$nameid = ifelse(eidos_result$nametype != "Aceptado/válido",
                               eidos_result$acceptednameid,
                               eidos_result$idtaxon)

  # After setting that, the acceptedmeid of an invalid name should be idtaxon,
  # which corresponds to the id of the accepted name
  eidos_result$acceptednameid = ifelse(eidos_result$nametype != "Aceptado/válido",
                                       eidos_result$idtaxon,
                                       eidos_result$idtaxon)

  # Now, idtaxon should be equal to nameid. These columns seem to be
  # redundant in the API
  eidos_result$idtaxon = ifelse(eidos_result$nametype != "Aceptado/válido",
                                eidos_result$nameid,
                                eidos_result$idtaxon)
  return(eidos_result)
}
