
#' Get legal status from EIDOS
#'
#' Retrieves the legal status of a given taxon identifier from the EIDOS database.
#' Identifiers can be retrieved using the
#' eidos_fuzzy_names() or eidos_taxon_by_name() functions.
#'
#' @param taxon_id An integer. A valid taxon identifier from EIDOS.
#'
#' @returns A data frame.
#' @export
#'
#' @examples
#' eidos_legal_status_by_id(taxon_id = 1)
eidos_legal_status_by_id <- function(taxon_id){
  ## Make sure ID is numeric ##
  taxon_id = as.numeric(taxon_id)

  ## Check if ID is correct (only numbers allowed) ##
  if(sum(is.na(taxon_id) != 0)){
    stop("At least one ID is invalid or includes letters")
  }

  ## Set base URL ##
  base_url = "https://iepnb.gob.es:443/api/especie/rpc/obtenerestadoslegalesportaxonid?_idtaxon="

  ## Create URL for API ##
  eidos_url = paste0(base_url, taxon_id)

  ## Query API ##
  eidos_query_list = lapply(eidos_url,
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
  eidos_query_list = eidos_query_list[!sapply(eidos_query_list, is.null)]

  ## Merge results ##
  eidos_query = do.call("rbind", eidos_query_list)

  # Substitute "" for NA
  eidos_query[eidos_query == ""] <- NA

  # Remove duplicates:
  eidos_query[!duplicated(eidos_query), ]

  ## Return results ##
  return(eidos_query)

}
