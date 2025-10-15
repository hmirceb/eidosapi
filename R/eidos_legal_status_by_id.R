
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

  # Stop if no results found
  if(length(eidos_query_list) == 0){
    stop("No matching IDs")
    }

  ## Merge results ##
  eidos_query = do.call("rbind", eidos_query_list)

  # Substitute "" for NA
  eidos_query[eidos_query == ""] <- NA

  # Remove duplicates:
  eidos_query <- eidos_query[!duplicated(eidos_query), ]

  # Remove any wierd whitespaces from table
  eidos_query_temp = as.data.frame(
    lapply(eidos_query, eidos_clean_whitespaces),
    check.names = FALSE
  )

  # Get names from EIDOS:
  taxonomic_information = eidos_taxon_by_id(taxon_id = taxon_id)
  taxonomic_information = taxonomic_information[taxonomic_information$nameid == taxonomic_information$acceptednameid,]
  taxonomic_information = taxonomic_information[c("nameid", "name")]
  taxonomic_information$name = eidos_clean_names(taxonomic_information$name)

  # Merge query and name
  eidos_query_final = merge(x = eidos_query_temp,
                      y = taxonomic_information,
                      by.x = "idtaxon",
                      by.y = "nameid")

  # Put name as first column:
  eidos_query_final = eidos_query_final[c("name",
                              colnames(eidos_query_final)[colnames(eidos_query_final) != "name"])]

  # Rename column:
  colnames(eidos_query_final)[1] <- "name_clean"

  ## Return results ##
  return(eidos_query_final)
}
