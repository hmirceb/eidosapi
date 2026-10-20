#' Retrieve species' geographic distributions from EIDOS
#'
#' @param taxa_list A vector or data.frame with taxa names (alternatively it can be a vector of taxonomic IDs from EIDOS). The data frame needs at least 2 columns: "genus" and "species". Optional columns are "subspecies" and "scientificnameauthorship."
#'
#' @returns A simple features (sf) object with the UTM cells in which the inputed taxa are present in Spain. Taxa without data return an empty geometry.
#' @export
#'
#' @examples
#'
#' taxa_list <- data.frame(genus = "Sus", species = "scrofa", subspecies = NA, scientificnameauthorship = "Linnaeus, 1758")
#' sps_distribution <- eidos_get_distribution(taxa_list)
#' sps_distribution <- sps_distribution[sps_distribution$acceptedname_clean == "Sus scrofa",]
#' plot(sps_distribution[3], main = sps_distribution$name_clean[1], key.pos = NULL)
#'
eidos_get_distribution <- function(taxa_list){
  # Check that all inputs are the same type
  if( suppressWarnings(length(unique(is.na(as.numeric(unlist(taxa_list))))) > 1) ) {
    stop("Mix of taxonomic names and IDs. All inputs must be of the same type")
  }

  # If all are numerics, assume they are IDs
  if( suppressWarnings(!all(is.na(as.numeric(unlist(taxa_list))))) ) {
    ids_table <- eidos_taxon_by_id(taxon_id = taxa_list)
    # Get accepted names only
    ids_table <- ids_table[ids_table$nametype == "Aceptado/v\u00e1lido",]
  }

  # If all are character strings, assume they are taxonomic names
  if( suppressWarnings(all(is.na(as.numeric(unlist(taxa_list))))) ) {
    names_table <- eidos_taxon_by_name(taxa_list = taxa_list)
    names_table <- names_table[sapply(unique(names_table$supplied_taxon), eidos_clean_names) %in%
                                 names_table$name_clean,]
    names_table <- names_table[!is.na(names_table$acceptednameid),]

    ids_table <- eidos_taxon_by_id(taxon_id = names_table$acceptednameid)
    # Get accepted names only
    ids_table <- ids_table[ids_table$nametype == "Aceptado/v\u00e1lido",]
  }

  # Base URL for API
  base_url <- "https://iepnb.gob.es/geoserver/especies/wfs?service=WFS&request=GetFeature&typeName=especies:distribucion_especies&outputFormat=application/json"

  # The second part are the API calls plus the taxonomic IDs
  api_calls <- paste0("&propertyName=id,cuadricula,geom&cql_filter=lista_idstaxon_filtro%20LIKE%20%27",
                  ids_table$acceptednameid,
                  "%2C%25%27%20OR%20lista_idstaxon_filtro%20LIKE%20%27%25%2C",
                  ids_table$acceptednameid,
                  "%2C%25%27%20OR%20lista_idstaxon_filtro%20LIKE%20%27%25%2C",
                  ids_table$acceptednameid,
                  "%27")
  full_urls <- paste0(base_url, api_calls)

  # Download data and load
  distributions_sf <- list()
  for (i in seq_along(ids_table$acceptednameid)) {
    # # Download the file from the URL and save it to memory
    temp_sf <- sf::read_sf(full_urls[[i]], quiet = TRUE)

    # Taxa with no data return empty shapefiles, add a row with NAs to keep them in the results
    if ( nrow(temp_sf) < 1 ){
      n_reps <- 1
      temp_sf[1,] <- rep(NA, times = ncol(temp_sf))
    } else {
      n_reps <- nrow(temp_sf)
    }
    # Add inputted information
    distributions_sf[[i]] <- cbind(nameid = rep(ids_table$nameid[i], times = n_reps),
                                   name = rep(ids_table$name[i], times = n_reps),
                                   name_clean = rep(eidos_clean_names(ids_table$name[i]), times = n_reps),
                                   acceptednameid = rep(ids_table$acceptednameid[i], times = n_reps),
                                   acceptedname = rep(ids_table$name[i], times = n_reps),
                                   acceptedname_clean = rep(eidos_clean_names(ids_table$name[i]), times = n_reps),
                                   temp_sf)
  }
  # Merge into one object and return
  distributions_df <- do.call("rbind", distributions_sf)
  return(distributions_df)
}

