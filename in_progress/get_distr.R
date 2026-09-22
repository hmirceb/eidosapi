#' Retrieve species' geographic distributions from EIDOS
#'
#' @param taxa_list A vector or data.frame with taxa names (alternatively it can be a vector of taxonomic IDs from EIDOS). The data frame needs at least 2 columns: "genus" and "species". Optional columns are "subspecies", "scientificnameauthorship."
#'
#' @returns
#' @export
#'
#' @examples
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
    ids_table <- eidos_taxon_by_id(taxon_id = names_table$acceptednameid)
    # Get accepted names only
    ids_table <- ids_table[ids_table$nametype == "Aceptado/v\u00e1lido",]
  }

  # Base URL for API
  base_url <- "https://geoserver.iepnb.es/geoserver/especies/wfs?service=WFS&request=GetFeature&typeName=especies:distribucion_especies&outputFormat=shape-zip&format_options=filename:"

  # Name fo the file to download. The structure is "Current date", "Taxonomic name", "Author".
  # Date
  curr_date <- gsub("-", "_", Sys.Date())
  # Taxonomic name (all commas are turnt to "_")
  taxo_name <- gsub(",", "_",
       gsub(" ", "_", ids_table$name))
  filename <- paste0(paste(sep = "_", curr_date, taxo_name), ".zip")

  # The second part are the API calls plus the taxonomic IDs
  api_calls <- paste0("&propertyName=id,cuadricula,geom&cql_filter=lista_idstaxon_filtro%20LIKE%20%27",
                  ids_table$acceptednameid,
                  "%2C%25%27%20OR%20lista_idstaxon_filtro%20LIKE%20%27%25%2C",
                  ids_table$acceptednameid,
                  "%2C%25%27%20OR%20lista_idstaxon_filtro%20LIKE%20%27%25%2C",
                  ids_table$acceptednameid,
                  "%27")
  full_urls <- paste0(base_url, filename, api_calls)

  # Download data and load
  distributions_sf <- list()
  for (i in seq_along(ids_table$acceptednameid)) {
    # Set the path for the temporary file
    temp_file_path <- tempfile(fileext = ".zip")
    # Download the file from the URL and save it
    download.file(url = full_urls[[i]],
                  destfile = temp_file_path,
                  mode = "wb",
                  quiet = TRUE)
    temp_dir <- tempdir()
    # Unzip shapefile and read
    unzip(temp_file_path, exdir = temp_dir)
    temp_sf <- sf::read_sf(paste0(temp_dir, "/distribucion_especies.shp"))

    # Taxa with no data return empty shapefiles, add a row with NAs to keep them in the results
    if ( nrow(temp_sf) < 1 ){
      n_reps <- 1
      temp_sf[1,] <- rep(NA, times = ncol(temp_sf))
    } else {
      n_reps <- nrow(temp_sf)
    }
    # Add inputted information
    distributions_sf[[i]] <- cbind(nameid = rep(names_table$nameid[i], times = n_reps),
                                   name = rep(names_table$name[i], times = n_reps),
                                   name_clean = rep(eidos_clean_names(names_table$name[i]), times = n_reps),
                                   acceptednameid = rep(ids_table$acceptednameid[i], times = n_reps),
                                   acceptedname = rep(ids_table$name[i], times = n_reps),
                                   acceptedname_clean = rep(eidos_clean_names(ids_table$name[i]), times = n_reps),
                                   temp_sf)
  }
  # Merge into one object and return
  distributions_df <- do.call("rbind", distributions_sf)
  return(distributions_df)
}

taxa_list <- data.frame(genus = c("Erodium", "Gypaetus"),
                        species = c("paularense", "barbatus"))

a <- eidos_get_distribution(taxa_list)
ggplot(data = a)+
  geom_sf(aes(fill = nameid))
