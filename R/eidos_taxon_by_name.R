#' Retrieve taxonomic information by name from EIDOS
#'
#' Connects to the EIDOS API and retrieves taxonomic information for the given taxa. Invalid names will return nothing.
#' Note that if the supplied data is a vector, prior to querying the names the function removes all traces of "subsp.", "var." "f." and scientific authorships.
#' Names have to be properly written, although the default API admits some common spelling errors like "i" instead of "ii" (e.g. Borderea chouardi instead of chouardii).
#' For anything else use eidos_fuzzy_names()
#'
#' @param taxa_list A vector or data.frame with taxa names. The data frame needs at least 2 columns: "genus" and "species".
#' Optional columns are "subspecies", "scientificnameauthorship."
#' @returns A data.frame with the supplied data and the taxonomic information from EIDOS for any matching taxa
#' @export
#'
#' @examples
#' example_taxo = data.frame(genus = "Alytes", species = "cisternasii", subspecies = NA)
#' eidos_taxon_by_name(taxa_list = example_taxo)
#' eidos_taxon_by_name(taxa_list = c("Alytes cisternasii", "Pinus nigra subsp. salzmannii"))
eidos_taxon_by_name = function(taxa_list) {

  ## If supplied list is a vector, generate appropiate data frame
  if(is.vector(taxa_list)){

    # Clean names beforehand
    taxa_list = sapply(taxa_list, eidos_clean_names)

    # Split vector and extract genus, species and subspecies:
    taxa_split = strsplit(x = taxa_list, split = " ")
    genera = sapply(taxa_split, FUN = function(x){x[1]})
    species = sapply(taxa_split, FUN = function(x){x[2]})
    subspecies = sapply(taxa_split, FUN = function(x){x[3]})

    # Generate data frame
    taxa_list = data.frame(genus = genera,
               species = species,
               subspecies = subspecies)
    rm(genera, species, subspecies)
  }

  ## Check if genus data is ok: ##
  if(sum(is.na(taxa_list$genus)) > 0){
    stop("Missing genus data")
  }

  ## Check if species data is ok: ##
  if(sum(is.na(taxa_list$species)) > 0){
    stop("Missing species data")
  }

  ## Set API URL ##
  api_url_base = "https://iepnb.gob.es:443/api/especie/rpc/obtenertaxonespornombre?_nombretaxon="

  ## Generate taxon-specific URLs ##

  # Separate between taxa with species and subspecies
  # Species
  if(is.null(taxa_list$subspecies) |
     sum(is.na(taxa_list$subspecies)) == 0){
    # If subspecies column is empty or does not exist, keep taxa_list as it is
    sp_list = taxa_list
  }else{
    # Else subset only species
    sp_list = taxa_list[is.na(taxa_list$subspecies),]
  }

  # Create URLs for species
  sp_urls = apply(
    X = sp_list,
    MARGIN = 1,
    simplify = T,
    FUN = function(X){

      # Create URL from API base url, genus and species. Use %20 as separator
      url = paste0(
        api_url_base,
        X[1], "%20",
        X[2]
      )

      # Dataframe with supplied data and URL for each taxon
      # Suppress harmless warning
      df = suppressWarnings(
        data.frame(genus = X[1],
                      species = X[2],
                      subspecies = X[3],
                      scientificnameauthorship = X[4],
                      url = url)
                      )
    }
  )

  # Create URLs for subspecies
  subsp_list = taxa_list[!is.na(taxa_list$subspecies),]

  subsp_urls = apply(
    X = subsp_list,
    MARGIN = 1,
    FUN = function(X){
      # For subspecies the API uses 2 different formats, one with "subsp."
      # between species and subspecies, and another without it.
      # We cannot know which one is which a priori so we try both
      # URLs

      # Create URL from API base url, genus, species and subspecies without "subsp."
      url1 = paste0(
        api_url_base,
        X[1], "%20",
        X[2], "%20",
        X[3]
      )

      # Same but with "subsp."
      url2 = paste0(
        api_url_base,
        X[1], "%20",
        X[2], "%20subsp.%20",
        X[3]
      )

      # Create data frame with urls to query
      df = suppressWarnings(
        data.frame(genus = X[1],
                      species = X[2],
                      subspecies = X[3],
                      scientificnameauthorship = X[4],
                      url = c(url1, url2))
        )
      return(df)
    }
  )

  # Join DFs with species and subspecies URLs
  df_urls = do.call("rbind", c(sp_urls, subsp_urls))

  # Add the Scientific Authority
  df_urls$url = paste0(df_urls$url,
                       "%20",
                       df_urls$scientificnameauthorship)

  # Remove unnecessary text if scientific authority is unavailable
  df_urls$url = gsub("%20NA",
                     "",
                     df_urls$url)

  ## Query the EIDOS API ##
  eidos_result_temp = apply(df_urls,
             1,
             function(X){
               # DF with supplied information
               supplied_data = data.frame(supplied_genus = X[1],
                                          supplied_species = X[2],
                                          supplied_subspecies = X[3],
                                          supplied_scientificnameauthorship = X[4])

               # Query EIDOS API
               eidos_data = jsonlite::fromJSON(txt = X[5])

               # If the URL is not correct, EIDOS returns an empty DF
               # Add error message
               if(is.null(dim(eidos_data))){
                 eidos_data = data.frame(error = "No matches found")
               }
               # Join supplied data and EIDOS data
               suppressWarnings(cbind(supplied_data, eidos_data))

             })

  # Get taxa with no matches in EIDOS (DFs with only 5 columns):
  no_matches = which(sapply(eidos_result_temp, ncol) == 5)

  # Remove those from list
  if(length(no_matches) == 0){
    eidos_result = do.call("rbind", eidos_result_temp)
  }else{
    eidos_result = do.call("rbind", eidos_result_temp[-no_matches])
  }

  # Remove rownames:
  rownames(eidos_result) = NULL

  # Add the supplied taxon (genus species subspecies) to final df
  # Paste names
  supplied_taxon = paste(eidos_result$supplied_genus,
        eidos_result$supplied_species,
        eidos_result$supplied_subspecies,
        sep = " ")

  # Remove " NA" if no subspecies
  supplied_taxon = gsub(pattern = " NA",
       replacement = "",
       x = supplied_taxon)

  # Attach to final df and return:
  eidos_result = cbind(supplied_taxon, eidos_result)

  # Add clean name in eidos
  eidos_result$name_clean = paste(eidos_result$genus,
                                   eidos_result$specificepithet,
                                   eidos_result$infraspecificepithet,
                                   sep = " ")
  eidos_result$name_clean = gsub(pattern = " NA",
                        replacement = "",
                        x = eidos_result$name_clean)

  # Rename "taxonid" to "idtaxon" for consistency
  names(eidos_result)[names(eidos_result)=="taxonid"] <- "idtaxon"

  # Substitute "" for NA
  eidos_result[eidos_result == ""] <- NA

  # Remove duplicates:
  eidos_result = eidos_result[!duplicated(eidos_result), ]

  # Remove any wierd whitespaces from table
  eidos_result = as.data.frame(
    lapply(eidos_result, eidos_clean_whitespaces),
    check.names = FALSE
  )

  # For accepted names, the EIDOS API returns the wrong ID in the
  # "nameid" and "acceptednameid" columns.
  # If name is not accepted, nameid should be the ID for the invalid name
  # NOT for the accepted name because it leas to confussion.
  eidos_result$nameid = ifelse(eidos_result$nametype != "Aceptado/válido",
                               eidos_result$acceptednameid,
                               eidos_result$nameid)

  # After setting that, the acceptedmeid of an invalid name should be idtaxon,
  # which corresponds to the id of the accepted name
  eidos_result$acceptednameid = ifelse(eidos_result$nametype != "Aceptado/válido",
                                        eidos_result$idtaxon,
                                        eidos_result$acceptednameid)

  # Now, idtaxon should be equal to nameid. These columns seem to be
  # redundant in the API
  eidos_result$idtaxon = ifelse(eidos_result$nametype != "Aceptado/válido",
                                       eidos_result$nameid,
                                       eidos_result$idtaxon)
  return(eidos_result)
}
