#' Retrieve taxonomic information by name from EIDOS
#'
#' EIDOS is the Taxonomy Web Service of the Spanish Inventory of Natural Patrimony and Biodiversity (IEPNB)
#'
#' The function 'get_taxon_by_name' connects to the EIDOS API and retrieves taxonomic information for the taxon. Invalid names will return nothing
#'
#' @param taxon_list A vector or data.frame with taxa names. The data frame needs at least 2 columns: "genus" and "species".
#' Optional columns are "subspecies", "scientificnameauthorship."
#' @returns A data.frame with the supplied data and the taxonomic information from EIDOS for any matching taxa
#' @export
#'
#' @examples
#' example_taxo = data.frame(genus = "Alytes", species = "cisternasii", subspecies = NA)
#' eidos_taxon_by_name(taxon_list = example_taxo)
#' eidos_taxon_by_name(taxon_list = c("Alytes cisternasii", "Pinus nigra subsp. salzmannii"))
eidos_taxon_by_name = function(taxon_list) {

  ## If supplied list is a vector, generate appropiate data frame
  if(is.vector(taxon_list)){

    # Clean names beforehand
    taxon_list = sapply(taxon_list, clean_names)

    # Split vector and extract genus, species and subspecies:
    taxa_split = strsplit(x = taxon_list, split = " ")
    genera = sapply(taxa_split, FUN = function(x){x[1]})
    species = sapply(taxa_split, FUN = function(x){x[2]})
    subspecies = sapply(taxa_split, FUN = function(x){x[3]})

    # Generate data frame
    taxon_list = data.frame(genus = genera,
               species = species,
               subspecies = subspecies)
    rm(genera, species, subspecies)
  }

  ## Check if genus data is ok: ##
  if(sum(is.na(taxon_list$genus)) > 0){
    stop("Missing genus data")
  }

  ## Check if species data is ok: ##
  if(sum(is.na(taxon_list$species)) > 0){
    stop("Missing species data")
  }

  ## Set API URL ##
  api_url_base = "https://iepnb.gob.es:443/api/especie/rpc/obtenertaxonespornombre?_nombretaxon="

  ## Generate taxon-specific URLs ##

  # Separate between taxa with species and subspecies
  # Species
  if(is.null(taxon_list$subspecies) |
     sum(is.na(taxon_list$subspecies)) == 0){
    sp_list = taxon_list
  }else{
    sp_list = taxon_list[is.na(taxon_list$subspecies),]
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
      df = data.frame(genus = X[1],
                      species = X[2],
                      subspecies = X[3],
                      scientificnameauthorship = X[4],
                      url = url)
    }
  )

  # Create URLs for subspecies
  subsp_list = taxon_list[!is.na(taxon_list$subspecies),]

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

      df = data.frame(genus = X[1],
                      species = X[2],
                      subspecies = X[3],
                      scientificnameauthorship = X[4],
                      url = c(url1, url2))
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
               cbind(supplied_data, eidos_data)

             })

  # Get taxa with no matches in EIDOS (DFs with only 5 columns):
  no_matches = which(sapply(eidos_result_temp, ncol) == 5)

  # Remove those from list
  if(length(no_matches) == 0){
    eidos_result = do.call("rbind", eidos_result_temp)
  }else{
    eidos_result = do.call("rbind", eidos_result_temp[-no_matches])
  }

  return(eidos_result)
}
