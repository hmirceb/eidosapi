# eidos_api

[!WARNING] This package works best (if not almost exclusively) for taxa in Spain as it makes use of the taxonomic API of the Spanish Inventory of Natural Patrimony and Biodiversity

When working with species data it is important to have the correct taxonomic information, specially if you are using data from several sources that might differ in their taxonomic classification. A common practice is to standardize names against the taxonomy of services such as GBIF, but sometimes these do not include very rare species or regional names. Specilized services like World Flora Online or Plants of the World Online help with these issues, but are limited in their taxonomic scope. In addition, any of these solutions are not useful if we are interested in retrieving information about the legal or conservation status of species at the national o regional level, common names or .

This package makes use of the API provided by to access the taxonomic services of the Spanish Inventory of Natural Patrimony and Biodiversity. I have avoided using any external dependencies as much as possible, but the package still depends on the *fromJSON* function of the **jsonlite** package.

## Functions

Each function corresponds to one of the available APIs

### get_taxon_by_name()

This function sends a query to the API based on the genus and specific epithet of a species (additionally, it can include subspecies and the scientific authority that described that taxon). Even if the taxon of interest do not have any subspecies it is necessary to include the subspecies column. The function returns a data frame with the submitted data and all the information avaible in IEPNB. If no matches are found due to spelling errors or the species not existing in the IEPNB it returns nothing.

``` r
library(eidosapi)
example_data <- data.frame(genus = c("Ales", "Borderea"),
                           species = c("cisternasii", "chouardii"),
                           subspecies = NA)
info_taxo <- get_taxon_by_name(taxon_list = example_data)
print(info_taxo)
```

This function is useful to retrieve the unique identifiers *idtaxon* from IEPNB that can be used to make accessing other APIs easier.

### get_taxon_by_id()

Retrieve information associated to a list of identifiers. The input is a vector of numeric identifiers and the function returns a data frame with all the information from EIDOS. One identifier might be associated with several taxa, for example if a species has several synonyms. The returned data frame includes a row for each taxa associated to the identifier along with the taxonomic validity of the name.

``` R
library(eidosapi)
example_ids <- c(124, "2764")
info_taxo <- get_taxon_by_id(taxon_id = example_ids)
print(info_taxo)
```

### get_tables()

This function retrieves the tables available [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/bdn_listas_patron.html) through these [API](https://iepnb.gob.es/recursos/servicios-interoperables/api-catalogo). The most important ones are related to the checklist of species (*Lista Patrón*) and they are downloaded by default.
