# eidos_api

[!WARNING] This package works best (if not almost exclusively) for taxa in Spain as it makes use of the taxonomic API of the Spanish Inventory of Natural Patrimony and Biodiversity

[!WARNING] The Spanish Inventory of Natural Patrimony and Biodiversity is a work in progress. Taxa might change and things can break from time to time do to changes in the APIs.

When working with species data it is important to have the correct taxonomic information, specially if you are using data from several sources that might differ in their taxonomic classification. A common practice is to standardize names against the taxonomy of services such as GBIF, but sometimes these do not include very rare species or regional names. Specilized services like World Flora Online or Plants of the World Online help with these issues, but are limited in their taxonomic scope. In addition, any of these solutions are not useful if we are interested in retrieving information about the legal or conservation status of species at the national o regional level, common names or .

This package makes use of the API provided by to access the taxonomic services of the Spanish Inventory of Natural Patrimony and Biodiversity. I have avoided using any external dependencies as much as possible, but the package still depends on the *fromJSON* function of the **jsonlite** package.

## Functions

Each function corresponds to one of the available APIs

### eidos_taxon_by_name()

This function sends a query to the API based on the genus and specific epithet of a species (additionally, it can include subspecies and the scientific authority that described that taxon). Even if the taxon of interest do not have any subspecies it is necessary to include the subspecies column. The function returns a data frame with the submitted data and all the information available in IEPNB. If no matches are found due to spelling errors or the species not existing in the IEPNB it returns nothing.

``` r
library(eidosapi)
example_data <- data.frame(genus = c("Alytes", "Ales", "Borderea"),
                           species = c("cisternasii", "cisternasii", "chouardii"),
                           subspecies = NA)
info_taxo <- eidos_taxon_by_name(taxon_list = example_data)
print(info_taxo)
```

This function is useful to retrieve the unique identifiers *idtaxon* from IEPNB that can be used accessed other APIs.

If the supplied name is not accepted but it is included in IEPNB it will return its status as a synonym and the identifier for the accepted taxon. The API does not allow fuzzy matching and thus names have to be written without typos.

### eidos_taxon_by_id()

Retrieve information associated to a list of identifiers. The input is a vector of numeric identifiers and the function returns a data frame with all the information from EIDOS. One identifier might be associated with several taxa, for example if a species has several synonyms. The returned data frame includes a row for each taxa associated to the identifier along with the taxonomic validity of the name.

``` r
library(eidosapi)
example_ids <- c(124, "2764")
info_taxo <- eidos_taxon_by_id(taxon_id = example_ids)
print(info_taxo)
```

If one of the identifiers is invalid because it includes letters the function will throw an error. If the identifier is numeric but the taxon is not present in IEPNB the function returns nothing.

### eidos_conservation_by_id()

Retrieves the conservation status from a given identifier (IUCN categories). By default the API does not return the taxonomic information associated to that identifier. If needed, the function calls eidos_taxon_by_id() and includes the information regarding the **accepted** name for that identifier. Many taxa have been assessed several times throughout the years and thus appear in different categories. The function returns all of them by default, but includes the option of returning only the latest assessment.

``` r
library(eidosapi)
example_ids <- c(124, 1)
info_taxo_default <- eidos_conservation_by_id(taxon_id = example_ids, taxo_info = F, latest = F)
print(info_taxo)

info_taxo <- eidos_conservation_by_id(taxon_id = example_ids, taxo_info = T, latest = F)

info_taxo_latest <- eidos_conservation_by_id(taxon_id = example_ids, taxo_info = T, latest = T)
```

### eidos_legal_status_by_id()

Returns the legal status of a given identifier. This includes the catalogues in which it is listed, its IUCN categories or Habitats Directive Annex, the legal norm that set that status and the spatial scope of the norm (regional, national or international). By default the API returns the accepted name associated with the identifier as well as the names that appear in those legal norms.

### eidos_tables()

This function retrieves the tables available [here](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/bdn_listas_patron.html) through these [API](https://iepnb.gob.es/recursos/servicios-interoperables/api-catalogo). The most important ones are related to the checklist of species (*Lista Patrón*) and they are downloaded by default.

### eidos_fuzzy_names()

This function only works with species in the checklist (*Lista Patrón*) because it first downloads the whole list with synonyms and then matches any. You can also supply the table as an object after retrieving it using eidos_table(), which makes it much faster.
