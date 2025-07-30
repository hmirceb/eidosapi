---
editor_options: 
  markdown: 
    wrap: 72
---

# eidos_api

[!WARNING] This package works best (if not almost exclusively) for taxa
in Spain as it makes use of the taxonomic API of the Spanish Inventory
of Natural Patrimony and Biodiversity

When working with species data it is important to have the correct
taxonomic information, specially if you are using data from several
sources that might differ in their taxonomic classification. A common
practice is to standardize names against the taxonomy of services such
as GBIF, but sometimes these do not include very rare species or
regional names. Specilized services like World Flora Online or Plants of
the World Online help with these issues, but are limited in their
taxonomic scope. In addition, any of these solutions are not useful if
we are interested in retrieving information about the legal or
conservation status of species at the national o regional level, common
names or .

This package makes use of the API provided by to access the taxonomic
services of the Spanish Inventory of Natural Patrimony and Biodiversity.

## Functions

Each function corresponds to one of the avilable APIs

### get_taxon_by_name

### get_taxon_by_id

### get_tables
