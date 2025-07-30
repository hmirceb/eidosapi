#' Title
#'
#' @param table Name of the table to query
#'
#' @returns The desired table from the EIDOS API
#' @export
#'
#' @examples
get_tables <- function(table = "listapatronespecie"){

  if(!table %in% c("comunidades_autonomas",
                  "listapatronespecie_codigos",
                  "listapatronespecie",
                  "componente_tema",
                  "regbiogeograf_termar",
                  "listapatronespecie_sinonimos",
                  "pais",
                  "norma",
                  "provincias",
                  "listapatronespecie_normas")
  ){
    stop("The desired table is not available")
  }

  # Set base URL
  base_url = "https://iepnb.gob.es/api/catalogo/"

  # Create URL to desired table:
  table_url = paste0(base_url,
         "v_", # for some reason all of them have this
         table)

  ## Query the API ##
  api_table = jsonlite::fromJSON(table_url)

  return(api_table)
}
