#' Retrieve one of the tables related to the Spanish checklist
#'
#' @param table Name of the table to query. Must be one of "comunidades_autonomas", "listapatronespecie_codigos",
#' "listapatronespecie", "componente_tema", "regbiogeograf_termar", "listapatronespecie_sinonimos", "pais",
#' "norma", "provincias" or "listapatronespecie_normas"
#'
#' @returns The desired table from the EIDOS API
#' @export
#'
#' @examples
#' eidos_tables(eidos_table = "listapatronespecie")
eidos_tables <- function(eidos_table = "listapatronespecie"){

  # Clean names just in case:
  eidos_table = gsub(pattern = " ",
                     replacement = "_",
                     x = tolower(
                       x = eidos_table
                     )
  )

  if(!eidos_table %in% c("comunidades_autonomas",
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
    stop('The desired table is not available. Choose one of c("comunidades_autonomas",
                         "listapatronespecie_codigos",
                         "listapatronespecie",
                         "componente_tema",
                         "regbiogeograf_termar",
                         "listapatronespecie_sinonimos",
                         "pais",
                         "norma",
                         "provincias",
                         "listapatronespecie_normas")')
  }

  # Set base URL
  base_url = "https://iepnb.gob.es/api/catalogo/"

  # Create URL to desired table:
  table_url = paste0(base_url,
                     "v_", # for some reason all of them have this
                     eidos_table)

  ## Query the API ##
  api_table = jsonlite::fromJSON(table_url)

  # Return the table
  return(api_table)
}
