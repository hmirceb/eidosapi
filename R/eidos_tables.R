#' Retrieve one of the tables related to the Spanish Checklist of Wildlie Species
#'
#' The tables are downloaded verbatim from the API. This means that column names may not match those in other tables from EIDOS.
#'
#' @param eidos_table Name of the table to query. The function is case insensitive, admits whitespaces and has partial matching for arguments, but these must match one of "comunidades_autonomas", "listapatronespecie_codigos",
#' "listapatronespecie", "componente_tema", "regbiogeograf_termar", "listapatronespecie_sinonimos", "pais",
#' "norma", "provincias" or "listapatronespecie_normas".
#'
#' @returns The desired table from the EIDOS API
#' @export
#'
#' @examples
#' eidos_tables(eidos_table = "listapatronespecie")
eidos_tables <- function(eidos_table = c("comunidades_autonomas",
                                         "listapatronespecie_codigos",
                                         "listapatronespecie",
                                         "componente_tema",
                                         "regbiogeograf_termar",
                                         "listapatronespecie_sinonimos",
                                         "pais",
                                         "norma",
                                         "provincias",
                                         "listapatronespecie_normas")){

  # Set table name in lower case and remove whitespaces just in case:
  eidos_table = gsub(pattern = " ",
                     replacement = "_",
                     x = tolower(
                       x = trimws(
                         x = eidos_table
                         )
                     )
  )

  # Match input to one of the arguments:
  table = match.arg(eidos_table)

  # Set base URL
  base_url = "https://iepnb.gob.es/api/catalogo/"

  # Create URL to desired table:
  table_url = paste0(base_url,
                     "v_", # for some reason all of them have this
                     table)

  ## Query the API ##
  api_table = jsonlite::fromJSON(table_url)

  # Substitute "" for NA
  api_table[api_table == ""] <- NA

  # Remove duplicates:
  api_table[!duplicated(api_table), ]

  # Remove any wierd whitespaces from table
  api_table = as.data.frame(
    lapply(api_table, eidosapi:::eidos_clean_whitespaces),
    check.names = FALSE
    )

  # Return the table
  return(api_table)
}
