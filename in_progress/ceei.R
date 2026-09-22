#' Function to download large Excel files from MITECO
#'
#' @param url URL to download.
#' @param destfile Path to downloaded file.
#' @param tries Number of download tries.
#'
#' @returns A file
#' @keywords internal
#' @noRd
download_with_retry <- function(url, destfile) {
  h <- curl::new_handle()
  curl::handle_setopt(h,
                      timeout = 60,
                      low_speed_time = 30,
                      low_speed_limit = 1,
                      connecttimeout = 15,
                      ssl_verifypeer = TRUE
  )

  for (i in seq_len(5)) {
    ok <- tryCatch({
      curl::curl_download(url, destfile, handle = h, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (ok && file.exists(destfile)) {
      con <- file(destfile, "rb")
      magic <- readBin(con, "raw", 2)
      close(con)
      if (identical(as.character(magic), c("50", "4b"))) {
        return(invisible(TRUE))
      }
    }
  }
  warning("Download failed after 5 attempts",
          call. = FALSE)
  return(invisible(NULL))
}

#' Retrieve one of the tables related to the Spanish Checklist of Wildlie Species
#'
#' The tables are downloaded verbatim from the API. This means that column names may not match those in other tables from EIDOS.
#' The exception are the Checklists. Due to limits in the download size of files from the API these are downloaded to the disk as xlsx files and then loaded to environment.
#'
#' @param eidos_table Name of the table to query. The function is case insensitive, admits whitespaces and has partial matching for arguments, but these must match one of "comunidades_autonomas", "listapatronespecie_codigos",
#' "listapatronespecie", "componente_tema", "regbiogeograf_termar", "listapatronespecie_sinonimos", "pais",
#' "norma", "provincias" or "listapatronespecie_normas".
#'
#' @returns A data.frame with the desired table from the EIDOS API
#' @export
#'
#' @examples
#' eidos_tables(eidos_table = "comunidades_autonomas")
eidos_tables <- function(eidos_table = c("comunidades_autonomas",
                                         "listapatronespecie",
                                         "listapatronespecie_sinonimos",
                                         "listapatronespecie_normas",
                                         "listapatronespecie_codigos",
                                         "componente_tema",
                                         "regbiogeograf_termar",
                                         "pais",
                                         "norma",
                                         "provincias",
                                         "ceei")){

  # Set table name in lower case and remove whitespaces just in case:
  eidos_table <- gsub(pattern = " ",
                      replacement = "_",
                      x = tolower(
                        x = trimws(
                          x = eidos_table
                        )
                      )
  )

  # Match input to one of the arguments:
  table <- match.arg(eidos_table)

  # Get exotic species catalogue
  if( grepl("ceei", table) ){
    # make url
    table_url <- "https://iepnb.gob.es/sites/default/files/2026-06/TablaCEEI.xlsx"
    destfile <- "TablaCEEI.xlsx"
    # download, load and remove file from disk
    a <- download_with_retry(url = table_url, destfile = destfile)
    if(is.null(a)){
      api_table <- a
    } else {
      api_table <- readxl::read_excel(destfile)
      file.remove(destfile)
    }
  }

  if( !grepl("ceei", table) ) {
    # Set base URL
    base_url <- "https://des.iepnb.es/api/catalogo/"

    # Create URL to desired table:
    table_url <- paste0(base_url,
                        "v_", # for some reason all of them have this
                        table)

    ## Query the API ##
    api_table <- parse_api_json(url = table_url)
  }

  ## Check if table download was correct ##
  if(is.null(api_table)){
    warning("Unable to retrieve table. EIDOS or your internet connection is down.",
            call. = FALSE)
    return(invisible(NULL))
  } else {
    # Substitute "" for NA
    api_table[api_table == ""] <- NA

    # Remove duplicates:
    api_table <- api_table[!duplicated(api_table), ]

    # Remove any wierd whitespaces from table
    api_table <- as.data.frame(
      lapply(api_table, eidos_clean_whitespaces),
      check.names = FALSE
    )

    # Return the table
    return(api_table)
  }
}

#### clean CEEI ####

a <- eidos_tables("ceei")
a$genus <- unlist(lapply(strsplit(a$Especie, " "), function(x) x[[1]]))
a$grupo <- NA
headers <- c("Hongos",
             "Algas",
             "Flora",
             "Invertebrados",
             "Artrópodos",
             "Crustáceos",
             "Peces",
             "Anfibios",
             "Reptiles",
             "Aves",
             "Mamíferos")
header_rows <- which(a$genus %in% headers)
for(i in seq_along(header_rows)){
  row_range <- if(i == length(header_rows)){
    header_rows[i]:nrow(a)
  } else {
    header_rows[i]:(header_rows[i+1]-1)
  }
  a[row_range,]$grupo <- headers[i]
}
a <- a[a$Especie != a$grupo,]
a[grepl("Excepto | excepto", a$Especie),]


