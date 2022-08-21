

#' @title Imports metadata
#' @description Imports define metadata from a data source. Returns
#' a list of tibbles.
#' @param location The location of the metadata.  For a file data
#' source, this is the path to the file.
#' @param type The type of data source for the metadata.  Currently
#' only supports the "excel" source type, which is the default.
#' @returns A list of tibbles.
#' @import readxl
import_metadata <- function(location, type = "excel") {

  if (!type %in% c("excel")) {

   stop(paste0("Source type ", type, " not supported."))
  }

  ret <- list()

  if (type == "excel") {

    if (!file.exists(location)) {

      stop(paste0("Location '", location, "' does not exist."))
    }

    # Get sheets from excel file
    shts <- excel_sheets(location)

    for (sht in shts) {

      ret[[sht]] <- read_excel(location, sheet = sht)

    }

  }


  return(ret)

}
