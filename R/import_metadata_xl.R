

#' @title Imports metadata from an Excel file
#' @description Imports define metadata from xls or xlsx files. Returns
#' a list of tibbles.
#' @param file_path The path to the Excel metadata file.
#' @returns A list of tibbles, one for each sheet in the excel file.
#' @import readxl
#' @export
import_metadata_xl <- function(file_path) {

  if (!file.exists(file_path)) {

    stop(paste0("File '", file_path, "' does not exist."))
  }

  ret <- list()

  # Get sheets from excel file
  shts <- excel_sheets(file_path)

  for (sht in shts) {

    ret[[sht]] <- read_excel(file_path, sheet = sht)

  }


  return(ret)

}
