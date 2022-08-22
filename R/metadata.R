
# Write Metadata ----------------------------------------------------------


#' @title Creates define XML metadata
#' @description The function generates define.xml metadata and
#' writes it to the file system. The function can generate a blank metadata
#' spreadsheet, a demonstration spreadsheet, or metadata read
#' from existing data files. The \code{type} parameter can be used
#' to request an SDTM or ADAM spreadsheet.
#' @details Frequently, organizations already have study metadata stored
#' in Excel spreadsheets, and in the correct structure.  This structure
#' is used commonly in the industry by other software.
#'
#' The purpose of the \code{write_metadata} function is to create an
#' initial metadata template for studies in which you have no metadata.
#' This task will ensure that you have the proper metadata structure
#' to later create the define.xml.
#'
#' By default, the function creates an unpopulated metadata spreadsheet.
#' If the \code{demo} parameter is set to TRUE, the function will create
#' the same spreadsheet populated with sample data.  This sample data
#' can help you to understand how to populate the metadata for your study.
#' Note that this demo spreadsheet is populated correctly, and has no
#' conformance errors.  Use this demo spreadsheet as a guide to fill
#' out your metadata.
#'
#' The demo spreadsheet was created by CDISC, and is
#' included in the \strong{defineR} package for convenience.
#'
#' @param dir The directory in which to produce the metadata. The default
#' is the current working directory.
#' @param type The type of metadata file to produce.  Valid values are
#' "SDTM" and "ADAM". Default is "SDTM".
#' @param ver The version of the metadata file.  Default is
#' the preferred version of the define standard.  The function
#' currently supports only version "2.0.0".
#' @param check Whether or not to run the checks.
#' @param src_dir If the metadata will be generated from existing datasets,
#' supply the directory path for those datasets.  If the metadata will
#' not be created from datasets, this parameter should be NULL.
#' @param demo If this parameter is TRUE, the function will generate
#' a metadata file populated with sample data. If the parameter is FALSE,
#' a blank metadata spreadsheet will be produced. Default is FALSE.
#' @return The full path of the generated metadata file.
#' @seealso \code{\link{write_define}} to create a define.xml document and
#' associated files.
#' @export
#' @examples
#' # Get temporary directory
#' tmp <- tempdir()
#'
#' # Create demo spreadsheet
#' out1 <- write_metadata(tmp, demo = TRUE)
#'
#' # View file
#' # file.show(out1)
#'
#' # Create unpopulated spreadsheet
#' out2 <- write_metadata(tmp, demo = FALSE)
#'
#' # View file
#' # file.show(out2)
write_metadata <- function(dir = ".", type = "sdtm", ver = NULL,
                           src_dir = NULL, check = TRUE,
                           demo = FALSE) {

  if (is.null(dir))
    stop("Target directory cannot be NULL.")

  if (is.null(ver))
    ver <- "2.0.0"

  if (ver != "2.0.0")
    stop("Only version 2.0.0 is supported.")

  if (!is.null(type)) {
    if (!tolower(type) %in% c("sdtm", "adam")) {

      stop("Type parameter is invalid.  Allowed values are 'sdtm' or 'adam'.")
    }

  } else {

    stop("Type parameter cannot be NULL.")
  }

  ret <- NULL

  if (is.null(src_dir)) {

    ret <- copy_template(dir, type, ver, demo)


  } else {

    ret <- generate_template(dir, type, ver, src_dir, check)

  }

  return(ret)
}



copy_template <- function(dir, type, ver, demo) {

  ret <- NULL

  # Get external data directory
  extd <- system.file("extdata", package = "defineR")

  # Get path to source xlsx
  if (ver == "2.0.0") {

    if (demo) {
      src <- file.path(extd, ver, "metadata/demo")
    } else {
      src <- file.path(extd, ver, "metadata/templates")
    }

    if (tolower(type) == "sdtm") {
      src <- file.path(src, "SDTM_METADATA.xls")
    } else if (tolower(type) == "adam") {
      src <- file.path(src, "ADAM_METADATA.xls")
    } else {
      stop("Type '" %p% type %p% "' no supported.")
    }

  } else {

    stop("Version " %p% ver %p% " not supported.")

  }

  if (!dir.exists(dir)) {

    dir.create(dir, recursive = TRUE)

    if (!dir.exists(dir)) {

     stop("Output directory could not be created.")
    }
  }

  path <- file.path(dir, basename(src))

  if (file.exists(path)) {

    file.remove(path)

    #stop("Target file '" %p% path %p% "' already exists.")
  }

  ret <- file.copy(src, path)


  return(path)

}

generate_template <- function(dir, type, ver, src_dir, check) {

 ret <- NULL



 return(ret)
}


# Import Metadata ---------------------------------------------------------



#' @title Imports metadata
#' @description Imports define metadata from a data source. Returns
#' a list of tibbles.
#' @param location The location of the metadata.  For a file data
#' source, this is the path to the file.
#' @param type The type of data source for the metadata.  Currently
#' only supports the "excel" source type, which is the default.
#' @returns A list of tibbles.
#' @import readxl
#' @noRd
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
