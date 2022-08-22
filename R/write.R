
# Exported Write Functions ------------------------------------------------

#' @title Creates a define XML document
#' @description The \code{write_define} function inputs an SDTM or ADAM
#' metadata file and outputs a define.xml and associated files to a specified
#' directory. Possible associated files are the HTML transformation of
#' the define.xml and a check report. By default, the check report will
#' also be shown in the viewer.
#' @details
#' The define.xml document is used by the FDA (and others) to review
#' study data. All datasets used in the study are listed, along with
#' variable and value metadata, where clauses, and more.  The define.xml
#' can be used along with an XSLT style sheet to transform the XML into
#' an HTML report.  This HTML report has links that allow you to easily
#' navigate the data structures.
#'
#' The \code{write_define} function
#' creates both the define XML and the define HTML. The \code{path}
#' parameter identifies the location of the Excel metadata, and the
#' \code{dir} parameter specifies an output directory.  You can create
#' both SDTM and ADAM metadata by passing the appropriate value on the
#' \code{type} parameter.
#'
#' Importantly, the function also produces a check report.  This report
#' will compare the define.xml file against the XSD schema supplied by
#' CDISC.  Any discrepancies will be writted to the check report.  By default,
#' the check report is also shown in the viewer, and returned as a vector of
#' strings by the function.  This discrepancy list allows you to verify
#' that the metadata has been filled out correctly.
#'
#' For instance, the
#' conformance check will ensure that each object ID (OID) is unique within
#' the document.  Any non-unique IDs will be identified and written to the
#' check report. The check report may therefore be used iteratively to
#' fix the metadata.
#'
#' Any errors that prevent proper functioning of the procedure will stop
#' execution, and be displayed in the console.  All other errors and
#' warnings will be sent to the check report, and will not stop execution.
#'
#' Note that the \code{check} parameter can be used to turn off the
#' conformance check mechanism, and just create the XML.  Also note that this
#' parameter accepts a "V5" option, to perform checks that ensure the
#' data complies with version 5 transport file constraints.
#'
#' The XSD schema and XSLT transformation documents were created by CDISC,
#' and are included in the \strong{defineR} package for convenience.
#'
#' @param path The path to the metadata file.  Currently only Excel metadata
#' files are supported.  Other metadata sources may be added if there is sufficient
#' interest.
#' @param dir The output directory to create the define.xml and associated files.
#' By default, the current working directory will be used. If the directory
#' does not exist, the function will attempt to create it.
#' @param type The type of define.xml to create.  Valid values are
#' "sdtm" and "adam".  Default is "sdtm".
#' @param ver The version of the define XML to produce.  Default is the
#' preferred version of the FDA. Currently only version "2.0.0" is supported.
#' @param check Whether or not to perform consistency checks.  If so,
#' a check report will be produced in the output directory. Valid values
#' are TRUE, FALSE, and "V5".  Default is TRUE.  The "V5" option will include
#' checks that metadata conforms to SAS Transport file version 5 requirements.
#' @param html Whether or not to produce the HTML output associated with the
#' define.xml.  Valid values are TRUE and FALSE.  Default is TRUE.
#' @param view Whether or not to show the check report in the viewer.  Valid
#' values are TRUE and FALSE.  Default is TRUE.
#' @param report_type The output type of the check report, if requested.
#' Valid values are "TXT", "RTF", "PDF", "HTML" and "DOCX".  Default is
#' "PDF".
#' @return The define.xml file and any associated files will be written
#' to the directory specified on the \code{dir} parameter.
#' @seealso \code{\link{write_metadata}} to create a metadata template.
#' @export
#' @examples
#' # Get temp directory
#' tmp <- tempdir()
#'
#' # Create demo metadata
#' pth <- write_metadata(tmp, demo = TRUE)
#'
#' # Generate define files
#' res <- write_define(pth, tmp)
#'
#' # View check results
#' res
#' # NULL
#'
#' # View XML
#' # file.show(file.path(tmp, "define.sdtm.xml"))
#'
#' # View check report
#' # file.show(file.path(tmp, "check.sdtm.pdf"))
#'
#' # View HTML
#' # file.show(file.path(tmp, "define.sdtm.html"))
write_define <- function(path, dir = ".", type = "sdtm", ver = NULL,
                         check = TRUE, html = TRUE, view = TRUE,
                         report_type = "PDF") {

  if (!file.exists(path)) {

    stop("Input metadata file '" %p% path %p% "' not found.")

  }


  if (!dir.exists(dir)) {

    res <- dir.create(dir, recursive = TRUE)

    if (!length(res) > 0)
       stop("Output directory '" %p% dir %p% "' cannot be created.")

  }

  if (is.null(ver)) {
    ver <- "2.0.0"
  } else if (ver != "2.0.0") {

    stop("Version '" %p% ver %p% "' not supported.")
  }

  if (check == "V5") {

    check <- TRUE
    v5flg <- TRUE
  } else {

   v5flg <- FALSE
  }

  dfl <- "define." %p% type %p% ".xml"
  hfl <- "define." %p% type %p% ".html"
  cfl <- "check." %p% type %p% "." %p% tolower(report_type)

  dpth <- file.path(dir, dfl)
  hpth <- file.path(dir, hfl)
  cpth <- file.path(dir, cfl)

  # Create list for message results
  msg <- c()

  mdt <- import_metadata(path)

  if (check == TRUE) {

    msg <- check_metadata(mdt, ver, v5flg, msg)

  }

  if (tolower(type) == "sdtm") {

    xml <- create_sdtm_xml(mdt, ver)

  } else if (tolower(type) == "adam") {

    xml <- create_adam_xml(mdt, ver)

  }


  # Remove existing files
  if (file.exists(dpth))
    file.remove(dpth)

  if (file.exists(hpth))
    file.remove(hpth)

  if (file.exists(cpth))
    file.remove(cpth)


  # Create define.xml
  ret <- write_markup(xml, dpth)

  if (check) {

    msg <- check_xsd(dpth, ver, msg)

  }

  # Create HTML define file, if requested
  if (html) {

    msg <- write_HTML(dpth, hpth, ver, msg)

  }

  if (check) {

    write_check_report(cpth, msg)

  }

  # If requested, show check results in the viewer
  if (check & view) {


    view_check_report(msg)


  }


  return(msg)

}






# Write Markup ------------------------------------------------------------



#' @title Writes an markup document
#' @description The function writes an XML or HTML document to the file system.
#' @param x The markup to write.
#' @param file_path The path to write the markup file.
#' @return The full path to the file.
#' @noRd
write_markup <- function(x, file_path) {



  if (file.exists(file_path)) {

    file.remove(file_path)

  }

  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))

  }


  f <- file(file_path, open="w", encoding = "native.enc")

  writeLines(x, con = f, useBytes = TRUE)


  close(f)

  return(file_path)

}



# Write HTML --------------------------------------------------------------

#' @import xml2
#' @import xslt
#' @noRd
write_HTML <- function(xmlpth, htmlpth, ver, msg) {


  if (!file.exists(xmlpth)) {

    msg[length(msg) + 1] <- "ERROR: XML File '" %p% xmlpth %p% "' does not exist."
  } else {

    # Get external data directory
    extd <- system.file("extdata", package = "defineR")

    # Get path to xsd
    if (ver == "2.0.0") {
      xsl <- file.path(extd, ver, "cdisc-xsl/define2-0.xsl")

    } else {

      stop("Version '" %p% ver %p% "' not supported.")
    }

    if (!file.exists(xsl)) {

      msg[length(msg) + 1] <- "ERROR: XSL File '" %p% xsl %p% "' does not exist."
    } else {

      #browser()
      # Open file
      doc <- NULL
      res1 <- NULL
      res2 <- NULL

      res1 <- tryCatch({
        doc <- read_xml(xmlpth)
        NULL
      }, warning = function(cond) {

        return(c("WARNING: Reading " %p% xmlpth, "WARNING: " %p% as.character(cond)))

      }, error = function(cond) {
        return(c("ERROR: Reading " %p% xmlpth, "ERROR: " %p% as.character(cond)))
      })

      if (!is.null(res1))
        msg <- append(msg, res1)


      style <- NULL
      res2 <- tryCatch({
        style <- read_xml(xsl)
        NULL
      }, warning = function(cond) {
        return(c("WARNING: Reading " %p% xsl, "WARNING: " %p% as.character(cond)))
      }, error = function(cond) {
        return(c("ERROR: Reading " %p% xsl, "ERROR: " %p% as.character(cond)))
      })

      #browser()
      if (!is.null(res2))
        msg <- append(msg, as.character(res2))


      if (is.null(doc)) {

        msg[length(msg) + 1] <- "ERROR: Input XML is not readable."

      } else if (is.null(style)) {

        msg[length(msg) + 1] <- "ERROR: XSL style sheet is not readable."

      } else {

        # Run transformation
        html <- xml_xslt(doc, style)

        # Kill existing output file
        if (file.exists(htmlpth))
          file.remove(htmlpth)

        # Write new output file
        res <- write_html(html, htmlpth)

        # if (res == FALSE) {
        #
        #   msg <- append(msg, attr(res, "errors"))
        # }

      }

    }
  }

  return(msg)

}



# Utilities ---------------------------------------------------------------



