#' @title Creates Define XML documents
#'
#' @description The \strong{defineR} package contains functions to
#' create CDISC compliant define.xml documents suitable for
#' submission to the FDA. The package also contains related capabilities
#' to help create and manage metadata needed to create the define.xml.
#'
#' @details
#' The \strong{defineR} package uses spreadsheet-based metadata.  The package
#' can use the metadata to produce the define.xml, define.html, and a check
#' report.  It also has the capability to create a template to get
#' you started defining metadata for your project.
#'
#' The \strong{defineR} package is simple to use.  It contains only two functions:
#' \itemize{
#'   \item \code{\link{write_define}}: Creates a define.xml document and related files.
#'   \item \code{\link{write_metadata}}: Creates a metadata template.
#' }
#' See the [Getting Started](https://defineR.r-sassy.org/articles/defineR.html)
#' page for an overview of these functions.  Additional
#' information is available in the function references.
#'
#' @import glue
#' @import common
#' @import xml2
#' @import xslt
#' @docType package
#' @name defineR
NULL
