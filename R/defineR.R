#' @title Creates Define XML documents
#'
#' @description The \strong{defineR} package contains functions to
#' create CDISC compliant define.xml documents suitable for
#' submission to the FDA. The package also contains related capabilities
#' to help create and manage metadata needed to create the define.xml.
#'
#' @details
#' The \strong{logr} package helps create log files for R scripts.  The package
#' provides easy logging, without the complexity of other logging systems.
#' It is
#' designed for analysts who simply want a written log of the their program
#' execution.  The package is designed as a wrapper to
#' the base R \code{sink()} function.
#'
#' @section How to use:
#' There are only three \strong{logr} functions:
#' \itemize{
#'   \item \code{\link{log_open}}
#'   \item \code{\link{log_print}}
#'   \item \code{\link{log_close}}
#' }
#' The \code{log_open()} function initiates the log.  The
#' \code{log_print()} function prints an object to the log.  The
#' \code{log_close()} function closes the log.  In normal situations,
#' a user would place the call to
#' \code{log_open} at the top of the program, call \code{log_print()}
#' as needed in the
#' program body, and call \code{log_close()} once at the end of the program.
#'
#' Logging may be controlled globally using the options "logr.on" and
#' "logr.notes".  Both options accept TRUE or FALSE values, and control
#' log printing or log notes, respectively.
#'
#' See function documentation for additional details.
#' @import glue
#' @import common
#' @import xml2
#' @docType package
#' @name defineR
NULL
