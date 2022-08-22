

# XSD Check ---------------------------------------------------------------



#' @import xml2
check_xsd <- function(pth, ver, msg) {


  if (!file.exists(pth)) {

    msg[length(msg) + 1] <- "ERROR: XML File '" %p% pth %p% "' does not exist."
  } else {

    # Get external data directory
    extd <- system.file("extdata", package = "defineR")

    if (is.null(options()[["defineR.xsd"]]) == FALSE) {

      xsd <- options("defineR.xsd")[[1]]

    } else {


      # Get path to xsd
      if (ver == "2.0.0") {
        xsd <- file.path(extd, ver, "cdisc-define-2.0/define2-0-0.xsd")

      } else {

        stop("Version '" %p% ver %p% "' not supported.")
      }
    }

    if (!file.exists(xsd)) {

      msg[length(msg) + 1] <- "ERROR: XSD File '" %p% xsd %p% "' does not exist."
    } else {

      #browser()
      # Open file
      doc <- NULL
      res1 <- NULL
      res2 <- NULL

      res1 <- tryCatch({
        doc <- read_xml(pth)
        NULL
      }, warning = function(cond) {

        return(c("WARNING: Reading " %p% pth, "WARNING: " %p% as.character(cond)))

      }, error = function(cond) {
        return(c("ERROR: Reading " %p% pth, "ERROR: " %p% as.character(cond)))
      })

      if (!is.null(res1))
        msg <- append(msg, res1)


      schema <- NULL
      res2 <- tryCatch({
        schema <- read_xml(xsd)
        NULL
      }, warning = function(cond) {
        return(c("WARNING: Reading " %p% xsd, "WARNING: " %p% as.character(cond)))
      }, error = function(cond) {
        return(c("ERROR: Reading " %p% xsd, "ERROR: " %p% as.character(cond)))
      })

      #browser()
      if (!is.null(res2))
        msg <- append(msg, as.character(res2))


      if (is.null(doc)) {

        msg[length(msg) + 1] <- "ERROR: Input XML is not readable."

      } else if (is.null(schema)) {

        msg[length(msg) + 1] <- "ERROR: XSD schema is not readable."

      } else {

        res <- xml_validate(doc, schema)

        if (res == FALSE) {

          msg <- append(msg, attr(res, "errors"))

          msg <- remove_messages(msg)
        }

      }

    }
  }

  return(msg)

}

# These are persistent errors in ADAM 2.0 schema.  No reason to pass them
# on to user or explain them.  Makes more sense to just take them out of
# the check results.
remove_messages <- function(msg) {


  if (length(msg) > 0) {

    rmvect <- c()

    m1 <- paste0("Element '{http://www.w3.org/2001/XMLSchema}import': ",
    "Skipping import of schema located at ")

    m1pos <- grep(m1, msg, fixed = TRUE)
    if (!is.na(m1pos)) {

      rmvect[length(rmvect) + 1] <- m1pos
    }


    m2 <- paste0("Element '{http://www.cdisc.org/ns/odm/v1.3}CodeListItem': ",
    "Duplicate key-sequence ['1'] in unique identity-constraint ",
    "'{http://www.cdisc.org/ns/odm/v1.3}UC-CL-2'.")

    m2pos <- match(m2, msg)
    if (!is.na(m2pos)) {

      rmvect[length(rmvect) + 1] <- m2pos
    }

    m3 <- paste0("Element '{http://www.cdisc.org/ns/arm/v1.0}AnalysisResultDisplays': ",
    "This element is not expected. Expected is ( {http://www.cdisc.org/ns/def/v2.0}leaf ).")

    m3pos <- match(m3, msg)
    if (!is.na(m3pos)) {

      rmvect[length(rmvect) + 1] <- m3pos
    }


    msg <- msg[!seq(1, length(msg)) %in% rmvect]

  }

  return(msg)

}


# Check Metadata ----------------------------------------------------------

check_metadata <- function(lst, ver, v5flg, msg) {


  if (length(lst) == 0) {

    msg[length(msg) + 1] <- "ERROR: No metadata to check."

  } else {

    nms <- names(lst)

    vnms <- c()
    if (ver == "2.0.0")
      vnms <- c("DEFINE_HEADER_METADATA", "TOC_METADATA", "VARIABLE_METADATA",
                "VALUELEVEL_METADATA", "COMPUTATION_METHOD", "CODELISTS",
                "WHERE_CLAUSES", "COMMENTS", "EXTERNAL_LINKS")

    if (!all(vnms %in% nms)) {

      msg <- append(msg, paste0("WARNING: Metadata missing for ", vnms[!vnms %in% nms]))

    }

  }


  return(msg)

}


# Check Report ------------------------------------------------------------

write_check_report <- function(cpth, msg, rtype = "PDF", viewer = FALSE) {

  if (file.exists(cpth))
    file.remove(cpth)

  if (length(msg) == 0) {

    dt <- data.frame("ID" = 1, Messages = "No check messages to report.",
                     stringsAsFactors = FALSE)

  } else {

    dt <- data.frame("ID" = seq(1, length(msg)), Messages = msg,
                     stringsAsFactors = FALSE)

  }

  if (ncol(dt) > 2) {

   stop("Error writing report. Columns more than 2")
  }

  #attr(dt$Messages, "width") <- 5

  mrg <- 1
  if (viewer)
    mrg <- .5

  res <- output_report(list(dt), dirname(cpth), basename(cpth),
                       titles = "Define XML Check Report Results",
                       out_type = rtype, margins = mrg, viewer = viewer)

  return(res)

}



view_check_report <- function(msg) {

  tmp <- tempfile(fileext = ".html")

  res <- write_check_report(tmp, msg, rtype = "HTML", viewer = TRUE)


  ret <- show_viewer(tmp)


  return(ret)
}

