
# Report Utilities --------------------------------------------------------


#' @import reporter
#' @noRd
output_report <- function(lst,
                          dir_name, file_name, out_type = 'HTML',
                          style = NULL,
                          titles = NULL, margins = 1, viewer = FALSE) {


  if (is.null(dir_name)) {
    stop("Path cannot be null")

  } else {


    targetDir <- dir_name

    if (dir.exists(targetDir) == FALSE)
      dir.create(targetDir)

    flnm <- file_name
  }

  rpt <- create_report(font = 'Arial', orientation = 'portrait', missing = "")
  rpt <- set_margins(rpt, top = margins, bottom = margins,
                     left = margins, right = margins)
  rpt <- page_footer(rpt, Sys.time(), right = "Page [pg] of [tpg]")



  if (!is.null(style)) {

    if (any(class(style) %in% "style_spec")) {

      rpt <- add_style(rpt, style = style)

    } else if (all(class(style) %in% "character")) {

      rpt <- add_style(rpt, theme = style)

    } else {

      stop("Report style parameter value is invalid.")
    }

  } else {

    rpt <- add_style(rpt, theme = "SASDefault")

  }


  for (i in seq_len(length(lst))) {
    dt <- lst[[i]]


    if (viewer == TRUE) {

      if ("CAT" %in% names(dt)) {
        lbl <-  attr(dt$CAT, "label")
        if (is.null(lbl))
          lbl <- "CAT"
        nms <- names(dt)
        names(dt) <- gsub("CAT", "stub", nms, fixed = TRUE)
      }

      if ("stub" %in% names(dt)) {
        lbl <- attr(dt$stub, "label")
        if (is.null(lbl))
          lbl <- ""

      }

      # Create table
      tbl <- create_table(dt, borders = c("all"))

      # Dedupe stub column if it exists
      if ("stub" %in% names(dt)) {

        wth <- rpt$char_width * nchar(lbl)
        tbl <- define(tbl, "stub", dedupe = TRUE, label =lbl, width = wth,
                      standard_eval = "true")

      }

    } else {

      # Create table
      tbl <- create_table(dt, borders = c("outside"))
    }

    #

    # Add titles
    if (!is.null(titles) & i == 1) {
      tbl <- titles(tbl, titles)
    }

    # Append table to report
    rpt <- add_content(rpt, tbl, align = 'center', page_break = FALSE)


  }


  ret <- c()

  # Deal with multiple output types
  for (ot in out_type) {

    fl <- flnm
    if (all(grepl(".", flnm, fixed = TRUE) == FALSE))
      fl <- paste0(flnm, ".", tolower(ot))

    pth <- file.path(targetDir, fl)


    if (utils::packageVersion("reporter") >= "1.3.6") {

      res <- write_report(rpt, file_path = pth,
                          output_type = ot, log = !viewer)
    } else {
      res <- write_report(rpt, file_path = pth, output_type = ot)

    }

    ret[length(ret) + 1] <- res$modified_path

  }


  return(ret)
}

#' @noRd
show_viewer <- function(path) {

  pth <- ""

  if (file.exists(path)) {

    pth <- path

    viewer <- getOption("viewer")

    if (!is.null(viewer))
      viewer(pth)
    else
      utils::browseURL(pth)

    ret <- TRUE

  }


  return(pth)

}







# Encoding ----------------------------------------------------------------




encodeMarkup <- function(vect) {

  if (length(vect) > 0) {

    if (all(is.na(vect)))
        ret <- ""
    else {
      splt <- strsplit(vect, split = "")

      ret <- c()

      for (i in seq_len(length(splt))) {

          ret[i] <- paste0(vencode(splt[[i]]), sep = "", collapse = "")

      }
    }
  } else {

   ret <- vect
  }

  return(ret)
}


vencode <- Vectorize(function(char) {

  ret <- char

  if (!is.na(char)) {

    if (char == ">")
      ret <- "&gt;"
    else if (char == "<")
      ret <- "&lt;"
    else if (char == "&")
      ret <- "&amp;"
    else if (char == "\n")
      ret <- e$end_char #"\r" #"&#xD;&#xA;"  #&#xD;
    else if (char == "\U2122")
      ret <- "&#8482;"
    else if (char == "\U00AE")
      ret <- "&#174;"
    else if (char == "\U00A9")
      ret <- "&#169;"
    else if (char == ";")
      ret <- paste0(";") # e$end_char

  } else {
    ret = ""
  }

  return(ret)

})
