

# Write Define ------------------------------------------------------------



#' @title Writes an markup document
#' @description The function writes an XML or HTML document to the file system.
#' @param x The markup to write.
#' @param file_path The path to write the markup file.
#' @return The full path to the file.
#' @seealso \code{\link{create_sdtm_xml}} and \code{\link{create_adam_xml}}
#' function to generate XML.
#' @export
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



# Utilities ---------------------------------------------------------------

unescape_xml <- function(str){
  xml2::xml_text(xml2::read_xml(paste0("<x>", str, "</x>")))
}

unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}


