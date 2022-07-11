

# Write Define ------------------------------------------------------------



#' @title Writes a define XML document to the file system
#' @param xml The xml to write.
#' @param file_path The path to write the xml file.
#' @import stringi
#' @export
write_xml <- function(xml, file_path) {



  if (file.exists(file_path)) {

    file.remove(file_path)

  }

  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path))

  }


  f <- file(file_path, open="a", encoding = "native.enc")

  writeLines(xml, con = f, useBytes = TRUE)


  close(f)

  return(file_path)

}



# Utilities ---------------------------------------------------------------


