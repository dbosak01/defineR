

#' @title Creates a define XML document
#' @export
create_document <- function() {


  ret <- structure(list(), class = c("doc_spec", "list"))



  return(ret)

}





#' @title Adds a dataset to define XML document
#' @param doc A definition object.
#' @param ds A dataset definition.
#' @export
add_dataset <- function(doc, ds) {



}



#' @title Print a define document specification
#' @param x Object to print.
#' @param ... Follow on parameters.
#' @param verbose Whether to print in verbose mode.
#' @import crayon
#' @export
print.doc_spec <- function(x, ..., verbose = FALSE) {



}
