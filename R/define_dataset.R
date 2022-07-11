

#' @title Creates a dataset definition
#' @param name The name of the dataset.
#' @returns The newly created dataset specification.
#' @export
define_dataset <- function(name) {

  ret <- structure(list(), class = c("ds_spec", "list"))



  return(ret)

}



#' @title Add a variable to a dataset definition
#' @param dst The dataset definition to which this variable will be added.
#' @param name The name of the variable.
#' @param label The label for this variable.
#' @param width The width of this variable.
#' @param type The data type for this variable.
#' @returns The dataset spec with the variable appended.
#' @export
add_variable <- function(dst, name, label = NULL,
                       width = NULL, type = NULL) {

  ret <- structure(list(), class = c("var_spec", "list"))



  dst$variables[[name]] <- ret

  return(dst)

}
