#' Convert an object to a file
#'
#' Convert a ggplot, data frame or tibble to a file in a temporary location, and 
#' return the file path. The file name will be based on the variable name of the
#' object, and will be given an extension that corresponds to the method by
#' which the file was saved. If a file path corresponding to an existing file 
#' is given, then that file path is returned.
#' @param 
#' @keywords

to_file <- function(obj, ...) {
    if (ggplot2::is.ggplot(obj)) {
        file_path <- ggplot_to_file(obj, file_name = deparse(substitute(obj)), ...)
    } else if (is.data.frame(obj)) {
        file_path <- data_to_file(obj, file_name = deparse(substitute(obj)), ...)
    } else if (file.exists(obj)) {
        file_path <- obj
    } else {
        stop(paste0(
            deparse(substitute(obj)), " ",
            "is not a ggplot, data frame, tibble or valid file path. ",
            "Check that the file exists."
        ))
    }
    return(file_path)
}
