#' Convert an object to a file
#'
#' Convert a ggplot, data frame or tibble to a file in a temporary location, and 
#' return the file path. The file name will be based on the variable name of the
#' object, and will be given an extension that corresponds to the method by
#' which the file was saved. If a file path corresponding to an existing file 
#' is given, then that file path is returned.
#' @param obj Either a ggplot, data frame, tibble, or valid file path.
#' @param data_file_format Preferred format for data frames/tibbles when 
#' saved to a file.
#' @param image_file_format Preferred format for ggplots and images when 
#' saved to a file.
#' @param file_name Sets the name of the saved file, without the extension. If
#' this isn't provided, the name of the `data` variable will be used. "."
#' @param col_names Determines if column names (headers) are to be included.
#' Defaults to TRUE.

to_file <- function(
    obj, 
    data_file_format = "csv", 
    col_names = TRUE,
    image_file_format = "png",
    file_name = NULL
) {
    
    if (is.null(file_name)) {
        file_name <- deparse(substitute(obj))
    } 
    
    if (ggplot2::is.ggplot(obj)) {
        file_path <- ggplot_to_file(
            obj, 
            file_name = file_name,
            file_format = image_file_format
        )
    } else if (is.data.frame(obj)) {
        file_path <- data_to_file(
            obj, 
            file_name = file_name,
            file_format = data_file_format,
            col_names = col_names
        )
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
