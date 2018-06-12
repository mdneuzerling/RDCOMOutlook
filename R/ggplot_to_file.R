#' Convert a ggplot to a file
#'
#' Convert a ggplot to a file in a temporary location, and return the file path. 
#' The file name will be based on the variable name of the object, and will be 
#' given an extension that corresponds to the method by which the file was 
#' saved. The `device` (eg. "png" or "jpeg") is passed to the `ggsave` function.
#' @param gg A ggplot to be converted into a file.
#' @param file_format An argument passed onto the ggsave function that determines
#' how the plot is saved. It is recommended to use either "png" or "jpeg".
#' Defaults to "png".
#' @param file_name Sets the name of the saved file, without the extension. If
#' this isn't provided, the name of the `data` variable will be used. "." is a
#' forbidden `file_name` for this function, and is usually provided by a pipe 
#' (%>%); in this case, the file name will be changed to "data_", followed by a 
#' string of random numbers.
#' @keywords
 
ggplot_to_file <- function(gg, file_format = "png", file_name = NULL, ...) {
    
    # "jpg" is a common alternative to "jpeg"
    if (file_format == "jpg") {file_format <- "jpeg"}
    
    file_name <- if (!is.null(file_name)) {
        file_name
    } else { 
        deparse(substitute(gg)) # Name the file after the variable
    } 
    
    if (file_name == ".") {
        file_name <- paste0("data_", sample(1:999999, 1)) # random file name
    }
    
    file_path <- paste0(tempdir(), "/", file_name, ".", file_format)
    ggplot2::ggsave(file_path, plot = gg, device = file_format, ...)
    return(file_path)
}  
