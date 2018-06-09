#' Convert a ggplot to a file
#'
#' Convert a ggplot to a file in a temporary location, and return the file path. 
#' The file name will be based on the variable name of the object, and will be 
#' given an extension that corresponds to the method by which the file was 
#' saved. The `device` (eg. "png" or "jpeg") is passed to the `ggsave` function.
#' @param gg A ggplot to be converted into a file.
#' @param device An argument passed onto the ggsave function that determines
#' how the plot is saved.
#' Defaults to "png".
#' @keywords
 
ggplot_to_file <- function(gg, device = "png", ...) {
    # Convert device to lower-case and remove leading periods
    file_format <- tolower(gsub("^\\.*", "", device))
    file_path <- paste0(tempdir(), "\\", deparse(substitute(gg)), ".", device)
    ggplot2::ggsave(file_path, plot = gg, device = device, ...)
    return(file_path)
}  
