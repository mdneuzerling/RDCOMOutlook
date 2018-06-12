#' Convert a data frame or tibble to a file
#'
#' Convert a data frame or tibble to a file in a temporary location, and return
#' the file path. The file name will be based on the variable name of the
#' object, and will be given an extension that corresponds to the method by
#' which the file was saved. The `file_format` can be specified as "csv" 
#' (comma-separated file) or "tsv"/"txt" (tab-delimited). Support for Excel 
#' output is planned.
#' @param data A data frame or tibble to be converted into a file.
#' @param file_format A file_format which will determine how the data is saved
#' and the extension of the resulting file. Currently supports "csv" (comma-
#' separated), "tsv" (tab-separated), or "xlsx"/"excel". 
#' Defaults, "csv" (comma-separated)
#' @param file_name Sets the name of the saved file, without the extension. If
#' this isn't provided, the name of the `data` variable will be used. "." is a
#' forbidden `file_name` for this function, and is usually provided by a pipe 
#' (%>%); in this case, the file name will be changed to "data_", followed by a 
#' string of random numbers.
#' @keywords

data_to_file <- function(data, file_format = "csv", file_name = NULL, ...) {

# Excel files are saved as .xlsx    
    if (file_format == "excel") {file_format <- "xlsx"}
    
    if (is.null(file_name)) {
        file_name <- deparse(substitute(data))
    } 
    
    if (file_name == ".") {
        file_name <- paste0("data_", sample(1:999999, 1)) # random file name
    }
    
# Based on the cleansed `file_format`, determine how to save the data to a file
    file_path <- paste0(tempdir(), "/", file_name, ".", file_format)
    if (file_format == "csv") {
        readr::write_csv(data, path = file_path, ...)
    } else if (file_format == "tsv") {
        readr::write_tsv(data, path = file_path, ...)
    } else if (file_format == "xlsx") {
        writexl::write_xlsx(data, path = file_path, ...)
    } else {
        stop(paste0("Don't know how to write to ", file_format, ". ",
                    "Can only write to 'csv', 'tsv' or 'xlsx'/'excel'."))
    }
    
    return(file_path)
} 
