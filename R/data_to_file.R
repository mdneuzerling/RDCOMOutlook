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
#' and the extension of the resulting file.
#' Defaults to "csv" (comma-separated file)
#' @keywords

data_to_file <- function(data, file_format = "csv", ...) {
    # Convert file_format to lower-case and remove leading periods
    file_format <- tolower(gsub("^\\.*", "", file_format))
    
    # Cleanse the `file_format` to match the file extension
    file_format <- if (file_format == "tsv") { # tab-separated 
        "txt"
    } else if (file_format %in% c("excel", "xlsx", "xlsm", "xls")) {
        "xlsx"
    } else {
        file_format
    }
    
    # Based on the cleansed `file_format`, determine how to save the data to a file
    file_path <- paste0(tempdir(), "/", deparse(substitute(data)), ".", file_format)
    if (file_format == "csv") {
        write_csv(...)
    } else if (file_format == "txt") {
        write_tsv(...)
    } else if (file_format == "xlsx") {
        stop("No method implemented for writing data to excel files")
    } else {
        stop(paste0("Don't know how to write to ", file_format))
    }
    
    return(file_path)
}