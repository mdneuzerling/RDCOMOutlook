#' Convert a data frame or tibble to a HTML table
#'
#' Uses the xtable package to convert a data frame or tibble to HTML, so that it
#' can be inserted into the body of an email.
#' @param data A data frame or tibble to convert.
#' @param border A Boolean value which, if set to TRUE, puts a border around the
#' HTML output. 
#' Defaults to FALSE.
#' @keywords
#' @export

data_to_html <- function(data, border = FALSE) {

    if (ncol(dplyr::select_if(data, is.list))) {
        warning(paste0(
            "Unable to convert columns of type 'list' into HTML. ",
            "These columns will be ignored."
        ))
    }
    
    data <- dplyr::select_if(data, negate(is.list))
    
    print(
        xtable(data),
        type ='html',
        size = "large",
        include.rownames = FALSE,
        html.table.attributes = paste0(
            "border = '",
            as.numeric(border),
            "'"
        )
    )
}
