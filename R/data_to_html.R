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
