#' Search for emails
#'
#' Searches for all emails satisfying a search term. Searches can be performed
#' in one of several email attributes, such as subject or sender name. Searches
#' are always case sensitive. The DCOM search method used is asynchronous, in 
#' that R will not wait for the search to complete before continuing. We pause
#' R for a given amount of time (default is 10 seconds) to allow the search to
#' complete. See Details for more information.
#' 
#' There is a package for handling the AdvancedSearchComplete, RDCOMEvents, but
#' it is not easily available. The proper way to implement this search would be
#' to wait for the event to return that the search is complete, and then 
#' continue.
#' 
#' @param search_term A case-sensitive search query.
#' @param folder A folder in which to search.
#' Defaults to "Inbox".
#' @param scope The scope of the search: one of "subject", "body", 
#' "attachment_names", "from_name", "from_email", "cc_name", or "to_name".
#' @param partial_match If set to TRUE, will allow any number of characters to
#' appear either side of the search term. Does not apply to `received_after`.
#' Defaults to TRUE.
#' @param search_subfolders If set to TRUE, will search subfolders below the
#' specified `folder`.
#' Defaults to TRUE.
#' @param search_time The amount of time, in seconds, to wait for the search to
#' complete.
#' @keywords
#' @export

search_emails <- function(
    search_term, 
    folder = "Inbox",
    scope = "subject", 
    partial_match = TRUE, 
    search_subfolders = TRUE,
    search_time = 10 #seconds
    ) {

    outlook_app <- COMCreate("Outlook.Application")
    
    scope <- if (scope == "subject") {"subject"} 
    else if (scope == "body") {"textdescription"} 
    else if (scope == "attachment_names") {"attachmentfilename"}
    else if (scope == "from_name") {"fromname"} 
    else if (scope == "from_email") {"fromemail"} 
    else if (scope == "cc_name") {"displaycc"} 
    else if (scope == "to_name") {"displayto"} 
    else if (scope == "received_after") {"datereceived"}
    else {
        stop(paste0(
            "Unknown scope for search: ", scope, ". ",
            "Use one of subject, body, attachment_names, from_name, ",
            "from_email, cc_name, or to_name."
        ))
    }
    
    search_query <- paste0(
        "urn:schemas:httpmail:",
        scope,
        if (partial_match) {
            " LIKE '%"
        } else if (scope == "received_after") {
            " > '"
        } else {
            " = '"
        },
        search_term,
        if (!partial_match | scope == "received_after") {
            "'"
        } else {
            "'"
        }
    )

    search <- outlook_app$AdvancedSearch(
        folder,
        search_query,
        search_subfolders
    )
    
    Sys.sleep(search_time)
    
    results <- search$results()
    number_results <- results$Count()
    emails <- purrr::map(seq(number_results), function(x) results$Item(x))

    clean_emails(emails)

}
