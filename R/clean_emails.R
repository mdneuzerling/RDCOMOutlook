#' Clean and augment a list of emails
#' 
#' Returns a tibble in which the first column is the given list of emails (as
#' objects), and the other columns are common attributes of emails.
#' 
#' @param emails A list of DCOM email objects.

clean_emails <- function(emails) {
    
    get_attachment_names <- function(email) {
        number_attachments = email$Attachments()$Count()
        if (number_attachments == 0) {
            return("")
        }
        attachments <- purrr::map(
            seq(number_attachments), 
            function(x) email$Attachments(x)$FileName()
        )
        return(paste(attachments, sep = ", "))
    }
    
    augmented_emails <- tibble::tibble("email" = emails)
    
    augmented_emails <- dplyr::mutate(dplyr::rowwise(augmented_emails),
        subject = email$Subject(),
        received = as.POSIXct(email$ReceivedTime() * (24 * 60 * 60), 
                              origin="1899-12-30", tz="GMT"),
        from_name = email$SenderName(),
        from_email = email$SenderEmailAddress(),
        to_name = email$ReceivedByName(),
        cc_name = email$CC(),
        importance = email$Importance(),
        number_attachments = email$Attachments()$Count(),
        attachments = get_attachment_names(email),
        body = email$Body()
    )
    
    return(augmented_emails)

}
