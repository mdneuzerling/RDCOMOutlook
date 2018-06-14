#' Save an email's attachments
#' 
#' Saves the emails from a given mailItem DCOM object (from RDCOMClient) to a
#' given folder. If no folder is specified, a temporary directory will be used.
#' This function will return a vector of all file paths to which an attachment
#' has been saved. The attachments will keep their file names as they are 
#' displayed in the email. Embedded images, such as those that are present in
#' email signatures, will also be saved.
#' 
#' @param email A case-sensitive search query.
#' @param target_dir The folder in which to save the attachments. If no folder
#' is specified, a temporary directory will be used.
#' @export

save_attachments <- function(email, target_dir = tempdir()) {

    saved_attachments <- c()
    
    number_attachments <- email$Attachments()$Count()
    if (number_attachments == 0) {
        return(c())    
    }
    
    purrr::walk(
        seq(number_attachments),
        function(x) {
            attachment <- email$Attachments(x)
            attachment_location <- paste0(target_dir, "/", attachment$FileName())
            attachment$SaveAsFile(attachment_location)
            saved_attachments <<- c(
                saved_attachments, 
                normalizePath(attachment_location)
            )
        }
    )
    
    return(saved_attachments)

}
