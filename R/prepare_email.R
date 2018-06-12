#' Prepare an outgoing Outlook email
#'
#' Prepare an email in Microsoft Outlook, either sending the email immediately
#' or causing the window to appear on the user's screen. This function prepares
#' emails with DCOM, which is only available in Windows, and is exposed to R
#' through the RDCOMClient package. Attachments are supported, as well as 
#' "embeddings"---a vector of data frames, tibbles or ggplot objects that are
#' embedded into the body of the outgoing email. It is recommended that 
#' Microsoft Outlook be open in the background before using this function.
#' 
#' No argument is mandatory; running prepare_email() will cause a blank 
#' composing window to appear. CSS can be entered with the css argument, but
#' note that most CSS is disabled by Microsoft Outlook.
#' 
#' @param embeddings An optional list of all objects to be embedded into the 
#' email body or, in the case of a file path, the file will be attached instead, 
#' to allow files to be piped into the function. ggplots will be converted to 
#' images and data frames/tibbles will be converted to html.
#' @piaram body A html body for the email. This does not need to be enclosed in
#' paragraph tags.
#' Defaults to an empty string.
#' @param to A string containing the email addresses, separated by
#' semicolons, which are to receive the email.
#' Defaults to an empty string.
#' @param cc A string containing the email addresses, separated by
#' semicolons, which are to be CC'd in on the email.
#' Defaults to an empty string.
#' @param subject A string containing the subject line of the email.
#' Defaults to an empty string.
#' @param attachments A ggplot, data frame, tibble, or file path which will be
#' attached to the email (converting to a file if necessary), or a vector 
#' containing any combination of these things. ggplots will be converted to 
#' images and data frames/tibbles will be converted to delimited files. 
#' If providing a list of ggplot or data frames/tibbles, it it best to provide
#' a named list. Otherwise, the attachments will be named attachment_1, 
#' attachment_2, etc.
#' @param css An optional string of CSS that will modify the HTML body of the 
#' email. Note that only some CSS affects Outlook emails. 
#' @param send A Boolean value which, if set to TRUE, will send
#' the email immediately. If set to FALSE, will display the email on the user's
#' screen, but not send.
#' Defaults to FALSE.
#' @keywords
#' @export

prepare_email <- function(
    embeddings = NULL,
    body = "",
    to = "",
    cc = "",
    subject = "",
    attachments = NULL,
    css = "",
    send = FALSE,
    data_file_format = "csv",
    image_file_format = "png"
) {
    
# Store names of provided attachments and embeddings
# We will use this later in the case in which only one embedding or attachment
# is provided.
    embeddings_argument_name <- deparse(substitute(embeddings))
    attachments_argument_name <- deparse(substitute(attachments))
    
    outlook_app <- RDCOMClient::COMCreate("Outlook.Application")

# We want the embeddings and attachments to be lists, even if they are lists of
# just one element. List behaviour is inconsistent in R, so we're forced to use
# this lengthy ifelse block.
    make_list <- function(x) {
        if (is.null(x)) {
            x
        } else if (is.ggplot(x)) { # ggplots are lists
            list(x)
        } else if (is.data.frame(x)) { # data frames are lists
            list(x)  
        } else if (is.list(x)) { 
            x
        } else if (is.vector(x)) { # vectors are not lists
            as.list(x)
        } else {
            list(x) # single item case
        }
    }
    embeddings <- make_list(embeddings)
    attachments <- make_list(attachments)
    
# If only one attachment or embedding is provided, we can use the argument names
# we stored earlier to rename the single item in the list.
# This is only done if a named list isn't provided.
    if (is.null(names(embeddings)) & length(embeddings) == 1) {
        names(embeddings) <- embeddings_argument_name
    }
    if (is.null(names(attachments)) & length(attachments) == 1) {
        names(attachments) <- attachments_argument_name
    }

# If an unnamed list is provided, we give temporary names to avoid conflicts
    if (!is.null(attachments) & is.null(names(attachments))) {
        names(attachments) <- paste0("attachment_", seq_along(attachments))
    }
    if (!is.null(embeddings) & is.null(names(embeddings))) {
        names(embeddings) <- paste0("embedding_", seq_along(embeddings))
    }
    
# When we create an email, it contains only the user's signature.
    outlook_mail <- outlook_app$CreateItem(0) # Check if the 0 is necessary
    outlook_mail$GetInspector()
    signature <- outlook_mail[["HTMLBody"]]
    
    outlook_mail[["to"]] <- to
    outlook_mail[["cc"]] <- cc
    outlook_mail[["subject"]] <- subject
    
# Attach files (if any)
    purrr::walk(seq_along(attachments), function(x) {
        attachment <- attachments[[x]]
        attachment_name <- names(attachments)[[x]]
        file_path <- to_file(
            attachment,
            data_file_format = data_file_format,
            image_file_format = image_file_format,
            file_name = attachment_name
        )
        outlook_mail[["Attachments"]]$Add(file_path)
    })
    
# Embed files in the body of the email, if requested
    purrr::walk(seq_along(embeddings), function(x) {
        embedding <- embeddings[[x]]
        embedding_name <- names(embeddings)[[x]]
        if (is.data.frame(embedding)) {
            body <<- paste0(body, data_to_html(embedding)) 
        } else if (ggplot2::is.ggplot(embedding)) {
            file_path <- to_file(
                embedding,
                data_file_format = data_file_format,
                image_file_format = image_file_format,
                file_name = embedding_name
            )
            outlook_mail[["Attachments"]]$Add(file_path)
            body <<- paste0(
                body,
                embed_image_cid(file_path)
            )
            unlink(file_path)
        } else { # If a file can't be embedded, attach it
            file_path <- to_file(embedding) # validates file
            outlook_mail[["Attachments"]]$Add(file_path)
        }
    })
    
# Don't forget the paragraph tags in the body of the email.
    outlook_mail[["HTMLBody"]] <- paste0(
        css, 
        "<p>",
        body, 
        signature,
        "</p>"
    )
    
# The `Display` causes the email to pop up on the user's screen.
# The `Send` function causes it to be distributed immediately.
    if (send) {
        outlook_mail$Send()
    } else {
        outlook_mail$Display()
    }

}


