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
    send = FALSE
) {
    outlook_app <- RDCOMClient::COMCreate("Outlook.Application")
    
# When we create an email, it contains only the user's signature.
    outlook_mail <- outlook_app$CreateItem(0) # Check if the 0 is necessary
    outlook_mail$GetInspector()
    signature <- outlook_mail[["HTMLBody"]]
    
# Configure the parameters of the email using the arguments of the function.
    outlook_mail[["to"]] <- to
    outlook_mail[["cc"]] <- cc
    outlook_mail[["subject"]] <- subject
    
# Attach files (if any)
# Check that this doesn't mangle the attachment file names
    purrr::walk(attachments, ~function(x) {
        file_path <- to_file(x)
        outlook_mail[["Attachments"]]$Add(file_path)
        unlink(file_path)
    })
    
# Embed files in the body of the email, if requested
    purrr::walk(embeddings, ~function(x) {
        file_path <- to_file(x)
        if (is.data.frame(x)) {
            body <- data_to_html(x) # don't need the file path in this case
        } else if (is.ggplot(x)) {
            Email[["Attachments"]]$Add(file_path)    
            body <- paste0(
                body, 
                "<img src='cid:",
                basename(file_path),
                "'",
                #"width = '400' height = '400'", # Unsure if size needs to be specified
                ">"
            ) 
        } else {
            outlook_mail[["Attachments"]]$Add(file_path)
        }
        unlink(file_path)
    })
    
# Put the pieces of the email body together, including css and signature.
# Don't forget the paragraph tags!
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


