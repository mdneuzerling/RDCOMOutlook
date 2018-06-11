#' HTML tag to embed an image email attachment with a CID source
#'
#' Generate a HTML tag that uses a CID source to display an image. This is used
#' in emails for images, in which an attached image can be embedded into the
#' email body.
#'
#' @param file_path Path of an existing image file.
#' @param max_height The maximum height of the image, in pixels. The image will 
#' be scaled down to the largest size that meets this contraint whilst 
#' preserving the aspect ratio of the image.
#' @param max_width The maximum width of the image, in pixels. The image will 
#' be scaled down to the largest size that meets this contraint whilst 
#' preserving the aspect ratio of the image. 
#' @keywords

embed_image_cid <- function(
    file_path,
    max_height = 800,
    max_width = 800
) { 
    if (!file.exists(file_path)) {
        stop(paste0(file_path, " is not a valid file path, or does not exist."))
    }

    file_header <- readBin(file_path, "raw", n = 8)

    # Reference headers
    png_header <- as.raw(strtoi(c("89", "50", "4e", "47", "0d", "0a", "1a", "0a"), 16))
    jpg_header <- as.raw(strtoi(c("FF", "D8"), 16))
    bmp_header <- as.raw(strtoi(c("42", "4D"), 16))
    gif_header <- as.raw(strtoi(c("47", "49", "46"), 16))
    
    format <- if (identical(file_header, png_header)) {
        "png"
    } else if (identical(file_header[1:2], jpg_header)) {
        "jpg"
    } else if (identical(file_header[1:2], bmp_header)) {
        "bmp"
    } else if (identical(file_header[1:3], gif_header)) {
        "gif"
    } else {
        "unknown"
    }
    
    if (format %in% c("png", "jpg", "bmp")) {
        image_details <- dim(readbitmap::read.bitmap(file_path))
        original_height <- image_details[1]
        original_width <- image_details[2]
    }
    
    # Only reduce, never enlarge
    # Scale height then width but keep the aspect ratio in tact
    height_scaling_factor <- min(1, max_height / original_height)   
    scaled_height <- height_scaling_factor * original_height
    scaled_width <- height_scaling_factor * original_width
    width_scaling_factor <- min(1, max_width / scaled_width)
    scaled_height <- width_scaling_factor * scaled_height
    scaled_width <- width_scaling_factor * scaled_width    
    
    # Can only use an integral number of pixels
    scaled_height <- floor(scaled_height)
    scaled_width <- floor(scaled_width)
    
    cid_tag <- paste0(
        "<img src='cid:", basename(file_path),"' ",
        "width = '", scaled_width, "' ",
        "height = '", scaled_height, "'",
        ">"
    )
    
    return(cid_tag)
}
