#' HTML tag to embed an image email attachment with a CID source
#'
#' Generate a HTML tag that uses a CID source to display an image. This is used
#' in emails for images, in which an attached image can be embedded into the
#' email body. If the image is a png, jpg or bmp, the file will be scaled down
#' to the provided `max_height` and `max_width`, preserving the aspect ratio.
#' If the file is a gif, tiff or webp, the file will be scaled down without
#' preserving the aspect ratio.
#'
#' @param file_path Path of an existing image file.
#' @param max_height The maximum height of the image, in pixels. The image will 
#' be scaled down to the largest size that meets this contraint whilst 
#' preserving the aspect ratio of the image.
#' @param max_width The maximum width of the image, in pixels. The image will 
#' be scaled down to the largest size that meets this contraint whilst 
#' preserving the aspect ratio of the image. 
#' @keywords
#' @export

embed_image_cid <- function(
    file_path,
    max_height = 800,
    max_width = 800
) { 
    if (!file.exists(file_path)) {
        stop(paste0(file_path, " is not a valid file path, or does not exist."))
    }
    
    if (is.null(image_format(file_path))) {
        stop(paste0(
            file_path, 
            " is not an image (accepted formats: ",
            "png, jpg, bmp, gif, tiff or webp)"))   
    } else if (image_format(file_path) %in% c("png", "jpg", "bmp")) {
        image_details <- dim(readbitmap::read.bitmap(file_path))
        original_height <- image_details[1]
        original_width <- image_details[2]
    
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
    } else {
        scaled_height <- max_height
        scaled_width <- max_width
    }
    
        cid_tag <- paste0(
            "<img src='cid:", basename(file_path),"' ",
            "width = '", scaled_width, "' ",
            "height = '", scaled_height, "'",
            ">"
        )
    
    return(cid_tag)
}
