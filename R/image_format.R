#' Return the image format of an arbitrary file
#' 
#' Determines if a given file is a png, jpg, bmp, gif, tiff or webp image,
#' based on the file signature. If the file is none of these, NULL is returned.
#' If the file does not exist, an error is thrown.
#' 
#' @param file_path File path of a file that may or may not be an image.

image_format <- function(file_path) {

    if (!file.exists(file_path)) {
        stop(paste0(file_path, " is not a valid file path, or does not exist."))
    }
    
    file_header <- readBin(file_path, "raw", n = 12)
    
    string_to_raw <- function(string) {
        string_vector <- unlist(strsplit(string, " "))
        as.raw(strtoi(string_vector, 16))
    }
    
    # Reference headers
    png_header <- string_to_raw("89 50 4e 47 0d 0a 1a 0a")
    jpg_header <- string_to_raw("FF D8 FF")
    bmp_header <- string_to_raw("42 4D")
    gif_header <- string_to_raw("47 49 46")
    tiff_header_little_endian <- string_to_raw("49 49 2A 00")
    tiff_header_big_endian <- string_to_raw("4D 4D 00 2A")
    webp_start <- string_to_raw("52 49 46 46")
    webp_end <- string_to_raw("57 45 42 50") # separated by ?? ?? ?? ??
    
    format <- if (identical(file_header[1:8], png_header)) {
        "png"
    } else if (identical(file_header[1:3], jpg_header)) {
        "jpg"
    } else if (identical(file_header[1:2], bmp_header)) {
        "bmp"
    } else if (identical(file_header[1:3], gif_header)) {
        "gif"
    } else if (identical(file_header[1:4], tiff_header_little_endian)) {
        "tiff"
    } else if (identical(file_header[1:4], tiff_header_big_endian)) {
        "tiff"
    } else if (identical(file_header[1:4], webp_start) &
               identical(file_header[9:12], webp_end)) {
        "webp"
    } else {
        NULL
    }
    
    return(format)
    
}
