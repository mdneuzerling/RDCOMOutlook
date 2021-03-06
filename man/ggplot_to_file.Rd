% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_to_file.R
\name{ggplot_to_file}
\alias{ggplot_to_file}
\title{Convert a ggplot to a file}
\usage{
ggplot_to_file(gg, file_format = "png", file_name = NULL, ...)
}
\arguments{
\item{gg}{A ggplot to be converted into a file.}

\item{file_format}{An argument passed onto the ggsave function that determines
how the plot is saved. It is recommended to use either "png" or "jpeg".
Defaults to "png".}

\item{file_name}{Sets the name of the saved file, without the extension. If
this isn't provided, the name of the `data` variable will be used. "." is a
forbidden `file_name` for this function, and is usually provided by a pipe 
(%>%); in this case, the file name will be changed to "data_", followed by a 
string of random numbers.}
}
\description{
Convert a ggplot to a file in a temporary location, and return the file path. 
The file name will be based on the variable name of the object, and will be 
given an extension that corresponds to the method by which the file was 
saved. The `device` (eg. "png" or "jpeg") is passed to the `ggsave` function.
}
