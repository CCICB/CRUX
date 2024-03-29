% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_text_is_valid_input.R
\name{text_is_non_zero_string}
\alias{text_is_non_zero_string}
\title{Input is non-empty string}
\usage{
text_is_non_zero_string(text, test_custom_function = NULL)
}
\arguments{
\item{text}{input text to assert is a string (string)}

\item{test_custom_function}{a function which takes the value of the textInput and returns TRUE/FALSE depending on whether it meets expectations. If supplied, textInput value must be a non-emptystring AND pass the custom function's test(function)}
}
\value{
True if valid text is found in. FALSE if it is not (flag)
}
\description{
must be in server
}
