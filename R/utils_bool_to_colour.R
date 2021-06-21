#' Map logical to colour
#'
#' Hex codes
#'
#' @param boolean TRUE/FALSE (boolean)
#' @param colour_if_true  colour to return if true (string)
#' @param colour_if_false colour to return if false (string)
#'
#' @return colour code (string)
#'
#'
#' @examples
#' bool_to_colour(1==1)
bool_to_colour <- function(boolean, colour_if_true = "#69C776", colour_if_false = "#ED7676"){
  assertthat::assert_that(assertthat::is.flag(boolean), msg = utilitybelt::fmterror("bool_to_colour: boolean must be a TRUE/FALSE flag, not a ", class(boolean)))
  
  if(boolean)
    return(colour_if_true)
  else
    return(colour_if_false)
}
