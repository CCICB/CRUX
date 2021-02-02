

#' icon_down_arrow
#' 
#' @param alignment One of 'left', 'right', 'center' or 'inherit'(string)
#' @param fontsize a valid css fontsize, e.g. 60px (string)
#'
#' @return centered down arrow fluidrow()
#' @export
#'
#' @family arrows
icon_down_arrow <- function(fontsize="60px", alignment = "center"){
  utilitybelt::assert_non_empty_string(fontsize)
  utilitybelt::assert_non_empty_string(alignment)
  
  style_ = paste0("font-size: ",fontsize,"; text-align: center;")
  fluidRow(style="display: grid",
           tags$i(class = "glyphicon glyphicon-arrow-down", style = style_)
  )
}