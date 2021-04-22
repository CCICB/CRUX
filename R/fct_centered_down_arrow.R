

#' icon_down_arrow
#' 
#' @param alignment One of 'left', 'right', 'center' or 'inherit'(string)
#' @param fontsize a valid css fontsize, e.g. 60px (string)
#'
#' @return centered down arrow fluidrow()
#' @export
#'
#' @family arrows
icon_down_arrow <- function(fontsize="40px", alignment = "center"){
  utilitybelt::assert_non_empty_string(fontsize)
  utilitybelt::assert_non_empty_string(alignment)
  
  style_ = paste0("font-size: ",fontsize,"; text-align: center;")
  fluidRow(style="display: grid",
           tags$i(class = "glyphicon glyphicon-arrow-down", style = style_)
  )
}

#' HTML_alert
#'
#' @param text Alert text (string)
#' @param status Boostrap status. One of primary,secondary,success,danger,warning,info,light,dark (string)
#'
#' @return html-flagged text (string")
#' @export
html_alert <- function(text, status="warning"){
  HTML(text = paste0(p(text, class=paste0("alert alert-", status)))) %>%
    return()
}