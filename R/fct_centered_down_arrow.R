

#' icon_down_arrow
#' 
#' @param alignment One of 'left', 'right', 'center' or 'inherit'(string)
#' @param fontsize a valid css fontsize, e.g. 60px (string)
#'
#' @return centered down arrow fluidrow()
#'
#'
#' @family arrows
icon_down_arrow <- function(fontsize="40px", alignment = "center", break_after = FALSE){
  utilitybeltassertions::assert_non_empty_string(fontsize)
  utilitybeltassertions::assert_non_empty_string(alignment)
  
  style_ = paste0("font-size: ",fontsize,"; text-align: center;")
  f <- fluidRow(style="display: grid",
           tags$i(class = "glyphicon glyphicon-arrow-down", style = style_)
  )
  
  if(break_after){
   return(tagList(f, br())) 
  }
  else
    return(f)
}

#' HTML_alert
#'
#' @param text Alert text (string)
#' @param status Boostrap status. One of primary,secondary,success,danger,warning,info,light,dark (string)
#'
#' @return html-flagged text (string")
#'
html_alert <- function(text, status="warning"){
  HTML(text = paste0(p(text, class=paste0("alert alert-", status)))) %>%
    return()
}
