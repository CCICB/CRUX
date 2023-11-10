#' Link
#'
#' @param url URL
#' @param newtab shoule the link be opened in a new tab? (flag)
#' @param text display text for link
#'
#' @return list() with a shiny.tag class. Can convert to HTML string via as.character()

link <- function(url, newtab = TRUE, text){
  
  if(newtab){
    tags$a(
      href = url, 
      target="_blank",
      text
    )
  }
  else{
    tags$a(
      href = url, 
      text
    )
  }
}