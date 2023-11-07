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