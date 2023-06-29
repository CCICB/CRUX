#' Input is non-empty string
#'
#' must be in server
#'
#' @param test_custom_function a function which takes the value of the textInput and returns TRUE/FALSE depending on whether it meets expectations. If supplied, textInput value must be a non-emptystring AND pass the custom function's test(function)
#' @param text input text to assert is a string (string)
#' @return True if valid text is found in. FALSE if it is not (flag)
#'
text_is_non_zero_string <- function(text, test_custom_function = NULL){
  
  valid <- assertthat::is.string(text) & nzchar(text)
  
  other_test=TRUE
  if(!is.null(test_custom_function)){
    assertthat::assert_that(is.function(test_custom_function))
    assertthat::assert_that(
      utilitybeltassertions::fun_count_arguments(test_custom_function) >= 1, 
      msg = paste0("textInput_input_is_non_zero_string: test_custom_function function must take at least 1 argument (the value of the textInput Object)"))
    
    other_test <- test_custom_function(text)
  }
  
  if(!assertthat::is.flag(valid) | !assertthat::is.flag(other_test)) 
    return(FALSE)
  else
    return(valid)
}
