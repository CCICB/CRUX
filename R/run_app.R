#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @export
run_app <- function(
  ...
) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, 
      server = app_server,
      options = list(launch.browser = TRUE) #Hardcoding important options. Passing via `...` seems to be failing for some reason
    ), 
    golem_opts = list(...)
  )
}
