#' Run the Shiny Application
#'
#' @param options A list of  options to be passed to shinyApp. 
#' Common options include launch.browser & port. 
#' You must always set ALL options you care about in one go. 
#' E.g. if you use options=list(port=3838) we cannot guarantee the launch.browser option will remain true. 
#' If multiple options are important to you, force-set them all, e.g. \strong{options(list(launch.browser=TRUE, port = 3838))}
#' @param ... A series of options to be used inside the app.
#'
#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @export
run_app <- function(
  options = list(launch.browser = TRUE), # Passing options via ... seems to be failing. This options parameter lets us force-set shiny options while using sensible defaults
  ...
) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, 
      server = app_server,
      options = options # Directly feed options to shinyApp. Passing via `...` doesn't seem to work for me
    ), 
    golem_opts = list(...)
  )
}
