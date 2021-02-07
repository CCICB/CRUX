#Sets up button and download handler for downloading base R plots

#' Creates a shinyWidgets::downloadbttn
#'
#' @param id Module ID
#' @param label The label that should appear on the button.
#' @param style Style of the button, to choose between simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite.
#' @param color Color of the button : default, primary, warning, danger, success, royal.
#' @param size Size of the button : xs,sm, md, lg.
#' @param tooltip_placement Where the tooltip should appear relative to its target (top, bottom, left, or right). Defaults to "right" (string)
#' @param tooltip_text Tooltip text (string)
#' @inheritParams shinyWidgets::tooltipOptions 
#' @inheritParams shinyWidgets::dropdownButton
#' @return Nothing. Function run for its side effects
#' @export
moduleDownloadPlotUI <- function(id, circle = FALSE, label = "Download", style = "unite", color = "default", size="default", status = "default", icon = NULL, tooltip_placement = "right", tooltip_text = "", right = FALSE, up=FALSE, width="200px", margin="10px", inline = FALSE, ...){
  ns <- NS(id)
  tagList(
    #shinyWidgets::downloadBttn(outputId = ns("out_download_bttn"), label = label, style = style, color = color, size = size),
    shinyWidgets::dropdownButton(
      inputId = ns("in_dropdown_menu"),
      circle = circle, 
      status = status, 
      size = size, 
      icon = icon, 
      label = label, 
      tooltip = shinyWidgets::tooltipOptions(placement = tooltip_placement, title = tooltip_text, html = TRUE), 
      right = right, 
      up = up, 
      width = width, 
      margin = margin, 
      inline = inline, 
      ...,
      
      shiny::selectizeInput(
        inputId = ns("in_pick_download_format"), 
        label = "File Format", 
        choices = c("svg", "tiff", "pdf", "png"), 
        multiple = FALSE, 
        width = "100%"
        ),
      
      
      fluidRow(
        col_6(
          shiny::numericInput(inputId = ns("in_num_width"), label = "Width", min = 0, value = 7, step = 1) %>% bsplus::bs_embed_tooltip(title = "Measured in inches"),
          style = "padding-right:1px"
        ),
        col_6(
          shiny::numericInput(inputId = ns("in_num_height"), label = "Height", min = 0, value = 7, step = 1) %>% bsplus::bs_embed_tooltip(title = "Measured in inches"),
          style = "padding-left:1px"
        ),
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.in_pick_download_format == 'tiff' || input.in_pick_download_format == 'png'",
        shiny::numericInput(inputId = ns("in_num_resolution"), label = "Resolution (ppi)", min = 0, value = 300, step = 2, width = "100%")
      ),
      shiny::textInput(inputId = ns("in_text_file_prefix"), label = "Prefix", placeholder = "MyPlot", value = "", width = "100%"),
      shinyWidgets::downloadBttn(outputId = ns("in_button_download_plot"), label = "", color = "primary", style = "fill", block=TRUE)
      )
    )
  }




#' Saves Plots as SVG.
#'
#' @param id Module ID. Must be identical to moduleDownloadPlotUI
#' @param parent Session object of the calling module (used to extract plot details)
#' @param plotOutputId "ID of the rendered plot"
#' @param plotting_function "function that when run with no arguments will create the plot. Can make by wrapping the plot call in its own function. e.g. for plot(mtcars) you could do:  plotting_function <- reactive ({ function() {plot(mtcars)} }) then pass 'plotting_function()'"
#' @param default_filename defualt basename of downloaded file (string; non-reactive)
#'
#' @return
#' @export
#'
#' @examples
moduleDownloadPlotServer <- function(id, session_parent, plotOutputId, plotting_function, default_filename = "download"){
  moduleServer(id,
    function(input, output, session){
      plot_width <- reactive ({ get_rendered_plot_width(output_id = plotOutputId, session = session_parent, return_inches = T) })
      plot_height <- reactive ({ get_rendered_plot_height(output_id = plotOutputId, session = session_parent, return_inches = T) })
      
      observeEvent( input$in_dropdown_menu, {
        isolate({
        updateNumericInput(session = session, value = plot_width(), inputId = "in_num_width")
        updateNumericInput(session = session, value = plot_height(), inputId = "in_num_height")
        updateTextInput(placeholder = default_filename, value = default_filename, session = session, inputId = "in_text_file_prefix")
        })
      })
      
      fileName <- reactive({
        paste0(input$in_text_file_prefix, ".",input$in_pick_download_format)
        })
      
      
      
      output$in_button_download_plot <- downloadHandler(filename = fileName, content = function(file){
        #browser()
        if(input$in_pick_download_format == "svg")
          svg(file, width = input$in_num_width, height = input$in_num_height)
        else if(input$in_pick_download_format == "pdf")
          pdf(file, width = input$in_num_width, height = input$in_num_height, useDingbats = FALSE)
        else if(input$in_pick_download_format == "tiff")
          tiff(file, width = input$in_num_width, height = input$in_num_height, res = input$in_num_resolution, units = "in")
        else if(input$in_pick_download_format == "png")
          png(file, width = input$in_num_width, height = input$in_num_height, res = input$in_num_resolution, units = "in")
        plotting_function()
        dev.off()
        })
      }
  )
}

# Copy in UI
# moduleDownloadPlotUI("some_id")

# Copy in server
# moduleDownloadPlotServer("some_id", optional_argument)


