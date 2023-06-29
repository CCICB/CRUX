#Returns a taglist contianing a rendered datatable with a download button that appears only when data is loaded
mod_render_downloadabledataframe_ui <- function(id, downloadbttn_label="", class="btn-default", tooltip_text = "", tooltip_pos = "right", shinycssloader=TRUE){
  ns <- NS(id)
  tagList(
    br(),
    
    #fluidRow(
    div(
      id = "Big Container", style = "display: flex; height: auto; align-items: center",
      div(
        style = 'width: 97%; display: inline-block',
        conditionalUI(shinycssloader, tagList(
          DT::dataTableOutput(outputId = ns("out_dt_maf"), width = "inherit", height = "auto") %>% shinycssloaders::withSpinner(proxy.height = "200px")
        )),
        conditionalUI(shinycssloader==FALSE, tagList(
          DT::dataTableOutput(outputId = ns("out_dt_maf"), width = "inherit", height = "auto")
        )),
        br()
      ),
      div(
        style = 'width: 2%; height: 80%; display: inline-block; margin-left: 15px;',
        id="toolbar",
        conditionalPanel(
          condition = "output.cond", ns=ns, style = "height: 100%; width: 100%",
          #br(),
          downloadButton(outputId = ns("out_bttn_download_dataframe"), icon = icon("download"), label = downloadbttn_label, class=class, style = "width: 100%; padding: 0; height: 30px;display: grid; align-content: space-evenly;"),
          shinyBS::bsTooltip(id = ns("out_bttn_download_dataframe"), title = tooltip_text, placement = tooltip_pos),
          #shinyWidgets::awesomeCheckbox(inputId = ns("in_check_column_filter"), value = TRUE, label = "")
          #moduleDownloadDatatableUI(ns("mod_download_table"), downloadbttn_label, tooltip_text = tooltip_text, tooltip_pos=tooltip_pos)
        ),
      )
    )
    #)
  )
}


#' Title
#'
#' @param id shiny paramater
#' @param tabular_data_object tabular data object (usually data.frame or data.table)
#' @param basename name of downloaded file (flag)
#' @param rownames download with rownames (flag)
#' @param colnames download with colnames (flag)
#' @param filter Position of filter search box: one of 'top', 'bottom' or 'none'  (string)
#' @param message_if_tabular_data_is_null message if tabular data is null (string)
#'
mod_render_downloadabledataframe_server <- function(id, tabular_data_object, basename, rownames=FALSE, colnames=TRUE, filter="top", message_if_tabular_data_is_null = "Please select valid mutalisk files"){
  assertthat::assert_that(filter %in% c("top", "bottom", "none"), msg = "mod_render_downloadabledataframe_server: filter argument should be one of 'top', 'bottom' or 'none'")
  
  assertions::assert_reactive(tabular_data_object)
  moduleServer(id,
               function(input, output, session){
                 
                 
                 datatable_object <- reactive({ 
                   validate(need(!is.null(tabular_data_object()), message = message_if_tabular_data_is_null)) 
                   tabular_data_object()
                   })
                
                 output$cond <- reactive({
                   !is.null(datatable_object()) %>% return()
                 })
                 outputOptions(output, "cond", suspendWhenHidden = FALSE)
                 
                 toggle_column_filter <- reactive({
                   if(is.null(input$in_check_column_filter))
                     return('none')
                   else if(input$in_check_column_filter)
                     return('top')
                   else
                     return('none')
                 }) 
                 
                 output$out_dt_maf <- DT::renderDataTable({ 
                   DT::datatable(datatable_object(), options = list(scrollX = TRUE), class = "display nowrap", filter = filter)
                 })
                 
                 filename <- reactive({
                   prefix=ifelse(is.reactive(basename), yes=basename(), no = basename)
                   paste0(prefix, ".tsv")
                 })
                 
                 output$out_bttn_download_dataframe <- downloadHandler(filename = filename, function(file){
                   data.table::fwrite(datatable_object(), file = file, sep = "\t", col.names = colnames, row.names = rownames)
                 })
                 
                 #moduleDownloadDatatableServer(id = "mod_download_table", data_to_write = datatable_object(), filename_full = filename, rownames = rownames, colnames = colnames)
               }
  )
}

# Copy in UI
# mod_render_downloadabledataframe_ui("some_id")

# Copy in server
# mod_render_downloadabledataframe_server("some_id", optional_argument)


# utils -------------------------------------------------------------------

#' Conditional UI
#'
#' This function takes some UI element and a compiletime-evaluated expression 
#' and returns the UI element ONLY if the condition is true
#' 
#' @param expression compile-time evaluated expression (no server/reactive variables)
#' @param ui_element the UI element to display if expression = TRUE
#'
#' @return if expression==TRUE: taglist wrapping passed UI element. If expression == FALSE, empty taglist
#'
#' @examples
#' # In UI:
#' somevariable=TRUE
#' shinyWidgets::panel(
#'   heading="constitutivepanel",
#'   CRUX:::conditionalUI(
#'     somevariable, 
#'     shinyWidgets::panel(somevariable, heading="ConditionalPanel")
#'   ),
#'                     
#'   shiny::h1("constitutive title"),
#'   shiny::p("constitutive paragraph")
#' )
conditionalUI <- function (expression, ui_element) 
{
  assertthat::assert_that(is.logical(expression), msg = "expression needs to be logical")
  if (expression) 
    return(shiny::tagList(ui_element))
  else return(shiny::tagList())
}
