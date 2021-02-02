#Returns a taglist contianing a rendered datatable with a download button that appears only when data is loaded
mod_render_downloadabledataframe_ui <- function(id, downloadbttn_label="Download", tooltip_text = "", tooltip_pos = "right"){
  ns <- NS(id)
  tagList(
    br(),
    DT::dataTableOutput(outputId = ns("out_dt_maf")) %>% shinycssloaders::withSpinner(),
    
    conditionalPanel(condition = "output.cond", ns=ns,
        br(),
        #moduleDownloadDatatableUI(ns("mod_download_table"), downloadbttn_label, tooltip_text = tooltip_text, tooltip_pos=tooltip_pos)
      )
  )
}

mod_render_downloadabledataframe_server <- function(id, tabular_data_object, basename, rownames=F, colnames=T){
  moduleServer(id,
    function(input, output, session){
      
      datatable_object <- reactive({ 
        validate(need(!is.null(tabular_data_object()), "Please upload your data")) 
        tabular_data_object()})
      
      output$cond <- reactive({
        !is.null(datatable_object()) %>% return()
      })
      outputOptions(output, "cond", suspendWhenHidden = FALSE)
      
      output$out_dt_maf <- DT::renderDataTable({ datatable_object() }, options = list(scrollX = TRUE), class = "display nowrap")
      
      filename <- reactive({paste0(basename, ".tsv")})
      
      #moduleDownloadDatatableServer(id = "mod_download_table", data_to_write = datatable_object(), filename_full = filename, rownames = rownames, colnames = colnames)
  }
  )
}

# Copy in UI
# mod_render_downloadabledataframe_ui("some_id")

# Copy in server
# mod_render_downloadabledataframe_server("some_id", optional_argument)
