#Returns a taglist contianing a rendered datable and option of toggling between ALL and just synonymous
mod_tabulate_maf_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    DT::dataTableOutput(outputId = ns("out_dt_maf")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    
    conditionalPanel(condition = "output.cond", ns=ns,
      shinyWidgets::panel(
        heading = "Options",
        shinyWidgets::awesomeCheckbox(inputId = ns("in_check_toggle_synonymous"), label = "Show Silent Mutations", value = FALSE),
        moduleDownloadMafUI(ns("mod_download_maf"), label = "Download maf", tooltip_text = "Will download entire MAF file (non-synonymous AND silent) irrespective of any options flagged")
      )
      )
  )
  }

mod_tabulate_maf_server <- function(id, maf){
  moduleServer(id,
    function(input, output, session){
      
      
      maf_datatable <- reactive({ validate(need(!is.null(maf()),message = "Please import a valid MAF")); maftools_get_all_data(maf = maf(), include_silent_mutations = input$in_check_toggle_synonymous) })
      
      output$cond <- reactive({
        !is.null(maf_datatable()) %>% return()
      })
      outputOptions(output, "cond", suspendWhenHidden = FALSE)
      
      output$out_dt_maf <- DT::renderDataTable({ maf_datatable() }, options = list(scrollX = TRUE), class = "display nowrap", filter = "top")
      
      moduleDownloadMafServer(id = "mod_download_maf", maf=maf())
  }
  )
}

# Copy in UI
# mod_tabulate_maf_ui("some_id")

# Copy in server
# mod_tabulate_maf_server("some_id", optional_argument)
