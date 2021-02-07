#' data_import2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    div(id=ns("import_wizard_wrapper"),
      tabsetPanel(
        id=ns("import_wizard"),
        type = "hidden",
        
        tabPanel(
          title = "page_1_import_maf",
          shinyWidgets::panel(
             heading="Step 1: Import MAF",
             mod_shinyfiles_get_maf_path_ui(id = ns("mod_get_maf_path")),
             br(),
             mod_data_import_maf_path_to_maf_ui(id=ns("mod_data_import_maf_path_to_maf"))
           ),
          fluidRow(
            column(11),
            col_1(dipsaus::actionButtonStyled(inputId = ns("page_1_to_2"), label = "Continue", type = "danger", disabled = TRUE, width = "100%"), style='padding-left:0px;')
          )
        ),
        
        tabPanel(
          title = "page_2_add_cohortlevel_metadata",
          shinyWidgets::panel(
           heading="Step 2: Cohort Level Metadata",
           mod_data_import_step2_ui(id=ns("mod_data_import_step2"))
          ),
          fluidRow(
            col_10(),
            back_button(ns("in_bttn_restart1")),
            col_1(dipsaus::actionButtonStyled(inputId = ns("page_2_to_3"), label = "Continue", type = "danger", disabled = TRUE, width = "100%"), style='padding-left:0px;')
          )
        ),
        
        tabPanel(
          title = "page_3_add_tumor_level_metadata",
          shinyWidgets::panel(
           heading="Step 3: Tumor Level Metadata",
           mod_shinyfiles_get_clinical_featurefile_path_ui(id = ns("mod_get_clinical_featurefile_path")),
           hr(),
           mod_render_clinical_data_table_ui(ns("mod_render_clinical_data_table"))
          ),
          fluidRow(
            col_10(),
            back_button(ns("in_bttn_restart2")),
            col_1(dipsaus::actionButtonStyled(inputId = ns("page_3_to_4"), label = "Continue", type = "danger", disabled = TRUE, width = "100%"), style='padding-left:0px;')
          )
        ),
        
        tabPanel(
          title = "page_4_review_and_confirm",
          shinyWidgets::panel(heading = "Step 4: Review and Confirm",
            #mod_single_cohort_summary_tables_and_plots_ui(id = ns("mod_single_cohort_summary_tables_and_plots"))
            mod_single_cohort_summary_tables_ui(id = ns("mod_single_cohort_summary_tables"))
          ),
          fluidRow(
            column(9),
            back_button(ns("in_bttn_restart3")),
            col_2(dipsaus::actionButtonStyled(inputId = ns("in_bttn_send_to_data_pool"), label = "Add to data pool", type = "success", width = "100%"), style='padding-left:0px;')
          )
        )
      ),
      br()
    )
  )
}
    
#' data_import2 Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_path=mod_shinyfiles_get_maf_path_server(id = "mod_get_maf_path")
    metadata=mod_data_import_step2_server(id = "mod_data_import_step2")
    clinical_features_path=reactive({
      if(input$import_wizard == "page_1_import_maf") {
        return(NULL)
      }
      else{
        #message("Hi")
        mod_shinyfiles_get_clinical_featurefile_path_server(id = "mod_get_clinical_featurefile_path")()
      }
    })
    maf=mod_data_import_maf_path_to_maf_server(id = "mod_data_import_maf_path_to_maf", maf_path = maf_path, clinicalData = clinical_features_path)
    mod_render_clinical_data_table_server(id = "mod_render_clinical_data_table", maf = maf)

    # Wizard Manager ----------------------------------------------------------
    switch_page <- function(page_to_change_to) { #needs to be in server as it uses session argument
      updateTabsetPanel(session,  inputId = "import_wizard", selected = page_to_change_to)
    }
    
    #Change page conditionals
    valid_maf_loaded <- reactive({ !is.null(maf()) }) #Maf will only be valid if MAF file is valid AND clinical data file is either NULL or valid
    cohort_level_metadata_valid <- reactive({ metadata()[["all_valid"]] })
    
    
    observeEvent(input$page_1_to_2, { message("clickitied"); if(isolate(valid_maf_loaded())) switch_page("page_2_add_cohortlevel_metadata")} )
    observeEvent(input$page_2_to_3, { message("clickitied"); if(isolate(cohort_level_metadata_valid())) switch_page("page_3_add_tumor_level_metadata")} )
    observeEvent(input$page_3_to_4, { message("clickitied"); if(isolate(valid_maf_loaded())) switch_page("page_4_review_and_confirm")} )
    
    cohortName = reactive({ metadata()["display_name"] })
    
    #Modules
    mod_single_cohort_summary_tables_server(id = "mod_single_cohort_summary_tables", maf = maf, cohortName = cohortName)
    
    #Make buttons clickable when conditions are met:
    observeEvent( valid_maf_loaded() , {
      if(isolate(valid_maf_loaded()) == TRUE){
        dipsaus::updateActionButtonStyled(session = session, inputId = "page_1_to_2", type = "success",label = "Continue")
      }
      else
        dipsaus::updateActionButtonStyled(session = session, inputId = "page_1_to_2", label = "Continue", type = "danger", disabled = TRUE)
    })
    
    observeEvent( cohort_level_metadata_valid() , {
      if(isolate(cohort_level_metadata_valid()) == TRUE){
        dipsaus::updateActionButtonStyled(session = session, inputId = "page_2_to_3", type = "success",label = "Continue")
      }
      else
        dipsaus::updateActionButtonStyled(session = session, inputId = "page_2_to_3", label = "Continue", type = "danger", disabled = TRUE)
    })
    
    observeEvent( valid_maf_loaded() , {
      if(isolate(valid_maf_loaded()) == TRUE){
        dipsaus::updateActionButtonStyled(session = session, inputId = "page_3_to_4", type = "success",label = "Continue")
      }
      else
        dipsaus::updateActionButtonStyled(session = session, inputId = "page_3_to_4", label = "Continue", type = "danger", disabled = TRUE)
    })
    
    
    
    # Send to data pool -------------------------------------------------------
    observeEvent(input$in_bttn_send_to_data_pool, {
      message("Button Clicked")
      updated_maf_data_pool <- user_to_dataset_to_data_pool(
        maf_data_pool = maf_data_pool(), 
        filepath = maf_path(), 
        clinicalData = maftools::getClinicalData(maf()), #TODO remove. Simply ask user to specify a file to store their data, write to inst config file.
        display_name = metadata()[["display_name"]], 
        short_name = metadata()[["short_name"]], 
        description = metadata()[["description"]], 
        data_source = metadata()[["data_source"]],
        loaded_data = maf()
      )
      
      maf_data_pool(updated_maf_data_pool)
      shinyWidgets::sendSweetAlert(session = session, title = "Success !!", text = "Dataset has been successfully imported! ", type = "success")
      reset_data_import_wizard()
    })
    
    
    any_back_button_clicked <- reactive({ (input$in_bttn_restart1 + input$in_bttn_restart2 + input$in_bttn_restart3) > 0 })
    observeEvent( any_back_button_clicked() , {
      #message("Resetting")
      reset_data_import_wizard()
    })
    
    reset_data_import_wizard <- function(){
      shinyjs::reset(id = "import_wizard_wrapper")
      switch_page(page_to_change_to = "page_1_import_maf")
      #TODO find a way to reset shinyfile inputs
    }
    # output$out_dt_placeholder <- DT::renderDataTable({ clinical_data() }, options = list(scrollX = TRUE), class = "display nowrap")
  })
}


back_button <- function(id){
  return(tagList(
  col_1(
    style='padding-right: 2px;', 
    align = "right",
    dipsaus::actionButtonStyled(style = "padding: 6px 6px",inputId = id, label = tags$i(class = "fas fa-arrow-alt-circle-left", style="font-size: 14px; text-align: left; text-align: middle;"), type = "info", width = "30%"),
    shinyBS::bsTooltip(id = id, title = "Reset", placement = "top")
    )
  ))
}
## To be copied in the UI
# mod_data_import_ui("data_import2_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import2_ui_1")
