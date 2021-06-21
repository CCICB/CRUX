#' select_dataset_from_maf_data_pool UI Function
#'
#' @description A shiny Module.
#' 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label "a"
#'
#' @importFrom shiny NS tagList 
mod_select_datasets_from_maf_data_pool_ui <- function(id, label="select", panel_heading = "Select Dataset"){
  assertthat::assert_that(assertthat::is.string(label) | is.null(label))
  assertthat::assert_that(assertthat::is.string(panel_heading) | is.null(panel_heading))
  
  ns <- NS(id)
  
  tagList(
    shinyWidgets::panel(
      heading = panel_heading,
      
      shiny::uiOutput(outputId = ns("out_ui_pick_dataset")), #%>% shinycssloaders::withSpinner(proxy.height = "200px"),
      fluidRow(
        col_10(),
        col_2(
          shinyjs::hidden(
            div(id=ns("buttons"),
              dipsaus::actionButtonStyled(inputId = ns("in_ui_reset_button"), label = "", icon = icon("undo"), type = "warning", width = "20%"),
              dipsaus::actionButtonStyled(inputId = ns("in_btn_load_datasets"), label = label, type = "success", disabled=TRUE, width = "70%")
            )
            )
      )
      )
  )
  )
}
    
#' select_dataset_from_maf_data_pool Server Functions
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server
#' @param max_selected_datasets max number of items that can be selected. -1 means unlimited (int)
#' 
#' @returns a character vector containing the unique_names of all datasets selected or NULL (NULL / character) 
mod_select_datasets_from_maf_data_pool_server <- function(id, maf_data_pool, max_selected_datasets = -1){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  utilitybelt::assert_is_whole_number(max_selected_datasets)
    
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    choiceValues <- reactive({
        possible_data = maf_data_pool_to_dataframe(maf_data_pool = maf_data_pool())
        choice_values <- possible_data[["unique_name"]]
        return(choice_values)
      })
    
    choiceLabels <- reactive({
      possible_data = maf_data_pool_to_dataframe(maf_data_pool = maf_data_pool())
      choice_labels <- lapply(
        seq_along(possible_data[["short_name"]]), 
        function(x) 
          tagList( 
            possible_data[["display_name"]][[x]] %>% gsub(pattern = "_", replacement = " ", x = .),
            tags$code(possible_data[["short_name"]][[x]], style = "font-size: 60%; pointer-events: none; color: #143502; background-color: #d8eedf"),
            tags$code(possible_data[["name_of_data_source"]][[x]], style = "font-size: 60%; pointer-events: none; "),
           )
      )
      return(choice_labels)
    })
    
    observeEvent(input$in_ui_reset_button, {
        shinyWidgets::updateMultiInput(session = session, inputId = "in_multiin_dataset", selected = character(0))
        dipsaus::updateActionButtonStyled(session = session, inputId = "in_btn_load_datasets", disabled = TRUE)
      })
    
    
    #Only enable continue button when at least two datasets have been selected
    observeEvent(input$in_multiin_dataset, {
      input$in_ui_reset_button #Trigger when reset button pressed as well
      isolate({
        if(length(input$in_multiin_dataset) >= 2){
          dipsaus::updateActionButtonStyled(session = session, inputId = "in_btn_load_datasets", disabled = FALSE)
        }
        else{
          dipsaus::updateActionButtonStyled(session = session, inputId = "in_btn_load_datasets", disabled = TRUE)
        }
        })
      })
    
    #Only enable deselect all button when at least one dataset has been selected
    observeEvent(input$in_multiin_dataset, {
      isolate({
        if(length(input$in_multiin_dataset) == 0){
          dipsaus::updateActionButtonStyled(session = session, inputId = "in_ui_reset_button", disabled = TRUE)
        }
        else{
          dipsaus::updateActionButtonStyled(session = session, inputId = "in_ui_reset_button", disabled = FALSE)
        }
      })
    })
    
    
    #Render Multi select Input
    output$out_ui_pick_dataset <- renderUI({
      shinyjs::showElement(id = "buttons")
      #shinyjs::showElement(id = "in_ui_reset_button")
      shinyWidgets::multiInput(
        inputId = ns("in_multiin_dataset"), 
        label = "Dataset",  
        choiceValues = choiceValues(), 
        choiceNames = choiceLabels(),
        width = "100%", 
        options = list(
          limit=max_selected_datasets,
          non_selected_header='Available Datasets',
          selected_header='Selected Datasets'
          )
        )
    })
    
    selected_datasets <- eventReactive(input$in_btn_load_datasets,{
      isolate({
          return(input$in_multiin_dataset)
        })
      })
    
    return(selected_datasets)
  })
}

