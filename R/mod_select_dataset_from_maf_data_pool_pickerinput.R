#' select_dataset_from_maf_data_pool_pickerinput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_dataset_from_maf_data_pool_pickerinput_ui <- function(id, panel=TRUE){
  ns <- NS(id)
  tagList(
    utilitybeltshiny::conditionalUI(expression = panel==TRUE, 
      shinyWidgets::panel(
        heading = "Select Dataset",
        shiny::uiOutput(outputId = ns("out_ui_pick_dataset"))
      )
    ),
    
    utilitybeltshiny::conditionalUI(expression = panel==FALSE, 
        shiny::uiOutput(outputId = ns("out_ui_pick_dataset"))
    )
    
  )
}
    
#' select_dataset_from_maf_data_pool_pickerinput Server Functions
#' 
#' @inheritParams shinyWidgets::pickerOptions
#' @returns a character vector of unique_names or NULL if none are selected
mod_select_dataset_from_maf_data_pool_pickerinput_server <- function(id, maf_data_pool, label = "Dataset", style = "btn-outline-primary" ,multiple=FALSE, max_selected_datasets = 40){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # Choices Vector -----------------------------------------------------------------
    choiceValues <- reactive({
      #browser()
      possible_data = maf_data_pool_to_dataframe(maf_data_pool = maf_data_pool())
      choice_values <- possible_data[["unique_name"]]
      names(choice_values) <- paste0(possible_data[["display_name"]] %>% gsub(pattern = "_", replacement = " ", x = .), "   |",possible_data[["short_name"]], "|  ") 
      return(choice_values)
    })
    

    # Sources  (For Subtext) -----------------------------------------------------------------
    choiceLabels <- reactive({
      isolate({possible_data = maf_data_pool_to_dataframe(maf_data_pool = maf_data_pool())})
      choice_labels <- sapply(
        seq_along(possible_data[["short_name"]]),
        function(x)
          possible_data[["name_of_data_source"]][[x]]
      )
      return(choice_labels)
    })
    
    
    last_selected = reactiveVal(NULL)
    output$out_ui_pick_dataset <- renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("in_pickerinput_dataset"), 
        label = label, 
        multiple = multiple, 
        choices = choiceValues(),
        selected = last_selected(),
        width = "100%", 
        options = custom_picker_options(max_selected_datasets = max_selected_datasets, multiple, style = style),
        choicesOpt = list(
          subtext = choiceLabels()
          )
        )
      })
    selected_datasets <- reactive({ last_selected(input$in_pickerinput_dataset); input$in_pickerinput_dataset})
    #dataset_wrapper <- reactive({ maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = selected_datasets()) })
    
    return(selected_datasets)
  })
}
    

custom_picker_options <- function(max_selected_datasets, multiple, style){
  shinyWidgets::pickerOptions(
    liveSearch=TRUE,
    liveSearchNormalize = TRUE,
    actionsBox=multiple,
    maxOptions = max_selected_datasets,
    style = style
  )
}
## To be copied in the UI
# mod_select_dataset_from_maf_data_pool_pickerinput_ui("select_dataset_from_maf_data_pool_pickerinput_ui_1")
    
## To be copied in the server
# mod_select_dataset_from_maf_data_pool_pickerinput_server("select_dataset_from_maf_data_pool_pickerinput_ui_1")
