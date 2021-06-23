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
        shiny::uiOutput(outputId = ns("out_ui_pick_dataset")) %>% shinycssloaders::withSpinner(proxy.height = "80px")
      )
    ),
    
    utilitybeltshiny::conditionalUI(expression = panel==FALSE, 
        shiny::uiOutput(outputId = ns("out_ui_pick_dataset")) %>% shinycssloaders::withSpinner(proxy.height = "80px")
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
    
    maf_data_pool_df <- reactive({ maf_data_pool_to_dataframe(maf_data_pool()) })
    
    
    unique_dataset_names <- reactive({ 
      #browser()
      maf_data_pool_df()[["unique_name"]]
    } )
    
    
    ## Below code increments 'redraw_pickerinput' everytime the data pool is updated with more datasets. 
    #This affords us better control of when UI is redrawn. E.g. we do NOT want to redraw UI everytime maf_data_pool_df() changes since when we 'load' a dataset, it will change (status properties)
    old_unique_names <- reactiveVal(NULL)
    redraw_pickerinput <- reactiveVal(1)
    observeEvent(maf_data_pool_df(), isolate({
      new_unique_names = maf_data_pool_df()[["unique_name"]]
      if(identical(old_unique_names(),new_unique_names)){
        message("maf_data_pool_df updated BUT no new dataset added. Avoiding redraw")
      }
      else if(is.null(new_unique_names)){
        message("new_unique_names are NULL. Assuming somethings about to load to trigger repopulation of the data. Avoiding redraw")
      }
      else if(!identical(new_unique_names, old_unique_names)){
        message("maf_data_pool_df updated AND there are changes in datasets. Redrawing pickerinput and updading old_unique_names")
        old_unique_names(new_unique_names)
        redraw_pickerinput(redraw_pickerinput()+1)
      }
      else message("Should never end up here")
    }))
    
    short_dataset_names <- reactive({ 
      print(head(maf_data_pool_df()[["short_name"]]))
      maf_data_pool_df()[["short_name"]]
    } )
    
    unique_dataset_names_badge <- reactive({
      short_dataset_names() %>%
        gsub("(TCGA_)|(PCAWG_)", "", .) %>%
        paste0("<span class='label label-default' style='margin-left: 10px; font-size: xx-small' >",., "</span>")
    })
    
    
    display_names <- reactive({ 
      maf_data_pool_df()[["display_name"]] %>% 
        gsub("_", " ", .) 
    })
    
    data_sources <- reactive({
      data_sources_vec <- maf_data_pool_df()[["name_of_data_source"]][match(unique_dataset_names(), maf_data_pool_df()[["unique_name"]])]
      data_sources_vec <- paste0("<span class='label label-default' style='margin-left: 5px; font-size: xx-small' >",data_sources_vec, "</span>")
      return(data_sources_vec)
    })
    
    sample_numbers <- reactive({
      number_of_samples <- maf_data_pool_df()[["number_of_samples"]] %>%
        as.character() %>%
        ifelse(is.na(.), yes = "unknown", no = .) %>%
        paste0("N = ", .) %>%
        paste0("(", ., ")")
      
      number_of_samples_formatted <- number_of_samples
      #number_of_samples_formatted <- paste0("<span class='label label-info' style='margin-left: 5px; font-size: xx-small' >",number_of_samples, "</span>")
      return(number_of_samples_formatted)
      })
    
    
    output$out_ui_pick_dataset <- renderUI({
      redraw_pickerinput()
      isolate({
        shinyWidgets::pickerInput(
          inputId = ns("in_pickerinput_dataset"), 
          label = label, 
          multiple = multiple, 
          choices = unique_dataset_names(),
          width = "100%", 
          options = custom_picker_options(max_selected_datasets = max_selected_datasets, multiple, style = style),
          choicesOpt = list(
            content = paste0(
              display_names(),
              " ",
              sample_numbers(),
              unique_dataset_names_badge(),
              data_sources(),
            )
            )
          )
        })
      })
    selected_datasets <- reactive({ input$in_pickerinput_dataset})
    
    return(selected_datasets)
  })
}
    

custom_picker_options <- function(max_selected_datasets, multiple, style){
  shinyWidgets::pickerOptions(
    liveSearch=TRUE,
    liveSearchNormalize = TRUE,
    actionsBox=multiple,
    maxOptions = max_selected_datasets,
    style = style, 
  )
}
## To be copied in the UI
# mod_select_dataset_from_maf_data_pool_pickerinput_ui("select_dataset_from_maf_data_pool_pickerinput_ui_1")
    
## To be copied in the server
# mod_select_dataset_from_maf_data_pool_pickerinput_server("select_dataset_from_maf_data_pool_pickerinput_ui_1")
