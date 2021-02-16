



#external_tools_add_tool(tool_name = "A", tool_id = "a", tool_group = "A", tool_class = "a", tool_description = "a", website = "a", doi = "a")

#' external_tools UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_external_tools_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Step 1: Select Dataset --------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 1: Select Dataset",
      mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(id = ns("mod_select_dataset"),panel = FALSE),
    ),
    
    icon_down_arrow(), br(),
    
    # Step 2: Select Tool --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 2: Select Tool",
      shinyWidgets::pickerInput(
        inputId = ns("in_pick_tool"), 
        choices = external_tool_metadata %>% dplyr::pull(tool_name), 
        choicesOpt = list(subtext=external_tool_metadata %>% dplyr::pull(tool_class)),
        options = shinyWidgets::pickerOptions(actionsBox=TRUE, liveSearch = TRUE), 
        width = "100%"
        ),
      wellPanel(htmlOutput(outputId = ns("out_html_tool_description")))
    ), #%>% div(style = "display: flex; justify-content: center"),
                               
    icon_down_arrow(), br(),
    
    # Step 3: Download Data --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 3: Download Data",
      downloadButton(outputId = ns("out_downloadbttn_exported_data"), label = "Download Formatted Data")
    ), #%>% div(style = "display: flex; justify-content: center") ,
    
    icon_down_arrow(), br(),
    
    # Step 4: Navigate to Website --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 4: Navigate to Website",
      htmlOutput(outputId = ns("out_html_link_to_website"))# %>% div(style = "display: flex; justify-content: center")
    ), # %>% iv(style = "display: flex; justify-content: center"),
    
    icon_down_arrow(), br(),
    
    # Step 5: Navigate to Website --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 5: Configure and Run Analysis",
      htmlOutput(outputId = ns("out_html_instructions"))
    ),
    
    icon_down_arrow(), br(),
    
    # Step 6: Cite --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 6: Cite",
      wellPanel("Please don't forget to cite the authors of all tools you use"),
    )
  )
}

#' external_tools Server Functions
#'
#'
mod_external_tools_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Select Data -------------------------------------------------------------
    maf_dataset_wrapper <- reactive({
      validate(need(!is.null(maf_data_pool()), message = "Please wait while we load data"))
      maf_dataset_wrapper_ <- mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server(id = "mod_select_dataset", maf_data_pool = maf_data_pool)
      return(maf_dataset_wrapper_())
    })
    
    #An observe to make sure we get maf_dataset_wrapper in a non-lazy manner
    observe({ maf_dataset_wrapper() })
    
    maf <- reactive({
      validate(need(!is.null(maf_dataset_wrapper()), message = "Please wait while we load data"));
      maf_dataset_wrapper()$loaded_data
    })
    
    display_name <- reactive({ 
      maf_dataset_wrapper()$display_name 
    })
    
    #output$test <- renderPrint({ display_name() })
    
    tool_name <- reactive({ validate(need(!is.null(input$in_pick_tool), message = "Please wait while we load data")); input$in_pick_tool })
    

    # Description of Tool -----------------------------------------------------
    output$out_html_tool_description <- renderText({
      paste0(
        h5("Summary"),
        as.character(external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "tool_description")),
        hr(),
        h5("Class of Tool"),
        as.character(external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "tool_class"))
      )
    })
    
    
    # Link to Website ---------------------------------------------------------
    output$out_html_link_to_website <- renderText({
      website_url = external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "website")
      #tags$a(class="btn btn-primary", href=website_url,type="submit", "Go To Website")
      as.character(tags$a(class="btn btn-primary", target="_blank", href=website_url, shiny::HTML(paste0("Go To ", tags$strong(tool_name())))))
    })
    
    
    # Instructions ------------------------------------------------------------
    output$out_html_instructions <- renderText({
      
      instructions = external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "instructions")
      doi = external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "doi")
      as.character(
        paste0(
          h4("Instructions"),
          p(shiny::HTML(instructions)),
          br(),
          p("Check out ", tags$b(tags$a("this link", target="_blank", href=doi)), " for more info on how the tool works")
        )
      )
    })
    
    
    # Download ----------------------------------------------------------------
    #What extension to use
    extension <- reactive({ external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "extension") })
    
    #Create filename
    filename <- reactive({ paste0(maf_dataset_wrapper()$display_name,"_", tool_name(), ".", extension())})
    
    #Download
    output$out_downloadbttn_exported_data <- downloadHandler(filename = filename, function(file){
      #browser()
      validate(need(!is.null(maf()), message = "Loading ... "))
      conversion_function = external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "maf_conversion_function")
      conversion_function(maf(), file)
    })
    
  })
}

## To be copied in the UI
# mod_external_tools_ui("external_tools_ui_1")

## To be copied in the server
# mod_external_tools_server("external_tools_ui_1")
