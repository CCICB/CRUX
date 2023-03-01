



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
      mod_select_maf_dataset_wrapper_ui(id = ns("mod_select_dataset"), panel = FALSE),
      #mod_select_maf_dataset_wrapper_ui(id = ns("mod_select_dataset"),panel = FALSE),
    ),

    # Step 1.5: render name of dataset to make sure updates to selected maf are carried through to the download button ----------------------------------------------------------------
    icon_down_arrow(), br(),
    
    shinyWidgets::panel(
      heading = "Step 1.5: Ensure Dataset is Ready for Export", 
      textOutput(ns("out_txt_data_ready")) %>% shinycssloaders::withSpinner(proxy.height = "80px")
    ),
    
    
    icon_down_arrow(), br(),
    
    # Step 2: Select Tool --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 2: Select Tool",
      shinyWidgets::pickerInput(
        inputId = ns("in_pick_tool"), 
        choices = external_tool_metadata %>% dplyr::pull(tool_name), 
        choicesOpt = list(
          content=
          paste0(
          external_tool_metadata$tool_name,
          external_tool_metadata$tool_class %>%
            paste0("<span class='label label-primary' style='margin-left: 10px' >",., "</span>"),
          external_tool_metadata$tool_group %>%
            paste0("<span class='label label-warning' style='margin-left: 10px' >",., "</span>"),
          external_tool_metadata$platform %>%
            paste0("<span class='label label-danger' style='margin-left: 10px' >",., "</span>")
          #subtext=
          )),
        options = shinyWidgets::pickerOptions(actionsBox=TRUE, liveSearch = TRUE), 
        width = "100%"
        ),
      wellPanel(htmlOutput(outputId = ns("out_html_tool_description")))
    ), #%>% div(style = "display: flex; justify-content: center"),
                               
    icon_down_arrow(), br(),
    
    
    
    # Step 2.5: Select Gene if required ---------------------------------------
    conditionalPanel(condition = 'output.requires_gene_selection', ns = ns,
      shinyWidgets::panel(
        heading="Step 3: Select Gene",
        mod_select_genes_ui(ns("mod_select_gene"), multiple = FALSE)
      ),
      
      icon_down_arrow(), br()
    ),
    

    # Step 3: Download Data --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 3: Export Data",
      downloadButton(outputId = ns("out_downloadbttn_exported_data"), label = "Export Data")
    ), #%>% div(style = "display: flex; justify-content: center") ,
    
    icon_down_arrow(), br(),
    
    # Step 4: Navigate to Website --------------------------------------------------
    shinyWidgets::panel(
      heading="Step 4: Navigate to Website",
      htmlOutput(outputId = ns("out_html_link_to_website"))# %>% div(style = "display: flex; justify-content: center")
    ), # %>% iv(style = "display: flex; justify-content: center"),
    
    icon_down_arrow(), br(),
    
    # Step 5: Show Instructions --------------------------------------------------
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
    
                     
    # Step 1: Select Data -------------------------------------------------------------
    maf_dataset_wrapper = mod_select_maf_dataset_wrapper_server("mod_select_dataset",maf_data_pool = maf_data_pool)
    
    
    # Step 1.5: render name of dataset to make sure updates to selected maf are carried through to the download button ----------------------------------------------------------------
    output$out_txt_data_ready <- renderText({ 
      if(!maf_ready()) 
        "Please select a dataset"
      else
        paste0(maf_dataset_wrapper()$display_name, " dataset is ready for export")
      })
    
    #Get MAF
    maf <- reactive({
      maf_dataset_wrapper()$loaded_data
    })
    
    # Check if maf is ready
    maf_ready <- reactive({
      !is.null(maf_dataset_wrapper()) & !is.null(maf())
      })
    
    # Disable download button on load
    shinyjs::disable("out_downloadbttn_exported_data")
    
    # Enable download when all is ready
    observeEvent(maf_ready(), isolate({
      if(!maf_ready()) {
        shinyjs::disable(id = "out_downloadbttn_exported_data")
      }
      else
        shinyjs::enable(id = "out_downloadbttn_exported_data")
    }))
    
    # Get MAF Name
    display_name <- reactive({ 
      maf_dataset_wrapper()$display_name 
    })
    
    
    # Populate Gene List ------------------------------------------------------
    selected_gene <- mod_select_genes_server("mod_select_gene", maf)

    
    # Get tool name
    tool_name <- reactive({ validate(need(!is.null(input$in_pick_tool), message = "Please wait while we load data")); input$in_pick_tool })
    
    #Conditionally Render the gene selection UI
    output$requires_gene_selection <- reactive({ 
      maf() # Run when maf updates
      if(external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "requires_gene_selection")) return(TRUE)
      else return(FALSE)
    })
    outputOptions(output, "requires_gene_selection", suspendWhenHidden = FALSE)

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
    filename <- reactive({maf();  paste0(maf_dataset_wrapper()$display_name,"_", tool_name(), ".", extension())})
    
    #Download
    output$out_downloadbttn_exported_data <- downloadHandler(filename = filename, function(file){
      validate(need(!is.null(maf()), message = "Please select a dataset ... "))
      conversion_function = external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "maf_conversion_function")
      requires_gene_name = external_tools_get_property_by_tool_name(tool_name = tool_name(), property_to_retrieve = "requires_gene_selection")
      
      if(requires_gene_name)
        conversion_function(maf_dataset_wrapper(), file, selected_gene())
      else
        conversion_function(maf_dataset_wrapper(), file)
    })
    
  })
}

## To be copied in the UI
# mod_external_tools_ui("external_tools_ui_1")

## To be copied in the server
# mod_external_tools_server("external_tools_ui_1")
