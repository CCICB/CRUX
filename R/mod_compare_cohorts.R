#' mod_compare_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_compare_cohorts_ui <- function(id){
  ns <- NS(id)
  tagList(
      # shinyWidgets::radioGroupButtons(
      #   inputId = ns("in_radiogroup_select_type_of_data_import"),
      #   #choices = c("Two Distinct Datasets", "Subsets of a Single Dataset"), 
      #   choiceNames = c("Two Distinct Datasets", "Subsets of a Single Dataset"),
      #   choiceValues = c("two_datasets", "two_subsets"),
      #   label = "What Do You Want To Compare?", 
      #   status="info",
      #   justified = FALSE, 
      #   individual = TRUE
      #   ),
      
      #Select Two Distinct Datasets
      shinyWidgets::panel(heading = "Step 1: Select Data",
        # conditionalPanel(
        #   condition = "input.in_radiogroup_select_type_of_data_import == 'two_datasets'", 
        #   ns = ns,
          #mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui(ns("select_dataset1"), panel = FALSE),
          #mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui(ns("select_dataset2"), panel = FALSE),
          fluidRow(
          col_6(mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(ns("select_dataset1"), panel = FALSE)),
          col_6(mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(ns("select_dataset2"), panel = FALSE))
          ),
          # ),
        
        #Select A subset of 1 dataet
        # conditionalPanel(
        #   condition = "input.in_radiogroup_select_type_of_data_import == 'two_subsets'", 
        #   ns = ns,
        #   p("Selecting Two Subsets")
        # ),
      ),
      
      icon_down_arrow(fontsize = "40px",alignment = "center"),br(),
      
      shinyWidgets::panel(heading = "Step 2: Configure Analysis",
        numericInput(inputId = ns("in_num_minmut"), label = "Minimum Mutated Samples", value = 5, min = 0, step = 1),
        shinyBS::bsTooltip(id = ns("in_num_minmut"), title = "Consider only genes with at least X samples mutated in at least one of the cohorts. Helpful to ignore single mutated genes.", placement = "right")
      ),
      
      icon_down_arrow(fontsize = "40px", alignment = "center"),br(),
      
      shinyWidgets::panel(heading = "Step 3: View Results",
        tabsetPanel(
          tabPanel(title = "Tabular Summary", DT::dataTableOutput(outputId = ns("out_maf_compare_summary")) %>% shinycssloaders::withSpinner(proxy.height = "200px")),
          tabPanel(title = "Rainforest Plot Summary",modulePlotRainforestUI(id = ns("mod_plot_rainforest")) %>% shinycssloaders::withSpinner(proxy.height = "200px")),
          tabPanel(title = "Lollipop", modulePlotLollipop2UI(ns("id_lollipop")) %>% shinycssloaders::withSpinner(proxy.height = "200px")),
          tabPanel(title = "coBarplot", modulePlotCobarplotUI(ns("id_cobarplot")) %>% shinycssloaders::withSpinner(proxy.height = "200px")),
          tabPanel(title = "coOncoplot", modulePlotCooncoplotUI(ns("id_cooncoplot")) %>% shinycssloaders::withSpinner(proxy.height = "200px"))
        ),
        br()
      )
  )
}
    
#' mod_compare_cohorts Server Functions
#'
#' @noRd 
mod_compare_cohorts_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(object = maf_data_pool)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #maf1=mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server(id = "select_dataset1",maf_data_pool = maf_data_pool, label = NULL)
    #maf2=mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server(id = "select_dataset2",maf_data_pool = maf_data_pool, label = NULL)
    
    maf_dataset_wrapper_1 = mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server(id = "select_dataset1", maf_data_pool = maf_data_pool, label = "Cohort 1")
    maf_dataset_wrapper_2 = mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server(id = "select_dataset2", maf_data_pool = maf_data_pool, label = "Cohort 2")
    
    maf1 = reactive({ maf_dataset_wrapper_1()$loaded_data })
    maf2 = reactive({ maf_dataset_wrapper_2()$loaded_data })
    
    maf1_name = reactive({ maf_dataset_wrapper_1()$display_name })
    maf2_name = reactive({ maf_dataset_wrapper_2()$display_name })
    
    
    maf_compare = reactive({
      validate(need(!is.null(maf1()), message = "Please Select Dataset"))
      validate(need(!is.null(maf2()), message = "Please Select Dataset"))
      validate(need(maf_dataset_wrapper_1()$unique_name != maf_dataset_wrapper_2()$unique_name, message = "Please Select Two Different Datasets"))
      maftools::mafCompare(m1 = maf1(), m2 = maf2(), m1Name = maf1_name(), m2Name = maf2_name(), minMut = input$in_num_minmut)
      })
    
    
    #Output Modules
    output$out_maf_compare_summary <- DT::renderDataTable(maf_compare()$results, options = list(scrollX = TRUE), class = "display nowrap")
    modulePlotRainforestServer(id = "mod_plot_rainforest", mafCompareObject = maf_compare)
    modulePlotLollipop2Server(id = "id_lollipop", maf1 = maf1, maf2 = maf2, name_cohort1 = maf1_name, name_cohort2 = maf2_name)
    modulePlotCooncoplotServer(id="id_cooncoplot", maf1 = maf1, maf2 = maf2, name_cohort1 = maf1_name, name_cohort2 = maf2_name)
    modulePlotCobarplotServer(id="id_cobarplot", maf1 = maf1, maf2 = maf2, name_cohort1 = maf1_name, name_cohort2 = maf2_name)
  })
}
    
## To be copied in the UI
# mod_mod_compare_cohorts_ui("mod_compare_cohorts_ui_1")
    
## To be copied in the server
# mod_mod_compare_cohorts_server("mod_compare_cohorts_ui_1")
