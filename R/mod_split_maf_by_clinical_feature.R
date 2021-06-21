moduleSubsetByClinicalFeaturesUI <- function(id, panel_title="Define Cohorts"){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(heading = panel_title,
      uiOutput(outputId = ns("out_ui_grouping_variable")),
      uiOutput(outputId = ns("out_ui_group1")),
      uiOutput(outputId = ns("out_ui_group2")),
      br()
    )
    )
}



#' moduleSubsetByClinicalFeaturesServer
#'
#' @param id 
#' @param maf 
#'
#' @return named list: 'maf1' (type maf), 'maf2' (type string), 
#'
#'
moduleSubsetByClinicalFeaturesServer <- function(id, maf, clinicalData){
  utilitybeltshiny::assert_reactive(maf)
  utilitybeltshiny::assert_reactive(clinicalData)
  
  moduleServer(id,
    function(input, output, session){
      
      validated_maf <- reactive({ validate(need(!is.null(maf()), message = "Please import a maf file")); maf() })
      faceting_variable_options <- reactive({ 
        colnames(clinicalData()) %>% sort() 
        })
      
      output$out_ui_grouping_variable <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_grouping_variable"), label = "Column Defining Groups", choices = faceting_variable_options(), options = shinyWidgets::pickerOptions(liveSearch = T), multiple = F)  })
      
      chosen_facet <- reactive({ 
        validate(need((clinicalData()[input$in_pick_grouping_variable] %>% base::unique() %>% levels()) >= 2, expr = "Grouping variable needs to have at least two levels"))
        return(input$in_pick_grouping_variable)
      })
      
      level_options <- reactive({ clinicalData()[chosen_facet()] %>% unique() })
      
      output$out_ui_group1 <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_group_1"), label = "Cohort 1", choices = level_options(), options = shinyWidgets::pickerOptions(liveSearch = T), multiple = F)  })
      output$out_ui_group2 <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_group_2"), label = "Cohort 2", choices = level_options(), options = shinyWidgets::pickerOptions(liveSearch = T), multiple = F)  })
      pick_group_1 <- reactive({input$in_pick_group_1})
      group_1_maf <- reactive({  input$in_pick_group_1; maftools::subsetMaf(maf = validated_maf(), clinQuery = paste0(chosen_facet(), " %in% '",  input$in_pick_group_1, "'"), isTCGA = TRUE) })
      group_2_maf <- reactive({ input$in_pick_group_2;maftools::subsetMaf(maf = validated_maf(), clinQuery = paste0(chosen_facet(), " %in% '",  input$in_pick_group_2, "'"), isTCGA = TRUE) })
      
      group_1_name <- reactive({input$in_pick_group_1})
      group_2_name <- reactive({input$in_pick_group_2})
      
      result <- reactive({
        list(
          maf1=group_1_maf(),
          maf1_name=group_1_name(),
          maf2=group_2_maf(),
          maf2_name=group_2_name()
        )
        })
      return(result)
  }
  )
}

# Copy in UI
# moduleSubsetByClinicalFeaturesUI("some_id")

# Copy in server
# moduleSubsetByClinicalFeaturesServer("some_id", optional_argument)
