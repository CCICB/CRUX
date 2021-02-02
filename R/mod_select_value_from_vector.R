# Reads a dataframe and returns :
#[UI] a taglist containing a drop-down box that lets you select any value from the supplied vector_of_values
#[Server] returns the selected value

moduleSelectValuesFromVectorUI <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(outputId = ns("out_ui_values_list"))
    )
}

#vector_of_values contains all the possible options for drop-down list

moduleSelectValuesFromVectorServer <- function(id, vector_of_values, label, multiple_selectable, bonus_options=NULL, livesearch = T, actionbox = T){
  moduleServer(id,
    function(input, output, session){
      
      #Get options
      options.v <- reactive({
        #browser()
        validate(need(!is.null(vector_of_values()), "moduleSelectValueFromvector_of_valuesServer requires a non-null vector_of_values... please import data"))
        return(c(bonus_options, vector_of_values()))
        }) 
      
      #Render picker
      output$out_ui_values_list <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_from_names"), label = label, choices = options.v(), options = shinyWidgets::pickerOptions(liveSearch = livesearch, actionsBox = actionbox), multiple = multiple_selectable) })
      
      #Return value
      selectedvalue <- reactive({ input$in_pick_from_names })
      
      return(selectedvalue)
  }
  )
}
