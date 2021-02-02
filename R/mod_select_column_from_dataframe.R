# Reads a dataframe and returns :
#[UI] a taglist containing a drop-down box that lets you select any column name from the supplied table
#[Server] returns the selected value

moduleGetColumnNameUI <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(outputId = ns("out_ui_colname_list"))
    )
}

#named_data can be a dataframe or ANYTHING which will return the desired options when names() is run on it

moduleGetColumnNameServer <- function(id, named_data, label, multiple_selectable, bonus_options=NULL, livesearch = T, actionbox = T){
  moduleServer(id,
    function(input, output, session){
      
      #Get options
      options.v <- reactive({
        validate(need(!is.null(named_data()), "moduleGetColumnNameServer requires a dataframe ... please import data"))
        
        return(c(bonus_options, names(named_data())))
        }) 
      
      #Render picker
      output$out_ui_colname_list <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_from_names"), label = label, choices = options.v(), options = shinyWidgets::pickerOptions(liveSearch = livesearch, actionsBox = actionbox), multiple = multiple_selectable) })
      
      #Return value
      selectedvalue <- reactive({ input$in_pick_from_names })
      
      return(selectedvalue)
  }
  )
}
