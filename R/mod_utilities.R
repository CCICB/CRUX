moduleUtilitiesUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    shiny::tabsetPanel(id = ns("utilities"), 
       #         tabPanel(title = "Merge Datasets", value = 'merge', shinyWidgets::panel(moduleMergeMafsUI(id = ns("mod_merge")))),
                tabPanel(title = "Subset Data", value = 'subset', shinyWidgets::panel(moduleSubsetMafsUI(id=ns("mod_subset")),style = "background: #4d5375")),
                tabPanel(title = "Merge data", value = 'merge', shinyWidgets::panel(mod_merge_ui(id=ns("mod_merge")),style = "background: #4d5375"))
    )
  )
}

moduleUtilitiesServer <- function(id, maf_data_pool){
  moduleServer(id,
    function(input, output, session){
      #moduleMergeMafServer(id = "mod_merge", maf_data_pool)
      mod_merge_server(id = "mod_merge", maf_data_pool = maf_data_pool)
      moduleSubsetMafsServer(id = "mod_subset", maf_data_pool)
  }
  )
}
