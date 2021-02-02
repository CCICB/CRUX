moduleUtilitiesUI <- function(id){
  ns <- NS(id)
  tagList(
    shiny::tabsetPanel(id = ns("utilities"), 
       #         tabPanel(title = "Merge Datasets", value = 'merge', shinyWidgets::panel(moduleMergeMafsUI(id = ns("mod_merge")))),
                tabPanel(title = "Subset Data", value = 'subset', shinyWidgets::panel(moduleSubsetMafsUI(id=ns("mod_subset")))),
                tabPanel(title = "Merge data", value = 'merge', shinyWidgets::panel(mod_merge_ui(id=ns("mod_merge"))))
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
