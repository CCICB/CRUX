#' shinyfile_import UI Function
#'
#' @description Adds a shinyFilesButton
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param tooltip_text The content of the tooltip (string / HTML)
#' @param tooltip_placement Where the tooltip should appear relative to its target (top, bottom, left, or right). Defaults to "right"
#' @param trigger What action should cause the tooltip to appear? (hover, focus, click, or manual). Defaults to "hover". 
#' @inheritParams shinyFiles::shinyFilesButton
#'
#'
#' @importFrom shiny NS tagList 
mod_shinyfile_import_ui <- function(id, title, label, multiple=FALSE, buttonType = "dark", style = NULL, viewtype = "detail", tooltip_text="", tooltip_placement = "right", trigger = "hover"){
  ns <- NS(id)
  
  tagList(
    shinyFiles::shinyFilesButton(id = ns("id_shinyfiles_button"), title = title, multiple = multiple, label=label, buttonType = buttonType, style = style, viewtype = viewtype),
    shinyBS::bsTooltip(id = ns("id_shinyfiles_button"), title = tooltip_text, placement = tooltip_placement, trigger = trigger)
  )
}
    
#' shinyfile_import Server Functions
#'
#'
#' @inheritParams mod_shinyfile_import_ui
#'
#' @return chosen filepath (string) (reactive). If multiple = TRUE, it returns a character vector instead of a string. When no file is selected, returns character(0)
#' 
#'
mod_shinyfile_import_server <- function(id){
  #browser()
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #browser()
    volumes <- c(Home = fs::path_home(), wd = ".", shinyFiles::getVolumes()(), example_data = system.file(package="CRUX", "example_data"))
    shinyFiles::shinyFileChoose(input, id = "id_shinyfiles_button", roots = volumes, session = session)
    
    chosenfilepath <- reactive({
      validate(need(length(input$id_shinyfiles_button), message = "Please import maf file"))
      shinyFiles::parseFilePaths(volumes, selection = input$id_shinyfiles_button)$datapath %>% return()
    })
    
    return(chosenfilepath)
  })
}


#' shinyfile_import UI Function
#'
#' @description Adds a shinyFilesButton
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param tooltip_text The content of the tooltip (string / HTML)
#' @param tooltip_placement Where the tooltip should appear relative to its target (top, bottom, left, or right). Defaults to "right"
#' @param trigger What action should cause the tooltip to appear? (hover, focus, click, or manual). Defaults to "hover". 
#' @inheritParams shinyFiles::shinyFilesButton
#'
#'
#' @importFrom shiny NS tagList 
mod_shinydir_import_ui <- function(id, title, label, multiple=FALSE, buttonType = "dark", style = NULL, viewtype = "detail", tooltip_text="", tooltip_placement = "right", trigger = "hover"){
  ns <- NS(id)
  
  tagList(
    shinyFiles::shinyDirButton(id = ns("id_shinyfiles_button"), title = title, multiple = multiple, label=label, buttonType = buttonType, style = style, viewtype = viewtype),
    shinyBS::bsTooltip(id = ns("id_shinyfiles_button"), title = tooltip_text, placement = tooltip_placement, trigger = trigger)
  )
}


#' shinyfile_import Server Functions
#'
#'
#' @inheritParams mod_shinyfile_import_ui
#'
#' @return chosen filepath (string) (reactive). If multiple = TRUE, it returns a character vector instead of a string. When no file is selected, returns character(0)
#' 
#'
mod_shinydir_import_server <- function(id){
  #browser()
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #browser()
    volumes <- c(Home = fs::path_home(), wd = ".", shinyFiles::getVolumes()(), example_data = system.file(package="CRUX", "example_data"))
    shinyFiles::shinyDirChoose(input, id = "id_shinyfiles_button", roots = volumes, session = session)
    
    chosenfilepath <- reactive({
      validate(need(length(input$id_shinyfiles_button), message = "Please select a directory"))
      shinyFiles::parseDirPath(volumes, selection = input$id_shinyfiles_button) %>% return()
    })
    
    return(chosenfilepath)
  })
}

