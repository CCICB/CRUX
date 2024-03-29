#' data_import2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id) {
  ns <- NS(id)
  
  # Step 1: Choose Input Data Type
  tagList(
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 1: "), "Choose Input Data Type"),
      fluidRow(
        shinyWidgets::awesomeRadio(
          inputId = ns("in_radio_input_data_type"),
          label = "Genomic Data Filetype",
          choices = c('MAF', 'ANNOVAR', 'VCF (in development)' = 'VCF'),
          selected = "MAF",
          inline = TRUE,
          checkbox = TRUE
        ) %>% col_3(),
        shinydashboard::box(
          title = "Choose your input filetype",
          width = "100%",
          "CRUX supports 2 different filetypes for describing your cohorts mutation data. MAFs and ANNOVAR files.",
          tags$br(),tags$br(),
          shinyWidgets::alert(status = 'info', icon("circle-info"), "Please ensure your datasets do not include ", 
          link(url="https://www.ncbi.nlm.nih.gov/books/NBK553131/", text = tags$strong("Protected Health Information (PHI)")),
          ". See the ",tags$strong("privacy module"), "for more information")
        ) %>% column(width = 9)
      )
    ),
    icon_down_arrow(break_after = TRUE),
    
    # Step 2: Import Mutation Data
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 2: "), "Import Mutation Data"),
      fluidRow(
        shiny::fileInput(inputId = ns("in_file_mutations"), label = "Select Mutation File", accept = c(".maf", ".txt", ".vcf", '.gz')) %>% col_3(),
        shiny::conditionalPanel(
          condition = "input.in_radio_input_data_type == 'MAF'", 
          ns = ns,
          shinydashboard::box(
            title = "Mutation Annotation Format (MAF) Files",
            width = "100%",
            "MAF files are tabular files that store a list of mutations.
            To learn more about how to get your data in MAF format: ",
            link(url = "https://crux-docs.readthedocs.io/en/latest/usage/importing_data.html", newtab = TRUE, text = "see here"),
            tags$br(),tags$br(),
            shiny::downloadLink(outputId = ns("out_download_mafdemo"), label = "Download Demo MAF"),
          ) %>% column(width = 9),
        ),
        shiny::conditionalPanel(
          condition = "input.in_radio_input_data_type == 'ANNOVAR'", 
          ns = ns,
          shinydashboard::box(
            title = "ANNOVAR Annotated Mutation Files",
            width = "100%",
            "ANNOVAR is a popular tool for variant annotations wihch produces mutation-level, tab-separated datasets describing each mutations impact.
            Many types of files can be converted to ANNOVAR tables. To learn more about how to get your data into ANNOVAR format: ",
            link(url = "https://crux-docs.readthedocs.io/en/latest/usage/importing_data.html", newtab = TRUE, text = "see here"),
            tags$br(),tags$br(),
            shiny::downloadLink(outputId = ns("out_download_annovar"), label = "Download Demo ANNOVAR file"),
          ) %>% column(width = 9)
          ),
        shiny::conditionalPanel(
            condition = "input.in_radio_input_data_type == 'VCF'", 
            ns = ns,
            shinydashboard::box(
              title = "VCF Files Not Yet Supported! Please Convert to ANNOVAR / MAF",
              width = "100%",
              status = "warning",
              "
              VCF files are not directly supported in CRUX due to the need for mutation impact annotations, 
              which may be missing in standard, vanilla VCFs. 
              To work with VCF files in CRUX, you can use the command-line tool ", link(url="https://github.com/mskcc/vcf2maf", newtab=TRUE,"vcf2maf")," for annotation and conversion to MAF format. 
              If you prefer a web-based approach, consider converting VCF files to annovar format as described ",
            link(url = "https://crux-docs.readthedocs.io/en/latest/usage/importing_data.html", newtab = TRUE, text = "here")
            ) %>% column(width = 9),
        ),
      )
    ),
    icon_down_arrow(break_after = TRUE),

     # Step 3: Import Clinical Annotations
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 3: "), "Import Clinical Annotations"),
      fluidRow(
        shiny::fileInput(inputId = ns("in_file_clindata"), accept = c(".tsv", ".txt", ".csv"), label = HTML("Select Clinical Annotations File (", as.character(shiny::actionLink(inputId = ns("in_action_clear_clindata"), label = "reset")), ")")) %>% col_3(),
        shinydashboard::box(
          title = "Clinical Annotation Files",
          width = "100%",
          "Clinical data associated with each sample (a.k.a Tumor_Sample_Barcode) in the Mutation File. Can be a csv or a tsv file.
          The only requirement is that this file includes a Tumour_Sample_Barcode column. 
          Ideally, annotation file would also include columns describing clinical data, survival information and other interesting features associated with samples.
          To learn more about how to prepare your clinical annotations file: ",
          link(url = "https://crux-docs.readthedocs.io/en/latest/usage/importing_data.html", newtab = TRUE, text = "see here"),
          tags$br(),tags$br(),
          shiny::downloadLink(outputId = ns("out_download_clindemo"), label = "Download Demo Annotations", class = "info"),
        ) %>% column(width = 9)
      )
    ),
    icon_down_arrow(break_after = TRUE),

    # Step 4: Add Cohort Level Metadata
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 4: "), "Add Cohort Level Metadata"),
      shiny::fluidRow(
        shiny::textInput(inputId = ns("in_text_displayname"), label = "Display Name", placeholder = "High Grade Glioma", width = "100%") %>% col_4(),
        shiny::textInput(inputId = ns("in_text_shortname"), label = "Short Name", placeholder = "HGG", width = "100%") %>% col_4(),
        shiny::textInput(inputId = ns("in_text_data_source"), label = "Source", placeholder = "TCGA", width = "100%") %>% col_4(),
        shiny::textInput(inputId = ns("in_text_description"), label = "Description", placeholder = "High Grade Glioma from a pediatric cohort with survival <30%", width = "100%") %>% col_12()
      )
    ),
    icon_down_arrow(break_after = TRUE),

    # Step 5: Import Data
    shinyWidgets::panel(
      heading = "Step 5: Import Data!",
      shiny::actionButton(
        inputId = ns("in_bttn_import"),
        label = "Import",
        width = "100%",
        icon = icon("file-import"),
        class = "btn btn-primary"
      )
    )
  )
}

#' data_import2 Server Functions
#'
#' @noRd
mod_data_import_server <- function(id, maf_data_pool) {
  assertions::assert_reactive(maf_data_pool)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define some basic reactive values
    cohort_metdata_filled <- reactive({
      nchar(input$in_text_displayname) > 0 &
        nchar(input$in_text_shortname) > 0 &
        nchar(input$in_text_data_source) > 0 &
        nchar(input$in_text_description) > 0
    })
    
    expected_mutation_filetype <- reactive({input$in_radio_input_data_type})

    # Define clindata path as a reactive value so we can reset it
    rv <- reactiveValues()
    observe({ 
      req(input$in_file_clindata)
      rv$path_clindata <- input[["in_file_clindata"]]$datapath 
      })

    # Clear selected clinical annotations when reset button clicked
    observeEvent(input$in_action_clear_clindata, handlerExpr = isolate({
      shinyjs::reset(id = "in_file_clindata")
      rv$path_clindata <- NULL
    }))
    

    # When clicking the button 'add to data pool'
    observeEvent(input$in_bttn_import, isolate({
      
      # Check a maf file has been supplied
      if (is.null(input[["in_file_mutations"]]$datapath)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Missing MAF",
          text = "Please select a MAF file",
          type = "warning"
        )
        return(NULL)
      }

      # Check all metadata has been supplied
      if (!cohort_metdata_filled()) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Missing metadata",
          text = tags$div("Please fill out all fields in ",tags$strong("Step 4: Add cohort level metadata")),
          html = TRUE,
          type = "warning"
        )
        return(NULL)
      }


      
      # Try and read maf file
      # If it fails, save error message to maf variable instead
      
      # But first start the modal spinner
      shinybusy::show_modal_spinner(text = "Importing your data ...", session = session)
      maf <- tryCatch(
        {
          read_maf_flexible(
            path_mutations = input[["in_file_mutations"]]$datapath,
            path_clindata = rv$path_clindata
          )
          # maftools::read.maf(maf = input[["in_file_mutations"]]$datapath, clinicalData = input[["in_file_clindata"]]$datapath)
        },
        error = function(err) {
          return(as.character(err))
        },
        warning = function(warn) {
          return(as.character(warn))
        }
      )

      # If MAF read failed, inform user of error
      if (is.character(maf)) {
        message <- if (!is.null(input[["in_file_clindata"]])) "and clinical metadata " else " "
        shinyWidgets::sendSweetAlert(
          session = session,
          title = paste0("Failed to Read Mutation File"),
          text = tags$div(
            "Please ensure ",expected_mutation_filetype()," file/s ", message, "are formatted correctly.", 
            tags$br(), tags$br(), 
            err2html(maf)
          ),
          html = TRUE,
          type = "warning"
        )
        
        # Remove modal Spinner error in reading MAF
        shinybusy::remove_modal_spinner(session = session)
        return(NULL)
      }

      # Add to data pool
      updated_maf_data_pool <- user_to_dataset_to_data_pool(
        maf_data_pool = maf_data_pool(),
        filepath = input[["in_file_mutations"]]$datapath,
        display_name = input[["in_text_displayname"]],
        short_name = input[["in_text_shortname"]],
        description = input[["in_text_description"]],
        data_source = input[["in_text_data_source"]],
        loaded_data = maf
      )
      maf_data_pool(updated_maf_data_pool)
      
      # Remove Modal Spinner
      shinybusy::remove_modal_spinner(session = session)
      
      # Send success message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Dataset Succesfully Added",
        text = "Go to 'Single Cohort Genomics' module and select your new dataset to start deriving insights!",
        type = "success"
      )
    }))


    # Download Handlers
    output$out_download_mafdemo <- downloadHandler(filename = "demo.maf", content = function(file) {
      system.file("example_data/APL_primary_and_relapse.maf", package = "CRUX") %>%
        data.table::fread() %>%
        data.table::fwrite(file, sep = "\t")
    })

    output$out_download_clindemo <- downloadHandler(
      filename = function() { 
        if(input$in_radio_input_data_type %in% c("MAF", "ANNOVAR"))
          return(paste0("demo_clinical_features.",input$in_radio_input_data_type, ".tsv"))
        else 
          return("demo_clinical_features.MAF.tsv")
        }, 
      content = function(file) {
      if(input$in_radio_input_data_type == "MAF")
        clinpath = system.file("example_data/APL_primary_and_relapse.clinical_features.tsv", package = "CRUX")
      else if(input$in_radio_input_data_type == "ANNOVAR")
        clinpath = system.file("example_data/multianno.clinical_features.tsv", package = "CRUX")
      else
        clinpath = system.file("example_data/APL_primary_and_relapse.clinical_features.tsv", package = "CRUX")
      
      clinpath %>%
        data.table::fread() %>%
        data.table::fwrite(file, sep = "\t")
    })
    
    output$out_download_annovar <- downloadHandler(filename = "demo_annovar.multianno.txt", content = function(file) {
      system.file("example_data/variants.hg19_multianno.txt", package = "CRUX") %>%
        data.table::fread() %>%
        data.table::fwrite(file, sep = "\t")
    })
    
  
  })
}
