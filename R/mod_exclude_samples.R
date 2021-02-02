
moduleExcludeSamplesUI <- function(id){
  ns <- NS(id)
  tagList(
    moduleSelectValuesFromVectorUI(ns("mod_select_samples_to_exclude_names"))
    )
  }

#Returns maf fed in except with samples excluded
moduleExcludeSamplesServer <- function(id, maf){
  moduleServer(id,
    function(input, output, session){
      
      sample_names <- reactive({
        validate(need(!is.null(maf()), "Please import MAF"))
        #browser()
       maftools::getSampleSummary(x = maf()) %>% 
          dplyr::pull(Tumor_Sample_Barcode) %>% 
          as.character() %>%
          return()
      })
      
      selected_samples <- moduleSelectValuesFromVectorServer("mod_select_samples_to_exclude_names",  vector_of_values = sample_names, label = "Samples to exclude", multiple_selectable = T)
    
      selected_to_exclude <- reactive({ sample_names()[sample_names() != selected_samples()]  })
      new_maf <- reactive({
        if (!is.null(selected_samples()) && length(selected_samples()) !=  length(sample_names()))
            maftools::subsetMaf(maf = maf(), tsb = selected_to_exclude()) %>% return()
        else 
          return(maf())
        })
    return(new_maf)
  }
  )
}

# Copy in UI

# moduleExcludeSamplesUI("some_id")

# Copy in server

# moduleExcludeSamplesServer("some_id", optional_argument)
