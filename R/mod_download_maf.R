moduleDownloadMafUI <- function(id, label = "Download", tooltip_text="", tooltip_pos="right", style = "unite", color = "default", size="sm"){
  ns <- NS(id)
  tagList(
    shinyWidgets::downloadBttn(outputId = ns("out_download_bttn"), label = label, style = style, color = color, size = size) %>%
      shinyBS::tipify(title=tooltip_text, placement = tooltip_pos)
    )
}


#Downloads the WHOLE MAF file (w/ silent mutations).
moduleDownloadMafServer <- function(id, maf, basename= "download"){
  moduleServer(id,
    function(input, output, session){
      
    output$out_download_bttn <- downloadHandler(filename = paste0(basename, "_maftools.maf"), content = function(file) {
      download_maf(maf=maf, file = file)
      })
  }
  )
}


# download)<- function (maf, basename = NULL) 
# {
#   if (is.null(basename)) {
#     stop("Please provide a basename for output file.")
#   }
#   write.table(x = getGeneSummary(maf), file = paste(basename, "_geneSummary.txt", sep = ""), sep = "\t", quote = FALSE, row.names = FALSE)
#   write.table(x =maftools::getSampleSummary(maf), file = paste(basename, "_sampleSummary.txt", sep = ""), sep = "\t", quote = FALSE, row.names = FALSE)
#   write.table(x = maf@summary, file = paste(basename, "_summary.txt", sep = ""), sep = "\t", quote = FALSE, row.names = FALSE)
#   
# }

# Copy in UI
# moduleDownloadMafUI("some_id")

# Copy in server
# moduleDownloadMafServer("some_id", optional_argument)
