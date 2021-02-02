moduleIntroduceAppUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(
      #h3("Introduction"),
      shinyWidgets::panel(heading = "Introduction",
          p("Welcome to", tags$b(" ShinyMaftools"), ", a user interface for the ", tags$b("maftools"), " package"),
          p("This tool requires two files as input:"),
          
          tags$ol(
            tags$li(p("A single", tags$b(" MAF "), "file containing all samples you're interested in."), 
              tags$ul(tags$li("If unsure of what this is, or you have one MAF per cohort / individual please see FAQ")), br()),
            tags$li("A", tags$b(" Clinical Feature File", "."),
              tags$ul(
                tags$li(" This is a tsv file containing the clinical data associated each sample (e.g. primary / relapse). "),
                tags$li("To create one, see the FAQ"))),
            
            ),
      ),
      #p("The quickest way to start is to upload your MAF file using the sidebar and start clicking tabs to explore what is possible.",
      #"If you don't have an MAF file, or you have other questions, see the FAQ below"),
      
      hr(),
      
      shinyWidgets::panel(heading="Common Research Questions:",
            
            h4("What genes in my cohort are most frequently mutated?"),
            tags$ol(
              tags$li("Upload MAF"),
              tags$li("Upload/Create Clinical Feature File"),
              tags$li("Navigate to", tags$b(" Single Cohort Statistics "), "module"),
              tags$li("Navigate to", tags$b(" Plots => View Oncoplot "))
            ),
            br(),
            
            h4("What pathways in my cohort are most frequently mutated?"),
            tags$ol(
              tags$li("Upload MAF"),
              tags$li("Upload/Create Clinical Feature File"),
              tags$li("Navigate to", tags$b(" Single Cohort Statistics "), "module"),
              tags$li("Navigate to", tags$b(" Plots => Mutated Pathways & Pathway Specific Plots"))
            ),
            br(),
            
            h4("What genes are enriched for mutations in one of two cohorts?"),
            tags$ol(
              tags$li("Upload MAF "),
              tags$li("Upload/Create Clinical Feature File"),
              tags$li("Navigate to ", tags$b("Compare Cohorts "), "module"),
              ),
          
            
            br(),
            h4("What genes are enriched for mutations in one group, relative to 2 or more other groups?"),
            tags$ol(
              tags$li("Upload MAF "),
              tags$li("Upload/Create Clinical Feature File"),
              tags$li("Navigate to ", tags$b("Enrichment Analysis "), "module"))
      ),
      hr(),
      
      shinyWidgets::panel(heading="FAQ",
        h4("What is an MAF file and how do I get one?"),
        p(
          "An", tags$b("MAF"), " or ", tags$b("Mutation Annotation Format"), " file stores information about variants called from NGS data (similar to a VCF).",
          "See", tags$a("https://docs.gdc.cancer.gov/Data/File_Formats/MAF_Format/"), " for more detail.",
          "If you don't have an MAF file, but you do have a VCF file, you can convert using the commandline tool ", tags$a(href="https://github.com/mskcc/vcf2maf","vcf2maf", target="_blank"), ".",
          "If you are not comfortable with running commandline tools or you have NGS data but no variants have been called yet, consult your resident bioinformatician"
          ),
        
        br(),
        
        h4("I don't have a clinical feature file. What do I do?"),
        p("Its extremely easy to create one.", 
        "Navigate to ", tags$b("Utilities => Add Clinical Data"), "and follow the instructions"),
        
        br(),
        
        h4("I have one MAF for each individual in my cohort but the tool only lets me upload one?"),
        p("Navigate to", tags$b("Utilities => Merge"), " and follow the instructions to merge your MAFs."),
        
        br(),
        
        h4("I have one MAF for each of my cohorts but the tool only lets me upload one. How do I do a multi-cohort analysis?"),
        tags$ol(
          tags$li("Navigate to ", tags$b("Utilities => Merge "), "and follow the instructions to combine your MAFs."),
          tags$li("Navigate to ", tags$b("Utilities => Add Clinical Data "), "and check the", tags$b("My MAF was merged and I want to know the names of the source files"), " checkbox"),
          tags$li("Download the resulting clinical feature file (containing cohort information) then import it using the 'Import clinical feature file' sidebar panel"),
        ),
        
        br(),
        
        h4("How do I exclude certain samples / genes from the analysis?"),
        tags$ol(
          tags$li("Navigate to ", tags$b("Utilities => Subset"), "and follow the instructions to subset your MAFs"),
          tags$li("Download subsetted MAF and reimport using the 'Import MAF' sidebar panel")
        ),
        
        br(),
        
        h4("I have a suspicious number of variants in my MAF! How do I apply sensible filters?"),
        p(
          "The mandatory columns of an MAF file don't include any information about variant quality,", tags$strong(" HOWEVER"), " many tools that convert VCFs to MAFs (e.g. vcf2maf), add info and filter columns from the VCF to the MAF file.",
          "If your MAF has inherited these metadata columns from the original VCF, you can filter on them.", 
          "For example, we might want to only consider variants that passed all filters (FILTER == PASS):"
        ),
        tags$ol(
          
          tags$li("Navigate to ", tags$b("Utilities => Subset")),
          tags$li("Enable", tags$b(" Subset by anything else ")),
          tags$li("Select the field you want to filter based on (e.g. 'FILTER')"),
          tags$li("Select the values in this column you want to include in your subsetted MAF file (e.g. 'PASS')"),
          tags$li("Download subsetted MAF and reimport using the 'Import MAF' sidebar panel")
        ),
        
        p(
          "Information about what these vcf-inherited columns mean should be present in the header of the VCF.",
          "If unsure of what a particular filter means, consult whoever produced the vcf."
          )
      ),
      
      hr(),
     
      shinyWidgets::panel(heading="Citation",
        p("This tool is simply a shiny wrapper for the ", tags$b('Maftools '), "R package published by Mayakonda et al. in 2018:", 
          br(),
          tags$blockquote("Mayakonda A, Lin DC, Assenov Y, Plass C, Koeffler HP. 2018. Maftools: efficient and comprehensive analysis of somatic variants in cancer. Genome Resarch PMID: 30341162"))
        )
      )
    )
  }

moduleIntroduceAppServer <- function(id){
  moduleServer(id,
    function(input, output, session){
      
  }
  )
}

# Copy in UI
# moduleIntroduceAppUI("some_id")

# Copy in server
# moduleIntroduceAppServer("some_id", optional_argument)
