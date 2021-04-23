## To add a new tool

# [1] Write a 'maf conversion function'. see ?external_tools_add_tool_to_dataframe for details on the format
# [2] Write a function for adding the tool to an existing dataframe: see `external_tools_load_bbglab_oncodrive_fml` for an example
# [3] Add the function from [2] to external_tools_load_all_tools
# [4] run `devtools::load_all()` in console, then runexternal_tools_update_builtin_dataset to update the dataframe that our external tool module pulls from


#' external_tools_add_tool_to_dataframe
#'
#' Appends tool metadata on the end of a dataframe
#'
#' @param external_tools_df the dataframe to add tool metadata to. By default, will create and return a new dataframe
#' @param tool_name name of tool (string)
#' @param tool_id id of tool (string)
#' @param tool_group research group that built/maintains the tool (string)
#' @param tool_class class of tool. Usually 'Positive Selection', 'Variant Interpretation' (string)
#' @param tool_description brief description of tool (string)
#' @param instructions brief description of how to use the tool. Any HTML tags in the string will be correctly resolved. (string)
#' @param website url of tool (string)
#' @param doi publicatoin doi (string)
#' @param requires_maf_export does the tool require a maf to be exported in some other form (flag)
#' @param maf_conversion_function only relevent if requires_maf_export == true. A function that takes a MAF object (first argument), a filepath (second argument) and, if requires_gene_selection == TRUE, a gene name (third argument)  writes a file to that filepath. The idea is that said file can then be used as input to the specified tool.
#' @param extension what type of file is written by maf_conversion_function. defualt is 'tsv'. Used to appropriately name downloaded file  (string)
#' @param requires_gene_selection does user need to select a specific gene for export to work? (bool)
#' 
#' @return dataframe containing external_tool_metadata
#' @export
external_tools_add_tool_to_dataframe <- function(external_tools_df = dplyr::tibble(), tool_name, tool_id, tool_group, tool_class, tool_description, instructions = "No instructions available yet. You're on your own buddy", website, doi, requires_maf_export = TRUE, requires_gene_selection = FALSE, maf_conversion_function = NA, extension="tsv") {
  assertthat::assert_that(is.data.frame(external_tools_df))
  utilitybelt::assert_non_empty_string(tool_name)
  utilitybelt::assert_non_empty_string(tool_id)
  utilitybelt::assert_non_empty_string(tool_group)
  utilitybelt::assert_non_empty_string(tool_class)
  utilitybelt::assert_non_empty_string(tool_description)
  assertthat::assert_that(assertthat::is.string(instructions)) 
  utilitybelt::assert_non_empty_string(website)
  utilitybelt::assert_non_empty_string(doi)
  assertthat::assert_that(assertthat::is.flag(requires_maf_export))
  assertthat::assert_that(assertthat::is.flag(requires_gene_selection))
  assertthat::assert_that(assertthat::is.string(extension))
  
  if(requires_maf_export) { 
    assertthat::assert_that(is.function(maf_conversion_function), msg = "Must supply a maf_conversion_function to external_tools_add_tool_to_dataframe. 'maf_conversion_function' cannot be NA if 'requires_maf_export' is TRUE")
    expected_number_of_args = ifelse(requires_gene_selection, yes=3, no=2) # should maf export function take 2 or 3 arguments. If we need to specify a gene we need a third argument
    assertthat::assert_that(utilitybelt::fun_count_arguments(maf_conversion_function) == expected_number_of_args, msg = "maf_conversion_function must have 2 or 3 arguments. The first should take a maf object, the second a filepath. The third is only used if `requires_gene_selection` is TRUE and should take the name of a gene (Hugo SYmbol). See ?external_tools_add_tool_to_dataframe for details")
  }
  
  new_df <- data.frame(
    tool_name, tool_id, tool_group, tool_class, tool_description, instructions, website, doi, requires_maf_export, requires_gene_selection, I(list(maf_conversion_function)), extension
  )
  
  names(new_df) <- c("tool_name","tool_id","tool_group","tool_class","tool_description", "instructions","website","doi", "requires_maf_export", "requires_gene_selection", "maf_conversion_function", "extension")
  
  new_df <- rbind(external_tools_df, new_df)
  
  new_df %>% dplyr::distinct() %>% dplyr::tibble()
}

#' external_tools_get_property_by_tool_name
#'
#' @param external_tools_df the dataframe to add tool metadata to. By default, will use global variable: GLOBAL_external_tools_dataframe
#' @param tool_name name of tool
#' @param property_to_retrieve which property to retrieve. see details for options. (string)
#'
#' @return
#' @export
#'
#' @details 
#' 
#' \strong{ Options for property_to_retrieve }
#' 
#' \itemize{
#' \item tool_name
#' \item tool_id
#' \item tool_group
#' \item tool_class
#' \item tool_description
#' \item instructions
#' \item website
#' \item doi
#' \item requires_maf_export
#' \item requires_gene_selection
#' \item maf_conversion_function 
#' \item extension
#' }
#' 
#' See \code{?external_tools_add_tool_to_dataframe} for more info on these properties
#' 
#' run \code{external_tool_metadata} to see the built in tool metadata dataframe
#' 
#' @examples 
#' external_tools_get_property_by_tool_name(tool_name = "OncodriveFML", "website")
external_tools_get_property_by_tool_name <- function(tool_name, property_to_retrieve, external_tools_df=shinymaftools::external_tool_metadata){
  #browser()
  assertthat::assert_that(assertthat::is.string(property_to_retrieve))
  assertthat::assert_that(
    any(
      property_to_retrieve %in% c(
        "tool_name",
        "tool_id",
        "tool_group",
        "tool_class",
        "tool_description",
        "instructions",
        "website",
        "doi",
        "requires_gene_selection",
        "maf_conversion_function",
        "extension"
      )), msg = "[external_tools_get_property_by_tool_name] Invalid Choice of property_to_retrieve. Please see ?external_tools_get_property_by_tool_name for options"
  )
  
  #browser()
  retrieved_value <- external_tools_df[external_tools_df[["tool_name"]]==tool_name, property_to_retrieve] %>% unlist()
  assertthat::assert_that(length(retrieved_value) == 1, msg = paste0("[external_tools_get_property_by_tool_name] found '",length(retrieved_value),"' instead of '1' value using the query: tool_name == '", tool_name,"' AND property_to_retrieve == '",property_to_retrieve,"'"))
  
  if(property_to_retrieve=="maf_conversion_function")
    retrieved_value <- retrieved_value %>% purrr::pluck(1) 
  
  return(retrieved_value)
}


#' Load tool metadata into global variable
#' 
#' Loads metadata for the OncodriveFML tool into a global variable \strong{GLOBAL_external_tools_dataframe}
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame)
#' @export
#'
#' @examples
#' external_tools_load_bbglab_cgi()
external_tools_load_bbglab_oncodrive_fml <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df, 
    tool_name = "OncodriveFML",
    tool_id = "bbglab_oncodrive_fml",
    tool_group = "BBGLab",
    tool_class = "Positive Selection",
    instructions = as.character(
      tags$ol(
        tags$li("Make a free BBGLab OncodriveFML account by clicking 'Log In ==> Sign Up'"),
        tags$li("Import downloaded file into 'Mutations file' slot and select the type of sequencing used to generate your dataset"),
        tags$li("Configure as required and run")
      )
    ),
    tool_description = "A method to analyze the pattern of somatic mutations across tumors in both coding and non-coding genomic regions to identify signals of positive selection",
    website = "http://bbglab.irbbarcelona.org/oncodrivefml/analysis",
    doi = "https://doi.org/10.1186/s13059-016-0994-0",
    maf_conversion_function = external_tools_convert_maf_to_bbglab,
    extension = "tsv"
  )
}



#' Load tool metadata into global variable
#' 
#' Appends  metadata for the OncodriveCLUSTL tool onto external_tools_df
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame)
#' @export
#'
#' @examples
#' external_tools_load_bbglab_cgi()
external_tools_load_bbglab_oncodrive_clustl <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "OncodriveCLUSTL",
    tool_id = "bbglab_oncodrive_clustl",
    tool_group = "BBGLab",
    tool_class = "Positive Selection",
    tool_description = "A new method to identify clustering of somatic mutations in both coding and non-coding genomic regions to detect signals of positive selection.",
    website = "http://bbglab.irbbarcelona.org/oncodriveclustl/analysis",
    doi = "https://doi.org/10.1093/bioinformatics/btz501",
    maf_conversion_function = external_tools_convert_maf_to_bbglab,
    extension = "tsv"
  ) 
}

#' Conversion Function: MAF to BBGLab Cancer Genome Interpreter Input
#'
#' Convets MAF object to BBGLab Oncodrive mutation format.
#'
#' @param maf (MAF)
#'
#' @return
#' @export
#'
external_tools_convert_maf_to_oncodrive_return_dataframe <- function(maf){
  maf %>% 
    maftools_get_all_data() %>%
    dplyr::select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>% 
    dplyr::arrange(suppressWarnings(readr::parse_number(Chromosome)), Chromosome, Start_Position) %>% 
    dplyr::rename(chr=Chromosome, pos=Start_Position, ref=Reference_Allele, alt=Tumor_Seq_Allele2, sample=Tumor_Sample_Barcode) %>%
    return()
}



#' Load tool metadata into environment. Returns 
#' 
#' Appends  metadata for the "Cancer Genome Interpreter (cgi) tool onto external_tools_df
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame)
#' @export
#'
#' @examples
#' external_tools_load_bbglab_cgi()
external_tools_load_bbglab_cgi <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Cancer Genome Interpreter",
    tool_id = "bbglab_oncodrive_cgi",
    tool_group = "BBGLab",
    tool_class = "Variant Interpretation",
    tool_description = "Platform to facilitate the interpretation of alterations in a patient’s tumor.",
    website = "https://www.cancergenomeinterpreter.org/analysis",
    doi = "https://genomemedicine.biomedcentral.com/articles/10.1186/s13073-018-0531-8",
    maf_conversion_function = external_tools_convert_maf_to_bbglab,
    extension = "tsv"
  )
}

#' Conversion Function: MAF to BBGLab Cancer Genome Interpreter Input
#'
#' Convets MAF object to BBGLab Cancer Genome Interpreter Input (Genomic tabular format) then writes to a file.
#' Works as a maf_conversion_function. Please don't change arguments
#'
#'
#' @param maf (MAF)
#' @param filepath (string)
#'
#' @return
#' @export
#'
external_tools_convert_maf_to_bbglab <- function(maf, filepath){
  #browser()
  maf %>% 
    external_tools_convert_maf_to_bbglab_return_dataframe() %>% 
    data.table::fwrite(file = filepath, sep="\t", col.names = TRUE)
}

#' Conversion Function: MAF to BBGLab Cancer Genome Interpreter Input
#'
#' Convets MAF object to BBGLab Cancer Genome Interpreter Input (Genomic tabular format) (a tsv).
#'
#' @param maf (MAF)
#'
#' @return
#' @export
#'
external_tools_convert_maf_to_bbglab_return_dataframe <- function(maf){
  maf %>% 
    maftools_get_all_data() %>%
    dplyr::select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>% 
    dplyr::arrange(readr::parse_number(as.character(Chromosome)), Chromosome, Start_Position) %>% 
    dplyr::rename(chr=Chromosome, pos=Start_Position, ref=Reference_Allele, alt=Tumor_Seq_Allele2, sample=Tumor_Sample_Barcode) %>%
    return();
}


#' MAF + Gene --> cBioPortal mutation_mapper_input
#'
#' Takes a MAF and converts variants hitting a particular gene to the form the cBioPortal Mutation Mapper uses as input.
#' 
#' You can find the tool at: https://www.cbioportal.org/mutation_mapper
#'
#' @param maf a maftools maf object (maf)
#' @param gene_hugo_symbol hugo symbol representing the gene to export (string)
#'
#' @return dataframe in a form usable as input at https://www.cbioportal.org/mutation_mapper (data.frame)
#' @export
#'
external_tools_convert_maf_to_cbioportal_mutation_mapper_return_dataframe <- function(maf, gene_hugo_symbol){
  maf %>% 
    maftools_get_all_data(include_silent_mutations = TRUE) %>% 
    dplyr::filter(Hugo_Symbol == gene_hugo_symbol) %>% 
    dplyr::select(
      Sample_ID=Tumor_Sample_Barcode, 
      Chromosome, 
      Start_Position,
      End_Position,
      Reference_Allele, 
      Variant_Allele = Tumor_Seq_Allele2
      )
}

external_tools_convert_maf_to_cbioportal_mutation_mapper <- function(maf, filepath, gene_hugo_symbol){
  external_tools_convert_maf_to_cbioportal_mutation_mapper_return_dataframe(maf, gene_hugo_symbol = gene_hugo_symbol) %>%
    data.table::fwrite(file = filepath, sep = "\t", col.names = TRUE)
}

external_tools_load_cbioportal_mutation_mapper <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "cBioportal Mutation Mapper",
    tool_id = "cbioportal_mutation_mapper",
    tool_group = "cBioPortal",
    tool_class = "Lollipop Plot",
    tool_description = "Interprets mutations with protein annotations",
    requires_gene_selection = TRUE,
    website = "https://www.cbioportal.org/mutation_mapper",
    doi = "https://www.cbioportal.org/mutation_mapper",
    instructions = as.character(
      tags$ol(
        tags$li("Go to https://www.cbioportal.org/mutation_mapper"),
        tags$li("Upload File"),
        tags$li("Click Visualise")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_cbioportal_mutation_mapper,
    extension = "tsv"
  )
}

external_tools_convert_maf_to_signal2_return_dataframe <- function(maf){
  maf %>%
    maftools_get_all_data(include_silent_mutations = TRUE) %>% 
    dplyr::filter(Start_Position==End_Position) %>% #Prob should filter for SNVs only
    dplyr::filter(Tumor_Seq_Allele2 %in% c("A", "C", "T", "G")) %>%
    dplyr::select(
      "Sample Name" = Tumor_Sample_Barcode, 
      "Chromosome" = Chromosome, 
      "Position" = Start_Position, 
      "Original base" = Reference_Allele, 
      "Mutated base" = Tumor_Seq_Allele2
      ) 
}

external_tools_convert_maf_to_signal2 <- function(maf, filepath){
  df = external_tools_convert_maf_to_signal2_return_dataframe(maf) 
  sample_names = unique(df[["Sample Name"]])
  
  temp_directory = tempdir()
  
  #Download 30 samples at a time
  chunked_names <- split(sample_names, ceiling(seq_along(sample_names)/30)) #Change 30 to higher numbers to do larger batches. Once signal is beefed up, we should be able to upload all samples at once
  
  files =  paste0(tempdir(), "/signal_input", seq_along(chunked_names), ".tsv")
  
  for (i in seq_along(chunked_names)) {
    samplenames_in_chunk = chunked_names[[i]]
    df %>%
      dplyr::filter(`Sample Name` %in% samplenames_in_chunk) %>%
      write.table(file = files[i], col.names = FALSE, sep = "\t", quote = FALSE, row.names = FALSE)
  }
  
  zip(filepath, files, flags = "-j")
}

external_tools_load_maf_to_signal2 <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Signal",
    tool_id = "signal",
    tool_group = "Mutational Signature Analysis",
    tool_class = "Mutational Signature Analysis",
    tool_description = "Identifies known mutational signatures in each sample",
    requires_gene_selection = FALSE,
    website = "https://signal.mutationalsignatures.com/analyse2",
    doi = "https://doi.org/10.1038/s43018-020-0027-5",
    instructions = as.character(
      tags$ol(
        tags$li("Click 'Upload Files'"),
        tags$li("Unzip downloaded file. Upload one of the 'signal_input' file (cannot do all at once since Signal can't handle the stress)."),
        tags$li("Set 'Format' to [Variants] TSV/TXT."),
        tags$li("Select reference build (Human GRCh37 if using pre-packaged TCGA/PCAWG datasets)"),
        tags$li("Set organ [Optional but HIGHLY RECCOMENDED unless you want to scan for manually-selected signatures]"),
        tags$li("Choose/create a project to save to and which analyses to run then click 'Submit'"),
        tags$li("Repeat for all other 'signal_input' files")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_signal2,
    extension = "zip"
  )
}

#' Load tool metadata into global variable
#' 
#' Loads metadata for all tools, returning a dataframe.
#' If any of the constitutent functions are changed, run \code{external_tools_update_builtin_dataset}
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame). see \code{?external_tools_add_tool_to_dataframe} for more info on the columnss in this data.frame
#' @export
#'
#' @examples
#' external_tools_load_all_tools()
external_tools_load_all_tools <- function(){
  external_tools_load_bbglab_oncodrive_fml() %>%
    external_tools_load_bbglab_oncodrive_clustl() %>%
    external_tools_load_bbglab_cgi() %>%
    external_tools_load_cbioportal_mutation_mapper() %>% 
    external_tools_load_maf_to_signal2() %>%
    return()
}

#' Update builtin external_tools dataset
#'
#' @description Takes the output of external_tools_load_all_tools and saves it to shinymaftools/data as the dataset: \strong{external_tool_metadata}.
#' This saved dataset is what is used by the app. If you make change to any external_tools_load_... function, you must rerun this function.
#'
external_tools_update_builtin_dataset <- function(){
  external_tool_metadata = external_tools_load_all_tools()
 usethis::use_data(external_tool_metadata, overwrite = TRUE) 
}

