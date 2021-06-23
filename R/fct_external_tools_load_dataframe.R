## To add a new tool


# Instructions for adding tools -------------------------------------------
# [1] Write a 'maf conversion function'. see ?external_tools_add_tool_to_dataframe for details on the format
# [2] Write a function for adding the tool to an existing dataframe: see `external_tools_load_bbglab_oncodrive_fml` for an example
# [3] Add the function from [2] to external_tools_load_all_tools
# [4] run `devtools::load_all()` in console, then run external_tools_update_builtin_dataset() to update the dataframe that our external tool module pulls from



# Core functionality ------------------------------------------------------
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
#' @param platform what platform do we access the tool from. Examples include web, desktop app, cli  (string)
#' @param website url of tool (string)
#' @param doi publicatoin doi (string)
#' @param requires_maf_export does the tool require a maf to be exported in some other form (flag)
#' @param maf_conversion_function only relevent if requires_maf_export == true. A function that takes a maf_dataset_wrapper object (first argument), a filepath (second argument) and, if requires_gene_selection == TRUE, a gene name (third argument)  writes a file to that filepath. The idea is that said file can then be used as input to the specified tool.
#' @param extension what type of file is written by maf_conversion_function. default is 'tsv'. Used to appropriately name downloaded file  (string)
#' @param requires_gene_selection does user need to select a specific gene for export to work? (bool)
#' @return dataframe containing external_tool_metadata
#'
external_tools_add_tool_to_dataframe <- function(external_tools_df = dplyr::tibble(), tool_name, tool_id, tool_group, tool_class, tool_description, instructions = "No instructions available yet. You're on your own buddy", platform = "Web App", website, doi, requires_maf_export = TRUE, requires_gene_selection = FALSE, maf_conversion_function = NA, extension="tsv") {
  assertthat::assert_that(is.data.frame(external_tools_df))
  utilitybelt::assert_non_empty_string(tool_name)
  utilitybelt::assert_non_empty_string(tool_id)
  utilitybelt::assert_non_empty_string(tool_group)
  utilitybelt::assert_non_empty_string(tool_class)
  utilitybelt::assert_non_empty_string(tool_description)
  assertthat::assert_that(assertthat::is.string(instructions))
  assertthat::assert_that(assertthat::is.string(platform))
  utilitybelt::assert_non_empty_string(website)
  utilitybelt::assert_non_empty_string(doi)
  assertthat::assert_that(assertthat::is.flag(requires_maf_export))
  assertthat::assert_that(assertthat::is.flag(requires_gene_selection))
  assertthat::assert_that(assertthat::is.string(extension))
  
  if(requires_maf_export) { 
    assertthat::assert_that(is.function(maf_conversion_function), msg = "Must supply a maf_conversion_function to external_tools_add_tool_to_dataframe. 'maf_conversion_function' cannot be NA if 'requires_maf_export' is TRUE")
    expected_number_of_args = ifelse(requires_gene_selection, yes=3, no=2) # should maf export function take 2 or 3 arguments. If we need to specify a gene we need a third argument
    assertthat::assert_that(utilitybelt::fun_count_arguments(maf_conversion_function) == expected_number_of_args, msg = "maf_conversion_function must have 2 or 3 arguments. The first should take a maf_dataset_wrapper object, the second a filepath. The third is only used if `requires_gene_selection` is TRUE and should take the name of a gene (Hugo Symbol). See ?external_tools_add_tool_to_dataframe for details")
  }
  
  new_df <- data.frame(
    tool_name, tool_id, tool_group, tool_class, tool_description, instructions, platform, website, doi, requires_maf_export, requires_gene_selection, I(list(maf_conversion_function)), extension
  )
  
  names(new_df) <- c("tool_name","tool_id","tool_group","tool_class","tool_description", "instructions", "platform","website","doi", "requires_maf_export", "requires_gene_selection", "maf_conversion_function", "extension")
  
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
#'
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
external_tools_get_property_by_tool_name <- function(tool_name, property_to_retrieve, external_tools_df=CRUX::external_tool_metadata){
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


# BBGlab ------------------------------------------------------------------


#' Load tool metadata into global variable
#' 
#' Loads metadata for the OncodriveFML tool into a global variable \strong{GLOBAL_external_tools_dataframe}
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame)
#'
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
#'
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
#'
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
#'
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
#'
#'
external_tools_convert_maf_to_bbglab <- function(maf_dataset_wrapper, filepath){
  #browser()
  maf <- maf_dataset_wrapper$loaded_data
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
#'
#'
external_tools_convert_maf_to_bbglab_return_dataframe <- function(maf){
  maf %>% 
    maftools_get_all_data() %>%
    dplyr::select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>% 
    dplyr::arrange(readr::parse_number(as.character(Chromosome)), Chromosome, Start_Position) %>% 
    dplyr::rename(chr=Chromosome, pos=Start_Position, ref=Reference_Allele, alt=Tumor_Seq_Allele2, sample=Tumor_Sample_Barcode) %>%
    return();
}



# cBioPortal --------------------------------------------------------------


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

external_tools_convert_maf_to_cbioportal_mutation_mapper <- function(maf_dataset_wrapper, filepath, gene_hugo_symbol){
  maf <- maf_dataset_wrapper$loaded_data
  external_tools_convert_maf_to_cbioportal_mutation_mapper_return_dataframe(maf, gene_hugo_symbol = gene_hugo_symbol) %>%
    data.table::fwrite(file = filepath, sep = "\t", col.names = TRUE)
}

external_tools_load_cbioportal_mutation_mapper <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "cBioportal Mutation Mapper",
    tool_id = "cbioportal_mutation_mapper",
    tool_group = "cBioPortal",
    tool_class = "Lollipop",
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
    extension = "txt"
  )
}


# Signal 2 ----------------------------------------------------------------

external_tools_convert_maf_to_signal2_return_dataframe <- function(maf){
  maf %>%
    maftools_get_all_data(include_silent_mutations = TRUE) %>% 
    dplyr::filter(Start_Position==End_Position) %>% #Filter for SNVs only
    dplyr::filter(Tumor_Seq_Allele2 %in% c("A", "C", "T", "G")) %>% #No indels allowed
    dplyr::select(
      "Sample Name" = Tumor_Sample_Barcode, 
      "Chromosome" = Chromosome, 
      "Position" = Start_Position, 
      "Original base" = Reference_Allele, 
      "Mutated base" = Tumor_Seq_Allele2
      ) 
}

external_tools_convert_maf_to_signal2 <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
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
      data.table::fwrite(file = files[i], col.names = FALSE, sep = "\t", quote = FALSE, row.names = FALSE)
  }
  
  zip(filepath, files, flags = "-j")
}

external_tools_load_maf_to_signal2 <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Signal",
    tool_id = "signal",
    tool_group = "Nik-Zainal Group",
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


# Mutalisk ----------------------------------------------------------------
#' Maf to VCF
#' 
#' Convert a maf to a vanilla vcf (single sample)
#'
#' @param maf_df either a maf object or a dataframe created using maftools_get_all_data
#'
#' @return data.frame with VCF columns
#' 
external_tools_convert_maf_to_vanilla_vcf_return_dataframe <- function(maf_df){
  if (maf_df %>% class() == "MAF") 
    maf_df <- maftools_get_all_data(maf_df, include_silent_mutations = TRUE)
  
  maf_df %>%
    dplyr::filter(Start_Position==End_Position) %>% #Filter for SNVs only
    dplyr::filter(Tumor_Seq_Allele2 %in% c("A", "C", "T", "G")) %>% #No indels allowed
    dplyr::mutate(ID= ".", QUAL=".", FILTER=".", INFO=".") %>%
    dplyr::select(
      "#CHROM" = Chromosome, 
      "POS" = Start_Position, 
      "ID" = ID,
      "REF" = Reference_Allele, 
      "ALT" = Tumor_Seq_Allele2,
      "QUAL" = QUAL, 
      "FILTER" = FILTER, 
      "INFO" = INFO
    ) 
}

external_tools_convert_maf_to_multiple_vanilla_vcfs <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  tumor_sample_barcodes <- maf %>% 
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) %>% 
    unique()
  
  files =  paste0(tempdir(), "/mutalisk_input_", tumor_sample_barcodes, ".vcf")
  
  message("Saving files to ", (tempdir()))
  
  sapply(seq_along(tumor_sample_barcodes), function(index){
    
    write("##fileformat=VCFv4.2", files[index])
    message(tumor_sample_barcodes[index])
    
    maf %>%
      maftools_get_all_data() %>% 
        dplyr::filter(Tumor_Sample_Barcode == tumor_sample_barcodes[index]) %>%
      external_tools_convert_maf_to_vanilla_vcf_return_dataframe() %>%
        data.table::fwrite(file = files[index], sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE,append = TRUE) 
  })
  
  zip(filepath, files, flags = "-j")
    
}

external_tools_convert_maf_to_one_vanilla_vcf <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
    write("##fileformat=VCFv4.2", filepath)
    maf %>%
      external_tools_convert_maf_to_vanilla_vcf_return_dataframe() %>%
      data.table::fwrite(file = filepath, sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE,append = TRUE)
}

external_tools_load_maf_to_mutalisk_sample_level <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Mutalisk: Sample Level",
    tool_id = "mutalisk-sample_level",
    tool_group = "Mutational Signature Analysis",
    tool_class = "Mutational Signature Analysis",
    tool_description = "Identifies mutational signatures in each sample",
    requires_gene_selection = FALSE,
    website = "http://mutalisk.org/analyze.php",
    doi = "https://doi.org/10.1093/nar/gky406",
    instructions = as.character(
      tags$ol(
        tags$li("Unzip downloaded file"),
        tags$li("Click 'Upload Files' and select all samples you want to run signature analysis on"),
        tags$li("Select reference build (Human GRCh37 if using pre-packaged TCGA/PCAWG datasets)"),
        tags$li("Select Disease Type and Signatures to include in analysis"),
        tags$li("Run analysis")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_multiple_vanilla_vcfs,
    extension = "zip"
  )
}


# Mutalisk Cohort Level ---------------------------------------------------
external_tools_load_maf_to_mutalisk_cohort_level <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Mutalisk: Cohort Level",
    tool_id = "mutalisk-cohort_level",
    tool_group = "Lee et al.",
    tool_class = "Mutational Signature Analysis",
    tool_description = "Identifies mutational signatures in a cohort",
    requires_gene_selection = FALSE,
    website = "http://mutalisk.org/analyze.php",
    doi = "https://doi.org/10.1093/nar/gky406",
    instructions = as.character(
      tags$ol(
        tags$li("Upload vcf"),
        tags$li("Select reference build (Human GRCh37 if using pre-packaged TCGA/PCAWG datasets)"),
        tags$li("Select Disease Type and Signatures to include in analysis"),
        tags$li("Run analysis")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_one_vanilla_vcf,
    extension = "vcf"
  )
}


# MuSic -------------------------------------------------------------------
#Third mutational signatures



# OpenCRAVAT ------------------------------------------------------------------
external_tools_convert_maf_to_cravat_return_dataframe <- function(maf){
  maf_df = maf %>%
    maftools_get_all_data() 
  
  if(! "Strand" %in% colnames(maf_df)){
    message("[Export MAF to Cravat] No strand column found in MAF. Assuming all variants are described relative to the sense strand (+)")
    maf_df <- dplyr::mutate(maf_df, Strand="+")
  }
  
  maf_df %>%
    dplyr::select(
      Chromosome = Chromosome,
      Position = Start_Position,
      Strand = Strand,
      "Reference Base" = Reference_Allele,
      "Alternate Allele" = Tumor_Seq_Allele2,
      Sample = Tumor_Sample_Barcode
    )
}

external_tools_convert_maf_to_cravat <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  external_tools_convert_maf_to_cravat_return_dataframe(maf) %>%
    data.table::fwrite(file = filepath, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
}

external_tools_load_maf_to_cravat <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "OpenCRAVAT",
    tool_id = "open_cravat",
    tool_group = "KarchinLab",
    tool_class = "Variant Annotation",
    tool_description = "Highly customisable annotation of all variants in a cohort",
    requires_gene_selection = FALSE,
    website = "https://run.opencravat.org/submit/nocache/index.html",
    doi = "https://ascopubs.org/doi/10.1200/CCI.19.00132",
    instructions = as.character(
      tags$ol(
        tags$li("Upload file"),
        tags$li("Select reference build (Human GRCh37 if using pre-packaged TCGA/PCAWG datasets)"),
        tags$li("Select annotation modules of interest"),
        tags$li("Run annotation")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_cravat,
    extension = "tsv"
  )
}


# Pecan Protein Paint (Simple) -----------------------------------------------------
# external_tools_convert_maf_to_proteinpaint_simple_return_dataframe <- function(maf, gene){
#   maf_df <- maf %>%
#     maftools_get_all_data() %>%
#     #dplyr::filter(Hugo_Symbol == gene) %>%
#     dplyr::mutate(class = dplyr::case_when(
#       Variant_Classification == "Missense_Mutation" ~ "M",
#       Variant_Classification %in% c("Nonsense_Mutation", "Nonstop_Mutation") ~ "N", ## add rest stoploss, nonstop, and stopgain
#       Variant_Classification %in% c("Frame_Shift_Del", "Frame_Shift_Ins", "Frameshift_INDEL") ~ "F",
#       Variant_Classification == "Silent" ~ "S",
#       Variant_Classification == "In_Frame_Del" ~ "D", #Calling inframe INDELs (i.e. MNPs) deletions for now for lack of a better option.
#       Variant_Classification ==  "Inframe_INDEL" ~ "mnv",
#       Variant_Classification == "In_Frame_Ins" ~ "I",
#       Variant_Classification == "Splice_Site" ~ "L",
#       Variant_Classification == "3'UTR" ~ "Utr3",
#       Variant_Classification == "5'UTR" ~ "Utr5",
#       Variant_Classification == "Intron" ~ "Intron",
#       Variant_Classification %in% c("RNA", "IGR", "5'Flank", "3'Flank") ~"N",
#       Variant_Classification == c("Translation_Start_Site") ~ "M", # Start-loss is currently classified as missense 
#       Variant_Classification == "Unknown" ~ "X", 
#       TRUE ~ "X"
#     ))
#   maf_colnames = colnames(maf_df)
#   
#   maf_df %>% 
#     dplyr::mutate(
#       position = paste0(sub(pattern = "^(chr)?", replacement = "chr", Chromosome,), ":", Start_Position),
#       name = get_amino_acid_changes(maf_df),
#       name = ifelse(is.na(name), position, name),
#       genomepaint = paste(name, position, class, sep=";")
#     ) %>%
#     dplyr::pull(genomepaint)
# }


  

# Pecan Protein Paint (Full) -----------------------------------------------------
external_tools_convert_maf_to_proteinpaint_return_dataframe <- function(maf){
  maf_df <- maf %>%
    maftools_get_all_data() 
  
  maf_colnames <- colnames(maf_df)
  
  maf_df %>%
    dplyr::mutate(class = dplyr::case_when(
      Variant_Classification == "Missense_Mutation" ~ "missense",
      Variant_Classification %in% c("Nonsense_Mutation", "Nonstop_Mutation", "Stop_Codon_Ins", "Stop_Codon_Del") ~ "nonsense", ## add rest stoploss, nonstop, and stopgain
      Variant_Classification %in% c("Frame_Shift_Del", "Frame_Shift_Ins", "Frameshift_INDEL", "De_novo_Start_OutOfFrame") ~ "frameshift",
      Variant_Classification == "Silent" ~ "silent",
      Variant_Classification %in% c("In_Frame_Del", "Inframe_INDEL", "Start_Codon_Del") ~ "proteinDel", #Calling inframe INDELs (i.e. MNPs) deletions for now for lack of a better option.
      Variant_Classification == "In_Frame_Ins" ~ "proteinIns",
      Variant_Classification == "Splice_Site" ~ "splice",
      Variant_Classification == "3'UTR" ~ "utr_3",
      Variant_Classification == "5'UTR" ~ "utr_5",
      Variant_Classification == "Intron" ~ "intron",
      Variant_Classification %in% c("RNA", "IGR", "5'Flank", "3'Flank") ~"noncoding",
      Variant_Classification == c("Translation_Start_Site") ~ "missense", # Start-loss is currently classified as missense 
      Variant_Classification == "De_novo_Start_InFrame" & Variant_Type %in% c("SNP", "DNP", "MNP") ~ "missense",
      Variant_Classification == "De_novo_Start_InFrame" & Variant_Type == "DEL" ~ "proteinDel",
      Variant_Classification == "De_novo_Start_InFrame" & Variant_Type == "INS" ~ "proteinIns",
      Variant_Classification == "Unknown" ~ "Unknown", 
      TRUE ~ "Unclassified" #Unknown refers to  variants classified as unknown in MAF whereas Unclassified means the variant class has not been accounted for by this conversion script
      )) %>%
    dplyr::select(
      gene = Hugo_Symbol,
      chromosome = Chromosome,
      start = Start_Position,
      class = class,
      sample = "Tumor_Sample_Barcode"
    ) %>%
    dplyr::mutate(
      aachange = get_amino_acid_changes(maf_df = maf_df),
      refseq = maftools_get_transcript_refseq(maf_df)
      )
}

external_tools_convert_maf_to_proteinpaint <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  external_tools_convert_maf_to_proteinpaint_return_dataframe(maf) %>%
    data.table::fwrite(file = filepath, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
}


longest_refseq_df = readRDS(system.file("extdata", "protein_domains.RDs", package = "maftools")) %>%
  dplyr::select(HGNC, refseq.ID, aa.length) %>%
  dplyr::group_by(HGNC, refseq.ID) %>%
  dplyr::slice(which.max(aa.length), .preserve = FALSE)


#' Get Longest Transcript Refseq ID
#'
#' Returns the transcript id from the 'Transcript_ID' column of a maf_df.
#' If no Transcript_ID column is found, will return the longest refseq transcript for each gene name.
#'
#' @param maf_df (HGNC ID of gene)
#'
#' @return vector of the same length as maf_df containing transcript refseq ids (character)
#'
#'
maftools_get_transcript_refseq <- function(maf_df){
  maf_colnames = colnames(maf_df)
  if("Transcript_ID" %in% maf_colnames) {
    message("Found a 'Transcript_ID' column', pulling refseq id's from there")
    return(maf_df[["Transcript_ID"]])
  }
  else {
    message("No Transcript_ID column found, retrieving the longest transcript")
    longest_refseq_df[match(maf_df$Hugo_Symbol, longest_refseq_df[["HGNC"]]),"refseq.ID"] %>% 
    unlist() %>% 
    return()
  }
}

#Returns vector of amino acid changes. if no valid aachange column is found, will return
#  chrom:pos ref>alt format strings, or NA if 'return_genomic_description_if_no_protein_column_found' is FALSE
# 
# If aachange column is found but some variants have empty aachange fields (e.g. if they are noncoding)
#   these will be replaced with chrom:pos ref>alt format strings.
get_amino_acid_changes = function(maf_df, return_genomic_description_if_no_protein_column_found = TRUE){
  maf_colnames = colnames(maf_df)
  
  aachange = NA
  if("HGVSp_Short" %in% maf_colnames) aachange <- maf_df[["HGVSp_Short"]]
  else if("HGVSp" %in% maf_colnames) aachange <- maf_df[["HGVSp"]]
  else if("Protein_Change" %in% maf_colnames) aachange <- maf_df[["Protein_Change"]]
  else if(!return_genomic_description_if_no_protein_column_found) return(NA)
  
  genomic_change <- paste0(maf_df[["Chromosome"]],":", maf_df[["Start_Position"]], " ", maf_df[["Reference_Allele"]], ">", maf_df[["Tumor_Seq_Allele2"]])
  
  return(ifelse(aachange == "" || is.na(aachange), genomic_change, aachange))
}


#' Load tool metadata into global variable
#' 
#' Appends  metadata for the OncodriveCLUSTL tool onto external_tools_df
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame)
#'
#'
external_tools_load_proteinpaint <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Protein Paint",
    tool_id = "protein_paint",
    tool_group = "St. Jude",
    tool_class = "Lollipop",
    tool_description = "A new method to identify clustering of somatic mutations in both coding and non-coding genomic regions to detect signals of positive selection.",
    website = "https://proteinpaint.stjude.org/",
    doi = "https://doi.org/10.1038/ng.3466",
    requires_gene_selection = FALSE,
    instructions =  as.character(
      tags$ol(
        tags$li("Select the appropriate reference genome build (hg19 if using pre-packaged TCGA/PCAWG datasets)"),
        tags$li("Select the Lollipop App"),
        tags$li("Once example loads, click '+', then 'Upload text files'"),
        tags$li("Browse & navigate to downloaded file"),
        tags$li("A pop-up box should appear. Click 'Genes', then select a gene of interest by clicking its name"),
        tags$li("You should see a lollipop plot appear. Try clicking 'Pediatric' / 'COSMIC' to compare mutational profile to PeCan cohorts")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_proteinpaint,
    extension = "tsv"
  ) 
}


# Xena Browser ------------------------------------------------------------
external_tools_convert_maf_to_xena_return_dataframe <- function(maf){
  maf %>%
    maftools_get_all_data() %>% 
    dplyr::select(sample=Tumor_Sample_Barcode, chr=Chromosome, start=Start_Position, end=End_Position, reference = Reference_Allele, alt = Tumor_Seq_Allele2, gene = Hugo_Symbol) %>% 
    dplyr::filter(nchar(reference) < 1000)  # We filter out very large deletions (>1000bp) because Xena can't handle them
}

external_tools_convert_expression_df_to_xena_expression <- function(expression_df){
  expression_df %>% 
    dplyr::select(Sample=Hugo_Symbol, TPM, Tumor_Sample_Barcode) %>%
    tidyr::pivot_wider(names_from = Tumor_Sample_Barcode, values_from = TPM) 
}

external_tools_convert_maf_to_xena <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  temp_dir <- tempdir()
  mutation_path <- paste0(temp_dir, "/mutations.txt")
  expression_path <- paste0(temp_dir, "/expression.txt")
  metadata_path <- paste0(temp_dir, "/metadata.txt")
  
  
  message("Writing temporary files to: \n", mutation_path, "\n AND \n",metadata_path)
  
  # Genomic Data
  external_tools_convert_maf_to_xena_return_dataframe(maf) %>%
    data.table::fwrite(file = mutation_path, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
  
  # Expression Data
  if(maf_data_wrapper_has_rnaseq_data(maf_dataset_wrapper)){
    maf_data_wrapper_get_rnaseq_df(maf_dataset_wrapper) %>%
      external_tools_convert_expression_df_to_xena_expression() %>%
      data.table::fwrite(file = expression_path, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
    }
  
  # Clinical Data
  maf %>% 
    maftools::getClinicalData() %>%
    data.table::fwrite(file = metadata_path, col.names = TRUE, row.names = FALSE, quote = TRUE, sep = "\t")
  
  zip(filepath, c(mutation_path, metadata_path, expression_path), flags = "-j")
}

external_tools_load_xena <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "Xena Browser",
    tool_id = "xena",
    tool_group = "UCSC",
    tool_class = "Multiomics Visualisation",
    tool_description = "Allows users to explore functional genomic data sets for correlations between genomic and/or phenotypic variables.",
    website = "https://xenabrowser.net/",
    doi = "https://doi.org/10.1038/s41587-020-0546-8",
    requires_gene_selection = FALSE,
    instructions =  as.character(
      tags$ol(
        tags$li("Click 'VIEW MY DATA'"),
        tags$li("Follow prompts to install UCSC Xena"),
        tags$li("Unzip downloaded files"),
        tags$li("Import mutations.txt as 'positional data'"),
        tags$li("Follow prompts to import data. Note reference genome is hg19/GRCh37 if using any of the pre-loaded datasets"),
        tags$li("Import metadata.txt as phenotypic data")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_xena,
    extension = "zip"
  ) 
}


# UCSC (BED format) -------------------------------------------------------
external_tools_convert_maf_to_bed_return_dataframe <- function(maf){
  maf %>%
    maftools_get_all_data(include_silent_mutations = TRUE) %>% 
    dplyr::mutate(name = paste0(Tumor_Sample_Barcode, "_", Reference_Allele, ">", Tumor_Seq_Allele2)) %>% 
    dplyr::select(chrom = Chromosome, chromStart = Start_Position, chromEnd = End_Position, name = name) %>%
    dplyr::mutate(chromStart = chromStart-1)  #Make Start is 0 based #maybe sort
}

external_tools_convert_maf_to_bed <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  maf %>%
    external_tools_convert_maf_to_bed_return_dataframe() %>%
    data.table::fwrite(file = filepath, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

external_tools_convert_maf_to_ucsc <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  write('track name=CRUX description="CRUX Track"', file = filepath)
  
  maf %>%
    external_tools_convert_maf_to_bed_return_dataframe() %>%
    data.table::fwrite(file = filepath, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  
}


external_tools_load_ucsc <- function(external_tools_df = data.frame()){
  external_tools_add_tool_to_dataframe(
    external_tools_df = external_tools_df,
    tool_name = "UCSC Browser",
    tool_id = "ucsc",
    tool_group = "UCSC",
    tool_class = "Multiomics Visualisation",
    tool_description = "Allows users to explore functional genomic data sets for correlations between genomic and/or phenotypic variables.",
    website = "https://genome.ucsc.edu/cgi-bin/hgCustom?hgsid=1097698701_fTXkn1uTAlRTQ8UGVIH7Kc9vi5tI",
    doi = "http://www.genome.org/cgi/doi/10.1101/gr.229102",
    requires_gene_selection = FALSE,
    instructions =  as.character(
      tags$ol(
        tags$li("Select the appropritate reference genome. This is is GRCh37/hg19 if using any of the pre-loaded datasets"),
        tags$li("Click 'browse'"),
        tags$li("Upload exported file"),
        tags$li("Submit")
      )
    ),
    maf_conversion_function = external_tools_convert_maf_to_ucsc,
    extension = "bed"
  ) 
}


# UCSC TumorMap -----------------------------------------------------------
external_tools_convert_expression_df_to_tumormap_return_dataframe <- function(expression_df){
  expression_df %>% 
    dplyr::select(Sample=Hugo_Symbol, TPM, Tumor_Sample_Barcode) %>%
    tidyr::pivot_wider(names_from = Tumor_Sample_Barcode, values_from = TPM) 
}


external_tools_convert_maf_to_return_tumormap_attributes_dataframe <- function(maf){
  maf %>%
    maftools::getClinicalData() %>% 
    dplyr::rename(sample = Tumor_Sample_Barcode) #%>%
    # dplyr::select(sample, where(function(vec) { 
    #   if(dplyr::n_distinct(vec) < 20 && is.character(vec))
    #     return(TRUE)
    #   else if(is.numeric(vec))
    #     return(TRUE)
    #   }))
  }
# external_tools_convert_maf_to_tumormap_return_dataframe <- function(maf, topn = 20){
#   topn_altered_genes <- maf %>% 
#     maftools::getGeneSummary() %>%
#     dplyr::arrange(desc(MutatedSamples)) %>%
#     dplyr::pull(Hugo_Symbol) %>%
#     head(n=topn)
#     
#   tsbs = maf %>% 
#     maftools_get_all_data() %>%
#     dplyr::pull(Tumor_Sample_Barcode)
#   
#   genes <- maf %>% 
#     maftools_get_all_data() %>% 
#     dplyr::pull(Hugo_Symbol)
#   
#   combos_in_mafs = paste(tsbs, genes)
#   
#   crossed_df <- tidyr::crossing(topn_altered_genes, tsbs) 
#   
#   crossed_df["Gene_Mutated"] <- (paste(crossed_df[["tsbs"]], crossed_df[["topn_altered_genes"]]) %in% combos_in_mafs) %>%
#     as.numeric()
#   
#   
#   genes_mutated_df <- crossed_df %>% tidyr::pivot_wider(names_from = "tsbs", values_from = Gene_Mutated)
#   return(genes_mutated_df)
# }

external_tools_convert_maf_to_tumormap <- function(maf_dataset_wrapper, filepath){
  maf <- maf_dataset_wrapper$loaded_data
  temp_dir <- tempdir()
  expression_path <- paste0(temp_dir, "/expression.txt")
  metadata_path <- paste0(temp_dir, "/metadata.txt")
  
  #Expression Data
  if(maf_data_wrapper_has_rnaseq_data(maf_dataset_wrapper)){
    maf_data_wrapper_get_rnaseq_df(maf_dataset_wrapper) %>%
      external_tools_convert_expression_df_to_tumormap_return_dataframe() %>%
      data.table::fwrite(file = expression_path, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
  }
  
  #Clinical Data
  external_tools_convert_maf_to_return_tumormap_attributes_dataframe(maf) %>%
    data.table::fwrite(file = metadata_path, col.names = TRUE, row.names = FALSE, quote = TRUE, sep = "\t")
  
  zip(filepath, c(metadata_path, expression_path), flags = "-j")
}



external_tools_load_tumormap <- function(external_tools_df = data.frame()){
    external_tools_add_tool_to_dataframe(
      external_tools_df = external_tools_df,
      tool_name = "UCSC TumorMap",
      tool_id = "ucsc_tumormap",
      tool_group = "UCSC",
      tool_class = "Expression",
      tool_description = "an interactive browser that allows biologists, who may not have computational expertise, to richly explore the results of high-throughput cancer genomics experiments",
      website = "https://tumormap.ucsc.edu/",
      doi = "https://doi.org/10.1158/0008-5472.CAN-17-0580",
      requires_gene_selection = FALSE,
      instructions =  as.character(
        tags$ol(
          tags$li("Unzip exported data"),
          tags$li("Navigate to TumorMap website"),
          tags$li("Click 'Create Map' button"),
          tags$li("Import expression.txt as 'Layout' and metadata.txt as 'Color Attributes'")
        )
      ),
      maf_conversion_function = external_tools_convert_maf_to_tumormap,
      extension = "zip"
    ) 
}

# jaccard <- function(df, vec1, vec2) {
#   sums = rowSums(df[,c(vec1, vec2)])
#   
#   similarity = length(sums[sums==2])
#   total = length(sums[sums==1]) + similarity
#   
#   similarity/total
# }

#dist = TCGAmutations::tcga_load("ACC") %>% external_tools_convert_maf_to_tumormap_return_dataframe() %>% dplyr::select(-1) %>% t() %>% dist(method = "binary")
#(dist) %>% as.matrix() %>% as.data.frame() %>% tibble::rownames_to_column("Sample") %>% data.table::fwrite(file = "~/Downloads/ACC.test.tsv", sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE

# For all tools -----------------------------------------------------------
#' Load tool metadata into global variable
#' 
#' Loads metadata for all tools, returning a dataframe.
#' If any of the constitutent functions are changed, run \code{external_tools_update_builtin_dataset}
#'
#' @return external_tools_df with metadata of tool appeneded (data.frame). see \code{?external_tools_add_tool_to_dataframe} for more info on the columnss in this data.frame
#'
#'
#' @examples
#' external_tools_load_all_tools()
external_tools_load_all_tools <- function(){
  external_tools_load_bbglab_oncodrive_fml() %>%
    external_tools_load_bbglab_oncodrive_clustl() %>%
    external_tools_load_bbglab_cgi() %>%
    external_tools_load_cbioportal_mutation_mapper() %>% 
    external_tools_load_maf_to_signal2() %>%
    external_tools_load_maf_to_mutalisk_sample_level() %>%
    external_tools_load_maf_to_mutalisk_cohort_level() %>%
    external_tools_load_maf_to_cravat() %>%
    external_tools_load_proteinpaint() %>%
    external_tools_load_xena() %>%
    external_tools_load_ucsc() %>%
    external_tools_load_tumormap() %>%
    return()
}





#' Update builtin external_tools dataset
#'
#' @description Takes the output of external_tools_load_all_tools and saves it to CRUX/data as the dataset: \strong{external_tool_metadata}.
#' This saved dataset is what is used by the app. If you make change to any external_tools_load_... function, you must rerun this function.
#'
external_tools_update_builtin_dataset <- function(){
  external_tool_metadata = external_tools_load_all_tools()
  usethis::use_data(external_tool_metadata, overwrite = TRUE) 
}

