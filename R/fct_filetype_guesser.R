
#' Guesses mutation filetype
#' 
#' Reads the first line of a file and attempt to determine the filetype based on the header
#'
#' @param path path to file
#'
#' @return one of MAF, ANNOVAR, VCF, OTHER
#'
guess_genomic_mutation_filetype <- function(path){
  
  # Read First Row of Header Only
  df_header <- data.table::fread(path, nrows = 0)
  df_header_start_chrom <- tryCatch(
    expr = { 
      data.table::fread(path, nrows = 0, skip = "CHROM")   
    },
    error = function(err){
      return(character(0))
    }
  )
  
  df_header_start_hugo <-tryCatch(
    expr = { 
      data.table::fread(path, nrows = 0, skip = "Hugo_Symbol")
    },
    error = function(err){
      return(character(0))
    }
  )
  
  cols <- colnames(df_header)
  cols_hugo <- colnames(df_header_start_hugo)
  cols_chrom <- colnames(df_header_start_chrom)
  cols_pre_dot  <- sub(x=cols, pattern = "\\..*$", replacement = "") # e.g.  Func.refGene OR Func.ensGene will become Func. 
  
  # Definitions of what to expect in MAFs / ANNOVAR files / VCF
  maf_cols <- c("Tumor_Sample_Barcode", "Hugo_Symbol", "Chromosome", "Start_Position", 
                     "End_Position", "Reference_Allele", "Tumor_Seq_Allele2", "Variant_Classification", 
                     "Variant_Type")
  annovar_cols = c("Chr", "Start", "End", "Ref", "Alt", "Func", "Gene", "GeneDetail", "ExonicFunc", "AAChange")

  
  vcf_cols = c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", 
               "FORMAT")
  
  filetype = dplyr::case_when(
    all(maf_cols %in% cols_hugo) ~ "MAF",  
    all(annovar_cols %in% cols_pre_dot) ~ "ANNOVAR",
    all(vcf_cols %in% cols_chrom) ~ "VCF",
    TRUE ~ "OTHER"
    )
  
  return(filetype)
}

#' Guess reference table used for annovar annotation
#' 
#' Reads the first line of a file and confirms that it is an ANNOVAR file, then returns  'ensGene' or 'refGene' 
#' base on whether refseq or ensemble reference tables were for gene-based annotations
#'
#' @param path path to file
#'
#' @return one of MAF, ANNOVAR, VCF, OTHER
#'
annovar_guess_reference_table <- function(path){
  if(guess_genomic_mutation_filetype(path) != "ANNOVAR")
    stop('File is not an annovar file')
  
  # Read First Row of Header Only
  df <- data.table::fread(path, nrows = 0)
  cols <- colnames(df)
  cols_post_dot  <- sub(x=cols, pattern = "^.*\\.", replacement = "") # e.g.  Func.refGene OR Func.ensGene will become refGene OR ensGene. 

  if(any(cols_post_dot == "ensGene"))
   return('ensGene') 
  else if(any(cols_post_dot == "refGene"))
      return('refGene')
}


#' Read MAF from ANNOVAR OR MAF input
#' 
#' Automatically detects filetype from 
#'
#' @param path_mutations path to a MAF / ANNOVAR file
#' @param refBuild which reference version is being used
#' @param path_mutations path to mutation file (string)
#' @param path_clindata path to clinical annotation file (string)
#' @param filetype what type of filetype do we expect (string). If "AUTO" will automatically guess the filetype using [guess_genomic_mutation_filetype()]
#'
#' @return one of MAF, ANNOVAR, VCF, OTHER
#'
read_maf_flexible <- function(path_mutations, refBuild = NULL, path_clindata = NULL, filetype = c("AUTO", 'ANNOVAR', 'MAF')){
  filetype <- rlang::arg_match(filetype)
  auto_guessed_filetype <- guess_genomic_mutation_filetype(path_mutations)
  
  
  if(filetype == "AUTO")
    filetype <- auto_guessed_filetype
    
  if(filetype == "MAF"){
    maf <- maftools::read.maf(path_mutations,clinicalData = path_clindata)
  }
  else if(filetype == "ANNOVAR"){
    maf <- maftools::annovarToMaf(annovar = path_mutations, table = annovar_guess_reference_table(path_mutations), refBuild = refBuild, sampleAnno = path_clindata, MAFobj = TRUE) 
  }
  else if(filetype == c("VCF")){
   stop('Unsupported Mutation Filetype: ', filetype, "\n", 'Please see documentation for instructions on how to convert VCFs to ANNOVAR files') 
  }
  else if(filetype == c("OTHER")){
    stop('Unexpected Mutation Filetype: ', filetype, "\n", 'Please See documentation for instructions on supported formats') 
  }
  else
    stop('Unexpected filetype ', filetype)
}