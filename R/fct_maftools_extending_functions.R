#' Get MAF data 
#'
#' Input a maf object. Returns table containing ALL MAF data. Nonsynonymous AND synonymous
#'
#' @param maf 
#' @param include_silent_mutations 
#'
#' @return a data.frame in MAF form where each variant has a separate row (data.frame)
#'
#'
#' @examples
#' maftools_get_all_data(TCGAmutations::tcga_load("GBM"))
maftools_get_all_data <- function(maf, include_silent_mutations=T) {
  if(include_silent_mutations)
    data.table::rbindlist(list(maf@data, maf@maf.silent), use.names = TRUE, fill = TRUE) %>% return()
  else
    return(maf@data)
}



#' Maftools rainfallPlot Wrapper
#'
#' Wraps maftools::rainfallPlot. The original function plots a rainfall graph, prints the predicted kataegis coords as a df, and writes it to a file.
#' This function does all of this EXCEPT it hides the printed df, reads the df from the file and returns it. It also deletes the created file.
#'
#' @inheritParams maftools::rainfallPlot
#' @param tsb specify sample names (Tumor_Sample_Barcodes) (string)
#'
#' @return predicted kataegis sites (dataframe)
#'
#' 
maftools_plot_rainfall <- function(maf, tsb, detectChangePoints = TRUE, ref.build = "hg19", pointSize = 0.4, fontSize = 1.2){
  assertthat::assert_that(!is.null(tsb) | detectChangePoints == FALSE, msg = "[maftool_rainfall_kaetagis_table] this wrapper function ONLY allows tsb to be null IF detectChangePoints == FALSE. Otherwise it can't find the output file which is named after the tsv")
  #message("ref.build: ", ref.build)
  
  closeAllConnections()
  sink(nullfile())
  maftools::rainfallPlot(maf = maf, tsb = tsb, detectChangePoints = detectChangePoints, ref.build = ref.build, fontSize = fontSize, pointSize = pointSize)
  closeAllConnections()
  message("All done")
  expected_filename = paste0(tsb, "_Kataegis.tsv")
  
  if(detectChangePoints==TRUE && !is.null(tsb) && file.exists(expected_filename)){
    kataegis_df = data.table::fread(file = expected_filename)
    unlink(expected_filename)
    return(kataegis_df)
  }
  
  return(NULL)
}

#' maftools_clinical_data_get_levels
#'
#' @inheritParams maftools_clinical_data_visually_summarise
#'
#'
#' @return Number of distinct levels of a clinical feature 
#'
#'
maftools_clinical_data_get_levels <- function(maf, clinical_feature){
  utilitybelt::assert_non_empty_string(clinical_feature)
  clindata <- maftools::getClinicalData(maf)
  
  assertthat::assert_that(clinical_feature %in% colnames(clindata), msg = utilitybelt::fmterror("[maftools_clinical_data_get_levels] the clinical feature '", clinical_feature, "' was not found in clinical data"))
  dplyr::n_distinct(clindata[[clinical_feature]]) %>%
    return()
}

maftools_clinical_data_lowest_number_of_samples_per_level <- function(maf, clinical_feature){
  utilitybelt::assert_non_empty_string(clinical_feature)
  clindata <- maftools::getClinicalData(maf)
  assertthat::assert_that(clinical_feature %in% colnames(clindata), msg = utilitybelt::fmterror("[maftools_clinical_data_get_levels] the clinical feature '", clinical_feature, "' was not found in clinical data"))
  min(table(clindata[[clinical_feature]])) %>%
    return()
}

#' Plot Clinical Data
#' Plot a particular column of clinical DATA.
#' @param maf MAF object (MAF)
#' @param clinical_feature Name of a clinical feature (string)
#'
#' @return ggplot / grob
#'
#'
maftools_clinical_data_visually_summarise <- function(maf, clinical_feature = "Tumor_Sample_Barcode", threshold=NULL, selected_items=NULL, distance_from_bar=2){
  feature.v = maf %>% 
    maftools_fix_clinical_data_types() %>%
    maftools::getClinicalData() %>%
    dplyr::pull(clinical_feature) 
  #browser()
  
  
  if(is.numeric(feature.v)){
    p = feature.v %>% ggplot2::qplot(alpha = 0.8, fill = "bla") + 
      ggplot2::xlab(clinical_feature) + 
      ggplot2::ylab("Count") + 
      ggthemes::theme_fivethirtyeight() +
      utilitybelt::theme_common_adjustments(no_background = TRUE) +
      ggplot2::scale_fill_manual(values="steelblue") +
      utilitybelt::theme_no_legend() 
    if(!is.null(threshold) && is.numeric(threshold))
      p = p + ggplot2::geom_vline(xintercept = threshold, linetype = "dashed")
    return(p)
  }
  else{
    topn=10
    feature.v <- as.character(feature.v)
    p =feature.v %>%
      forcats::fct_lump_n(n=topn, ties.method = "first", other_level = paste0("Other (n=", dplyr::n_distinct(.)-topn,")")) %>%
      forcats::fct_infreq() %>%
      forcats::fct_rev() %>%
      ggplot2::qplot(fill="bla", alpha = 0.8) +
      ggplot2::coord_flip()  +
      ggplot2::ylab("Count") +
      ggplot2::xlab(clinical_feature) +
      ggthemes::theme_fivethirtyeight() +
      utilitybelt::theme_common_adjustments(no_background = TRUE) +
      ggplot2::scale_fill_manual(values="steelblue") +
      utilitybelt::theme_no_legend() +
      utilitybelt::geom_barplot_counts(distance_from_bar = distance_from_bar, color = "mediumvioletred", alpha = 0.9) +
      ggplot2::ggtitle("Original Dataset")
    
    #browser()
    if(!is.null(selected_items) && all(selected_items %in% feature.v)){
      #feature_filter_flag.v <- 
      #feature_filter_flag.v <- ifelse(feature_filter_flag.v, yes = "Passes Filter", no="Fails Filter")
      p2 <- ggplot2::qplot(x=feature.v %in% selected_items, fill="bla", alpha = 0.8) +
        ggthemes::theme_fivethirtyeight() +
        utilitybelt::theme_common_adjustments(no_background = TRUE) +
        ggplot2::scale_fill_manual(values="steelblue") +
        utilitybelt::theme_no_legend() +
        utilitybelt::geom_barplot_counts(distance_from_bar = distance_from_bar, color = "mediumvioletred", alpha = 0.9) +
        ggplot2::xlab("Passes Filter") + ggplot2::ggtitle("Effect of Filter")
      #return(p2)
      p <- cowplot::plot_grid(p, p2)
    }
    return(p)
  }
}

maftools_flagged_genes <- function(){
  flags  = c("TTN", "MUC16", "OBSCN", "AHNAK2", "SYNE1", "FLG", "MUC5B",
             "DNAH17", "PLEC", "DST", "SYNE2", "NEB", "HSPG2", "LAMA5", "AHNAK",
             "HMCN1", "USH2A", "DNAH11", "MACF1", "MUC17")
}


maftools_gistic = function(gistic){
  gistic@data
  #gistic@cnMatrix %>%  as.data.frame() %>% tibble::rownames_to_column("Cytoband") %>% tidyr::pivot_longer(2:ncol(.),names_to = "Sample", values_to = "MutationType") %>% dplyr::tibble() 
}

#' Convert chromosomes 23 and 24 to x and y in maf object. This will convert  
#' 
#' Takes a maf object and returns that same object but converts and chromosomes named 23 or 24  (or chr23 / chr24) to X and Y.
#' @param maf A MAF object (MAF)
#'
#' @return a MAF object with chr23/chr24 converted to "X" & "Y"
#'
#'
maftools_chrom_23_and_24_to_X_and_Y <- function(maf){
  maf@data <- maf@data %>% 
    dplyr::mutate(Chromosome  =dplyr::case_when(
      Chromosome == "23" ~ "X",
      Chromosome == "chr23" ~ "chrX",
      Chromosome == "24" ~ "Y",
      Chromosome == "chr24" ~ "chrY",
      TRUE ~ as.character(Chromosome)
    ) %>% as.character()
    )
  
  maf@maf.silent <- maf@maf.silent %>% 
    dplyr::mutate(Chromosome  =dplyr::case_when(
      Chromosome == "23" ~ "X",
      Chromosome == "chr23" ~ "chrX",
      Chromosome == "24" ~ "Y",
      Chromosome == "chr24" ~ "chrY",
      TRUE ~ as.character(Chromosome)
    ) %>% as.character()
    )
  return(maf)
}

maftools_escape_special_characters <- function(string){
  assertthat::assert_that(assertthat::is.string(string))
  
  special_characters = c("\\'", '\\"')  
  
  cleaned_string = string
  
  for (i in seq_along(special_characters)) {
    cleaned_string = gsub(pattern = special_characters[i], replacement = paste0("\\", special_characters[i]), x=cleaned_string)
  }
  
  return(cleaned_string)
}

#' Filter dubious genes from MAF
#' 
#' Filter out genes likely to appear in many analyses from your MAF (e.g. TTN & Olfactory receptors )
#' 
#' @param maf a MAF object
#' @param genelist a character vector containing HUGO Symbols to remove from MAF
#'
#' @return MAF object
#'
#' @examples
#' maftools_remove_dubious_genes(TCGAmutations::tcga_load("gbm"), "TTN")
maftools_remove_dubious_genes <- function(maf, genelist = somaticflags::somaticflags){
  assertthat::assert_that(is.character(genelist))
  filtered_maf <- maftools::filterMaf(maf = maf, genes = genelist)
  return(filtered_maf)
}

maf_data_set_wrapper_remove_dubious_genes <- function(maf_dataset_wrapper, genelist){
  if(is.na(maf_dataset_wrapper$loaded_data)){
    message("maf_data_set_wrapper_remove_dubious_genes: MAF not loaded ... returning original maf_dataset_wrapper")
    return(maf_dataset_wrapper)
  }
  else if(class(maf_dataset_wrapper$loaded_data) == "MAF"){
    message("Filtering dubious genes from maf_dataset_wrapper")
    maf_dataset_wrapper$loaded_data <-  maftools_remove_dubious_genes(maf_dataset_wrapper$loaded_data)
    return(maf_dataset_wrapper)
  }
  else
    stop("maf_data_set_wrapper_remove_dubious_genes: maf_dataset_wrapper$loaded_data is neither NA, nor a MAF")
}


maftools_fix_tcga_survival_curve_metadata <- function(maf){
  #browser()
  if (all(c("days_to_last_followup", "vital_status", "days_to_death") %in% colnames(maf@clinical.data))){
    maf@clinical.data$days_to_last_followup <- ifelse(is.na(maf@clinical.data$days_to_last_followup) & maf@clinical.data$vital_status==1, yes = maf@clinical.data$days_to_death, no = maf@clinical.data$days_to_last_followup)
    return(maf)
  }
  else
    return(maf)
}
