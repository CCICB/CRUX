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
#' CRUX:::maftools_get_all_data(maftools::tcgaLoad("GBM"))
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
  utilitybeltassertions::assert_non_empty_string(clinical_feature)
  clindata <- maftools::getClinicalData(maf)
  
  assertthat::assert_that(clinical_feature %in% colnames(clindata), msg = paste0("[maftools_clinical_data_get_levels] the clinical feature '", clinical_feature, "' was not found in clinical data"))
  dplyr::n_distinct(clindata[[clinical_feature]]) %>%
    return()
}

maftools_clinical_data_lowest_number_of_samples_per_level <- function(maf, clinical_feature){
  utilitybeltassertions::assert_non_empty_string(clinical_feature)
  clindata <- maftools::getClinicalData(maf)
  assertthat::assert_that(clinical_feature %in% colnames(clindata), msg = paste0("[maftools_clinical_data_get_levels] the clinical feature '", clinical_feature, "' was not found in clinical data"))
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
      utilitybeltgg::theme_fivethirtyeight_two() +
      ggplot2::scale_fill_manual(values="steelblue") +
      utilitybeltgg::theme_no_legend() 
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
      utilitybeltgg::theme_fivethirtyeight_two() +
      ggplot2::scale_fill_manual(values="steelblue") +
      utilitybeltgg::theme_no_legend() +
      utilitybeltgg::geom_barplot_counts(distance_from_bar = distance_from_bar, color = "mediumvioletred", alpha = 0.9) +
      ggplot2::ggtitle("Original Dataset")
    
    #browser()
    if(!is.null(selected_items) && all(selected_items %in% feature.v)){
      #feature_filter_flag.v <- 
      #feature_filter_flag.v <- ifelse(feature_filter_flag.v, yes = "Passes Filter", no="Fails Filter")
      p2 <- ggplot2::qplot(x=feature.v %in% selected_items, fill="bla", alpha = 0.8) +
        utilitybeltgg::theme_fivethirtyeight_two() + 
        ggplot2::scale_fill_manual(values="steelblue") +
        utilitybeltgg::theme_no_legend() +
        utilitybeltgg::geom_barplot_counts(distance_from_bar = distance_from_bar, color = "mediumvioletred", alpha = 0.9) +
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
#' CRUX:::maftools_remove_dubious_genes(maftools::tcgaLoad("gbm"), "TTN")
maftools_remove_dubious_genes <- function(maf, genelist = somaticflags::somaticflags){
  assertthat::assert_that(is.character(genelist))
  filtered_maf <- maftools::filterMaf(maf = maf, genes = genelist)
  filtered_maf <- maftools_fix_clinical_data_types(filtered_maf)
  return(filtered_maf)
}

maf_data_set_wrapper_remove_dubious_genes <- function(maf_dataset_wrapper, genelist){
  if(!is_maf(maf_dataset_wrapper$loaded_data)){
    stop("maf_data_set_wrapper_remove_dubious_genes: MAF not loaded ... quitting early")
  }
  else{
    message("Filtering dubious genes from maf_dataset_wrapper")
    maf_dataset_wrapper$loaded_data <-  maftools_remove_dubious_genes(maf_dataset_wrapper$loaded_data)
    return(maf_dataset_wrapper)
  }
}


# maftools_fix_tcga_survival_curve_metadata <- function(maf){
#   if (all(c("days_to_last_followup", "vital_status", "days_to_death") %in% colnames(maf@clinical.data))){
#     maf@clinical.data$days_to_last_followup <- ifelse(
#       is.na(maf@clinical.data$days_to_last_followup) & maf@clinical.data$vital_status==1, 
#       yes = maf@clinical.data$days_to_death, 
#       no = maf@clinical.data$days_to_last_followup)
#     return(maf)
#   }
#   else
#     return(maf)
# }

# Adds a column to clinical data called days_to_last_follow_up_complete which uses days_to_last_followup column + days_to_death if the former is not available and vital_status == 1.
# Note that days_to_last_followup is relative to initial diagnosis. days_to_death can be relative to diagnosis, sample collection, genome sequencing or other timepoints.
maftools_fix_tcga_survival_curve_metadata <- function(maf){
  if (all(c("days_to_last_followup", "vital_status", "days_to_death") %in% colnames(maf@clinical.data))){
    maf@clinical.data <- maf@clinical.data %>% 
      dplyr::mutate(days_to_last_followup = dplyr::case_when(
        is.na(days_to_last_followup) & vital_status==1 & is.na(days_to_last_followup) ~ days_to_death,
        TRUE ~ days_to_last_followup
      ))
    return(maf)
  }
  else
    return(maf)
}

maftools_dubious_genes_present_in_maf <- function(maf){
    genes= maf %>%
      maftools_get_all_data() %>% 
      dplyr::pull(Hugo_Symbol) %>% 
      unique()
    
    paste0(genes[genes %in% somaticflags::somaticflags], collapse = ", ") %>%
      paste0("Genes removed: ", .) %>%
      return()
}

maftools_samples_with_mutated_gene <- function(maf, gene, include_silent_mutations=TRUE, invert = FALSE){
  if(invert==FALSE){
    maf %>%
      maftools_get_all_data(include_silent_mutations = TRUE) %>%
      dplyr::filter(Hugo_Symbol %in% gene) %>% 
      dplyr::pull(Tumor_Sample_Barcode) %>% 
      unique() %>%
      as.character() %>%
      return()
  }else{
    maf %>%
      maftools_get_all_data(maf, include_silent_mutations = TRUE) %>%
      dplyr::filter(!(Hugo_Symbol %in% gene)) %>% 
      dplyr::pull(Tumor_Sample_Barcode) %>% 
      unique() %>%
      as.character() %>%
      return()
  }
}

#' Number of samples in a MAF
#'
#' @param maf @inherit maftools::read.maf
#'
#' @return int
#'
#' @examples
#' CRUX:::maftools_number_of_samples(maftools::tcgaLoad("GBM"))
maftools_number_of_samples <- function(maf){
  return(as.numeric(maf@summary[["summary"]][3])) 
}

maftools_anonymise_maf <- function(maf){
  message("Anonymising data")
  all_sample_ids <- levels(maftools::getSampleSummary(maf)[[1]])
  anonymised_ids <- sample(x = paste0("Sample", seq_along(all_sample_ids)), replace = FALSE, size = length(all_sample_ids))
  
  for (name in slotNames(maf)){
    if(!"Tumor_Sample_Barcode" %in% colnames(slot(maf, name))) next()
    slot(maf, name)[["Tumor_Sample_Barcode"]] <- anonymised_ids[match(slot(maf, name)[["Tumor_Sample_Barcode"]], all_sample_ids)]
  }
  
  return(maf)
}

maf_data_set_wrapper_anonymise_maf <- function(maf_dataset_wrapper){
  if(!is_maf(maf_dataset_wrapper$loaded_data))
    stop("maf_data_set_wrapper_anonymise_maf: MAF not loaded ... quitting early")
  else {
    maf_dataset_wrapper$loaded_data <- maftools_anonymise_maf(maf_dataset_wrapper$loaded_data)
    return(maf_dataset_wrapper)
  }
}

is_maf <- function(object){
  length(object) == 1 && inherits(object, "MAF")
}

#' Extract geneset
#' 
#' Create a genelist containing the genes mutated in the most samples in a maf. 
#' If topn > total number of mutated genes then all mutated genes are returned.
#'
#' @inheritParams maftools::read.maf
#' @param topn How many genes to include in the gene-set. 
#'
#' @return  The names of genes mutated in the most samples (character vector). 
#'
#' @examples
#' CRUX::maftools_extract_geneset_by_altered_samples(maftools::tcgaLoad("GBM"), topn=50)
maftools_extract_geneset_by_altered_samples <- function(maf, topn = 100){
  genes_by_number_of_altered_samples <- maf %>%
    maftools::getGeneSummary() %>%
    dplyr::select(Hugo_Symbol, AlteredSamples) %>%
    dplyr::arrange(dplyr::desc(AlteredSamples)) %>%
    dplyr::pull(Hugo_Symbol)
  
  if(topn <= length(genes_by_number_of_altered_samples)){
    genes_by_number_of_altered_samples %>%
      head(n=topn) %>%
      return()
  }
  else{
    total_genes=length(genes_by_number_of_altered_samples)
    message("maftools_extract_geneset: cannot return ", topn, " genes since only ", total_genes, " were mutated. Returning all ", total_genes, " genes")
    return(genes_by_number_of_altered_samples)
  }
}

# maftools_calculate_dn_ds <- function(maf){
#   mutations_df <- maftools_get_all_data(maf, include_silent_mutations = TRUE) %>%
#     dplyr::select(sampleID = Tumor_Sample_Barcode, chr = Chromosome, pos=Start_Position, ref = Reference_Allele, mut = Tumor_Seq_Allele2) %>%
#     dplyr::mutate(chr = sub("24", "Y", sub("23", "X", chr)))
#   
#   dndsout = dndscv::dndscv(mutations_df)
#   sel_cv = dndsout$sel_cv
#   return(sel_cv)
# }
# 
# maftools_filter_dn_ds_output_for_significant_genes <- function(sel_cv, threshold = 0.1){
#   signif_genes = sel_cv[sel_cv$qglobal_cv<0.1, c("gene_name","qglobal_cv")]
#   return(signif_genes)
# }

#' maftools_cluster_samples
#' 
#'  Identify and visualise clusters of cancer samples based on somatic mutation data (gene-level differences visualised, not variant-level)
#'  
#'  Approach involves:
#'  \enumerate{
#'  \item Selecting a geneset of interest (by default uses genes mutated in the most samples).
#'  \item Calculating \strong{1-jaccard} distance between samples based on which genes of the genesets are mutated.
#'  \item Running heirarchical clustering algorithm using \strong{pheatmap}.
#'  \item Visualise resulting heatmap with user-selected annotations.
#'  }
#'   
#' 
#' @param maf maf object from maftools package (maf)
#' @param custom_genelist_to_cluster_by names of genes to base clustering on. If unsure what genes to use. By default, the top 50 genes ranked by how manys samples they are mutated in will be used (character vector)
#' @param number_of_genes number of genes to cluster based on. Chooses genes which are mutated in the most samples  (only matters if not supplying \strong{custom_genelist_to_cluster_by}) (integer)
#' @param genes_to_annotate a custom list of genes to plot as a pseudo oncoplot (string)
#' @param annotate_most_altered_genes automatically annotate with mutational status of genes mutated in the most samples (this is always based on coding/splice site mutation) (bool)
#' @param topn_genes if annotate_most_altered_genes is true, how many genes to automatically visualise (integer)
#' @param metadata_columns name of metadata columns to annotate heatmap based on (character)
#' @param include_silent_mutations consider a gene mutated even if the only mutations present are silent (bool) 
#' @param show_rownames show sample names on rows (bool)
#' @param show_colnames show sample names on columns  (bool)
#'
#' @return pheatmap object
#' @export
#'
#' @examples
#' maf <- maftools::tcgaLoad("GBM", source = "Firehose") 
#' maftools_cluster_samples(maf)
maftools_cluster_samples <- function(maf, custom_genelist_to_cluster_by = NULL, number_of_genes = 50, genes_to_annotate=NULL, annotate_most_altered_genes=TRUE, topn_genes = 5, metadata_columns=NULL, include_silent_mutations = FALSE, show_rownames = FALSE, show_colnames=FALSE, annotation_legend=TRUE, fontsize=10){
  rlang::check_installed("pheatmap", reason = "to run somatic clustering")
  
  genes_in_maf <- maf %>% 
    maftools_get_all_data(include_silent_mutations = include_silent_mutations) %>% 
    dplyr::pull(Hugo_Symbol) %>%
    unique()
  
  
  # Identify 'important_genes' to base clustering off
  if(is.null(custom_genelist_to_cluster_by)){
    message("Clustering based on ", number_of_genes, " genes mutated in the most samples")
  important_genes <- maf %>% 
    maftools::getGeneSummary() %>% 
    head(number_of_genes) %>% 
    dplyr::pull(Hugo_Symbol)
  }
  else{
    message("Clustering based on a custom genelist. Ignoring `number_of_genes`")
    assertthat::assert_that(is.character(custom_genelist_to_cluster_by), msg = paste0("Custom genelist should be a character vector. Not a ", class(custom_genelist_to_cluster_by)) )
    important_genes <- unique(custom_genelist_to_cluster_by)
    genes_not_found <- important_genes[!important_genes %in% genes_in_maf]
    assertthat::assert_that(length(genes_not_found) == 0, msg = paste0("Some of the genes in the genelist supplied are not mutated in any samples: ", paste0(genes_not_found,collapse=", ")))
  }
  
  
  tumor_sample_barcodes <- maf %>% maftools::getSampleSummary() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
  
  # Construct Distance matrix
  df <- maf %>%
    maftools_get_all_data(include_silent_mutations = include_silent_mutations) %>%
    dplyr::select(Hugo_Symbol, Tumor_Sample_Barcode) %>%
    dplyr::filter(Hugo_Symbol %in% important_genes) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Val = 1) %>%
    tidyr::complete(Hugo_Symbol, Tumor_Sample_Barcode) %>%
    tidyr::pivot_wider(names_from = Hugo_Symbol, values_from = Val) %>%
    tibble::column_to_rownames("Tumor_Sample_Barcode") %>%
    replace(is.na(.), 0)
  
  jaccard.dist <- dist(df, method = "binary") # Binary = 1-jaccard
  
  tsbs = rownames(as.matrix(jaccard.dist))
  
  
  
  # Reformat selected sample-level metadadata columns to annotate heatmap with
  if (!is.null(metadata_columns)){
    clindata=maftools::getClinicalData(maf)
    assertthat::assert_that(all(metadata_columns %in% colnames(clindata)), msg = paste0("No column called: ", metadata_columns[!metadata_columns %in% colnames(clindata)], " in clinical data", collapse = " or "))
    row_annotations_df <- clindata[match(tsbs, clindata$Tumor_Sample_Barcode), metadata_columns] %>% as.data.frame()
    rownames(row_annotations_df) <- tsbs
  }
  else
    row_annotations_df=NULL
  
  # Add genes altered in many samples to the list of those to annotate heatmap with 
  if(annotate_most_altered_genes){
    most_altered_genes = maf %>% 
      maftools::getGeneSummary() %>% 
      dplyr::pull(Hugo_Symbol) %>% 
      head(n=topn_genes)
    genes_to_annotate <- unique(c(genes_to_annotate, most_altered_genes))
  }
  
  tsb_list=NULL
  if(!is.null(genes_to_annotate)){
    assertthat::assert_that(all(genes_to_annotate %in% genes_in_maf), msg = paste0("Could not find the following genes in maf: ", paste0(genes_to_annotate[!genes_to_annotate %in% genes_in_maf], collapse = ", ")))
    tsb_list <- sapply(genes_to_annotate, function(gene){
      maf %>% 
        maftools::subsetMaf(genes = gene) %>% 
        maftools::getSampleSummary() %>% 
        dplyr::pull(Tumor_Sample_Barcode) %>%
        unique()
    })
  }
  
  if(!is.null(tsb_list)){
    anno_colors = sapply(genes_to_annotate, function(gene){ list(c("FALSE"="white", "TRUE"="black"))})
    if (is.null(row_annotations_df))
      row_annotations_df <- data.frame(row.names = tsbs)
    for (gene in names(tsb_list)){
      row_annotations_df[gene] <- as.character(tsbs %in% tsb_list[[gene]])
    }
  }
  else
    anno_colors = NULL
  
  # Run heirarhical clustering and build heatmap
  pheatmap::pheatmap(
    as.matrix(jaccard.dist),
    annotation_col = row_annotations_df,
    show_rownames = show_rownames, 
    show_colnames = show_colnames, 
    annotation_colors = anno_colors, 
    annotation_legend = annotation_legend,
    fontsize = fontsize
  )
  
  #heatmap(as.matrix(df), distfun = function(x) {dist(x, method = "binary")}) # This clusters samples as above but is in sample x feature space 
  
}

#' Add clindata to MAF
#'
#' Adds 'extra' clinical metadata to a maf object with existing metadata
#' 
#' @param maf an existing MAF object as produced by maftools::read.maf (MAF)
#' @param clindata_path Eithera path to a csv/tsv that contains sample level metadata. Must include a 
#'
#' @return maf object with
#' @export
#'
maftools_add_clinical_data <- function(maf, clindata_path){
  #assertions
  assertthat::assert_that(file.exists(clindata_path), msg = paste0("Failed to find file: ", clindata_path))
  
  clindata_df <- data.table::fread(clindata_path) %>%
    type.convert(as.is = TRUE)
  
  # Check for Tumor_Sample_Barcode column
  assertthat::assert_that("Tumor_Sample_Barcode" %in% names(clindata_df), msg = "Sample level metadata file needs to contain a 'Tumor_Sample_Barcode' column")
  
  #Ensure no TSBs are duplicated
  dup_index <- anyDuplicated(clindata_df[["Tumor_Sample_Barcode"]])
  assertthat::assert_that(dup_index == 0, msg = paste0("Duplicate TSBs found: ", paste0(dup_index, collapse = ", ")))
  
  maf@clinical.data <- dplyr::left_join(x = maf@clinical.data, y = clindata_df)
  return(maf)
}
