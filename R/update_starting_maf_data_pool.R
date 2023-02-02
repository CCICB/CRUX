update_starting_maf_data_pool <- function(filepath = paste0(system.file(package = "CRUX"), "/default_data_pool.Rds")){
  starting_maf_data_pool <- new_maf_data_pool()
  
  # prepare TCGA data
  message("Loading TCGA data")
  starting_maf_data_pool <- tcga_datasets_to_data_pool(starting_maf_data_pool, source = "Firehose")
  message("  > done")
  
  # prepare PCAWG datasets
  message("Loading PCAWG data")
  starting_maf_data_pool <- pcawg_datasets_to_data_pool(starting_maf_data_pool)
  message("  > done")
  
  # Write to RDS file in inst/default_data_pool.Rds
  message("Writing to default_data_pool.Rds")
  saveRDS(object = starting_maf_data_pool, file = filepath)
  message("  > done")
  
  return(invisible(NULL))
}