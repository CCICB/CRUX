#' 
#' #' Network Map View of Data Pool
#' #'
#' #' Takes a maf_data_pool and visualizes it as a of a sigmaJS network
#' #'
#' #' @param maf_data_pool data pool to visualise (maf_data_pool)
#' #'
#' #' @return sigmaJS htmlwidget 
#' #'
#' #'
#' #' @examples
#' #' maf_data_pool_too_network_map(pcawg_datasets_to_data_pool(maf_data_pool = new_maf_data_pool()))
#' maf_data_pool_too_network_map <- function(maf_data_pool, size_of_sources = 10, size_of_datasets = 7){
#'   assert_that_class_is_maf_data_pool(maf_data_pool)
#'   assertthat::assert_that(is.numeric(size_of_sources))
#'   assertthat::assert_that(is.numeric(size_of_datasets))
#'   
#'   maf_data_pool_df <- maf_data_pool_to_dataframe(maf_data_pool)
#'   
#'   assertthat::assert_that("name_of_data_source" %in% colnames(maf_data_pool_df), msg = utilitybeltassertions::fmterror("maf_data_pool_too_network_map: assumes maf_data_pool_df contains the column [name_of_data_source]. No such column was found."))
#'   assertthat::assert_that("unique_name" %in% colnames(maf_data_pool_df), msg = utilitybeltassertions::fmterror("maf_data_pool_too_network_map: assumes maf_data_pool_df contains the column [unique_name]. No such column was found."))
#'   assertthat::assert_that("short_name" %in% colnames(maf_data_pool_df), msg = utilitybeltassertions::fmterror("maf_data_pool_too_network_map: assumes maf_data_pool_df contains the column [short_name]. No such column was found."))
#'   assertthat::assert_that("derived_from" %in% colnames(maf_data_pool_df), msg = utilitybeltassertions::fmterror("maf_data_pool_too_network_map: assumes maf_data_pool_df contains the column [derived_from]. No such column was found."))
#'   
#'   data_sources.v <- unique(maf_data_pool_df[["name_of_data_source"]])
#'   
#'   nodes <- dplyr::tibble(
#'     id = c(data_sources.v, maf_data_pool_df[["unique_name"]]),
#'     label = c(data_sources.v, maf_data_pool_df[["short_name"]])
#'   )
#'   nodes[["size"]] <- c(
#'     rep(size_of_sources, length(data_sources.v)),
#'     rep(size_of_datasets, length(maf_data_pool_df[["unique_name"]]))
#'   )
#'   
#'   edges <- dplyr::tibble(
#'     id = maf_data_pool_df[["unique_name"]],
#'     source = maf_data_pool_df[["unique_name"]],
#'     target = ifelse(!is.na(maf_data_pool_df[["derived_from"]]), yes = maf_data_pool_df[["derived_from"]], no=maf_data_pool_df[["name_of_data_source"]])
#'   )
#'   
#'   #What happens if there are two sources (e.g. comma separated). We need to be able to split it
#'   sigmajs::sigmajs() %>% # initialise
#'     sigmajs::sg_nodes(nodes, id, label, size) %>% # add nodes
#'     sigmajs::sg_edges(edges, id, source, target) %>% # add edges
#'     sigmajs::sg_layout(layout = igraph::layout_nicely) %>%  # layout
#'     sigmajs::sg_cluster(quiet = TRUE) %>% # cluster
#'     sigmajs::sg_drag_nodes() %>% # allows user to drag nodes
#'     sigmajs::sg_neighbours() %>%
#'     sigmajs::sg_settings(mouseWheelEnabled=FALSE, labelSize = "proportional")
#'   
#' }
#' 
