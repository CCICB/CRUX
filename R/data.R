#' Prices of 50,000 round cut diamonds.
#'
#' A dataset describing the metadata of external tools used in the cancer space
#' diamonds.
#'
#' @format A data frame with 3 rows and 10 variables:
#' \describe{
#' \item{external_tools_df} {the dataframe to add tool metadata to. By default, will create and return a new dataframe}
#' \item{tool_name}{name of tool (string)}
#' \item{tool_id}{id of tool (string)}
#' \item{tool_group}{research group that built/maintains the tool (string)}
#' \item{tool_class}{class of tool. Usually 'Positive Selection', 'Variant Interpretation' (string)}
#' \item{tool_description}{brief description of tool (string)}
#' \item{instructions}{brief description of how to use the tool. Any HTML tags in the string will be correctly resolved. (string)}
#' \item{website}{url of tool (string)}
#' \item{doi}{publicatoin doi (string)}
#' \item{requires_maf_export}{does the tool require a maf to be exported in some other form (flag)}
#' \item{maf_conversion_function}{only relevent if requires_maf_export == true. A function that takes a MAF object (first argument), and a filepath (second argument) and writes a file to that filepath. The idea is that said file can then be used as input to the specified tool.}
#' ...
#' }
#' @source shinymaftools::external_tools_load_all_tools()
"external_tool_metadata"

