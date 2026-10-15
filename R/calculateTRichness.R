#' @title calculateTRichness: calculates the taxa richness with the output from
#' getInvtCounts function.
#'
#' @importFrom dplyr filter summarise n_distinct
#' @importFrom tidyselect all_of
#'
#' @description This function calculates the taxa richness from HTLN
#' invertebrate data.
#'
#' @param data The dataframe for calculating taxa richness.
#'
#' @examples
#' \dontrun{
#'
#' library(HTLNaquatics)
#'
#' # Default: returns calculated taxa richness of HTLN aquatic invertebrate data.
#' calculateTRichness()
#' }
#'
#' @returns Returns a data frame of taxa richness data
#'
#' @export

calculateTRichness <- function(data){

  # Bug handling

  ## Checking data structure
  col_names <- c("LocationID",
                 "Season",
                 "RiffleNo",
                 "Replicate",
                 "Order",
                 "TaxonCode")

  df_cols <- colnames(data)

  missing_cols <- setdiff(col_names, df_cols)

  if(length(missing_cols) > 0) {
    stop("Missing the following columns for taxa richness calculations: ",
         paste0(missing_cols,
                collapse = ", "))
  }

  # Calculations

  # Columns to group by
  cols <- c("LocationID",
            "Season",
            "RiffleNo",
            "Replicate")

  ## removing NA
  taxa1 <- data |>
    dplyr::filter(if_all(all_of(cols), ~!is.na(.)))

  # Calculating Taxa Richness
  taxa2 <- taxa1 |>
    dplyr::summarise(taxa_richness = dplyr::n_distinct(TaxonCode),
                     .by = all_of(cols))

}
