#' @title calculateHBI: calculates the Hilsenhoff Biotic Index with the output
#' from getInvtCounts function.
#'
#' @importFrom dplyr filter summarise
#' @importFrom tidyselect all_of
#'
#' @description This function calculates the HBI from HTLN invertebrate data.
#'
#' @param data The dataframe for claculating HBI.
#'
#' @examples
#' \dontrun{
#'
#' library(HTLNaquatics)
#'
#' importData()
#'
#' # Default returns calculated HBI of HTLN aquatic invertebrate data.
#' calculateHBI()
#'
#' }
#'
#' @returns Returns a data frame of HBI data
#'
#' @export

calculateHBI <- function(data){

  # Bug handling

  ## Checking data structure
  col_names <- c("LocationID",
                 "Season",
                 "RiffleNo",
                 "Replicate",
                 "RepCount_Calc",
                 "TVxcount")

  df_cols <- colnames(data)

  missing_cols <- setdiff(col_names, df_cols)

  if(length(missing_cols) > 0) {
    stop("Missing the following columns for HBI calculations: ",
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
  hbi1 <- data |>
    dplyr::filter(if_all(all_of(cols), ~!is.na(.)))

  # calculating sums
  hbi2 <- hbi1 |>
    dplyr::summarize(tv_total = sum(TVxcount, na.rm = TRUE),
                     count_total = sum(RepCount_Calc, na.rm = TRUE),
                     .by = tidyselect::all_of(cols))

  # calculate hbi
  hbi3 <- hbi2 |>
    dplyr::mutate(hbi = tv_total / count_total)

  return(hbi3)
}
