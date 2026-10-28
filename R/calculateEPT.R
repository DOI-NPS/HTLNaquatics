#' @title calculateEPT: calculates the EPT richness and EPT ratio with the
#' output from getInvtCounts function.
#'
#' @importFrom dplyr mutate filter summarise n n_distinct
#' @importFrom tidyselect all_of
#'
#' @description This function calculates the EPT richness and ratio from HTLN
#' invertebrate data.
#'
#' @param data The dataframe for calculating EPT.
#' @param type Allows you to select the type of EPT (richness or ratio or both).
#' \describe{
#' \item{"all"}{Default. Includes both EPT richness and ratio in the output}
#' \item{"richness"}{Only include EPT richness calculation in the output}
#' \item{"ratio"}{Only include EPT ratio calulcation in the output}}
#'
#' @examples
#' \dontrun{
#'
#' library(HTLNaquatics)
#'
#' calculateEPT()
#'
#' # Default: return both calculated EPT richness and ratio of HTLN aquatic
#' # invertebrate data.
#' calculateEPT()
#'
#' # Specific type (ratio)
#' calculateEPT(type = "ratio")
#' }
#'
#' @returns Returns a data frame of EPT ratio and/or richness data
#'
#' @export

calculateEPT <- function(data, type = 'all'){

  # Bug handling

  ## type of calculation
  type_list <- c("ratio", "richness")

  if(identical(type, "all")){type <- type_list} else {type <- match.arg(type,
                                                                        choices = type_list,
                                                                        several.ok = TRUE)}

  ## Checking data structure
  col_names <- c("LocationID",
                 "Park",
                 "Season",
                 "RiffleNo",
                 "Replicate",
                 "Order",
                 "TaxonCode")

  df_cols <- colnames(data)

  missing_cols <- setdiff(col_names, df_cols)

  if(length(missing_cols) > 0) {
    stop("Missing the following columns for EPT calculations: ",
         paste0(missing_cols,
                collapse = ", "))
  }

  # Orders for EPT
  ept_order <- c("Ephemeroptera",
                 "Plecoptera",
                 "Trichoptera")

  # Columns to group by
  cols <- c("LocationID",
            "Park",
            "Season",
            "RiffleNo",
            "Replicate")

  # Calculations

  ### removing NA
  ept1 <- data |>
    dplyr::filter(if_all(all_of(cols), ~!is.na(.)))

  ### filtering order and family for calculations
  ept2 <- ept1 |>
    dplyr::filter(Order %in% ept_order |
                    TaxonCode == "CHIR") # for Chironomidae

  # Calculating EPT
  ept3 <- ept2 |>
    dplyr::summarise(ept_count = sum(Order %in% ept_order, na.rm = TRUE),
                     total_samp = dplyr::n(),
                     ept_richness = dplyr::n_distinct(Order[Order %in% ept_order], na.rm = TRUE),
                     ept_ratio = ept_count / total_samp,
                     .by = all_of(cols))

  # Averaging to riffle
  ept4 <- ept3 |>
    dplyr::summarise(ept_richness = mean(ept_richness, na.rm = TRUE),
                     ept_ratio = mean(ept_ratio, na.rm = TRUE),
                     .by = c(Park,
                             LocationID,
                             Season,
                             RiffleNo))

  # Site level measurement
  ept5 <- ept4 |>
    dplyr::summarise(ept_richness_mean = mean(ept_richness, na.rm = TRUE),
                     ept_richness_sd = sd(ept_richness, na.rm = TRUE),
                     ept_ratio_mean = mean(ept_ratio, na.rm = TRUE),
                     ept_ratio_sd = mean(ept_ratio, na.rm = TRUE),
                     .by = c(Park,
                             LocationID,
                             Season))

  # output columns
  cols_out <- c("Park", "LocationID", "Season")

  if ("ratio" %in% type)    cols_out <- c(cols_out, "ept_ratio_mean", "ept_ratio_sd")
  if ("richness" %in% type) cols_out <- c(cols_out, "ept_richness_mean", "ept_richness_sd")

  out <- dplyr::select(ept5, dplyr::all_of(cols_out))

  return(out)

}
