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

  # WORKING HERE:

  if ("ratio" %in% type)    cols <- c(cols, "ept_ratio")
  if ("richness" %in% type) cols <- c(cols, "ept_richness")

  out <- dplyr::select(ept_all, dplyr::all_of(cols))

  return(out)

}
