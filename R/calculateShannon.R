#' @title calculateShannon: calculates the Shannon's Diversity Index with the output from getInvtCounts function.
#'
#' @importFrom dplyr mutate filter summarise left_join
#' @importFrom tidyselect all_of
#'
#' @description This function calculates the Shannon's Diversity Index from HTLN
#' invertebrate data.
#'
#' @param data The dataframe for calculating SDI.
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

calculateShannon <- function(data){

  # Bug handling

  ## Checking data structure
  col_names <- c("LocationID",
                 "Park",
                 "Season",
                 "RiffleNo",
                 "Replicate",
                 "Order",
                 "TaxonCode",
                 "RepCount_Calc")

  df_cols <- colnames(data)

  missing_cols <- setdiff(col_names, df_cols)

  if(length(missing_cols) > 0) {
    stop("Missing the following columns for EPT calculations: ",
         paste0(missing_cols,
                collapse = ", "))
  }

  # Columns to group by
  cols <- c("LocationID",
            "Park",
            "Season",
            "RiffleNo",
            "Replicate")

  ### removing NA
  shannon1 <- data |>
    dplyr::filter(if_all(all_of(cols), ~!is.na(.)))

  # getting total number of individuals counted
  shannon2 <- shannon1 |>
    dplyr::mutate(total_count = sum(RepCount_Calc, na.rm = TRUE),
                  .by = tidyselect::all_of(cols))

  # proportions
  shannon3 <- shannon2 |>
    dplyr::mutate(proportion = RepCount_Calc / total_count)

  # natural log
  shannon4 <- shannon3 |>
    dplyr::mutate(nat_log = log(proportion))

  # proportion * natural log
  shannon5 <- shannon4 |>
    dplyr::mutate(prop_log = proportion * nat_log)

  # Shannon Diversity Index
  shannon6 <- shannon5 |>
    dplyr::summarise(H = sum(-prop_log),
                     .by = c(Park,
                             LocationID,
                             Season,
                             RiffleNo,
                             Replicate,
                             total_count))

  # getting richness
  richness <- calculateTRichness(data,
                                 type = "invertebrate")

  # merging richness with shannon
  shannon7 <- dplyr::left_join(shannon6,
                               richness,
                               by = join_by(Park,
                                            LocationID,
                                            Season,
                                            RiffleNo,
                                            Replicate))

  # Shannon Evenness
  shannon8 <- shannon7 |>
    dplyr::mutate(J = H / log(taxa_richness)) # this should be richness?

  # summarising to riffle
  shannon9 <- shannon8 |>
    dplyr::summarise(H = mean(H, na.rm = TRUE),
                     J = mean(J, na.rm = TRUE),
                     .by = c(Park,
                             LocationID,
                             Season,
                             RiffleNo))

  # summarising to site level
  shannon10 <- shannon9 |>
    dplyr::summarise(H_mean = mean(H, na.rm = TRUE),
                     H_sd = sd(H, na.rm = TRUE),
                     J_mean = mean(J, na.rm = TRUE),
                     J_sd = sd(J, na.rm = TRUE),
                     .by = c(Park,
                             LocationID,
                             Season))

  return(shannon10)

}
