#' @title getInvtSummaryStats: gets the benthic macro invertebrate summary
#' statistics for HTLN data
#'
#' @importFrom dplyr left_join select
#'
#' @description This function calculates the taxa richness, Ephemeroptera,
#' Plecoptera, Trichoptera (EPT) richness, Shannon’s Diversity Index, and
#' Hilsenhoff Biotic Index for HTLN invertebrate data.
#'
#' @param data The dataframe for calculating invertebrate summary statistics.
#'
#' @examples
#' \dontrun{
#'
#' library(HTLNaquatics)
#'
#' getInvtSummaryStats()
#'
#' # Default: return all summary statistics for HTLN invertebrate data.
#' getInvtSummaryStats()
#' }
#'
#' @returns Returns a data frame of invertebrate summary statistics.
#'
#' @export

getInvtSummaryStats <- function(data){

  # EPT
  ept <- calculateEPT(data, type = "all")

  # Shannon
  shannon <- calculateShannon(data)

  # Taxon Richness
  taxon_richness <- calculateTRichness(data, type = "invertebrate")

  # HBI
  hbi <- calculateHBI(data)

  # Merging Data
  summary_stats <- ept |>
    dplyr::left_join(shannon,
                     by = join_by(LocationID, Season,
                                  RiffleNo, Replicate)) |>
    dplyr::left_join(taxon_richness,
                     by = join_by(LocationID, Season,
                                  RiffleNo, Replicate, taxa_richness)) |>
    dplyr::left_join(hbi,
                     by = join_by(LocationID, Season,
                                  RiffleNo, Replicate))

  # selecting columns
  summary_stats1 <- summary_stats |>
    dplyr::select(LocationID,
                  Seawson,
                  RiffleNo,
                  Replicate,
                  taxa_richness,
                  ept_richness,
                  ept_ratio,
                  H,
                  J,
                  hbi)

  return(summary_stats1)

}
