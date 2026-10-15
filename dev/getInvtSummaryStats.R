#' @title getInvtSummaryStats: gets the benthic macro invertebrate summary
#' statistics for HTLN data
#'
#' @importFrom dplyr mutate filter summarise n n_distinct
#'
#' @description This function calculates the taxa richness, Ephemeroptera,
#' Plecoptera, Trichoptera (EPT) richness, Shannon’s Diversity Index, and
#' Hilsenhoff Biotic Index for HTLN invertebrate data.
#'
#' @param df The dataframe for calculating EPT.
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

getInvtSummaryStats <- function(data){
  list(ept = calculateEPT(data),
       shannon = calculateShannon(data),
       taxon_richness = calculateTRichness(data),
       hbi = calculateHBI(data))
}
