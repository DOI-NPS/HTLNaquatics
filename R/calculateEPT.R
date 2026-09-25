#' @title calculateEPT: calculates the EPT richness and EPT ratio with the
#' output from getInvtCounts function.
#'
#' @importFrom dplyr mutate filter summarise n n_distinct
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
  type_list <- c("ratio", "richness")

  if(identical(type, "all")){type <- type_list} else {type <- match.arg(type,
                                                                        choices = type_list,
                                                                        several.ok = TRUE)}

  # Orders for EPT
  ept_order <- c("Ephemeroptera",
                 "Plecoptera",
                 "Trichoptera")

  # Columns to group by
  cols <- c("LocationID",
            "Season",
            "RiffleNo",
            "Replicate")

  ## calculating ept ratio and ept richness
  ept_all <- data |>
    dplyr::filter(if_all(c(LocationID, Season, RiffleNo), ~!is.na(.))) |>
    dplyr::summarise(ept_count = sum(Order %in% ept_order, na.rm = TRUE),
                     total_samp = dplyr::n(),
                     ept_richness = dplyr::n_distinct(Order[Order %in% ept_order], na.rm = TRUE),
                     ept_ratio = ept_count / total_samp,
                     .by = cols)

  # WORKING HERE:

  if ("ratio" %in% type)    cols <- c(cols, "ept_ratio")
  if ("richness" %in% type) cols <- c(cols, "ept_richness")

  out <- dplyr::select(ept_all, dplyr::all_of(cols))


  # if(setequal(type %in% c("ratio", "richness"))){
  #
  #   ## type = "all"
  #   out <- ept_all
  #
  # } else if (identical(type, "ratio")){
  #
  #   ## type == "ratio"
  #   out <- ept_all |>
  #     select(LocationID,
  #            Season,
  #            RiffleNo,
  #            Replicate,
  #            ept_ratio)
  #
  # } else if (identical(type, "richness")){
  #
  #   ## selecting ept_richness
  #   out <- ept_all |>
  #     select(LocationID,
  #            Season,
  #            RiffleNo,
  #            Replicate,
  #            ept_richness)
  # }

  return(out)

}
