#' @title calculateTRichness: calculates the taxa richness with the output from
#' getInvtCounts function or fish data function (not yet enabled).
#'
#' @importFrom dplyr filter summarise n_distinct if_all
#' @importFrom tidyselect all_of
#'
#' @description This function calculates the taxa richness from HTLN
#' invertebrate or fish (not yet enabled) data.
#'
#' @param data The dataframe for calculating taxa richness.
#'
#' @param type Indicate what type of dataset to calculate taxa richness on.
#' \describe{
#' \item{"invertebrate"}{Default. Calculates taxa richness for the invertebrate dataset}
#' \item{"fish"}{Calculates taxa richness for the fish dataset (not yet enabled)}
#' }
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

calculateTRichness <- function(data, type = "invertebrate"){

  # Bug handling for type
  type <- match.arg(type, c("invertebrate", "fish"))

  if(type == "invertebrate"){

  # Code for invertebrate data

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
            "Park",
            "Season",
            "RiffleNo",
            "Replicate")

  ## removing NA
  taxa1 <- data |>
    dplyr::filter(dplyr::if_all(tidyselect::all_of(cols), ~!is.na(.)))

  # Calculating Taxa Richness
  taxa <- taxa1 |>
    dplyr::summarise(taxa_richness = dplyr::n_distinct(TaxonCode),
                     .by = tidyselect::all_of(cols))

  } else{

    taxa <- NULL

    }

  return(taxa)
}
