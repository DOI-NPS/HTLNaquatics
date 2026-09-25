#' @title getInvtCounts: get count data from views with filtering options.
#'
#' @importFrom dplyr mutate filter
#'
#' @description This function gets the count data from the views downloaded
#' with importData.
#'
#' @param park Filter on park code. Can select more than one. Valid inputs:
#' \itemize{
#' \item{"all":}{Includes all parks in the network}
#' \item{"ARPO":}{Arkansas Post NM only}
#' \item{"BUFF":}{Buffalo NR only}
#' \item{"CUVA":}{Guyahoga Valley NP only}
#' \item{"EFMO":}{Effigy Mounds NM only}
#' \item{"GWCA":}{George Washington Carver NM only}
#' \item{"HEHO":}{Herbert Hoover NHS only}
#' \item{"HOME":}{Homestead NHP only}
#' \item{"HOCU":}{Hopewell Culture NHP}
#' \item{"HOSP":}{Hot Springs NP only}
#' \item{"LIBO":}{Lincoln Boyhood NM only}
#' \item{"OZAR":}{Ozark NSR only}
#' \item{"OZRS":}{Dont know?}
#' \item{"PERI":}{Pea Ridge NMP only}
#' \item{"PIPE":}{Pipestone NM only}
#' \item{"TAPR":}{Tallgrass Prairie NP only}
#' \item{"WICR":}{Wilson's Creek NB only}
#' }
#'
#' @param season Numeric. Filter on years based on Season. Default is all years.
#'
#' @param taxon Quoted string to return a particular taxonomic group based on
#' TaxonCode. If a taxonomic group is specified that does not occur in the
#' imported data, function will error out with a list of unmatched TaxonCodes.
#'
#' @examples
#' \dontrun{
#'
#' library(HTLNaquatics)
#'
#' importData()
#'
#' # Default return all count data of HTLN aquatic invertebrate or fish
#' # for all parks and dates.
#' counts <- getCounts()
#'
#' # return data for BUFF and CUVA for 2005 - 2008
#' counts <- getCounts(park = c("BUFF", "CUVA"), years = 2005:2008)
#'
#' # Specific taxa
#' getInvtCounts(taxon = c("INSECTA", "COLLEMBOLA"))
#'
#' # Combo
#' getInvtCounts(park = "CUVA", season = 2010:2020, taxon = c("INSECTA"))
#' }
#'
#' @returns Returns a data frame of count data
#'
#' @export

getInvtCounts <- function(park = 'all', season = NULL, taxon = "all"){

  # Bug handling ----

  ## park handling
  park_list <-  c("ARPO", "BUFF", "CUVA", "EFMO",
                  "GWCA", "HEHO", "HOME", "HOCU",
                  "HOSP", "LIBO", "OZAR", "OZRS",
                  "PERI", "PIPE", "TAPR", "WICR")

  if(identical(park, "all")){park <- park_list} else {park <- match.arg(park,
                                                                   choices = park_list,
                                                                   several.ok = TRUE)}

  ## season handling
  ### NULL == all season
  if(!is.null(season)){
    if(!is.numeric(season)) stop("Season must be numeric (e.g. 2001:2020).")
    season <- as.integer(season)
  }

  # Compile Data ----

  env <- if(exists("HTLNInvert_VIEWS")){HTLNInvert_VIEWS} else {.GlobalEnv}

  # Get table
  invt_count1 <- tryCatch(get("tbl_Count",
                              envir = env),
                          error = function(e){stop("Count table not found. Please import database tables.")})

  temp12 <- tryCatch(get("tbl_Temp12",
                         envir = env),
                     error = function(e){stop("Temp12 table not found. Please import database tables.")})

  # Error for no park data
  parks_in_data <- unique(substr(invt_count1$LocationID, 1, 4))

  ## Error
  missing_data <- all(!(park %in% parks_in_data))
  if(missing_data){
    stop(paste0("No data recorded in the Count Table for the following park(s): "),
         paste0(park, collapse = ", "))
  }

  ## Return some but not missing parks
  missing_some <- setdiff(park, parks_in_data)
  if(length(missing_some) > 0){
    warning("No data recorded in the Count Table fore the following park(s): ",
            paste0(missing_some, collapse = ", "),
            ". Returning data for available park(s): ",
            paste0(setdiff(park, missing_some), collapse = ", "))
    park <- intersect(park, parks_in_data)
  }

  # Taxon handling
  taxon_names <- sort(unique(invt_count1$TaxonCode))

  if(any(taxon == "all")){
    taxon <- taxon_names
  } else {
    # check for typos
    typos <- setdiff(taxon,
                     taxon_names)
    if(length(typos) > 0){
      stop(paste0("The following specified taxon codes do not have matching records in the Count Table: "),
           paste0(typos, collapse = ", "))
    }
  }

  # Filtering ----
  invt_count2 <- invt_count1 |>
    dplyr::mutate(Park = substr(LocationID, 1, 4)) |>
    dplyr::filter(Park %in% park &
                    (is.null(season) || Season %in% season) &
                    TaxonCode %in% taxon)

  # joining with temp 12 file
  invt_count <- invt_count2 |>
    dplyr::left_join(temp12)


  return(invt_count)
}
