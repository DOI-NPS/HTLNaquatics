library(HTLNaquatics)


# connecting to databases ----

db_path <- file.path("../../HTLN/HTLN Database")

# importData(instance = "local",
#            path = db_path,
#            name = "HTLNFish4.7.2.accdb")

importData(instance = "local",
           path = db_path,
           name = "HTLNInvert3.9.5.accdb")

# getInvtCounts
test <- getInvtCounts()
park = 'all'
years = 2005:format(Sys.Date(), "%Y")
taxon = "all"

data = test
type = "all"
type = "richness"


test <- calculateEPT(data = test)


df_cols <- c("LocationID", "Season")

data <- getInvtCounts()
