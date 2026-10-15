#And now we'll calculate HBI which first requires us to create a small dataframe from our original data that has rows
#without Tolerance Values removed.
A <- as.data.frame(cbind(Inverts$LocationID, Inverts$Season, Inverts$Tag, Inverts$Tag2)) #R fussy, won't bind all
B <- cbind(Inverts$RepCount_Calc, Inverts$TVxcount)                                      #col w/o making them chars
InvertsHBI <- cbind(A, B)
names(InvertsHBI) <- c('Location', 'Season','Tag', 'Tag2', 'RepCount_Calc', 'TVxcount')
InvertsHBI <- na.omit(InvertsHBI)

HBI_summary <- InvertsHBI %>%
  filter(!is.na(InvertsHBI$Tag)) %>%
  group_by(InvertsHBI$Location, InvertsHBI$Season, InvertsHBI$Tag, InvertsHBI$Tag2) %>%
  summarize(HBI = sum(TVxcount)/sum(RepCount_Calc))
