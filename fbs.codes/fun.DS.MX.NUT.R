#' Function to compile nutrient parameters and attributes of trade
#'
#' This function compiles nutrient parameters and attributes of trade
#' @param HSm - relevant FBS trade codes from UNSD and attributes
#' @param HScodes - contains trade codes, descriptions and attributes (primary or processed)
#' @param meHS - mean of distribution of nutrients
#' @param seHS - 2 standard deviations of distribution of nutrients
#' @export
#' fun.DS.MX.NUT()


fun.DS.MX.NUT = function (HSm,
                          meHS,
					      sdHS) {
					  
  meHS <- meHS[c(1, 8:10, 12, 14)]
  sdHS <- sdHS[c(1, 8:10, 12, 14)]
  colnames(HSm)[1] <- "DF.HS_code"
  
  HScodes <- merge(merge(meHS, sdHS, by = ("DF.HS_code"), all = TRUE), HSm, 
                 by = ("DF.HS_code"), 
				 all = FALSE)
  colnames(HScodes)[colnames(HScodes) == "DF.HS_code"] <- "ItemCode"
  HScodes$ItemCode <- paste0(substr(HScodes$ItemCode, 1, 5), substr(HScodes$ItemCode, 7, 8))

return(HScodes)
}