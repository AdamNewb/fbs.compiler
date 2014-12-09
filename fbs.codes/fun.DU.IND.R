#' Function to compile industrial use
#'
#' This function imports biofuel data from the aglink-cosimo dataset and then adds a further
#' 5 percent from primary DS for total industrial use. It also allocates the domestic supply 
#' of oilseeds, which can be used as food, into industrial use in the form of domestic crush
#' ensuring full accounting

#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param Refa - standard column references for output
#' @param bio - wide format data file from aglink-cosimo
#' @param Indust - vector of industrially-used commodities 
#' @param PrimaryAreaCodes - data.frame of nutrient and commodity descriptor codes by country/year 
#' @param lDSprimary - data.frame of primary domestic supply compiled earlier

#' @keywords industrial use
#' @export
#' @examples
#' fun.DU.IND()



fun.DU.IND = function (sYr, lYr, 
                       Refa, 
					   bio, 
					   Indust, 
					   PrimaryAreaCodes, 
					   lDSprimary){
					   
#reshape bio in to long format
         a <- which(colnames(bio) == paste0("X", sYr))
         b <- which(colnames(bio) == paste0("X", lYr))
         bio <- bio[, c(1, 3, a:b)]
         BIOdf <- melt(bio,
              id.vars = c("AreaCode", "ItemCode"), 
			  value.name = "BIO", 
			  variable.name = "Year")
			  
         BIOdf$Year <- substr(BIOdf$Year,2,5)
         BIOdf <- merge(unique(BIOdf), PrimaryAreaCodes, 
                        by = c("ItemCode", "AreaCode", "Year"), all = FALSE)

#calculate and aggregate nutrient equivalence of bio production	to FBS commodity list					
         BIOdf[is.na(BIOdf)] <- 0
         BIOdf["BIO"] <- BIOdf$BIO * 1000
         BIOdf["BIOe"] <- BIOdf$BIO * BIOdf$ENERC_KCAL
         BIOdf["BIOp"] <- BIOdf$BIO * BIOdf$Protein / 100
         BIOdf["BIOf"] <- BIOdf$BIO * BIOdf$Lipid_Tot / 100
         BIOdf["BIOc"] <- BIOdf$BIO * BIOdf$CHOCDF / 100

         aggLBIOdf <- aggregate(x = BIOdf[,c("BIO", "BIOe", "BIOp", "BIOf", "BIOc")], 
                       by = list(BIOdf$AreaCode, BIOdf$Group, BIOdf$Commodity, BIOdf$Year), 
					   FUN = sum)
         colnames(aggLBIOdf) <- c(Refa, "BIO", "BIOe", "BIOp", "BIOf", "BIOc")
         lBIO <- aggLBIOdf

# get the primary domestic supply of commodities subject to industrial use (we use primary
# because we don't put nachos to ethanol use, only maize). 

         xIndustList <- unique(Indust)
         lIND <- merge(lBIO, lDSprimary, by = c(Refa), all = TRUE)
         lIND <- lIND[lIND$Commodity %in% xIndustList == TRUE, ]
		 
# put oilseeds crushed into industrial use
		
		 lIND[lIND$Group == "Oilseeds", "INDe"] <- 
		                    (1-(lIND[lIND$Group == "Oilseeds", "QPe"] /
		                        lIND[lIND$Group == "Oilseeds", "rawQPe"])) * 
							    lIND[lIND$Group == "Oilseeds", "DSe"]
								
		 lIND[lIND$Group == "Oilseeds", "INDp"] <- 
		                    (1-(lIND[lIND$Group == "Oilseeds", "QPp"] /
		                        lIND[lIND$Group == "Oilseeds", "rawQPp"])) * 
								lIND[lIND$Group == "Oilseeds", "DSp"]
													 
		 lIND[is.na(lIND)] <- 0

# for positive supply, add a further 5 percent to industrial use
		 posDSe <- lIND$DSe > 0 & lIND$Group != "Oilseeds"
		 
         lIND[posDSe, "INDe"] <- lIND[posDSe, "BIOe"] + (lIND[posDSe, "DSe"] * 0.05)
         lIND[posDSe, "INDp"] <- lIND[posDSe, "BIOp"] + (lIND[posDSe, "DSp"] * 0.05)
         lIND <- lIND[ ,c(Refa, "INDe", "INDp")]

		 
return(lIND)	
}