#' Function to process FBS-ready production from the server-extracted file
#'
#' This function further processes the server-extracted file by way of adding fish
#' production, merging nutrient and commodity descriptor codes and establishing milling
#' waste from cereal flour processing for livestock feed supply. The returned file forms
#' the basis from which FBS production is calculated. Fish production is downloaded as a 
#' flat file from fishstat.j 
#'
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param QPdf - data.frame of server-extracted production 
#' @param fishDF - data.frame of fish production
#' @param AreaCodes - data.frame of area codes
#' @param faoEU - vector of EU area codes in FAO classification
#' @param PrimaryAreaCodes - data.frame of nutrient and commodity descriptor codes by country/year 
#' @keywords production
#' @export
#' @examples
#' fun.DS.QP()


fun.DS.QP = function (sYr, lYr,
                      QPdf,
					  fishDF,
					  AreaCodes,
					  PrimaryAreaCodes) 
			{

            QPdf[is.na(QPdf)] <- 0
			
# wide fish data are reported by ISO3 area. Merge to get M49 areas and melt to normalize
            fishDF <- merge(fishDF, AreaCodes, 
				            by = c("ISO3"), all = FALSE)
		
				
            fishDF <- fishDF[c(12, 2:9)]
            colnames(fishDF)[1] = "AreaCode"
            
			lfishDF <- melt(fishDF,
                            id.vars = c("AreaCode", "ItemCode", "ElementCode"), 
			                value.name = "Production", 
			                variable.name = "Year")
#calculate EU fish QP (only for AMIS and for latest year, default EU28)

            euCodes = AreaCodes[AreaCodes$EU28==TRUE, "AreaCode"]
			EUfish <- lfishDF[which(lfishDF$AreaCode %in% euCodes), ]	
		
	        aEUfish = aggregate(x = EUfish[, c("Production")], 
                          by = list(ItemCode=EUfish$ItemCode, Year=EUfish$Year, ElementCode=EUfish$ElementCode), 
					      FUN = sum)
		
            colnames(aEUfish)[colnames(aEUfish) == "x"] <- "Production"
            aEUfish["AreaCode"] <- 5706
            lfishDF <- rbind(lfishDF, aEUfish)
			
            lfishDF["Flag"] <- ""
            colnames(QPdf)[colnames(QPdf) == "Value"] <- "Production"

            lQPdf <- rbind(QPdf, lfishDF)
            lQPdf$ElementCode <- NULL
            lQPdf <- lQPdf[lQPdf$Year >= sYr & lQPdf$Year <= lYr, ]

# get the nutrients of production
            lQPdf <- merge(lQPdf, PrimaryAreaCodes, 
                         by = c("ItemCode", "AreaCode", "Year"), 
			             all = FALSE)

            lQPdf["rawQPe"] <- lQPdf$Production * lQPdf$Edible * lQPdf$ENERC_KCAL
            lQPdf["rawQPp"] <- lQPdf$Production * lQPdf$Edible * lQPdf$Protein / 100
            lQPdf["rawQPf"] <- lQPdf$Production * lQPdf$Edible * lQPdf$Lipid_Tot / 100
            lQPdf["rawQPc"] <- lQPdf$Production * lQPdf$Edible * lQPdf$CHOCDF / 100
 
            lQPdf["QPe"] <- lQPdf$Production * lQPdf$ENERC_KCAL * lQPdf$Edible * lQPdf$Crush
            lQPdf["QPp"] <- lQPdf$Production * lQPdf$Protein * lQPdf$Edible * lQPdf$Crush / 100
            lQPdf["QPf"] <- lQPdf$Production * lQPdf$Lipid_Tot * lQPdf$Edible * lQPdf$Crush / 100
            lQPdf["QPc"] <- lQPdf$Production * lQPdf$CHOCDF * lQPdf$Edible * lQPdf$Crush / 100

# calculate residuals for feed, e.g. the milling waste from cereal flour processing
			lQPdf["FDRDe"] <- lQPdf[ , "Production"] * (1 - lQPdf[, "Extraction"]) * lQPdf[, "ENERC_KCAL"]			  
			lQPdf["FDRDp"] <- lQPdf[ , "Production"] * (1 - lQPdf[, "Extraction"]) * lQPdf[, "Protein"] / 100
				   
  
# aggregate by FBS commodities	

		
            aggQP <- aggregate(x = lQPdf[, c("Production", "rawQPe", "rawQPp", "rawQPf", "rawQPc",
                                             "QPe", "QPp", "QPf", "QPc", "FDRDe", "FDRDp")],
                               by = list(AreaCode = lQPdf$AreaCode, 
					                     Year = lQPdf$Year, 
							             Group = lQPdf$Group, 
							             Commodity = lQPdf$Commodity),
					           FUN = sum)

# data quality can be assessed by subsetting data flagged as forecast (F), trended (T) or imputed (Im)

            QPdq <- lQPdf[lQPdf$Flag == "F " | lQPdf$Flag == "T " |
			              lQPdf$Flag == "Im", ] #lQPdf[lQPdf$Flag != "  " | lQPdf$Flag != "* " , ]
            aggQPdq <- aggregate(x = QPdq[, c("Production", "rawQPe", "rawQPp", "rawQPf", "rawQPc",
                                             "QPe", "QPp", "QPf", "QPc")],
                               by = list(AreaCode = QPdq$AreaCode, 
					                     Year = QPdq$Year, 
							             Group = QPdq$Group, 
							             Commodity = QPdq$Commodity),
					           FUN = sum)
						
return(list(aggQP, aggQPdq))

}