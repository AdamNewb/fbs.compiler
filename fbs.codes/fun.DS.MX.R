#' Function to compile FBS trade (imports/exports)
#'
#' This function compiles trade data from UNSD by sub-setting product type and aggregating 
#' @param MX - balanced data.frame of trade from UNSD
#' @param TradeFlow - Imports or exports?
#' @param TradeCode - UNSD code for flow type - rtCode or ptCode
#' @param MXval - primary (e.g. Mp) or standardized (e.g. Xs) conditioned on TradeFlow
#' @param DQ - Booelan whether the process for Data Quality
#' @param MXdesc - description of MXval
#' @param MXsubval - code to filter MX
#' @param HScodes - trade data attributes, e.g, primary or standardized, dist. of nutrients
#' @param AreaCodes - input data.frame of area codes and names

#' @keywords trade
#' @export
#' fun.DS.MX()
						

fun.DS.MX = function (MX, 
                      TradeFlow,
					  TradeCode,
					  MXval,
					  DQ,
					  MXdesc,
					  MXsubval,
                      HScodes, 
					  AreaCodes) {
	

# reshape to semi-long format and get attributes of transactions	
  lMX <- melt(MX, id.vars = c(TradeCode, "ItemCode"),
               value.name = TradeFlow, 
			variable.name = "Year")
			
  lMX$Year <- as.numeric(as.character(lMX$Year))
  lMXdf <- merge(lMX, HScodes, 
               by = c("ItemCode"), all = FALSE)
  colnames(AreaCodes)[2] = TradeCode
  lMXdf <- merge(lMXdf, AreaCodes, 
               by = c(TradeCode), all = FALSE)

# define relevant column names			   
  if(DQ == FALSE){
     lMXdf <- lMXdf[which(lMXdf$TYPE == MXsubval), ]
	 }
  colnames(lMXdf)[colnames(lMXdf) == TradeFlow] <- paste0(TradeFlow, MXdesc)
  

# apply conversion factors for FBS and nutrient equivalence

  lMXdf[paste0(TradeFlow, MXdesc)] <- lMXdf[paste0(TradeFlow, MXdesc)] / 1000
  lMXdf[paste0(MXval, "e")] <- lMXdf[paste0(TradeFlow, MXdesc)] * lMXdf$ENERC_KCAL.x * lMXdf$Edible 
  lMXdf[paste0(MXval, "p")] <- lMXdf[paste0(TradeFlow, MXdesc)] * lMXdf$Protein.x * lMXdf$Edible / 100
  lMXdf[paste0(MXval, "f")] <- lMXdf[paste0(TradeFlow, MXdesc)] * lMXdf$Lipid_Tot.x * lMXdf$Edible / 100
  lMXdf[paste0(MXval, "c")] <- lMXdf[paste0(TradeFlow, MXdesc)] * lMXdf$CHOAVLDF.x * lMXdf$Edible / 100
  
# calculate residuals for feed
			lMXdf[paste0(MXval, "FDRDe")] <- lMXdf[paste0(TradeFlow, MXdesc)] * (1 - lMXdf$Extraction) * lMXdf$ENERC_KCAL.x			  
			lMXdf[paste0(MXval, "FDRDp")] <- lMXdf[paste0(TradeFlow, MXdesc)] * (1 - lMXdf$Extraction) * lMXdf$Protein.x / 100
		

# aggregate for FBS commodity list
		
  agglMXdf <- aggregate(x = lMXdf[, c(paste0(TradeFlow, MXdesc), 
                                      paste0(MXval, "e"), 
                                      paste0(MXval, "p"), 
									  paste0(MXval, "f"),
									  paste0(MXval, "c"),
									  paste0(MXval, "FDRDe"),
									  paste0(MXval, "FDRDp"))],
                       by = list(lMXdf$AreaCode, lMXdf$Year, lMXdf$Group, lMXdf$Commodity),
                       FUN = sum)

  colnames(agglMXdf) <- c("AreaCode", "Year", "Group", "Commodity", 
                          paste0(TradeFlow, MXdesc), 
                          paste0(MXval, "e"), paste0(MXval, "p"), 
						  paste0(MXval, "f"), paste0(MXval, "c"),
						  paste0(MXval, "FDRDe"), paste0(MXval, "FDRDp"))

  lMXdf <- agglMXdf

return(lMXdf)
}