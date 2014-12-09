#' Function to compile error on standardized trade
#'
#' This function compiles the total error on standardized or D1D2D23 trade given the underlying 
#' distribution of the nutrients of the particular commodity class

#' @param MX - balanced data.frame of trade from UNSD
#' @param TradeFlow - Imports or exports?
#' @param TradeCode - UNSD code for flow type - rtCode or ptCode
#' @param MXval - primary (e.g. Mp) or standardized (e.g. Xs) conditioned on TradeFlow
#' @param lEqMXsd - standard deviation on standardized trade
#' @param MXval - primary (e.g. Mp) or standardized (e.g. Xs) conditioned on TradeFlow
#' @param MXsubval - code to filter MX
#' @param HScodes - trade data attributes, e.g, primary or standardized, dist. of nutrients
#' @param AreaCodes - input data.frame of area codes and names
#' @param Refa - vector of standard meta column names

#' @keywords trade
#' @export
#' fun.DS.SMXsd()


fun.DS.SMXsd = function (MX,
                         TradeFlow,
						 TradeCode,
						 lEqMXsd,
						 MXval,
						 MXsubval,
                         HScodes,
						 AreaCodes,
						 Refa){

  lMX <- melt(MX, id.vars = c(TradeCode, "ItemCode"),
               value.name = TradeFlow, 
			   variable.name = "Year")
  lMXdf <- merge(lMX, HScodes, 
               by = c("ItemCode"), all = FALSE)
  colnames(AreaCodes)[2] = TradeCode
  lMXdf <- merge(lMXdf, AreaCodes, 
               by = c(TradeCode), all = FALSE)
  lMXsd <- lMXdf[which(lMXdf$TYPE == MXsubval), ]
  colnames(lMXsd)[colnames(lMXsd) == TradeFlow] <- paste0(TradeFlow, ".sd")

 
#apply nutrients and aggregate to FBS commodity list 
  lMXsd[paste0(TradeFlow, ".sd")] <- lMXsd[paste0(TradeFlow, ".sd")] / 1000
  lMXsd[paste0(MXval, "e")] <- lMXsd[paste0(TradeFlow, ".sd")] * lMXsd$ENERC_KCAL.y
  lMXsd[paste0(MXval, "p")] <- lMXsd[paste0(TradeFlow, ".sd")] * lMXsd$Protein.y / 100
  lMXsd[paste0(MXval, "f")] <- lMXsd[paste0(TradeFlow, ".sd")] * lMXsd$Lipid_Tot.y / 100
  lMXsd[paste0(MXval, "c")] <- lMXsd[paste0(TradeFlow, ".sd")] * lMXsd$CHOAVLDF.y / 100

  agglMXsd <- aggregate(x = lMXsd[, c(paste0(TradeFlow, ".sd"), 
                                      paste0(MXval, "e"), 
                                      paste0(MXval, "p"), 
									  paste0(MXval, "f"),
									  paste0(MXval, "c"))],
                       by = list(lMXsd$AreaCode, lMXsd$Year, lMXsd$Group, lMXsd$Commodity),
                       FUN = sum)

  colnames(agglMXsd) <- c("AreaCode", "Year", "Group", "Commodity", 
                          paste0(TradeFlow, ".sd"), 
                          paste0(MXval, "e"), paste0(MXval, "p"), 
						  paste0(MXval, "f"), paste0(MXval, "c"))
  lMXsd <- agglMXsd

#add D2D3 standardized trade errors
  lMXsd <- merge(lMXsd, lEqMXsd, by = c(Refa), all = TRUE)
  lMXsd[is.na(lMXsd)] <- 0
  lMXsd <- cbind(lMXsd[c(Refa, paste0(TradeFlow, ".sd"))], 
			     lMXsd[c(paste0(MXval, "e"), 
					     paste0(MXval, "p"), 
					     paste0(MXval, "f"), 
					     paste0(MXval, "c"))] + 
                 lMXsd[c("CalDerSD", "PrtDerSD", "FatDerSD", "CarbDerSD")])

return(lMXsd)
}