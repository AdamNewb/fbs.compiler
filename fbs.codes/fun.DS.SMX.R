#' Function to compile FBS total trade including standardized trade
#'
#' This function compiles total trade including standardized or D1D2D23 trade. First, the
#' simple case of D1 or first-line processed products are established from the balanced UNSD 
#' trade data file. D2D3 products are then added, finally with primary trade to form total trade   

#' @param lSMX - balanced D1 standardized trade
#' @param HScodes - trade data attributes, e.g, primary or standardized, dist. of nutrients
#' @param lPMX - balanced primary trade
#' @param lEqMX - D2D3 standardized trade (primary equivalent)
#' @param TradeFlow - Imports or exports?
#' @param MXval - primary (e.g. Mp) or standardized (e.g. Xs) conditioned on TradeFlow
#' @param Refa - vector of standard meta column names

#' @keywords trade
#' @export
#' fun.DS.SMX()


fun.DS.SMX = function (lSMX,
                       HScodes,
					   lPMX,
					   lEqMX,
					   TradeFlow,
					   MXval,
					   Refa) {
  
#aggregate nutrients to get default unit nutrients
  a <- c("ENERC_KCAL", "Protein", "Lipid_Tot", "CHOAVLDF")
  aggHSdf <- aggregate(x = HScodes[, c(paste0(a, ".x"),paste0(a, ".y"))],
             by = list(Commodity=HScodes$Commodity),
             FUN = mean,
			 na.rm = TRUE) 

#add D2D3 standardized trade 
  lSMX <- merge(lSMX, lEqMX, by = c(Refa), all = TRUE)
  lSMX[is.na(lSMX)] <- 0		

  lSMX <- cbind(lSMX[c(Refa)], 
                lSMX[c(paste0(TradeFlow, ".standardized"), 
			           paste0(MXval, "se"), 
					   paste0(MXval, "sp"), 
					   paste0(MXval, "sf"), 
					   paste0(MXval, "sc"))] + 
                lSMX[c("qd.SHARE", "CalDer", "PrtDer", "FatDer", "CarbDer")])
		
#establish total trade
  lMX <- merge(lPMX, lSMX, by = c(Refa), all = TRUE)
  
#calculate standardized quantities using primary nutrients or default nutrients if primary=NA
  lMX <- merge(lMX, aggHSdf, by = c("Commodity"), all = FALSE)
  lMX[paste0(TradeFlow, ".standardized")] <- with(lMX, 
      ifelse(is.na(lMX[paste0(TradeFlow, ".primary")]), 
			       lMX[paste0(MXval, "se")] / lMX$ENERC_KCAL.x, 
				   lMX[paste0(MXval, "se")] / (lMX[paste0(MXval, "pe")]  / 
				                               lMX[paste0(TradeFlow, ".primary")])
				   
		     )
			 )
				
  lMX[is.na(lMX)] <- 0
  lMX <- cbind(lMX[c(Refa)], 
               lMX[c(paste0(TradeFlow, ".primary"), 
			         paste0(MXval, "pe"), 
					 paste0(MXval, "pp"), 
					 paste0(MXval, "pf"), 
					 paste0(MXval, "pc"))] + 
               lMX[c(paste0(TradeFlow, ".standardized"), 
			         paste0(MXval, "se"), 
					 paste0(MXval, "sp"), 
					 paste0(MXval, "sf"), 
					 paste0(MXval, "sc"))]
			    )
					 
  colnames(lMX)[colnames(lMX) == paste0(TradeFlow, ".primary")] <- TradeFlow
  colnames(lMX)[colnames(lMX) == paste0(MXval, "pe")] <- paste0(MXval, "e")
  colnames(lMX)[colnames(lMX) == paste0(MXval, "pp")] <- paste0(MXval, "p")
  colnames(lMX)[colnames(lMX) == paste0(MXval, "pf")] <- paste0(MXval, "f")
  colnames(lMX)[colnames(lMX) == paste0(MXval, "pc")] <- paste0(MXval, "c")

return(lMX)
}