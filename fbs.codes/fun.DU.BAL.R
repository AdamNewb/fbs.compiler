#' Function to establish residual in FBS 
#'
#' This function establishes the residual - "stock changes" by default - but the balancing of 
#' feed supply against modelled demand is done in a later step
#' @param lFEED.AVL - data.frame of feed availabilities
#' @param lFOOD.AVL - data.frame of food availabilities

#' @keywords residual 
#' @export
#' @examples
#' fun.DU.BAL()


fun.DU.BAL = function (lFEED.AVL,
                       lFOOD.AVL) {

# bind feed and protein energy, and calculate per capita nutrients.
            lFOOD.AVL["FEEDe"] <- 0
            lFOOD.AVL["FEEDp"] <- 0
            lTU <- rbind(lFEED.AVL, lFOOD.AVL)

		    lTUeCols <- c("QPe", "DSe", "INDe", "LOSSe", "SEEDe", "FEEDe", "FOODe")
            lTUpCols <- c("QPp", "DSp", "INDp", "LOSSp", "SEEDp", "FEEDp", "FOODp")
		    lTU[lTUeCols] <- lTU[lTUeCols] / lTU$Population / 1000 / 365 * 10000
		    lTU[lTUpCols] <- lTU[lTUpCols] / lTU$Population / 1000 / 365 * 10000
		    lTU[is.na(lTU)] <- 0

# for commodities with zero loss apply a 3 percent loss rate
            lTU[lTU$QPe > 0 & lTU$LOSSe == 0, "LOSSe"] <- lTU[lTU$QPe > 0 & lTU$LOSSe == 0, "QPe"] * 0.03
            lTU[lTU$QPp > 0 & lTU$LOSSp == 0, "LOSSp"] <- lTU[lTU$QPp > 0 & lTU$LOSSp == 0, "QPp"] * 0.03
		    lTU$QPe <- lTU$QPp <- NULL

# for negative food set to zero
	        negFOODe <- lTU$FOODe < 0 
            lTU[negFOODe,"FOODe"] <- 0
            lTU[negFOODe,"FOODp"] <- 0
		
# calculate residual
		    lTU["STOCK.VARe"] <- (lTU$DSe - lTU$INDe - lTU$LOSSe - lTU$SEEDe - lTU$FEEDe - lTU$FOODe)
		    lTU["STOCK.VARp"] <- (lTU$DSp - lTU$INDp - lTU$LOSSp - lTU$SEEDp - lTU$FEEDp - lTU$FOODp)

            perCapTUe <- lTU[, c(Refa,"INDe", "LOSSe", "SEEDe", "FEEDe", "FOODe", "STOCK.VARe")]
            perCapTUp <- lTU[, c(Refa,"INDp", "LOSSp", "SEEDp", "FEEDp", "FOODp", "STOCK.VARp")]


return(list(perCapTUe, perCapTUp))
}