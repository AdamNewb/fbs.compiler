#' Function to compile feed.
#'
#' This function compiles feed from a vector of animal-feed ingredients after accounting for 
#' all other uses from the level of supply. An assumption is that stock changes are zero

#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param start year
#' @param Refa - standard column references for output
#' @param lAVL - data frame of availabilities of feed after accounting for other uses
#' @param FeedIngredList - vector of feed ingredient names

#' @keywords feed use
#' @export
#' @examples
#' fun.DU.FEED()

fun.DU.FEED = function (sYr, lYr,
                        Refa,
						lAVL,
                        FeedIngred){

# get the list of possible FBS(food) feed ingredients						
      FeedIngredList <- unique(FeedIngred)
      lFA <- which(lAVL$Commodity %in% FeedIngredList == TRUE) 
      lFEED.AVL <- lAVL[lFA, ]
      lNFA <- which(!(lAVL$Commodity %in% FeedIngredList == TRUE))
      lFOOD.AVL <- lAVL[lNFA, ]
      lFOOD.AVL$FEEDe <- lFOOD.AVL$FEEDp <- NULL
	  
	  divDU <- 10000
	  condFEEDe <- lFEED.AVL$QPe > 0 & lFEED.AVL$FEEDe == 0
	  condFEEDp <- lFEED.AVL$QPp > 0 & lFEED.AVL$FEEDp == 0

# calculate the FBS feed availabilities after accounting for all other uses - energy and proteins.
  
	  lFEED.AVL[condFEEDe, "FEEDe"] <- lFEED.AVL[condFEEDe, "DSe"] - 
	                                           lFEED.AVL[condFEEDe, "INDe"] - 
											   lFEED.AVL[condFEEDe, "LOSSe"] - 
	                                           lFEED.AVL[condFEEDe, "SEEDe"] - 
											   lFEED.AVL[condFEEDe, "FOODe"]
	  lFEED.AVL[condFEEDp, "FEEDp"] <- lFEED.AVL[condFEEDp, "DSp"] - 
	                                           lFEED.AVL[condFEEDp, "INDp"] - 
											   lFEED.AVL[condFEEDp, "LOSSp"] - 
	                                           lFEED.AVL[condFEEDp, "SEEDp"] - 
											   lFEED.AVL[condFEEDp, "FOODp"]

# in cases of negative availability, assume zero feed.
  
	  negFEEDe <- lFEED.AVL$FEEDe < 0
	  negFEEDp <- lFEED.AVL$FEEDp < 0
	  lFEED.AVL[negFEEDe,"FEEDe"] <- 0
	  lFEED.AVL[negFEEDp,"FEEDp"] <- 0

      lFEED <- lFEED.AVL[, c(Refa, "FEEDe", "FEEDp", "Population")]
      lFEED$FEEDe <- lFEED$FEEDe / lFEED$Population / 1000 / 365 * divDU
      lFEED$Population <- NULL

return(list(lFOOD.AVL, lFEED.AVL, lFEED))
}