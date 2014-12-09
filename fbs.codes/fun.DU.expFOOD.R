#' Function to compile expected food means
#'
#' This function compiles expected food means from a number of sources. Cereals are the average
#' CCBS (EST) and old FBS. USDA are used for oilseeds and vegetable oils. All other foodstuffs
#' are from old FBS. For past non-reported countries, e.g. Burundi, food is equated with domestic
#' supply. A large data.frame consisting of availabilities for feed is returned.  
#'
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param fclFOOD - data.frame of old FBS food quantities
#' @param FCLmap - mapping fclFOOD to new FBS commodities / groups
#' @param CCBSmap - mapping CCBS cereal food data to new FBS commodities / groups
#' @param USDAmap - mapping USDA data to new FBS commodities / groups
#' @param CCBSdat - data.frame of CCBS data
#' @param USDAdat - data.frame of USDA data
#' @param lDS - data.frame of earlier compiled domestic supply data
#' @param lIND - data.frame of earlier compiled industrial use data
#' @param lLOSS - data.frame of earlier compiled losses  data
#' @param lSEED - data.frame of earlier compiled seed use data
#' @param lPop - data.frame of population
#' @param PrimaryAreaCodes - input data.frame of nutrient and commodity descriptor codes by country/year 
#' @param AreaCodes - input data.frame of area codes and names
#' @param FBSpath - path to data
#'
#' @keywords food availability
#' @export
#' @examples
#' fun.DU.expFOOD()

fun.DU.expFOOD = function (sYr, lYr,
                           fclFOOD,
						   FCLmap,
						   CCBSmap,
						   USDAmap,
						   CCBSdat,
						   USDAdat,
						   lDS,
						   lIND, 
						   lLOSS, 
						   lSEED,
						   lPop,
	                       PrimaryAreaCodes,
						   AreaCodes,
						   FBSpath){
	 
# compile old FBS food quantities and remap to new FBS commodity list

	 fclCols <- c("AreaCode", "ItemName", "Year", "Value")
     fclFOOD <- fclFOOD[which(fclFOOD$Year >= sYr), c(fclCols)]
     colnames(fclFOOD)[colnames(fclFOOD) == "Value"] <- "FOOD"
     fclFOOD$FOOD <- fclFOOD$FOOD * 1000
	 mergedFOOD <- merge(fclFOOD, FCLmap)
     aggFOOD <- aggregate(x = mergedFOOD[ ,c("FOOD")],
				by = list(mergedFOOD$AreaCode, mergedFOOD$Year, mergedFOOD$Commodity), FUN = sum)
     colnames(aggFOOD) <- c("AreaCode", "Year", "Commodity", "FOOD")

# extract relevant external food items
	 CCBScomm <- CCBSmap[, c("Commodity")]
	 USDAcomm <- USDAmap[USDAmap$FBS == TRUE, c("Commodity")]

     extCOMM <- c(as.character(CCBScomm), as.character(USDAcomm))
	 CCBS.FD.dat <- CCBSdat[, c("AreaCode", "Year", "Commodity", "Food")]
	 colnames(CCBS.FD.dat)[colnames(CCBS.FD.dat) == "Food"] <- "FOOD"
	 
	 CCBS.FD.dat <- merge(CCBS.FD.dat, aggFOOD, 
	                      by = c("AreaCode", "Year", "Commodity"),
						  all = FALSE)
						  
#calculate average cereal food adjusting for mean-distorting zeros
	 CCBS.FD.dat[CCBS.FD.dat$FOOD.x == 0, "FOOD.x"] <- CCBS.FD.dat[CCBS.FD.dat$FOOD.x == 0, "FOOD.y"]
	 CCBS.FD.dat[CCBS.FD.dat$FOOD.y == 0, "FOOD.y"] <- CCBS.FD.dat[CCBS.FD.dat$FOOD.y == 0, "FOOD.x"]
	 CCBS.FD.dat["Ave.FD"] <- rowMeans(subset(CCBS.FD.dat, 
	                                          select = c(FOOD.x, FOOD.y)), 
	                                          na.rm = FALSE)
	 colnames(CCBS.FD.dat)[colnames(CCBS.FD.dat) == "Ave.FD"] <- "FOOD"	
	 CCBS.FD.dat$FOOD.x <- CCBS.FD.dat$FOOD.y <- NULL

	 
	 CCBSzero <- which(CCBS.FD.dat$FOOD == 0)
     CCBS.FD.dat <- CCBS.FD.dat[-CCBSzero, ]
	 
	 CCBS.FE.dat <- CCBSdat[, c("AreaCode", "Year", "Commodity", "Feed")]
	 colnames(CCBS.FE.dat)[colnames(CCBS.FE.dat) == "Feed"] <- "FEED"
	 CCBS.FE.dat["Group"] <- "Cereals"
	 CCBSzero <- which(CCBS.FE.dat$FEED == 0)
     CCBS.FE.dat <- CCBS.FE.dat[-CCBSzero, ]
   
	 USDAdat <- USDAdat[, c("AreaCode", "Market_Year", "Commodity", "Food Use Dom. Cons.")] 
	 names(USDAdat)<-names(CCBS.FD.dat)
	 USDAdat$FOOD <- USDAdat$FOOD * 1000
     extFOOD <- rbind(CCBS.FD.dat, USDAdat)

# add missing external food
	 CCBSarea <- CCBS.FD.dat$AreaCode
	 USDAarea <- USDAdat$AreaCode
	 extAREA <- c(as.character(CCBSarea), as.character(USDAarea))
	 extAREA <- unique(extAREA)

     faoFOOD.x <- aggFOOD[aggFOOD$Commodity %in% extCOMM &
	           !(aggFOOD$AreaCode %in% extAREA), ]
	 faoFOOD.y <- aggFOOD[!(aggFOOD$Commodity %in% extCOMM), ]

	 faoFOOD <- rbind(faoFOOD.x, faoFOOD.y)

	 aggFOOD.df <- rbind(extFOOD, faoFOOD)

# get nutrients and aggregate disaggregated food items
     aggregatedFOOD.df <- merge(aggFOOD.df, 
	            unique(PrimaryAreaCodes[,c("AreaCode", "Year", "Group", "Commodity", "Extraction", "ENERC_KCAL",
	            "Protein", "Lipid_Tot", "CHOCDF")]), 
	            by = c("AreaCode", "Year", "Commodity"), all = FALSE)

     aggregatedFOOD.df <- aggregate(x = aggregatedFOOD.df[, c("FOOD", "Extraction", "ENERC_KCAL", 
                                                              "Protein", "Lipid_Tot","CHOCDF")],
				          by = list(AreaCode=aggregatedFOOD.df$AreaCode, 
					                Year=aggregatedFOOD.df$Year, 
				                    Group=aggregatedFOOD.df$Group, 
							        Commodity=aggregatedFOOD.df$Commodity), 
			              FUN = mean)	

     merge.all <- function(by, ...) {
      frames <- list(...)
      return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
     } 
   
	 lAVL <- merge.all(by = c("AreaCode", "Commodity", "Group", "Year"), 
	                          lDS, lIND, lLOSS, lSEED, CCBS.FE.dat)
     lAVL <- merge(lAVL, aggregatedFOOD.df, all = TRUE)
	 

     lAVL[is.na(lAVL)] <- 0
     lAVL <- merge(lAVL, lPop, 
	         by = c("AreaCode", "Year"))

     lAVL$FOODp <- lAVL$FOOD * lAVL$Protein / 100 
     lAVL$FOODe <- lAVL$FOOD * lAVL$ENERC_KCAL 
	 
	 lAVL$FEEDp <- lAVL$FEED * lAVL$Protein / 100 
     lAVL$FEEDe <- lAVL$FEED * lAVL$ENERC_KCAL 
	 
	 lAVL$FEEDe <- lAVL$DSFDe + lAVL$FEEDe	
     lAVL$FEEDp <- lAVL$DSFDp + lAVL$FEEDp	

# set food to domestic supply for missing FBS countries
	 notReport <- setdiff(AreaCodes$AreaCode, unique(faoFOOD$AreaCode))
	 nonReport <- AreaCodes[AreaCodes$AreaCode %in% notReport, ]
     nonReport <- nonReport[nonReport$FBS == TRUE, "AreaCode"]
	 
	 lAVL[lAVL$AreaCode %in% nonReport & lAVL$FOODe == 0, "FOODe"] <- 
	         lAVL[lAVL$AreaCode %in% nonReport & lAVL$FOODe == 0, "DSe"]
	 lAVL[lAVL$AreaCode %in% nonReport & lAVL$FOODp == 0, "FOODp"] <- 
	         lAVL[lAVL$AreaCode %in% nonReport & lAVL$FOODp == 0, "DSp"]


return(list(extCOMM, lAVL))
}