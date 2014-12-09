#' Function to compile total feed energy and protein availability 
#'
#' This function compiles the totality of feed energy and protein availability (supply) to be 
#' subsequently contrasted with modelled requirements (demand). Availabilities are established from
#' both FBS foodstuffs and OCBS non-food, together with UNSD trade.

#' @param mergedOCBS - feed availabilities from OCBS and HS trade in non-food ingredients
#' @param lFEED - feed availabilities from FBS food
#' @param feed - feed requirements from feed model
#' @param lPop - data.frame of population in long format
#' @param AreaCodes - input data.frame of area codes and names

#' @keywords feed
#' @export
#' @examples
#' fun.FPE.EVAIL()

fun.FE.EPVAIL = function (mergedOCBS, 
                         lFEED, 
						 feed,
						 lPop,
						 AreaCodes) 
	{

# get energy and protein supplies from non food
	mergedOCBS <- mergedOCBS[!mergedOCBS$Commodity == "fish.meal", ] #fish meal is added later	 
    feedNonFoode <- mergedOCBS[ , c("AreaCode", "Year", "Commodity", "eEqDS", "Population")]
	feedNonFoodp <- mergedOCBS[ , c("AreaCode", "Year", "Commodity", "pEqDS")]

# establish energy and protein supplies from FBS food
    lFEED <- merge(lFEED, lPop, by = c("AreaCode", "Year"))
    lFEEDe <- lFEED[ , c("AreaCode", "Commodity", "Year", "FEEDe")]
	lFEEDp <- lFEED[ , c("AreaCode", "Commodity", "Year", "FEEDp")]

# reshape (long->wide) to get feed names in columns
    wFeedFoode <- dcast(lFEEDe, AreaCode + Year ~ Commodity, 
                    value.var = "FEEDe", 
					fun.aggregate = sum)
	wFeedFoodp <- dcast(lFEEDp, AreaCode + Year ~ Commodity, 
                    value.var = "FEEDp", 
					fun.aggregate = sum)
    wFeedNonFoode <- dcast(feedNonFoode, AreaCode + Year + Population  ~ Commodity, 
                       value.var = "eEqDS", 
					   fun.aggregate = sum)
    wFeedNonFoodp <- dcast(feedNonFoodp, AreaCode + Year  ~ Commodity, 
                       value.var = "pEqDS", 
					   fun.aggregate = sum)	

# calculate  total FBS avail.
	fbsFEColSum <- cbind(wFeedFoode[, c(1:2)], rowSums(wFeedFoode[, c(3: length(wFeedFoode))]))
	names(fbsFEColSum)[3] <- "FBS.EAvail" 

# calculate total energy and protein availabilities.
    EnergyAvail <- merge(wFeedFoode, wFeedNonFoode, by = c("AreaCode", "Year"))
    ProteinAvail <- merge(wFeedFoodp, wFeedNonFoodp, by = c("AreaCode", "Year"))
	
    x <- c("coconuts,.including.copra.x", "cottonseed.x", "groundnuts.x",
	       "palm.kernels.x", "rape.and.mustard.seed.x",	"soybeans.x",
		   "sunflower.and.safflower.seed.x")
    y <- c("coconuts,.including.copra.y", "cottonseed.y", "groundnuts.y", 
	       "palm.kernels.y", "rape.and.mustard.seed.y",	"soybeans.y", 
		   "sunflower.and.safflower.seed.y")
	
	OS <- EnergyAvail[, c(x)] + EnergyAvail[, c(y)] 
	names(OS) <- gsub(".x", "", names(OS))
	xy <- names(EnergyAvail) %in% c(x, y) 
    EnergyAvail <- EnergyAvail[!xy]
	EnergyAvail <- cbind(EnergyAvail,OS)

	OS <- ProteinAvail[, c(x)] + ProteinAvail[, c(y)] 
	names(OS) <- gsub(".x", "", names(OS))
	xy <- names(ProteinAvail) %in% c(x, y) 
    ProteinAvail <- ProteinAvail[!xy]
	ProteinAvail <- cbind(ProteinAvail,OS)
	

# what does the model say?
    colnames(feed)[colnames(feed) == "AREA"] <- "AreaCode"
    feed$LABAREA <- feed$AreaClass <- NULL
	
# obtain China from model results	
	Chn <- feed[which(feed$AreaCode==41 | feed$AreaCode==96 | 
	                 feed$AreaCode==128 | feed$AreaCode==214), ]
    cYr <- unique(Chn$Year)
    if("351" %in% Chn$AreaCode == FALSE){
	    ChinaFeed = aggregate( x = Chn[, c("EDemand", "PDemand")], 
                          by = list(Chn$Year), 
					      FUN = sum)
       }	
    colnames(ChinaFeed)[colnames(ChinaFeed) == "Group.1"] <- "AreaCode"
    ChinaFeed$AreaCode <- 351
    ChinaFeed$Year <- cYr
    feed <- rbind(feed, ChinaFeed)

# obtain EU from model results	
	euCodes = AreaCodes[AreaCodes$EU28==TRUE, "AreaCode"]
	EU <- feed[which(feed$AreaCode %in% euCodes), ]
    cYr <- unique(EU$Year)
    if("5706" %in% EU$AreaCode == FALSE){
	    EUFeed = aggregate( x = EU[, c("EDemand", "PDemand")], 
                          by = list(EU$Year), 
					      FUN = sum)
       }	
    colnames(EUFeed)[colnames(EUFeed) == "Group.1"] <- "AreaCode"
    EUFeed$AreaCode <- 5706
    EUFeed$Year <- cYr
    feed <- rbind(feed, EUFeed)
	
    feed <- feed[ which(feed$Year >= sYr), ]
    
     
	feed <- feed[, c("AreaCode", "Year", "EDemand", "PDemand")]
    efeed <- feed[, c("AreaCode", "Year", "EDemand")]
	pfeed <- feed[, c("AreaCode", "Year", "PDemand")]

# convert model MJ to KVAL
    EnergyAvail <- merge(EnergyAvail, efeed)
    EnergyAvail$EDemand <- EnergyAvail$EDemand * 0.239005736 / EnergyAvail$Population / 365 * 1000
    EnergyAvail$Population <- NULL
	
    EnergyAvail[is.na(EnergyAvail)] <- 0
    EnergyAvail <- merge(EnergyAvail, AreaCodes[ , c("AreaCode", "AreaName","FBS")], 
	                 by = c("AreaCode"))
    EnergyAvail <- EnergyAvail[EnergyAvail$FBS == TRUE, ]
	EnergyAvail$FBS <- NULL

#compare demand and supply
	EnergyAvail <- EnergyAvail[ , c(length(EnergyAvail), 1:(length(EnergyAvail)-1))]
	l <- (length(EnergyAvail)-1)
    EnergyAvail["EAvail"] <- rowSums(EnergyAvail[ , c(4:l)])
    EnergyAvail["S/D"] <- EnergyAvail$EAvail/EnergyAvail$EDemand
	EnergyAvail <- merge(EnergyAvail, fbsFEColSum)
	EnergyAvail["FBS.mult"] <- EnergyAvail$FBS.EAvail / EnergyAvail$EAvail
	
    ProteinAvail <- merge(ProteinAvail, pfeed)
    ProteinAvail[is.na(ProteinAvail)] <- 0

    ProteinAvail <- merge(ProteinAvail, AreaCodes[ , c("AreaCode", "AreaName", "FBS")], 
                  by = c("AreaCode"))
		  
    ProteinAvail <- ProteinAvail[ProteinAvail$FBS == TRUE, ]
	ProteinAvail$FBS <- NULL
	ProteinAvail <- ProteinAvail[ , c(length(ProteinAvail), 1:(length(ProteinAvail)-1))]
    l <- (length(ProteinAvail) -1 )
    ProteinAvail["PAvail"] <- rowSums(ProteinAvail[ , c(4:l)])
    ProteinAvail["S/D"] <- ProteinAvail$PAvail / ProteinAvail$PDemand

return(list(EnergyAvail,ProteinAvail))
}