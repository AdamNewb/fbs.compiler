#' Function to compile non-food feed trade
#'
#' This function compiles non-food feed trade from balanced HS trade data. The list of 
#' feed HS items is predefined and so too the nutrients. The returned data.frame converts
#' feed trade into energy and protein equivalents 
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param AreaCodes - input data.frame of area codes and names
#' @param feedUNSD - list of non-food ingredients in HS trade together with nutrients

#' @keywords feed
#' @export
#' @examples
#' fun.FE.FEMX()

fun.FE.FEMX = function (sYr,lYr,
                        AreaCodes,
                        feedUNSD){

    feedList <- feedUNSD$HSCODE
    feedName <- feedUNSD[c("HSCODE", "HSNAME")]
    feedCal <- feedUNSD[c("HSCODE", "kcal.100g")]
    feedProt <- feedUNSD[c("HSCODE", "prot.pc")]
    feedComm <- feedUNSD[c("HSCODE", "Commodity")]

# blank data.frames for r-binding
    a <- data.frame(Year = numeric(), HSCODE = character(), rtCode = character(), NW_M = numeric(), 
                stringsAsFactors = FALSE)
    c <- data.frame(Year = numeric(), HSCODE = character(), ptCode = character(), NW_X = numeric(), 
                stringsAsFactors = FALSE) 
		
    M <- m <- data.frame( HSCODE = character(), Year = numeric(), rtCode = character(), NW_M = numeric(), 	
                HSNAME = character(), kcal.100g = numeric(), prot.pc = numeric(),	
			    TOT.ENERGY.M = numeric(), TOT.PROTEIN.M = numeric(), 
			    stringsAsFactors = FALSE) 
    X <- x <- data.frame("HSCODE" = character(), Year = numeric(), ptCode = character(), NW_X = numeric(), 	
                HSNAME = character(), kcal.100g = numeric(), prot.pc = numeric(),	
			    TOT.ENERGY.X = numeric(), TOT.PROTEIN.X = numeric(), 
			    stringsAsFactors = FALSE) 

# loop through years

#read and bind imports
    for(i in sYr:lYr) {
        yr <- i
        allM <- read.csv(paste0("Data/MX/", paste0(yr, "allM.csv")), header = TRUE, sep = ",")
	    allM$ItemCode <- as.character(substr(allM$ItemCode, 2, 7))
	    allM <- allM[allM$ItemCode %in% feedList == TRUE, c("rtCode", "ItemCode", "NW_M")]
	    colnames(allM)[colnames(allM) == "ItemCode"] <- "HSCODE"
	    allM["Year"] <- 0
	    allM$Year <- yr
	    a <- rbind(a, allM)
		
#read and bind exports
	    allX <- read.csv(paste0("Data/MX/", paste0(yr, "allX.csv")), header = TRUE, sep = ",")
	    allX$ItemCode <- as.character(substr(allX$ItemCode, 2, 7))
	    allX <- allX[allX$ItemCode %in% feedList == TRUE, c("ptCode", "ItemCode", "NW_X")]
	    colnames(allX)[colnames(allX) == "ItemCode"] <- "HSCODE"
	    allX["Year"] <- 0
	    allX$Year <- yr
	    c <- rbind(c, allX)	
    }


    merge.all <- function(by, ...) {
    frames <- list(...)
    return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
    } 

# merge imports and rescale	
    m <- merge.all(by = c("HSCODE"), 
                 a, feedName, feedCal, feedProt, feedComm)
			 
    m["TOT.ENERGY.M"] <- m$NW_M * m$kcal.100g / 1000 #kilos to tonnes
    m["TOT.PROTEIN.M"] <- m$NW_M * m$prot.pc / 1000 #kilos to tonnes
    M <- rbind(M, m)

# merge exports and rescale
    x <- merge.all(by = c("HSCODE"), 
                 c, feedName, feedCal, feedProt, feedComm)
    x["TOT.ENERGY.X"] <- x$NW_X * x$kcal.100g / 1000 #kilos to tonnes
    x["TOT.PROTEIN.X"] <- x$NW_X * x$prot.pc / 1000 #kilos to tonnes
    X <- rbind(X, x)
 
    colnames(X)[colnames(X) == "ptCode"] <- "rtCode"
    M <- merge(M, AreaCodes)
    X <- merge(X, AreaCodes)

# rename cols
    M["IMe"] <- (M$TOT.ENERGY.M)
    X["EXe"] <- (X$TOT.ENERGY.X)
    M["IMp"] <- (M$TOT.PROTEIN.M)
    X["EXp"] <- (X$TOT.PROTEIN.X)

    M <- M[ , c("Year", "AreaCode", "Commodity", "IMe", "IMp")]
    X <- X[ , c("Year", "AreaCode", "Commodity", "EXe", "EXp")]
    namesM <- colnames(M)
    namesX <- colnames(X)

# aggregate to FBS 
    aggM <- aggregate(x = M[ , c("IMe", "IMp")], 
                  by = list(M$Year, M$AreaCode, M$Commodity), 
				  FUN = sum)
    aggX <- aggregate(x = X[ , c("EXe", "EXp")], 
                  by = list(X$Year, X$AreaCode, X$Commodity), 
				  FUN = sum)
				  
    colnames(aggM) <- namesM
    colnames(aggX) <- namesX

# merge imports and exports to a single data object
    EPmx <- merge(aggM, aggX, 
              by = c("AreaCode", "Year", "Commodity"), 
			  all = TRUE)
    EPmx[is.na(EPmx)] <- 0
  
  return(EPmx)
}

