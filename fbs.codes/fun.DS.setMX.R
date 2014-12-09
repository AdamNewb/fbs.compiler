#' Function to prepare FBS trade for processing
#'
#' This function reads the output of "API-eleXMLcvc-med-ver x.x.R" - a set of validated
#' and balanced trade files by HS commodity and by year for all importing and exporting 
#' M49 countries - and then consolidates them into single files by year and by trade flow. 
#' The output is written to file as processing time is exceptionally long, so that the user
#' has the option to read the output files or generate from scratch as below. The function
#' also allows for assessing data quality - subsetting mirrored data, which were not reported by
#' countries.
#'
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param UNSDlist - the list of validated and balanced trade files by HS commodity and by year 
#' @param UNSDpath - the location of the HS trade files
#' @param val - file suffix indicating the degree of validation (fully validated and balanced,
#'              i.e, "relVal" is the default value for FBS)
#' @param FBSpath - the location of the processed trade files
#' @param euCodes - a vector of member EU states to account for EU intra trade 
#'                 (which we exclude in EU-FBS)

#' @keywords trade
#' @export
#' @examples
#' fun.DS.setMX()

fun.DS.setMX = function (sYr, lYr,
                         HSm,
                         UNSDpath,
						 val,
						 FBSpath,
						 AreaCodes) {
  
  HSm <- as.data.frame(HSm[, c("UNSD.HS.CODE")])
  
#loop through years
  for (i in sYr:lYr) {
#create a null data.frame that will be populated and rbinded
      eu <- a <- data.frame(Year = character(), ItemCode = character(), rtCode = numeric(), 
	                  ptCode = numeric(), qtCode = numeric(), TV_M = numeric(), TV_X = numeric(),
					  NW_M = numeric(), NW_X = numeric(), UV_M = numeric(), UV_X = numeric(), 
					  metaData = character, stringsAsFactors = FALSE) 
      yr <- i
	  
#read-in HS trade files and bind
        for(i in 1:length(HSm[[1]])){
          csvUNSD <- as.character(HSm[i, 1]) 
          csvUNSD <- paste(substr(csvUNSD, 2, 5), substr(csvUNSD, 7, 8), sep = "")
          csvUNSD <- paste(substr(csvUNSD, 1, 6), yr, sep = ".")
          csvUNSD <- paste(paste(csvUNSD, val, sep = ""), ".csv", sep = "")
          setwd(UNSDpath)
            if(file.exists(csvUNSD)){
              print(csvUNSD)
              b <- read.csv(csvUNSD, header = TRUE, sep = ",")
              c <- csvUNSD
              Comm <- paste("x", c, sep = "")
              Comm <- substr(Comm, 0, 7)
              b["ItemCode"] <- Comm
              b <- b[c("Year", "ItemCode", "rtCode", "ptCode", "qtCode", 
			           "TV_M", "TV_X", "NW_M", "NW_X", "UV_M", "UV_X", "metaData")]
              a <- rbind(a, b)
#subset eu intra-trade data
             if(yr < 1995){euCodes = AreaCodes[AreaCodes$EU12==TRUE, "rtCode"] 
             } else if(yr >= 1995 & yr < 2005) {
               euCodes = AreaCodes[AreaCodes$EU15==TRUE, "rtCode"] 
             } else if(yr >= 2005 & yr < 2007) {
               euCodes = AreaCodes[AreaCodes$EU25==TRUE, "rtCode"] 
             } else if(yr >= 2005 & yr < 2013) {
               euCodes = AreaCodes[AreaCodes$EU27==TRUE, "rtCode"]
             } else {
               euCodes = AreaCodes[AreaCodes$EU28==TRUE, "rtCode"]
             }
  
			  ieu <- b[b$rtCode %in% euCodes | b$ptCode %in% euCodes, ]
			  ieu <- ieu[ieu$rtCode & ieu$ptCode %in% euCodes == FALSE, ]
			  eu <- rbind(eu, ieu)
            }
        }

setwd(paste0(FBSpath, "/Data/MX/"))

# aggregate by reporter/partner, calculate trade unit values and write to file. The same for
# DQ - establishing how much data are mirrored
	  tDQ <- a[a$metaData == "TX_Mmedian" | 
	           a$metaData == "TM_Xmedian" | 
		       a$metaData == "mirrored export for non-reporter" | 
			   a$metaData == "mirrored import for non-reporter", ]
			   
# likewise for EU
	  tDQeu <- eu[eu$metaData == "TX_Mmedian" | 
	           eu$metaData == "TM_Xmedian" | 
		       eu$metaData == "mirrored export for non-reporter" | 
			   eu$metaData == "mirrored import for non-reporter", ]
			   
			   
      M <- aggregate(cbind(NW_M, TV_M) ~ rtCode + ItemCode, data = a, FUN = sum)
	  Meu <- aggregate(cbind(NW_M, TV_M) ~ rtCode + ItemCode, data = eu, FUN = sum)
	  Mdq <- aggregate(cbind(NW_M, TV_M) ~ rtCode + ItemCode, data = tDQ, FUN = sum)
	  Mdqeu <- aggregate(cbind(NW_M, TV_M) ~ rtCode + ItemCode, data = tDQeu, FUN = sum)
      M$UV_M <- M$TV_M / M$NW_M * 1000
	  Mdq$UV_M <- Mdq$TV_M / Mdq$NW_M * 1000
	  Mdqeu$UV_M <- Mdqeu$TV_M / Mdqeu$NW_M * 1000
      write.csv(M, file = paste0(yr, "allM.csv"), row.names = FALSE)
	  write.csv(Mdq, file = paste0(yr, "allMdq.csv"), row.names = FALSE)
	  write.csv(Meu, file = paste0(yr, "allMeu.csv"), row.names = FALSE)
	  write.csv(Mdqeu, file = paste0(yr, "euMdq.csv"), row.names = FALSE)
	  
      X <- aggregate(cbind(NW_X, TV_X) ~ ptCode + ItemCode, data = a, FUN = sum)
	  Xeu <- aggregate(cbind(NW_X, TV_X) ~ ptCode + ItemCode, data = eu, FUN = sum)
	  Xdq <- aggregate(cbind(NW_X, TV_X) ~ ptCode + ItemCode, data = tDQ, FUN = sum)
	  Xdqeu <- aggregate(cbind(NW_X, TV_X) ~ ptCode + ItemCode, data = tDQeu, FUN = sum)
      X$UV_X <- X$TV_X / X$NW_X * 1000
	  Xdq$UV_X <- Xdq$TV_X / Xdq$NW_X * 1000
	  Xdqeu$UV_X <- Xdqeu$TV_X / Xdqeu$NW_X * 1000
      write.csv(X, file = paste0(yr, "allX.csv"), row.names = FALSE)
	  write.csv(Xdq, file = paste0(yr, "allXdq.csv"), row.names = FALSE)
	  write.csv(Xeu, file = paste0(yr, "allXeu.csv"), row.names = FALSE)
	  write.csv(Xdqeu, file = paste0(yr, "euXdq.csv"), row.names = FALSE)
      
# aggregate by item (eu, world totals), calculate trade unit values and write to file 
      Meu<- aggregate(cbind(NW_M, TV_M) ~ ItemCode, data = eu, FUN = sum)
      Meu$UV_M <- Meu$TV_M / Meu$NW_M * 1000
      write.csv(Meu, file = paste0(yr, "euM.csv"), row.names = FALSE)
      Xeu <- aggregate(cbind(NW_X, TV_X) ~ ItemCode, data = eu, FUN = sum)
      Xeu$UV_X <- Xeu$TV_X / Xeu$NW_X * 1000
      write.csv(Xeu, file = paste0(yr, "euX.csv"), row.names = FALSE)
	  
	  M <- aggregate(cbind(NW_M, TV_M) ~ ItemCode, data = a, FUN = sum)
      M$UV_M <- M$TV_M / M$NW_M * 1000
      write.csv(M, file = paste0(yr, "wldM.csv"), row.names = FALSE)
      X <- aggregate(cbind(NW_X, TV_X) ~ ItemCode, data = a, FUN = sum)
      X$UV_X <- X$TV_X / X$NW_X * 1000
      write.csv(X, file = paste0(yr, "wldX.csv"), row.names = FALSE)


    }

}