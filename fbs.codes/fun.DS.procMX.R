#' Function to compile FBS trade
#'
#' This function combines trade quantity information into single files, one for imports and 
#' one for exports from annual files compiled previously under function "fun.DS.setMX". 
#' The returned files are the basis from which all trade in food is calculated.
#'
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param val - file suffix indicating the degree of validation (fully validated and balanced is
#'              the default setting for FBS)
#' @param FBSpath - the location of the processed trade files
#' @param fileM - data.frame of food imports previously compiled
#' @param fileX - data.frame of food exports previously compiled
#' @param euM - data.frame of extra-eu food imports previously compiled
#' @param euX - data.frame of extra-eu food exports previously compiled

#' @keywords trade
#' @export
#' @examples
#' fun.DS.procMX()


fun.DS.procMX = function (sYr, lYr,
						  val,
						  FBSpath,
						  fileM,
						  fileX,
						  euM,
						  euX) {
			
		
#create a null data.frame that will be populated with imports and r-binded
        a <- data.frame(rtCode = numeric(), ItemCode = character(), NW_M = numeric(), 
                        TV_M = numeric(), UV_M = numeric(), Year = character(), 
						stringsAsFactors = FALSE) 
  
#read-in annual import files
        for (i in sYr:lYr) {
            fileMALL <- paste0(i, fileM)
			fileMeu <- paste0(i, euM)
            b <- read.csv(paste0(paste0(FBSpath, "Data/MX/"), fileMALL), 
			              header = TRUE, sep = ",")
#likewise for EU
			c <- read.csv(paste0(paste0(FBSpath, "Data/MX/"), fileMeu), 
			              header = TRUE, sep = ",")
            Year <- i
            b["Year"] <- Year
			c["Year"] <- Year
			c["rtCode"] <- 5706 #EU FBS code

			a <- rbind(a, b)
			a <- rbind(a, c)
			
        }

#reshape data into wide format for later processing
        Allx <- dcast(a, 
                      rtCode + ItemCode ~ Year, 
			          value.var = "NW_M", 
			          fun.aggregate = sum)
        MALLx <- Allx

#create a null data.frame that will be populated with exports and r-binded
        a <- data.frame(ptCode = numeric(), ItemCode = character(), NW_X = numeric(), 
                        TV_X = numeric(), UV_X = numeric(), Year = character(), 
						stringsAsFactors = FALSE) 

#read-in annual export files
        for (i in sYr:lYr) {
             fileXALL <- paste0(i, fileX)
			 fileXeu <- paste0(i, euX)
             b <- read.csv(paste0(paste0(FBSpath, "Data/MX/"), fileXALL), 
			               header = TRUE, sep = ",")
#likewise for EU
			 c <- read.csv(paste0(paste0(FBSpath, "Data/MX/"), fileXeu), 
			              header = TRUE, sep = ",")
             Year <- i
             b["Year"] <- Year
			 c["Year"] <- Year
			 c["ptCode"] <- 5706 #EU FBS code
             a <- rbind(a, b)
			 a <- rbind(a, c)
        }
 
#reshape data into wide format for later processing.
         Allx <- dcast(a,
                       ptCode + ItemCode ~ Year, 
			           value.var = "NW_X", 
			           fun.aggregate = sum)
         XALLx <- Allx
 
return(list(MALLx, XALLx))
}