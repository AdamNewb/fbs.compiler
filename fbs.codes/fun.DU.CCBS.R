#' Function to get cereal food from the CCBS database of EST
#'
#' This function imports CCBS data in the form of FCL area coded csv files
#' and then consolidates into a dataframe of cereal food availabilities. These quantities
#' inform FBS cereal food. Feed is also collected for reference.

#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param XCBSpath - path where CCBS csv files are located
#' @param XCBSmap - file by which to map CCBS to FBS cereals
#' @param XCBSele = vector of CCBS elements
#' @param XCBSelex = vector of CCBS element reference of relevance,
#' @param XCBSref = vector of CCBS reference elements
#' @param XCBSid = vector of CCBS reference IDs
#' @param XCBSitems = CCBS item to extract
#' @param XCBSareacode = CCBS area codes to extract
#' @param XCBSareaname = CCBS area names to extract

#' @keywords CCBS, food
#' @export
#' @examples
#' fun.DU.CCBS()

fun.DU.CCBS = function(sYr, lYr,
                       XCBSpath,
			           XCBSmap,
			           XCBSele,
			           XCBSelex,
			           XCBSref,
			           XCBSid,
			           XCBSitems,
			           XCBSareacode,
			           XCBSareaname){ 
			   
                setwd(XCBSpath)
                XCBSfiles <- list.files(pattern = "*.csv")
                XCBSdata <- do.call("rbind", 
				lapply(XCBSfiles, function(x) read.csv(x, stringsAsFactors = FALSE)))
                XCBSYears <- NULL
				# EU <- read.csv("C:/Users/prakash/Dropbox/CPC-FBS/Compile/inData/5706.csv", header = TRUE, sep = ",")
			    # print(str(XCBSdata))
                # print(str(EU))				

                for(i in sYr:lYr){
                  XCBSYears <- c(XCBSYears, paste0("X", i))
                }
                
				XCBSdata <- XCBSdata[XCBSdata$ELEM.DES %in% XCBSele == TRUE, 
				                     c(XCBSref, XCBSYears)]
	

                if(351 %in% XCBSdata$COUN.CODE == FALSE ){
                  ChnMeta <- XCBSdata[XCBSdata$COUN.CODE == 41, c(1:4)]
                  a <- which(colnames(XCBSdata) == paste0("X", sYr))
                  b <- which(colnames(XCBSdata) == paste0("X", lYr))
                  ChnData <- XCBSdata[XCBSdata$COUN.CODE == 41, c(a:b)] + 
				             XCBSdata[XCBSdata$COUN.CODE == 214, c(a:b)] +
                             XCBSdata[XCBSdata$COUN.CODE == 96, c(a:b)] + 
			                 XCBSdata[XCBSdata$COUN.CODE == 128, c(a:b)]

                  ChnMeta["COUN.CODE"] <- "351"
                  ChnMeta["COUN.DES"] <- "China"
                  Chn <- cbind(ChnMeta, ChnData)
                  XCBSdata <- rbind(XCBSdata, Chn)
                 }
 
                 mergedXCBS <- merge(XCBSdata, XCBSmap, 
                               by = c(XCBSitems), all = FALSE)
                 mergedXCBS <- mergedXCBS[c(-1)]		

                 lXCBS <- melt(mergedXCBS, id.vars = c(XCBSid),
                               variable.name = "Year")
			  
                 lXCBS$value <- as.numeric(as.character(lXCBS$value))
                 lXCBS$Year <- substr(lXCBS$Year,2,5)
                 lXCBS$Year <- as.integer(as.numeric(lXCBS$Year))
                 colnames(lXCBS)[colnames(lXCBS) == XCBSareacode] <- "AreaCode"
                 colnames(lXCBS)[colnames(lXCBS) == XCBSareaname] <- "AreaName"

                 castXCBS <- dcast(lXCBS, AreaCode + AreaName + Commodity + Year ~ ELEM.DES, 
                                   value.var = "value",
				                   fun.aggregate  =  mean)	

                 XCBS.DU <- castXCBS


return(XCBS.DU)
}
