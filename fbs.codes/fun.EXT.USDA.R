#' Function to compile USDA data from downloaded flat file
#'
#' This function compiles USDA data from a flat file located at:
#' http://apps.fas.usda.gov/psdonline/psdDownload.aspx ("all commodities" file). In 
#' particular, oilseed crushing rates are obtained as well as oilseeds as food.
#'
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param USDAdata - flat file of downloaded USDA data
#' @param USDAmap - mapping USDA data to new FBS commodities / groups
#' @param USDAele - input data.frame of elements to be used
#' @param AreaCodes - input data.frame of area codes and names
#'
#' @keywords external data
#' @export
#' @examples
#' fun.EXT.USDA()

fun.EXT.USDA = function (sYr, lYr,
                         USDAdata,
					     USDAmap,
						 USDAele,
					     AreaCodes)
	   {

#map USDA commodities to FBS commodity list, use values from 2000 and extract only relevant fields					 
        USDAdata <- merge(USDAdata, USDAmap, 
                          by.x = c("Commodity_Description"), 
				          by.y = c("cm_description"))
	    USDAdata <- USDAdata[USDAdata$FBS == TRUE, ]
        lUSDAdata <- USDAdata[USDAdata$Market_Year >= 2000, 
		                    c("Commodity_Description", "Country_Code", "Country_Name", 
							  "Market_Year", "Attribute_Description", "Unit_Description", "Value")]

#extract relevent elements
        lUSDAdata <- lUSDAdata[lUSDAdata$Attribute_Description %in% USDA.ele == TRUE, ] #USDAele$

#reshape data
        wUSDA <- dcast(lUSDAdata, 
		               Country_Code + Country_Name + Market_Year + Commodity_Description + Unit_Description ~ 
					   Attribute_Description, value.var = "Value")

#merge with FBS parameters to get FBS areas and commodities
	    wUSDA <- merge(wUSDA, AreaCodes[, c("AreaCode", "AreaName", "cn_name")],
                       by.x = c("Country_Name"), 
				       by.y = c("cn_name"),
                       all = FALSE)
        wUSDA <- merge(wUSDA, USDAmap[, c("cm_description", "Commodity", "Group")],
                       by.x = c("Commodity_Description"), 
				       by.y = c("cm_description"),
                       all = FALSE)

#calculate crushing rates for oilseeds, and oilseeds as food					   
	    wUSDA["Crush.R"] <- 0
        wUSDA[wUSDA$Group == "Oilseeds", "Crush.R"] <- 
		                    wUSDA[wUSDA$Group == "Oilseeds", "Crush"] /
                            wUSDA[wUSDA$Group == "Oilseeds", "Domestic Consumption"]

        wUSDA["FOODe"] <- wUSDA["Food Use Dom. Cons."] * 1000 
		
return(wUSDA)
}