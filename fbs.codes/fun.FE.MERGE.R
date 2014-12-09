#' Function to establish domestic supply of non-food feed ingredients 
#'
#' This function combines production and trade of non-food feed ingredients in protein and 
#' energy equivalence, and then by per capita
#' @param mergedOCBS - production of non-food feed ingredients
#' @param EPmx - trade in non-food feed ingredients
#' @param lPop - data.frame of population in long format

#' @keywords feed
#' @export
#' @examples
#' fun.FE.MERGE()


fun.FE.MERGE = function (mergedOCBS, 
                         EPmx, 
						 lPop) 
	{

# merge qp and mx of energy and protein feed and calculate domestic supply, DS

    mergedOCBS <- merge(mergedOCBS, EPmx, by = c("AreaCode", "Year", "Commodity"), all = TRUE)
    mergedOCBS$AreaName <- NULL
    mergedOCBS[is.na(mergedOCBS)] <- 0

    mergedOCBS["pEqDS"] <- mergedOCBS$pEqQP + mergedOCBS$pEqIM - 
                       mergedOCBS$pEqEX + mergedOCBS$IMp - mergedOCBS$EXp
    mergedOCBS["eEqDS"] <- mergedOCBS$eEqQP + mergedOCBS$eEqIM - 
                       mergedOCBS$eEqEX + mergedOCBS$IMe - mergedOCBS$EXe
	
#if negative DS, then ignore trade

    mergedOCBS["pAvg"] <- 0
    mergedOCBS[mergedOCBS$pEqDS > 0, "pAvg"] <- ave(mergedOCBS[mergedOCBS$pEqDS > 0, "pEqDS"], 
               mergedOCBS[mergedOCBS$pEqDS > 0, "AreaCode"],
			   mergedOCBS[mergedOCBS$pEqDS > 0, "Commodity"])
    mergedOCBS["eAvg"] <- 0
    mergedOCBS[mergedOCBS$eEqDS > 0, "eAvg"] <- ave(mergedOCBS[mergedOCBS$eEqDS > 0, "eEqDS"], 
               mergedOCBS[mergedOCBS$eEqDS > 0, "AreaCode"],
			   mergedOCBS[mergedOCBS$eEqDS > 0, "Commodity"])
					  
    mergedOCBS[mergedOCBS$pEqDS < 0, "pEqDS"] <- mergedOCBS[mergedOCBS$pEqDS < 0, "pAvg"]
    mergedOCBS[mergedOCBS$eEqDS < 0, "eEqDS"] <- mergedOCBS[mergedOCBS$eEqDS < 0, "eAvg"]

# return by per capita
    mergedOCBS <- merge(mergedOCBS, lPop, by = c("AreaCode", "Year"))
    mergedOCBS$eEqDS <- mergedOCBS$eEqDS / mergedOCBS$Population / 1000 / 365 * 10000 # kcal/cap/day
    
	mergedOCBS <- do.call(data.frame, 
                     lapply(mergedOCBS, 
					 function(x) replace(x, is.infinite(x),NA)))


return(mergedOCBS)
} 
