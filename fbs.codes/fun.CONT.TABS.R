#' Function to compile contingency tables
#'
#' This function compiles contingency tables - at the commodity and group levels. It is used
#' in the first stage to arrive at a complete balance with supply and utilization. Only the 
#' KCAL format of the FBS is balanced

#' @param perCapTSe - data.frame of total supply by type on a per capita KCAL basis
#' @param perCapTUe - data.frame of total utilization by type on a per capita KCAL basis
#' @param AreaCodes - input data.frame of area codes and names

#' @keywords balance

#' @export
#' @examples
#' fun.CONT.TABS()

fun.CONT.TABS = function (perCapTSe,
                          perCapTUe,
						  AreasCodes) 
						  {

        RefArea <- c("AreaName")
        RefComm <- c("AreaCode", "Group", "Commodity", "Year")
        RefGroup <- c("AreaCode", "Group", "Year")
        RefBAL <- c("Production", "Imports.primary", "Imports.standardized", "Imports.total",
                  "Exports.primary", "Exports.standardized", "Exports.total",
                  "Domestic.supply", "Imports.sd", "Exports.sd", "Industrial.use",    
                  "Losses", "Seed.use", "Feed.use", "Food.use", "Stock.change")

# construct balance at the commodity level
        BAL <- merge(perCapTSe, perCapTUe, 
		             by = c("AreaCode", "Group", "Commodity", "Year"), 
					 all = TRUE)				 
        BAL[is.na(BAL)] <- 0
		BAL <- merge(BAL, AreaCodes[ , c("AreaCode", "AreaName", "FBS")], 
		             by = c("AreaCode"))
        BAL <-BAL[BAL$FBS == TRUE, ]
        BAL$FBS <- NULL
        BAL <- BAL[ , c(length(BAL), 1:(length(BAL)-1))]
        colnames(BAL) <- c(RefArea, RefComm, RefBAL)
        BAL$Commodity <- gsub(",", ".", BAL$Commodity)
        BAL$AreaName <- gsub(",", ".", BAL$AreaName)

# aggregate to get the group level balance
        Group <- aggregate(x = BAL[6:length(BAL)], 
                  by = list(BAL$AreaName, BAL$AreaCode, BAL$Group, BAL$Year), 
				  FUN = sum)

        colnames(Group) <- c(RefArea, RefGroup, RefBAL)

return(list(BAL, Group))
}