#' Function to update "PrimaryAreaCodes" with oilseed crushing rates from USDA
#'
#' This function updates "PrimaryAreaCodes" with oilseed crushing rates from USDA. 
#' The USDA has extensive global outreach As USDA does 
#' not report member states, EU-wide crushing rates are assumed
#' @param EXT.USDA.dat - data.frame of previously processed external USDA data
#' @param AreaCodes - input data.frame of areas to get EU area attributes
#' @param OSxr - a vector of oilseed commodity names subject to crushing
#' @param PrimaryAreaCodes - input data.frame of nutrient and commodity descriptor codes by country/year 
#'
#' @keywords external data, oilseeds
#' @export
#' @examples
#' fun.EXT.OSXR()

fun.EXT.OSXR = function(EXT.USDA.dat,
                        AreaCodes,
						OSxr,
						PrimaryAreaCodes)
		{ 

         colnames(EXT.USDA.dat)[colnames(EXT.USDA.dat) == "Market_Year"] <- "Year"	   
         USDA.xr <- EXT.USDA.dat[EXT.USDA.dat$Group == "Oilseeds" , 
                                 c("AreaCode", "Commodity", "Year", "Crush.R")]

EUareas <- AreaCodes[AreaCodes$EU28==TRUE, c("AreaCode", "AreaName")]
								 
#copy EU group Crush rates to all member countries
        EUdef <- USDA.xr[USDA.xr$AreaCode == "452", ]
        EUdef <- merge(EUdef[-c(1)], EUareas)
        EUdef$AreaName <- NULL
        USDA.xr <- rbind(USDA.xr, EUdef)

        USDA.xr$Crush <- 1 - USDA.xr$Crush.R
        USDA.xr$Crush.R <- NULL

        OSPrimaryAreaCodes <- PrimaryAreaCodes[PrimaryAreaCodes$Commodity %in% OSxr == TRUE, ] 
        PrimaryAreaCodes <- PrimaryAreaCodes[!(PrimaryAreaCodes$Commodity %in% OSxr == TRUE), ] 
        OSPrimaryAreaCodes$Crush <- NULL

        USDA.xr <- merge(USDA.xr, OSPrimaryAreaCodes, 
		           by = c("Commodity", "AreaCode", "Year"), 
				   ALL = FALSE)

				   			
        PrimaryAreaCodes <- rbind(PrimaryAreaCodes, USDA.xr)

	  
return(PrimaryAreaCodes)
}
