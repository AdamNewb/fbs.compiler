#' Function to compile FBS supply per capita supply by source
#'
#' This function compiles domestic supply and puts together
#' element-by-element the sources of supply in per capita per day terms 
#' for earlier specified years (sYr, lYr).

#' @keywords supply

#' @param lTS - combined production and trade data set in long format
#' @param lPop  - population in long format.
#' @param Refa  - standard data frame names for FBS output.

#' @export
#' fun.TS()


        fun.TS = function (lTS, 
		                   lPop, 
						   Refa){

                lTS[is.na(lTS)] <- 0
                lTS <- merge(lTS, lPop, by = c("AreaCode", "Year"))

                divX <- 10000
                divMXe <- 10000

                lTSRef.vars <- c(Refa, "Population")
                lTSMXe.vars <- c("Mpe", "Mse", "Me", "Msde", "Xpe", "Xse", "Xe", "Xsde")
                lTSnum.vars <- names(lTS)[sapply(lTS, is.numeric)] 
                lTSx.vars <- lTSnum.vars[!(lTSnum.vars %in% c(lTSRef.vars, lTSMXe.vars))]
                lTS[lTSx.vars] <- lTS[lTSx.vars] / lTS$Population / 1000 / 365 * divX
                lTS[lTSMXe.vars] <- lTS[lTSMXe.vars] / lTS$Population / 1000 / 365 * divMXe

                lTS[lTS$Group == "Oilseeds", "QPe"] <- lTS[lTS$Group == "Oilseeds", "rawQPe"] 
                lTS[lTS$Group == "Oilseeds", "QPp"] <- lTS[lTS$Group == "Oilseeds", "rawQPe"] 
                lTS[lTS$Group == "Oilseeds", "QPf"] <- lTS[lTS$Group == "Oilseeds", "rawQPf"] 
                lTS[lTS$Group == "Oilseeds", "QPc"] <- lTS[lTS$Group == "Oilseeds", "rawQPf"] 


                lTS["Domestic.supply"] <- lTS$Production + lTS$Imports - lTS$Exports 
                lTS["DSe"] <- lTS$QPe + lTS$Me - lTS$Xe 
                lTS["DSp"] <- lTS$QPp + lTS$Mp - lTS$Xp 
                lTS["DSf"] <- lTS$QPf + lTS$Mf - lTS$Xf 
                lTS["DSc"] <- lTS$QPc + lTS$Mc - lTS$Xc 



                lDS <- lTS[, c(Refa, "Domestic.supply", "DSe", "DSp", "DSf", "DSc")]

                perCapTS <- lTS[, c(Refa, "Production", "Imports.primary", "Imports.standardized", 
                                    "Imports", "Exports.primary", "Exports.standardized", "Exports", 
					                "Domestic.supply", "Imports.sd", "Exports.sd")]	
									
                perCapTSe <- lTS[, c(Refa, "QPe", "Mpe", "Mse", "Me", "Xpe", "Xse", "Xe", "DSe", "Msde", "Xsde")]
                perCapTSp <- lTS[, c(Refa, "QPp", "Mpp", "Msp", "Mp", "Xpp", "Xsp", "Xp", "DSp", "Msdp", "Xsdp")]
                perCapTSf <- lTS[, c(Refa, "QPf", "Mpf", "Msf", "Mf", "Xpf", "Xsf", "Xf", "DSf", "Msdf", "Xsdf")]
                perCapTSc <- lTS[, c(Refa, "QPc", "Mpc", "Msc", "Mc", "Xpc", "Xsc", "Xc", "DSc", "Msdc", "Xsdc")]

# calculate weighted unit caloric conversions for per cap KG
               CalConv <- cbind(perCapTSe, perCapTS)
               CalConv["UnitCal"] = CalConv$DSe/CalConv$Domestic.supply
               CalConv <- CalConv[, c(Refa, "DSe", "Domestic.supply", "UnitCal")]

return(list(lDS, perCapTSe, perCapTSp, CalConv))
}