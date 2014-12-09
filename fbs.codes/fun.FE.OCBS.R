#' Function to compile feed availability from on-food and food (fish).
#'
#' This function imports OCBS data from EST and then calculates non-food and food (fish).
#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param OCBSpath - path where OCBS csv files are located
#' @param OCBSmap - file by which to map OCBS to FBS items
#' @param IMX - balanced import data from UNSD
#' @param EXX - balanced export data from UNSD
#' @param HSm - relevant FBS trade codes from UNSD and attributes
#' @param AreaCodes - input data.frame of area codes and names
#' @param QPdf - data.frame of FBS production
#' @param PrimaryAreaCodes - data.frame of nutrient and commodity descriptor codes by country/year 
#' @param feedUNSD - external file of feed commodities and nutrients

#' @keywords feed
#' @export
#' @examples
#' fun.FE.OCBS()

fun.FE.OCBS = function(sYr, lYr,
               OCBSpath,
			   OCBSmap,
			   IMx,
			   EXx,
			   HSm,
			   AreaCodes,
			   QPdf,
			   OSxr,
			   PrimaryAreaCodes,
			   feedUNSD){
			   
setwd(OCBSpath)

# get OCBS data and relevant info and merge into a data.frame
ocbsFiles <- list.files(pattern = "*.csv")
ocbsData <- do.call("rbind", 
					lapply(ocbsFiles, function(x) read.csv(x, stringsAsFactors = FALSE)))

ocbsEle <- c("Production [000 tonnes]", "Production Crush Rate [ratio]", "Protein Equiv. [ratio]",
              "Meal Extraction Rate (Dom.) [ratio]", "Meal Extraction Rate (Trade) [ratio]")
			  
Refa <- c("country.codes", "countries", "items", "item.codes", "elements", "ele.codes")
Ref <- c("AreaCode", "Commodity", "Year")

ocbsData <- ocbsData[ocbsData$elements %in% ocbsEle == TRUE, c(Refa, paste0("X", sYr:lYr))]
ocbsData[, c(paste0("X", sYr:lYr))] <- sapply(ocbsData[, c(paste0("X", sYr:lYr))], as.numeric)

# aggregate for China as OCBS does not have
 if("351" %in% ocbsData$country.codes == FALSE){
    ChnMeta <- ocbsData[ocbsData$country.codes == 41, Refa]
    a <- which(colnames(ocbsData) == paste0("X", sYr))
    b <- which(colnames(ocbsData) == paste0("X", lYr))
    ChnData <- ocbsData[ocbsData$country.codes == 41, c(a:b)]  + 
	           ocbsData[ocbsData$country.codes == 214, c(a:b)] +
               ocbsData[ocbsData$country.codes == 96, c(a:b)] + 
			   ocbsData[ocbsData$country.codes == 128, c(a:b)]
    ChnMeta$country.codes <- "351"
    ChnMeta$countries <- "CHINA"
    Chn <- cbind(ChnMeta, ChnData)
    ocbsData <- rbind(ocbsData, Chn)
 }

mergedOCBS <- merge(ocbsData, OCBSmap, by = c("items"), all = FALSE)
RemoveOCBS <- c("items", "item.codes.x", "item.codes.y", "ele.codes")
mergedOCBS <- mergedOCBS[ , !(names(mergedOCBS) %in% RemoveOCBS)]

lOCBS <- melt(mergedOCBS, id.vars = c("country.codes", "countries", "Commodity", "elements"),
              variable.name = "Year")
lOCBS$value <- as.numeric(as.character(lOCBS$value))
lOCBS$Year <- substr(lOCBS$Year,2,5)

colnames(lOCBS)[colnames(lOCBS) == "country.codes"] <- "AreaCode"
colnames(lOCBS)[colnames(lOCBS) == "countries"] <- "AreaName"
castOCBS <- dcast(lOCBS, AreaCode + AreaName + Commodity + Year ~ elements, 
                  value.var = "value")	

## get production of OS feedstuffs and aggregate to FBS feed production
# QPocbs <- c("soybeans", "cottonseed", "rape.and.mustard.seed", "groundnuts",
            # "sunflower.and.safflower.seed", "palm.kernels", "coconuts,.including.copra",
			# "oilcrops,.other")

QPdf <- unique(merge(QPdf, unique(PrimaryAreaCodes[ , c("Commodity", "ItemCode")]), 
                     by = c("ItemCode"), all = FALSE))
QPdf$ItemCode <- QPdf$ElementCode <- NULL
QPdf <- QPdf[QPdf$Commodity %in% OSxr == TRUE,]
aggQPdf <- aggregate(x = QPdf[ , c("Value")], 
                     by = list(QPdf$AreaCode, QPdf$Commodity, QPdf$Year), 
					 FUN = sum)
colnames(aggQPdf) <- c(Ref, "Production")

## likewise for trade
colnames(HSm)[colnames(HSm) == "UNSD.HS.CODE"] <- "ItemCode"
HSm$ItemCode <- paste0(substr(HSm$ItemCode, 1, 5), 
                       substr(HSm$ItemCode, 7, 8))
#imports
lIM <- melt(IMx, 
            id.vars=c("rtCode", "ItemCode"), 
			value.name="Imports",
			variable.name="Year")
lIMdf <- merge(lIM, HSm, by = c("ItemCode"), all = FALSE)
colnames(AreaCodes)[2] = "rtCode"
lIMdf <- merge(lIMdf, AreaCodes, by = c("rtCode"), all = FALSE)
lIMdf <- lIMdf[lIMdf$Commodity %in% OSxr == TRUE, c(Ref, "Imports")]
aggIMdf <- aggregate(x = lIMdf[ , c("Imports")], 
by = list(lIMdf$AreaCode, lIMdf$Commodity, lIMdf$Year), FUN = sum)
colnames(aggIMdf) <- c(Ref, "Imports")

#exports
lEX <- melt(EXx, 
            id.vars = c("ptCode", "ItemCode"), 
			value.name = "Exports",
			variable.name = "Year")

lEXdf <- merge(lEX, HSm, by = c("ItemCode"), all = FALSE)
colnames(AreaCodes)[2] = "ptCode"
lEXdf <- merge(lEXdf, AreaCodes, by = c("ptCode"), all = FALSE)
lEXdf <- lEXdf[lEXdf$Commodity %in% OSxr == TRUE, c(Ref, "Exports")]
aggEXdf <- aggregate(x = lEXdf[ , c("Exports")], 
by = list(lEXdf$AreaCode, lEXdf$Commodity, lEXdf$Year), FUN = sum)
colnames(aggEXdf) <- c(Ref, "Exports")

# merge production and trade and get nutrients
mergedOCBS <- merge(castOCBS, aggQPdf, by = c("AreaCode", "Commodity", "Year"), all = FALSE)
mergedOCBS <- merge(mergedOCBS, aggIMdf, by = c("AreaCode", "Commodity", "Year"), all = FALSE)
mergedOCBS <- merge(mergedOCBS, aggEXdf, by = c("AreaCode", "Commodity", "Year"), all = FALSE)
feedUNSD <- feedUNSD[feedUNSD$Commodity %in% OSxr == TRUE, c("Commodity", "kcal.100g")]
mergedOCBS <- merge(mergedOCBS,feedUNSD)

colnames(mergedOCBS)[colnames(mergedOCBS) == "Meal Extraction Rate (Dom.) [ratio]"] <- "mExtRD"
colnames(mergedOCBS)[colnames(mergedOCBS) == "Meal Extraction Rate (Trade) [ratio]"] <- "mExtRT"
colnames(mergedOCBS)[colnames(mergedOCBS) == "Production Crush Rate [ratio]"] <- "PCR"
colnames(mergedOCBS)[colnames(mergedOCBS) == "Protein Equiv. [ratio]"] <- "pEqR"
mergedOCBS["pEqQP"] <- mergedOCBS$mExtRD * mergedOCBS$PCR * mergedOCBS$pEqR * mergedOCBS$Production
mergedOCBS["pEqIM"] <- mergedOCBS$mExtRT * mergedOCBS$pEqR * mergedOCBS$Imports / 1000 
mergedOCBS["pEqEX"] <- mergedOCBS$mExtRT * mergedOCBS$pEqR * mergedOCBS$Exports / 1000
mergedOCBS["eEqQP"] <- mergedOCBS$kcal.100g * mergedOCBS$Production
mergedOCBS["eEqIM"] <- mergedOCBS$kcal.100g * mergedOCBS$Imports / 1000
mergedOCBS["eEqEX"] <- mergedOCBS$kcal.100g * mergedOCBS$Exports / 1000

return(mergedOCBS)
}
