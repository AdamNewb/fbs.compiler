#' Function to construct and compile modelled loss rates
#'
#' This function compiles modelled loss rates and constructs them where missing, from group
#' information. E.g. where model does not provide national loss rates for cassava, it will apply
#' the rates from the group "root.crops"

#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param Refa - standard column references for output
#' @param losses - long format output file from losses model
#' @param lQP - data.frame of production compiled earlier 
#' @param PrimaryAreaCodes - data.frame of nutrient and commodity descriptor codes by country/year 

#' @keywords losses
#' @export
#' @examples
#' fun.DU.LOSS()

          fun.DU.LOSS = function (sYr, lYr,
                                  Refa,
                                  losses,
	                              lQP,
	                              PrimaryAreaCodes){

          losses <- losses[ ,c("itemname", "ItemCode", "areaname",	"areacode",	"predicted_loss_rate")]
          losses$areaname <- NULL
          colnames(losses)[colnames(losses) == "areacode"] <- "AreaCode"
	   
#replace "China, mainland (41)" with missing "China (351)" as China, mainland will dominate 
#loss rates for all of China
	      if("351" %in% losses$AreaCode == FALSE){
           losses[losses$AreaCode == 41, "AreaCode" ] <- 351
          }
     
#default commodities where QP exists
          lQP[is.na(lQP)] <- 0
	   
          lQP <- merge(unique(lQP[, c(Refa,"Production")]), 
                       unique(PrimaryAreaCodes[, c("AreaCode", "Year", "Group", 
					                                "Commodity", "Extraction", 
			                                        "ENERC_KCAL", "Protein", 
													"Lipid_Tot", "CHOCDF")]), 
			           by = c("Group", "Commodity", "AreaCode", "Year"), 
			          all = FALSE)
			  
          agglQP <-  aggregate(lQP[, c("Production", "Extraction", "ENERC_KCAL", 
		                               "Protein", "Lipid_Tot", "CHOCDF")],
                               by = lQP[, c(Refa)], 
					          FUN = "mean")
			
          LOSSdf <- merge(losses, unique(PrimaryAreaCodes[, c("AreaCode", "Year", "ItemCode", 
		                                                      "Group", "Commodity")]), 
                                  by = c("ItemCode", "AreaCode"), 
				                 all = TRUE)
   
          aggLOSSdf <- aggregate(LOSSdf[, c("predicted_loss_rate")],
                                  by = LOSSdf[, c("Group", "AreaCode", "Commodity", "Year")], 
					             FUN = "mean")
	   
	      LOSSdfx <- merge(aggLOSSdf,agglQP,
				          by = c("AreaCode", "Group", "Commodity", "Year"), 
				         all = TRUE)
	      colnames(LOSSdfx)[colnames(LOSSdfx) == "x"] <- "predicted_loss_rate"

          lQPAggregated.df <- aggregate(x = lQP[,c("Extraction", "ENERC_KCAL", "Protein", 
		                                           "Lipid_Tot", "CHOCDF")],
					      by = list(Group=lQP$Group, Commodity=lQP$Commodity), 
					     FUN = mean)				
       
          LOSSdfx <- merge(LOSSdfx,lQPAggregated.df, all = TRUE)


##identify missing commodities
          missingLoss <- which(is.na(LOSSdfx$predicted_loss_rate) & !(is.na(LOSSdfx$Production)))
          complete.df <- LOSSdfx[-missingLoss, ]
          complete.df$AreaCode.1 <- NULL
          missing.df <- LOSSdfx[missingLoss, ]

#aggregate to get group-level loss rates by country
          aggregated.df <- aggregate(complete.df[, c("Extraction", "ENERC_KCAL", "Protein", 
	                                              "Lipid_Tot", "CHOCDF", "predicted_loss_rate")],
                           by = complete.df[, c("Group", "AreaCode")], 
						   FUN = "mean") 

#apply to missing commodities
          imputed.df <- merge(missing.df[,c(Refa,"Production", "Extraction", "ENERC_KCAL", 
	                                           "Protein", "Lipid_Tot", "CHOCDF")],
		                  aggregated.df[,c("AreaCode", "Group", "predicted_loss_rate")],
					by = c("Group", "AreaCode"), all = TRUE)

          lLOSS <- rbind(complete.df, imputed.df)
          lLOSS <- lLOSS[complete.cases(lLOSS), ]
          lLOSS["LOSS"]  <- lLOSS$Production * lLOSS$predicted_loss_rate / 100
          lLOSS["LOSSe"] <- lLOSS$Production * lLOSS$ENERC_KCAL * lLOSS$predicted_loss_rate / 100
          lLOSS["LOSSp"] <- lLOSS$Production * lLOSS$Protein / 100 * lLOSS$predicted_loss_rate / 100
          lLOSS["LOSSf"] <- lLOSS$Production * lLOSS$Lipid_Tot / 100 * lLOSS$predicted_loss_rate / 100
          lLOSS["LOSSc"] <- lLOSS$Production * lLOSS$CHOCDF / 100 * lLOSS$predicted_loss_rate / 100
          lLOSS <- lLOSS[, c(Refa, "LOSS", "LOSSe", "LOSSp", "LOSSf", "LOSSc")]
	
return(lLOSS)	
}