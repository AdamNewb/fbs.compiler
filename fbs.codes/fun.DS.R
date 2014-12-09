#' Function to compile FBS supply, both primary and total
#'
#' This function compiles total supply and primary domestic supply data (production and net 
#' primary trade, i.e. no standardized processed products) necessary for the calculation of feed 
#' availability. The logic is we don't feed animals with imported bread but we do with imported 
#' wheat grain.
#' @param lTS - data.frame of combined production and trade data set in long format
#' @param Refa - vector of standard meta column names

#' @keywords supply
#' @export
#' fun.DS()


        fun.DS = function (lTS, Refa) {

# establish primary and non primary data by columns		
           lTS[is.na(lTS)] <- 0		   
           lDSMXCols <- c("Me", "Mpe", "Mse", 
                          "Xe", "Xpe", "Xse")
 
           lDSCols <- c("rawQPe", "rawQPp", "QPe", "QPp", "FDRDe",	"FDRDp",
                        "Mp","Mpp", "Msp", "Xp", "Xpp", "Xsp",
                        "MpFDRDe", "MpFDRDp", "XpFDRDe", "XpFDRDp")

           lDSprimary <- lTS[, c(Refa, lDSMXCols, lDSCols)]
           lDSprimary[lDSMXCols] <- lDSprimary[lDSMXCols] 
           lDSprimary["DSe"] <- 0
           lDSprimary["DSp"] <- 0

# calculate primary domestic supply for non-oilseeds   
           lDSprimary[!(lDSprimary$Group == "Oilseeds"), "DSe"] <- 
		                       lDSprimary[!(lDSprimary$Group == "Oilseeds"), "QPe"] +
                               lDSprimary[!(lDSprimary$Group == "Oilseeds"), "Mpe"] - 
							   lDSprimary[!(lDSprimary$Group == "Oilseeds"), "Xpe"]				   
           lDSprimary[!(lDSprimary$Group == "Oilseeds"), "DSp"] <- 
                               lDSprimary[!(lDSprimary$Group == "Oilseeds"), "QPp"] +
                               lDSprimary[!(lDSprimary$Group == "Oilseeds"), "Mpp"] - 
							   lDSprimary[!(lDSprimary$Group == "Oilseeds"), "Xpp"]

# calculate primary domestic supply for oilseeds, i.e. using "raw" FBS production... 
# ...(inclusive of non edible crushed oilseeds)
           lDSprimary[lDSprimary$Group == "Oilseeds" , "DSe"] <- 
                               lDSprimary[lDSprimary$Group == "Oilseeds", "rawQPe"] +
                               lDSprimary[lDSprimary$Group == "Oilseeds", "Mpe"] - 
				               lDSprimary[lDSprimary$Group == "Oilseeds", "Xpe"]
           lDSprimary[lDSprimary$Group == "Oilseeds", "DSp"] <- 
                               lDSprimary[lDSprimary$Group == "Oilseeds", "rawQPp"] +
                               lDSprimary[lDSprimary$Group == "Oilseeds", "Mpp"] - 
			                   lDSprimary[lDSprimary$Group == "Oilseeds", "Xpp"]

           lDS <- lTS[, c(Refa, lDSMXCols, lDSCols)]
           lDS[lDSMXCols] <- lDS[lDSMXCols] 
           lDS["DSe"] <- 0
           lDS["DSp"] <- 0
		   
# calculate domestic supply for non-oilseeds   
           lDS[!(lDS$Group == "Oilseeds"), "DSe"] <- lDS[!(lDS$Group == "Oilseeds"), "QPe"] +
                                                     lDS[!(lDS$Group == "Oilseeds"), "Me"] - 
													 lDS[!(lDS$Group == "Oilseeds"), "Xe"]
           lDS[!(lDS$Group == "Oilseeds"), "DSp"] <- lDS[!(lDS$Group == "Oilseeds"), "QPp"] +
                                                     lDS[!(lDS$Group == "Oilseeds"), "Mp"] - 
													 lDS[!(lDS$Group == "Oilseeds"), "Xp"]
													 
# calculate domestic supply for oilseeds, i.e. using "raw" FBS production... 
# ...(inclusive of non edible crushed oilseeds)
           lDS[lDS$Group == "Oilseeds" , "DSe"] <- 
                                  lDS[lDS$Group == "Oilseeds" , "rawQPe"] +
                                  lDS[lDS$Group == "Oilseeds" , "Me"] - 
					              lDS[lDS$Group == "Oilseeds" , "Xe"]
           lDS[lDS$Group == "Oilseeds" , "DSp"] <- 
                                  lDS[lDS$Group == "Oilseeds" , "rawQPp"] +
                                  lDS[lDS$Group == "Oilseeds" , "Mp"] - 
				                  lDS[lDS$Group == "Oilseeds" , "Xp"]
								  
           lDS["DSFDe"] <- lDS$FDRDe + lDS$MpFDRDe - lDS$XpFDRDe
           lDS["DSFDp"] <- lDS$FDRDp + lDS$MpFDRDp - lDS$XpFDRDp
           lDS$FDRDe<-lDS$MpFDRDe<-lDS$XpFDRDe<-lDS$FDRDp<-lDS$MpFDRDp<-lDS$XpFDRDp <- NULL
   
   return(list(lDSprimary, lDS))
}