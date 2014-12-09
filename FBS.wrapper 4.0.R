## PACKAGE BUILDER ----------------------------------------------------
library("devtools")
library(roxygen2)
setwd("C:/Users/prakash/Dropbox/workspace/r.packages/")
if(file.exists("./fbs.compile"))
    unlink("fbs.compile", recursive = TRUE)
create("fbs.compile")
file.copy(list.files("fbs.codes/", "\\.R$", full.names = TRUE), "./fbs.compile/R/")
setwd("./fbs.compile")
document()
setwd("..")
# install("fbs.compile")
# install_github('fbs.compile','AdamNewb')				
												 
##### SET KEY PARAMETERS OF SCRIPT --------------------------------------------------------
sYr <- 2007 # start year
lYr <- 2012 # end year
QP = FALSE # connect to QP server (need network cable)? otherwise use saved copy
MX = FALSE # lengthy trade processing? otherwise use saved copy
val <- " relVal"   # select raw (raw) or validated (basicVal, fullVal) or 
                   # balanced trade (relVal), which should be default for FBS
				   
## COMPILE FBS ----------------------------------------------------
Sys.time()
library(plyr)
library(reshape2)
library(XML)

## READ CORE INPUT PARAMETERS/DATA----------------------------------------------------
#paths
D2D3path <- "C:/Users/prakash/Dropbox/workspace/java/x2fbs/war/ds/UNSD/D2D3/"
UNSDpath <- "C:/Users/prakash/Dropbox/workspace/java/x2fbs/war/ds/UNSD/"
FBSpath <- "C:/Users/prakash/Dropbox/CPC-FBS/Compile/"
OCBSpath <- "C:/Users/prakash/Dropbox/CPC-FBS/Compile/Data/ocbs/"
CCBSpath <- "C:/Users/prakash/Dropbox/CPC-FBS/Compile/Data/ccbs/"

setwd(FBSpath)

# pure data files
seed <- read.csv("Data/seed.csv", header = TRUE, sep = ",")
bio <- read.csv("Data/bio.csv",header = TRUE,sep = ",")
feed <- read.csv("Data/feed.csv", header = TRUE, sep = ",")
losses <- read.csv("Data/losses.csv", header = TRUE, sep = ",")
USDAdata <- read.csv("Data/USDA_alldata.csv", header = TRUE, sep = ",")
feedRanges <- read.csv("Data/feedRanges.csv", header = TRUE, sep = ",")
fishDF <- read.csv("Data/fish.csv", header = TRUE, sep = ",")
lPop <- read.csv("Data/lPop.csv", header = TRUE, sep = ",")
fclFOOD <- read.csv("Data/fclFOOD.csv", header = TRUE, sep = ",")

# parameter lists
AreaCodes <- read.csv("Params/AreaCodes.csv", header = TRUE, sep = ",")
PrimaryAreaCodes <- read.csv("Params/PrimaryAreaCodes.csv", header = TRUE, sep = ",")
USDAmap <- read.csv("Params/USDA.commodities.csv", header = TRUE,sep = ",")
feedUNSD <- read.csv("Params/feedUNSD.csv", header = TRUE, sep = ",")
FCLmap <- read.csv("Params/FCLmap.csv", header = TRUE, sep = ",")
SeedGroup <- read.csv("Params/seedGroup.csv", header = TRUE, sep = ",")
AllSeed <- read.csv("Params/AllSeed.csv", header = TRUE, sep = ",")
OCBSmap <- read.csv("Params/OCBSmap.csv", header=TRUE,sep = ",")
CCBSmap <- read.csv("Params/CCBSmap.csv", header=TRUE,sep = ",")
HSm <- read.csv("Params/HSm.csv", header = TRUE, sep = ",")
D2D3s <- read.csv("Params/D2D3s.csv", header = TRUE, sep = ",")
RawD2D3codes <- "Params/RawD2D3codes.csv"
meHS <- read.csv("Params/meHS.csv", header = TRUE, sep = ",")
sdHS <- read.csv("Params/sdHS.csv", header = TRUE, sep = ",")

# lists/vectors, read and assign names to vec objects
FBSvec.lists <- read.csv("FBSvec.lists.csv", header = TRUE, sep = ",")
for(i in 1:length(FBSvec.lists)) assign(names(FBSvec.lists)[i], FBSvec.lists[[i]])
Refa <- c("AreaCode", "Group", "Commodity", "Year") #standard column references for output

##### SET DOMESTIC SUPPLY DATA ------------------------------------------------------------

## production
if(QP == TRUE){
   DS.setQP.dat <- fun.DS.setQP(pathDriver = 
                      "C:/Users/prakash/Dropbox/workspace/java/libraries/sqljdbc4-3.0.jar")
   write.csv(DS.setQP.dat[[1]], file = paste0(FBSpath,"Data/AllProd.csv"), row.names = FALSE)
   write.csv(DS.setQP.dat[[2]], file = paste0(FBSpath,"Data/AllArea.csv"), row.names = FALSE)
   QPdf <- DS.setQP.dat[[1]]
   AHdf <- DS.setQP.dat[[2]]
} else {
   QPdf <- read.csv("Data/AllProd.csv", header = TRUE, sep = ",")
   AHdf <- read.csv("Data/AllArea.csv", header = TRUE, sep = ",")
}                  

## trade
if(MX == TRUE){
   DS.setMX <- fun.DS.setMX (sYr, lYr,
                             HSm,
                             UNSDpath,
						     val,
						     FBSpath,
							 AreaCodes)
}
#process trade for FBS
DS.procMX <- fun.DS.procMX (sYr, lYr,
						    val,
						    FBSpath,
						    fileM = "allM.csv",
						    fileX = "allX.csv",
							euM = "euM.csv",
							euX = "euX.csv")
IMx <- DS.procMX[[1]]
EXx <- DS.procMX[[2]]

#process trade for DQ

DS.procMX <-fun.DS.procMX (sYr, lYr,
						   val,
						   FBSpath,
						   fileM = "allMdq.csv",
						   fileX = "allXdq.csv",						
						   euM = "euMdq.csv",
						   euX = "euXdq.csv")
IMxdq <- DS.procMX[[1]]
EXxdq <- DS.procMX[[2]]


##### PRODUCTION --------------------------------------------------------------------------
## get USDA oilseed crush rates
EXT.USDA.dat = fun.EXT.USDA (sYr, lYr,
                             USDAdata,
					         USDAmap,
						     USDAele = FBSvec.lists$USDA.ele,
					         AreaCodes)
						  
## update USDA oilseed extraction rates in the country-specific TCF file
Primary.OSXR.dat = fun.EXT.OSXR (EXT.USDA.dat,
                                 AreaCodes,
							     OSxr = FBSvec.lists$OSxr,
						         PrimaryAreaCodes)
	
setwd(FBSpath)	
write.csv(Primary.OSXR.dat, file = "Params/PrimaryAreaCodes.csv", row.names = FALSE)								 
PrimaryAreaCodes <- read.csv("Params/PrimaryAreaCodes.csv", header = TRUE, sep = ",")

DS.QP.dat <- fun.DS.QP (sYr, lYr,
                        QPdf,
				        fishDF,
				        AreaCodes,
				        PrimaryAreaCodes=Primary.OSXR.dat)

#####TRADE--------------------------------------------------------------------------------------
## nutrients for trade
DS.MX.NUT.dat <- fun.DS.MX.NUT(HSm,
                               meHS,
					           sdHS)

## primary imports			   
DS.PM.dat <- fun.DS.MX (IMx, 
                        TradeFlow = "Imports",
						TradeCode = "rtCode",
					    MXval = "Mp",
						DQ = FALSE,
					    MXdesc = ".primary",
					    MXsubval = "P",
                        HScodes = DS.MX.NUT.dat, 
					    AreaCodes)


## primary domestic availability
DS.DA.dat <- fun.DS.DA (lQP = DS.QP.dat[[1]],
					    lPM = DS.PM.dat)

## data quality of trade
DQ.MX.dat <- fun.DS.MX (IMxdq, 
                        TradeFlow = "Imports",
						TradeCode = "rtCode",
					    MXval = "M",
						DQ = TRUE,
					    MXdesc = "",
					    MXsubval = "",
                        HScodes = DS.MX.NUT.dat, 
					    AreaCodes)
								  
#####STANDARDIZATION ----------------------------------------------------------------------------
DS.D2D3.dat <- fun.DS.D2D3(D2D3s,
                           sdHS,
						   meHS,
						   UNSDpath,
						   D2D3path,
						   AreaCodes,
						   lDA = DS.DA.dat,
						   FBSpath,
						   RawD2D3codes,
						   val = val)

lEqM <- DS.D2D3.dat[[1]]
lEqX <- DS.D2D3.dat[[2]]
lEqMsd <- DS.D2D3.dat[[3]]
lEqXsd <- DS.D2D3.dat[[4]]

# standardized imports		   
DS.SM.dat <- fun.DS.MX (IMx, 
                        TradeFlow = "Imports",
						TradeCode = "rtCode",
					    MXval = "Ms",
						DQ = FALSE,
					    MXdesc = ".standardized",
					    MXsubval = "D1",
                        HScodes = DS.MX.NUT.dat, 
					    AreaCodes)
						
						
# total imports
DS.M.dat <-	fun.DS.SMX (lSMX=DS.SM.dat,
                        HScodes=DS.MX.NUT.dat,
					    lPMX = DS.PM.dat,
					    lEqM,
						TradeFlow = "Imports",
						MXval = "M",
					    Refa)					


		
# error on standardized imports
DS.SMsd.dat <- fun.DS.SMXsd(IMx, 
                            TradeFlow = "Imports",
							TradeCode = "rtCode",
							lEqMsd,
					        MXval = "Msd",
					        MXsubval = "D1",
                            HScodes = DS.MX.NUT.dat, 
							AreaCodes,
					        Refa)						
				  
## exports
#primary exports			   
DS.PX.dat <- fun.DS.MX (EXx, 
                        TradeFlow = "Exports",
						TradeCode = "ptCode",
					    MXval = "Xp",
						DQ = FALSE,
					    MXdesc = ".primary",
					    MXsubval = "P",
                        HScodes = DS.MX.NUT.dat, 
					    AreaCodes)
# standardized exports		   
DS.SX.dat <- fun.DS.MX (EXx, 
                        TradeFlow = "Exports",
						TradeCode = "ptCode",
					    MXval = "Xs",
						DQ = FALSE,
					    MXdesc = ".standardized",
					    MXsubval = "D1",
                        HScodes = DS.MX.NUT.dat, 
					    AreaCodes)	
					
# total exports
DS.X.dat <-	fun.DS.SMX (lSMX=DS.SX.dat,
                        HScodes=DS.MX.NUT.dat,
					    lPMX = DS.PX.dat,
					    lEqX,
						TradeFlow = "Exports",
						MXval = "X",
					    Refa)					

						
# error on standardized exports
DS.SXsd.dat <- fun.DS.SMXsd(EXx, 
                            TradeFlow = "Exports",
							TradeCode = "ptCode",
							lEqXsd,
					        MXval = "Xsd",
					        MXsubval = "D1",
                            HScodes = DS.MX.NUT.dat, 
							AreaCodes,
					        Refa)	


merge.all <- function(by, ...) {
 frames <- list(...)
 return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
} 
lTS <- merge.all(by = c("AreaCode", "Group", "Commodity", "Year"), 
                 DS.QP.dat[[1]], DS.PM.dat, DS.SM.dat, DS.M.dat, 
				                 DS.PX.dat, DS.SX.dat, DS.X.dat, 
				                 DS.SMsd.dat, DS.SXsd.dat)	
							 
## DOMESTIC SUPPLY

DS.dat = fun.DS(lTS, Refa)
lDSprimary <- DS.dat[[1]]
lDS <- DS.dat[[2]]	

## TOTAL SUPPLY
TS.dat <- fun.TS (lTS, lPop, Refa)


## HOW MANY CALORIES ARE IMPUTED?
DQ.dat <- fun.DQ (QP=DS.QP.dat[[1]],
                  QPdq=DS.QP.dat[[2]],
				  Mdq=DQ.MX.dat,
				  M=DS.M.dat)

ndq <- merge(DQ.dat[[2]], AreaCodes[, c("AreaCode", "AreaName")], by=c("AreaCode"))
ndqx <- ndq[, c("AreaCode", "AreaName", "Year", "Commodity", "perc")]


##### INUSTRIAL USE -----------------------------------------------------------------------
DU.IND.dat <- fun.DU.IND (sYr, lYr, 
                          Refa, 
						  bio, 
						  Indust=FBSvec.lists$Indust, 
						  PrimaryAreaCodes, 
						  lDSprimary=DS.dat[[1]])
						  

##### LOSSES -----------------------------------------------------------------------
DU.LOSS.dat <- fun.DU.LOSS(sYr, lYr,
                           Refa,
                           losses,
	                       DS.QP.dat[[1]],
	                       PrimaryAreaCodes)
						   			   
##### SEED -------------------------------------------------------------------------
DU.SEED.dat <- fun.DU.SEED(sYr, lYr,
                           Refa,
                           seed,
						   AllSeed,
	                       AHdf,
						   SeedGroup,
	                       PrimaryAreaCodes)
	

DU.CCBS.dat <- fun.DU.CCBS(sYr, lYr,
                       XCBSpath = CCBSpath,
			           XCBSmap = CCBSmap,
					   XCBSele = c("Food", "Feed"),
					   XCBSelex = c("ELEM.DES"),
					   XCBSref = c("COUN.CODE", "COUN.DES", "PROD.DES", "ELEM.DES"),
                       XCBSid = c("COUN.CODE", "COUN.DES", "ELEM.DES", "Commodity"),
                       XCBSitems = c("PROD.DES"),
                       XCBSareacode = c("COUN.CODE"), 
                       XCBSareaname = c("COUN.DES")
					   )

setwd(FBSpath)
##### EXPECTED FOOD-----------------------------------------------------------------------						   
DU.expFOOD.dat = fun.DU.expFOOD (sYr, lYr,
                                 fclFOOD,
						         FCLmap,
								 CCBSmap,
								 USDAmap,
								 CCBSdat=DU.CCBS.dat,
								 USDAdat=EXT.USDA.dat,
                                 lDS=DS.dat[[2]],
						         lIND=DU.IND.dat,
						         lLOSS=DU.LOSS.dat, 
						         lSEED=DU.SEED.dat,
						         lPop,
	                             PrimaryAreaCodes,
								 AreaCodes,
						         FBSpath)	


##### F E E D------------------------------------------------------------------------------------------

DU.FEED.dat <- fun.DU.FEED (sYr, lYr,
                        Refa,
                        lAVL=DU.expFOOD.dat[[2]],
						FeedIngred=FBSvec.lists$FeedIngred)
						
setwd(FBSpath)
FE.OCBS.dat <-fun.FE.OCBS(sYr,lYr,
               OCBSpath,
			   OCBSmap,
			   IMx,
			   EXx,
			   HSm,
			   AreaCodes,
			   QPdf,
			   OSxr = FBSvec.lists$OSxr,
			   PrimaryAreaCodes,
			   feedUNSD)
			   
setwd(FBSpath)		   
FE.FEMX.dat <- fun.FE.FEMX (sYr,lYr, 
               AreaCodes,
               feedUNSD)
			   			   			 
FE.MERGE.dat <- fun.FE.MERGE(FE.OCBS.dat, 
               FE.FEMX.dat, 
			   lPop)

FE.FISH.dat <- FE.MERGE.dat[FE.MERGE.dat$Commodity == "fish.meal", 
                            c("AreaCode","Year", "Commodity", "pEqDS", "eEqDS")]
						
colnames(FE.FISH.dat)[colnames(FE.FISH.dat) == "eEqDS"] <- "FEEDe"
colnames(FE.FISH.dat)[colnames(FE.FISH.dat) == "pEqDS"] <- "FEEDp"	
FE.FISH.dat["Group"] <- "Meat and fish"
FE.FISH.dat$Commodity <- "pelagic.fish"

DU.FEED.dat[[3]] <- rbind(DU.FEED.dat[[3]], FE.FISH.dat)

FE.EPVAIL.dat <- fun.FE.EPVAIL (mergedOCBS=FE.MERGE.dat, 
                         lFEED=DU.FEED.dat[[3]], 
						 feed,
						 lPop,
						 AreaCodes)

write.csv(FE.EPVAIL.dat[[1]],file = "C:/Users/prakash/Dropbox/Public/EnergyAvail.csv", row.names = FALSE)
write.csv(FE.EPVAIL.dat[[2]],file = "C:/Users/prakash/Dropbox/Public/ProteinAvail.csv", row.names = FALSE)


##### R E S I D U A L------------------------------------------------------------------------------------------
DU.BAL.dat <- fun.DU.BAL (lFEED.AVL = DU.FEED.dat[[2]],
                            lFOOD.AVL = DU.FEED.dat[[1]])


##### CONTINGENCY TABLES------------------------------------------------------------------------------------------
DU.CONT.TABS <- fun.CONT.TABS (TS.dat[[2]],
                               DU.BAL.dat[[1]],
							   AreaCodes)

# calculate "grand totals" for each column and write to data.frame "xFBS"						   
xFBS <- ddply(DU.CONT.TABS[[1]],.(AreaName, AreaCode, Year), 
             function (x) colSums(x[, -c(1:5)], na.rm = TRUE))
xFBS["Group"] <- "GRAND TOTAL"
xFBS["Commodity"] <- "GRAND TOTAL"
xFBS <- rbind(DU.CONT.TABS[[1]], xFBS)


##### FEED.ADJ CONTINGENCY TABLES----------------------------------------------------------------------------------
DU.CONT.TABS.FDADJ <- fun.CONT.TABS.FDADJ (sYr, lYr,
                                           feedRanges,
										   AreaCodes,
										   lPop,
                                           Evail=FE.EPVAIL.dat[[1]],
                                           ConTab=DU.CONT.TABS[[1]],
                                           xFBS.FE.CON=xFBS)
										   
write.csv(DU.CONT.TABS.FDADJ[[1]], file = "C:/Users/prakash/Dropbox/Public/Adj.feedrange.csv", row.names = FALSE)
write.csv(DU.CONT.TABS.FDADJ[[3]], file = "C:/Users/prakash/Dropbox/Public/Adj.commodityContTab.csv", row.names = FALSE)
write.csv(DU.CONT.TABS.FDADJ[[4]], file = "C:/Users/prakash/Dropbox/Public/xFBSnew.csv", row.names = FALSE)
Sys.time()
