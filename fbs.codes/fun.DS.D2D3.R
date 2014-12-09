#' Function to standardize complex FBS trade
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param D2D3s - file containing trade codes, descriptions, parent composition and calorie conversion
#' @param meHS - mean of distribution of nutrients
#' @param sdHS - 2 standard deviations of distribution of nutrients
#' @param D2D3path - file path to complex trade files
#' @param AreaCodes - area code conversion
#' @param lDA - domestic availability (QP+M) to calculate parent shares 
#' @param RawD2D3codes - for each complex product, what are the candidate parents?
#' @export

#' fun.DS.D2D3()


fun.DS.D2D3 = function (D2D3s,
                        sdHS,
						meHS,
						UNSDpath,
						D2D3path,
						AreaCodes,
						lDA,
						FBSpath,
						RawD2D3codes,
						val) {

##compile parents, nutrients, errors of D2D3 commodities
  sdHS <- sdHS[, c(1, 8:10, 12)]
  meHS <- meHS[, c(1, 8:10, 12)]
  colnames(sdHS) <- c("DF.HS_code", "SD.KCAL", "SD.PROT", "SD.FAT", "SD.CARB")
  colnames(meHS) <- c("DF.HS_code", "ME.KCAL", "ME.PROT", "ME.FAT", "ME.CARB")
  x <- merge(meHS, sdHS, by = c("DF.HS_code"), all = TRUE)
  colnames(x)[1] <- "ItemCode"
  x$ItemCode <- paste0(substr(x$ItemCode, 1, 5), substr(x$ItemCode, 7, 8)) # remove "." in codes
  
  D2D3HS <- D2D3s[c("ItemCode", "UNSD.HS.NAME")]
  
  y <- merge(D2D3HS, x, 
             by = c("ItemCode"), 
			 all = FALSE)
			 
  colnames(y) <- c("ItemCode", "UNSD.HS.NAME", "dME.KCAL", "dME.PROT", "dME.FAT",
                  "dME.CARB", "dSD.KCAL", "dSD.PROT", "dSD.FAT", "dSD.CARB")
  PARENTHS <- D2D3s[c("PARENT.HS.LONG", "PARENT.HS.NAME")]
  colnames(PARENTHS)[1] <- "ItemCode"
  z <- merge(PARENTHS, x, by = c("ItemCode"), all = FALSE)
  colnames(z) <- c("PARENT.HS.LONG", "PARENT.HS.NAME", "pME.KCAL", "pME.PROT", 
                 "pME.FAT", "pME.CARB", "pSD.KCAL", "pSD.PROT","pSD.FAT", "pSD.CARB")
  a <- unique(merge(D2D3s, y, by = c("ItemCode"), all = FALSE))

  xD2D3s <- unique(merge(a, z, 
                       by = c("PARENT.HS.LONG"), 
					   all = FALSE))
					   
  xD2D3s <- xD2D3s[order(xD2D3s$ItemCode), ] 

## compile the trade of D2D3 commodities

#get the balanced HS UNSD files for required years
  xD2D3s.list <- unique(substr(xD2D3s$ItemCode, 2, 7))
  CommFiles = data.frame(x = character(), stringsAsFactors = FALSE)

  for (y in sYr:lYr) {
    yr <- y
    for(i in 1:length(xD2D3s.list)){
      Comm <- xD2D3s.list[i]
      Comm <- paste0(paste0(Comm, "."), yr)
      Comm <- paste0(UNSDpath, paste0(Comm, val))
      CommFiles <- rbind(CommFiles, data.frame(x = Comm))
    }
  }

 
#copy and compile into a dataframe
  file.copy(CommFiles$x, D2D3path) 
  flist <- list.files(D2D3path, pattern = "*.csv", full.names = TRUE)
  D2D3data = ldply(flist, 
             function(filename) {
              HS = read.csv(filename)
              HS["ItemCode"] <- "" 
              HS$ItemCode = paste0("x", substr(filename, 64, 69))
             return(HS)
             }
			 )

#information is extracted from exporters -> importers, so take NW_X (converted to tonnes)
  D2D3data <- D2D3data[c("Year", "rtCode", "ptCode", "NW_X", "ItemCode")]
  D2D3data$NW_X <- D2D3data$NW_X / 1000
  colnames(D2D3data)[colnames(D2D3data) == "NW_X"] <- "dNW_X"

#get the list of parent commodities from structure  
  parentlistComms <- unique(xD2D3s$PARENT.HS.NAME.x)
  parentlistCodes <- unique(xD2D3s$PARENT.HS.LONG)
  parentlistCodes <- c(parentlistCodes)
  parentlist <- unique(xD2D3s[c("PARENT.HS.NAME.x", "PARENT.HS.LONG")])
  colnames(parentlist)[colnames(parentlist) == "PARENT.HS.NAME.x"] <- "Commodity"

## now read in quantities of parent domestic availabilities (QP + IM) for 
## parent structures as these will be used to derive parent shares of complex trade
  AreaCodes <- AreaCodes[c("AreaCode", "rtCode")]
  DA <- merge(lDA, AreaCodes, by = ("AreaCode"), all = FALSE)
  DA <- merge(DA, parentlist, by = ("Commodity"), all = FALSE)
  DAx <- DA[DA$Commodity %in% parentlistComms == TRUE, ] 

#reshape to have parent domestic availabilities by parent HS code  
  colnames(DAx)[colnames(DAx) == "rtCode"] <- "ptCode"
  DAx$AreaCode <- NULL
  DAmelt <- melt(DAx, 
               id.vars = c("Commodity", "ptCode", "Group", "PARENT.HS.LONG")) #"rtCode",
  DAcast <- dcast(DAmelt, ptCode + variable ~ PARENT.HS.LONG, 
                value.var = "value", fun.aggregate = sum)
  colnames(DAcast)[colnames(DAcast) == "variable"] <- "Year"
  DAcast$Year <- substr(DAcast$Year, 3, 6)

## merge exports of complex D2D3 trade with parent domestic availabilities, by partner by year 
## and into long format
  rawTarget <- merge(D2D3data, DAcast, 
                   by = c("ptCode", "Year"), 
				   all = FALSE)
  
  rawTarget <- melt(rawTarget, 
                  id.vars = c("ptCode", "Year", "rtCode", "dNW_X", "ItemCode"))
				  
  colnames(rawTarget)[6] <- "PARENT.HS.LONG"
  colnames(rawTarget)[7] <- "pNW_X"

  setwd(FBSpath)

##get the lists of parent codes for each complex product code

#generalized way to read in flexible csv structure
  x <- as.list((readLines(RawD2D3codes)))
  y <- strsplit(as.character(x), ",")
# Extract the first vector element and set it as the list element name
  names(y) <- sapply(y, function(x) x[[1]])
# Remove the first vector element from each list element
  y <- lapply(y, function(x) x[- 1])
  m <- melt(y)
  s <- split(m, m$L1)

# create blank data frame for rbinding
  a <- data.frame(ptCode = numeric(), Year = character(), rtCode = numeric(), 
                 dNW_X = numeric(), ItemCode = character(), PARENT.HS.LONG = character(), 
				 pNW_X = numeric(), stringsAsFactors = FALSE) 
				 
# match the traded D2D3 commodities with parents
  for(i in 1:length(s)){
     iC <- substr(names(s[i]), 5, nchar(names(s[i])))
     r <- rawTarget[rawTarget$ItemCode ==  iC & rawTarget$PARENT.HS.LONG %in% s[[i]][, 1] , ]
     a <- rbind(a, r)
  }
  rawTarget <- a

  finTarget <- merge(rawTarget, xD2D3s, 
                   by = c("ItemCode", "PARENT.HS.LONG"))
  finTarget <- finTarget[order(finTarget$ItemCode, 
                             finTarget$ptCode, 
							 finTarget$rtCode), ]

  finTarget <- finTarget[, c("ItemCode", "PARENT.HS.LONG", "Year", "ptCode", "rtCode",
              "dNW_X", "pNW_X", "UNSD.HS.NAME.x", "PARENT.HS.NAME.x",
              "Group", "Type", "Share", "SubShare", "CalAGG",
              "dME.KCAL", "dME.PROT", "dME.FAT", "dME.CARB",
              "dSD.KCAL", "dSD.PROT", "dSD.FAT", "dSD.CARB",
              "pME.KCAL", "pME.PROT", "pME.FAT", "pME.CARB",
              "pSD.KCAL", "pSD.PROT", "pSD.FAT", "pSD.CARB")]

  colnames(finTarget)[colnames(finTarget) == "PARENT.HS.NAME.x"] <- "PARENT.HS.NAME"
  colnames(finTarget)[colnames(finTarget) == "UNSD.HS.NAME.x"] <- "UNSD.HS.NAME"
  
#aggregate for equal probability of shares (share = 1) 
  aggSh <- aggregate(pNW_X ~ ItemCode + ptCode + rtCode + Year,
                    data = finTarget[finTarget$Share == 1, ],
				    sum)
  aggSh <- aggSh[order(aggSh$ItemCode, aggSh$ptCode), ]

  rawSh <- merge(finTarget, aggSh, 
               by = c("ItemCode", "ptCode", "rtCode", "Year"), 
			   all = TRUE)

  rawSh["p.SHARE"] <- 1
  rawSh[rawSh$Share == 1, "p.SHARE"] <- rawSh[rawSh$Share == 1, "pNW_X.x"] / 
                                        rawSh[rawSh$Share == 1, "pNW_X.y"]

#sub parent product shares 
  rawSh$sp.SHARE <- rawSh$p.SHARE * rawSh$SubShare

#quantities of D2D3
  rawSh[!is.na(rawSh$SubShare), "qd.SHARE"] <- 
       rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
       rawSh[!is.na(rawSh$SubShare), "dNW_X"]
	  
  rawSh[is.na(rawSh$SubShare), "qd.SHARE"]  <- 
       rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	   rawSh[is.na(rawSh$SubShare), "dNW_X"]


##nutrients
#KCAL 
  rawSh["CalDer"] <- ""
  rawSh$CalDer <- as.numeric(as.character(rawSh$CalDer))
  rawSh[!is.na(rawSh$SubShare), "CalDer"] <- 
       rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	   rawSh[!is.na(rawSh$SubShare), "pME.KCAL"] * 
	   rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	   rawSh[!is.na(rawSh$SubShare), "dNW_X"]
	   
  rawSh[is.na(rawSh$SubShare), "CalDer"] <- 
       rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	   rawSh[is.na(rawSh$SubShare), "pME.KCAL"] * 
	   rawSh[is.na(rawSh$SubShare), "CalAGG"] *  
	   rawSh[is.na(rawSh$SubShare), "dNW_X"]
	   
  rawSh["CalDerSD"] <- ""
  rawSh$CalDerSD <- as.numeric(as.character(rawSh$CalDerSD))
  rawSh[!is.na(rawSh$SubShare), "CalDerSD"] <- 
       rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	   rawSh[!is.na(rawSh$SubShare), "pSD.KCAL"] * 
	   rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	   rawSh[!is.na(rawSh$SubShare), "dNW_X"]
	   
  rawSh[is.na(rawSh$SubShare), "CalDerSD"] <- 
       rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	   rawSh[is.na(rawSh$SubShare), "pSD.KCAL"] * 
	   rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	   rawSh[is.na(rawSh$SubShare), "dNW_X"]
	   
# usRaw <-rawSh[rawSh$ptCode==842 &rawSh$Year==2012,]
# write.csv(usRaw, file = "C:/Users/prakash/Dropbox/Public/usRaw.csv", row.names = FALSE)


#PROTEIN 
  rawSh["PrtDer"] <- ""
  rawSh$PrtDer <- as.numeric(as.character(rawSh$PrtDer))
  rawSh[!is.na(rawSh$SubShare), "PrtDer"] <- 
       (rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	   rawSh[!is.na(rawSh$SubShare), "pME.PROT"] * 
	   rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	   rawSh[!is.na(rawSh$SubShare), "dNW_X"]) / 100
	   
  rawSh[is.na(rawSh$SubShare), "PrtDer"] <- 
      (rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	  rawSh[is.na(rawSh$SubShare), "pME.PROT"] * 
	  rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[is.na(rawSh$SubShare), "dNW_X"]) / 100
  
  rawSh["PrtDerSD"] <- ""
  rawSh$PrtDerSD <- as.numeric(as.character(rawSh$PrtDerSD))
  rawSh[!is.na(rawSh$SubShare), "PrtDerSD"] <- 
      (rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	  rawSh[!is.na(rawSh$SubShare), "pSD.PROT"] * 
	  rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[!is.na(rawSh$SubShare), "dNW_X"]) / 100
	  
  rawSh[is.na(rawSh$SubShare), "PrtDerSD"] <- 
      (rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	  rawSh[is.na(rawSh$SubShare), "pSD.PROT"] * 
	  rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[is.na(rawSh$SubShare), "dNW_X"]) / 100 
	  
##FAT 

  rawSh["FatDer"] <- ""
  rawSh$FatDer <- as.numeric(as.character(rawSh$FatDer))
  rawSh[!is.na(rawSh$SubShare), "FatDer"] <- 
      (rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	  rawSh[!is.na(rawSh$SubShare), "pME.FAT"] * 
	  rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[!is.na(rawSh$SubShare), "dNW_X"]) / 100
	  
  rawSh[is.na(rawSh$SubShare), "FatDer"] <- 
      (rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	  rawSh[is.na(rawSh$SubShare), "pME.FAT"] * 
	  rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[is.na(rawSh$SubShare), "dNW_X"]) / 100
	  
  rawSh["FatDerSD"] <- ""
  rawSh$FatDerSD <- as.numeric(as.character(rawSh$FatDerSD))
  rawSh[!is.na(rawSh$SubShare), "FatDerSD"] <- 
      (rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	  rawSh[!is.na(rawSh$SubShare), "pSD.FAT"] * 
	  rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[!is.na(rawSh$SubShare), "dNW_X"]) / 100
	  
  rawSh[is.na(rawSh$SubShare), "FatDerSD"] <- 
      (rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	  rawSh[is.na(rawSh$SubShare), "pSD.FAT"] * 
	  rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[is.na(rawSh$SubShare), "dNW_X"]) / 100
	  
##CARBS
  rawSh["CarbDer"] <- ""
  rawSh$CarbDer <- as.numeric(as.character(rawSh$CarbDer))
  rawSh[!is.na(rawSh$SubShare), "CarbDer"] <- 
       (rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
       rawSh[!is.na(rawSh$SubShare), "pME.CARB"] * 
       rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
       rawSh[!is.na(rawSh$SubShare), "dNW_X"]) / 100

  rawSh[is.na(rawSh$SubShare), "CarbDer"] <- 
      (rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	  rawSh[is.na(rawSh$SubShare), "pME.CARB"] * 
	  rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[is.na(rawSh$SubShare), "dNW_X"]) / 100
  
  rawSh["CarbDerSD"] <- ""
  rawSh$CarbDerSD <- as.numeric(as.character(rawSh$CarbDerSD))
  rawSh[!is.na(rawSh$SubShare), "CarbDerSD"] <- 
      (rawSh[!is.na(rawSh$SubShare), "sp.SHARE"] * 
	  rawSh[!is.na(rawSh$SubShare), "pSD.CARB"] * 
	  rawSh[!is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[!is.na(rawSh$SubShare), "dNW_X"]) / 100
	  
  rawSh[is.na(rawSh$SubShare), "CarbDerSD"] <- 
      (rawSh[is.na(rawSh$SubShare), "p.SHARE"] * 
	  rawSh[is.na(rawSh$SubShare), "pSD.CARB"] * 
	  rawSh[is.na(rawSh$SubShare), "CalAGG"] * 
	  rawSh[is.na(rawSh$SubShare), "dNW_X"]) / 100
  
##calorie unit check (not needed) -------------------------
  rawSh["ImpCal"] <- ""
  rawSh$ImpCal <- as.numeric(as.character(rawSh$ImpCal))
  rawSh$ImpCal <- rawSh$CalDer / rawSh$dNW_X

 


##----------------------------------------------------------
  
  colnames(rawSh)[colnames(rawSh) == "PARENT.HS.NAME"] <- "Commodity"


  PrimaryEqM <- aggregate(qd.SHARE ~ rtCode + Year + Group + Commodity, 
                          data = rawSh, sum) 
  PrimaryEqX <- aggregate(qd.SHARE ~ ptCode + Year + Group + Commodity, 
                          data = rawSh, sum) 
  PrimaryEqMsd <- aggregate(qd.SHARE ~ rtCode + Year + Group + Commodity, 
                          data = rawSh, sum) 
  PrimaryEqXsd <- aggregate(qd.SHARE ~ ptCode + Year + Group + Commodity, 
                          data = rawSh, sum) 

  print("agg 1")
						  
  PrimaryEqMe <- aggregate(CalDer ~ rtCode + Year + Group + Commodity, 
                         data = rawSh, sum) 
  PrimaryEqXe <- aggregate(CalDer ~ ptCode + Year + Group + Commodity, 
                         data = rawSh, sum)
  PrimaryEqMsde <- aggregate(CalDerSD ~ rtCode + Year + Group + Commodity, 
                         data = rawSh, sum)		 
  PrimaryEqXsde <- aggregate(CalDerSD ~ ptCode + Year + Group + Commodity, 
                         data = rawSh, sum)

 print("agg 2")
  # PrimaryEqMe$CalDer <- PrimaryEqMe$CalDer / 1000
  # PrimaryEqXe$CalDer <- PrimaryEqXe$CalDer / 1000
  # PrimaryEqMsde$CalDerSD <- PrimaryEqMsde$CalDerSD / 1000
  # PrimaryEqXsde$CalDerSD <- PrimaryEqXsde$CalDerSD / 1000

  PrimaryEqMp <- aggregate(PrtDer ~ rtCode + Year + Group + Commodity, 
                        data = rawSh, sum) 
  PrimaryEqXp <- aggregate(PrtDer ~ ptCode + Year + Group + Commodity, 
                        data = rawSh, sum)
  PrimaryEqMsdp <- aggregate(PrtDerSD ~ rtCode + Year + Group + Commodity, 
                        data = rawSh, sum)
  PrimaryEqXsdp <- aggregate(PrtDerSD ~ ptCode + Year + Group + Commodity, 
                        data = rawSh, sum)
  print("agg 3")
  PrimaryEqMf <- aggregate(FatDer ~ rtCode + Year + Group + Commodity, 
                         data = rawSh, sum) 
  PrimaryEqXf <- aggregate(FatDer ~ ptCode + Year + Group + Commodity, 
                         data = rawSh, sum)
  PrimaryEqMsdf <- aggregate(FatDerSD ~ rtCode + Year + Group + Commodity, 
                         data = rawSh, sum) 
  PrimaryEqXsdf <- aggregate(FatDerSD ~ ptCode + Year + Group + Commodity, 
                         data = rawSh, sum)
  print("agg 4")
  PrimaryEqMc <- aggregate(CarbDer ~ rtCode + Year + Group + Commodity, 
                         data = rawSh, sum) 
  PrimaryEqXc <- aggregate(CarbDer ~ ptCode + Year + Group + Commodity, 
                         data = rawSh, sum)
  PrimaryEqMsdc <- aggregate(CarbDerSD ~ rtCode + Year + Group + Commodity, 
                         data = rawSh, sum) 
  PrimaryEqXsdc <- aggregate(CarbDerSD ~ ptCode + Year + Group + Commodity, 
                         data = rawSh, sum)
  print("agg 5")
  Refm <- c("rtCode", "Group", "Commodity", "Year")
  Refx <- c("ptCode", "Group", "Commodity", "Year")

  merge.all <- function(by, ...) {
     frames <- list(...)
     return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
  } 
  
  PrimaryEqM <- merge.all(by = c(Refm) 
            ,PrimaryEqM, PrimaryEqMe, PrimaryEqMp, PrimaryEqMf, PrimaryEqMc)
  PrimaryEqX <- merge.all(by = c(Refx)
            ,PrimaryEqX, PrimaryEqXe, PrimaryEqXp, PrimaryEqXf, PrimaryEqXc)
  PrimaryEqMsd <- merge.all(by = c(Refm)
            ,PrimaryEqMsde, PrimaryEqMsdp, PrimaryEqMsdf, PrimaryEqMsdc)
  PrimaryEqXsd <- merge.all(by = c(Refx)
            ,PrimaryEqXsde, PrimaryEqXsdp, PrimaryEqXsdf, PrimaryEqXsdc)
   
  print("merge")
  
  colnames(AreaCodes)[2] <- "rtCode"
  PrimaryEqM <- merge(PrimaryEqM, AreaCodes, by = "rtCode", all = FALSE)
  PrimaryEqMsd <- merge(PrimaryEqMsd, AreaCodes, by = "rtCode", all = FALSE)
  colnames(AreaCodes)[2] <- "ptCode"
  PrimaryEqX <- merge(PrimaryEqX, AreaCodes, by = "ptCode", all = FALSE)
  PrimaryEqXsd <- merge(PrimaryEqXsd, AreaCodes, by = "ptCode", all = FALSE)


return(list(PrimaryEqM, PrimaryEqX, PrimaryEqMsd, PrimaryEqXsd))
}