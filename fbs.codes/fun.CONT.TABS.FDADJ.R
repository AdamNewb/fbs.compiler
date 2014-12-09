#' Function to finalise residual in FBS after adjusting for feed
#'
#' This function finalizes the residual - "stock changes" by default - by taking into account
#' plausible feed ranges established in the feed model. As feed ranges are calculated for the
#' totality of energy from food and non-food feedstuffs, they are adjusted on a fixed percentage
#' basis for only food. After which, for those commodities that fall outside the lower and upper
#' bounds of the adjusted feed range, feed availability in the balance is then adjusted on a 
#' pro-rata basis

#' @param sYr - from which year to begin processing
#' @param lYr - from which year to end processing
#' @param feedRanges - data.frame of lower and upper bounds of plausible total energy feed
#' @param AreaCodes - input data.frame of area codes and names
#' @param lPop - data.frame of population in long format
#' @param Evail - data.frame of feed availabilities and feed demand, with ratio of disequilibrium
#' @param ConTab - input data.frame of "balance" unadjusted for feed disequilibrium 
#' @param xFBS.FE.CON - data.frame ConTab but with column totals - "grand total" - to establish and
#                       adjust for disequilibrium in feed demand and supply.	

#' @keywords  feed-adjusted contingency table
#' @export
#' @examples
#' fun.CONT.TABS.FDADJ()


fun.CONT.TABS.FDADJ = function (sYr, lYr,
                                feedRanges,
								AreaCodes,
								lPop,
                                Evail,
                                ConTab,
                                xFBS.FE.CON){

# calculate per capita feed ranges in energy equivalence								
  feedRanges <- feedRanges[feedRanges$Year >= sYr, ]
  feedRanges <- merge(feedRanges, lPop, by=c("AreaCode", "Year"))

# establish missing feed ranges for missing China and EU
   Chn <- feedRanges[which(feedRanges$AreaCode == 41 | feedRanges$AreaCode == 96 | 
	                 feedRanges$AreaCode == 128 | feedRanges$AreaCode == 214), ]
   vEDp <- ddply(Chn, .(Year),   
   function(x) data.frame(EDemand_p=weighted.mean(x$EDemand_p, x$Population)))
   vEDlb <- ddply(Chn, .(Year),   
   function(x) data.frame(EDemand_lb=weighted.mean(x$EDemand_lb, x$Population)))
   vEDub <- ddply(Chn, .(Year),   
   function(x) data.frame(EDemand_ub=weighted.mean(x$EDemand_ub, x$Population)))

   vEDp["AreaCode"] <- 351
   vEDp["AreaName"] <- "China"
   ChinaFeed <- cbind(vEDp, vEDlb[2], vEDub[2])
   feedRanges$Population <- NULL
   feedRanges <- rbind(feedRanges, ChinaFeed)
   
   feedRanges <- merge(feedRanges, lPop, by=c("AreaCode", "Year"))
   euCodes = AreaCodes[AreaCodes$EU28==TRUE, "AreaCode"]
   EU <- feedRanges[which(feedRanges$AreaCode %in% euCodes), ]
   vEDp <- ddply(EU, .(Year),   
   function(x) data.frame(EDemand_p=weighted.mean(x$EDemand_p, x$Population)))
   vEDlb <- ddply(EU, .(Year),   
   function(x) data.frame(EDemand_lb=weighted.mean(x$EDemand_lb, x$Population)))
   vEDub <- ddply(EU, .(Year),   
   function(x) data.frame(EDemand_ub=weighted.mean(x$EDemand_ub, x$Population)))

   vEDp["AreaCode"] <- 5706
   vEDp["AreaName"] <- "European.union"
   EUFeed <- cbind(vEDp, vEDlb[2], vEDub[2])
   feedRanges$Population <- NULL
   feedRanges <- rbind(feedRanges, EUFeed)
   

# adjust the feed ranges for FBS food commodities

   Adj.EDemand <- merge(Evail[, c(1:2, length(Evail))], feedRanges, 
                       by = c("AreaCode", "Year"))
   Adj.EDemand$EDemand_p <- Adj.EDemand$EDemand_p * Adj.EDemand$FBS.mult
   Adj.EDemand$EDemand_lb <- Adj.EDemand$EDemand_lb * Adj.EDemand$FBS.mult
   Adj.EDemand$EDemand_ub <- Adj.EDemand$EDemand_ub * Adj.EDemand$FBS.mult
 
# calculate the "Grand total" column sum to identify whether the resulting feed availability in 
# FBS are within the plausible feed ranges
   xFBS.FE.CON <- xFBS.FE.CON[-(which(xFBS.FE.CON$Commodity == "GRAND TOTAL")), ]
   xFBSx <- ddply(xFBS.FE.CON,.(AreaName, AreaCode, Year), 
             function (x) colSums(x[, -c(1:5)], na.rm = TRUE))
   xFBSx["Group"] <- "GRAND TOTAL"
   xFBSx["Commodity"] <- "GRAND TOTAL"
   xFBS.FE.CON <- rbind(xFBS.FE.CON, xFBSx)
   
   CntryRanges <- merge(xFBS.FE.CON[xFBS.FE.CON$Commodity == "GRAND TOTAL", 
                        c("AreaCode", "Year", "AreaName", "Feed.use")],
                            Adj.EDemand, 
					   by = c("AreaCode", "Year"))

   colnames(Adj.EDemand)[colnames(Adj.EDemand) == "AreaCode"] <- "AREA"
   colnames(Adj.EDemand)[colnames(Adj.EDemand) == "AreaName"] <- "LABAREA"
   Adj.EDemand <- Adj.EDemand[, c("AREA", "LABAREA", "Year", "FBS.mult", 
                                 "EDemand_p", "EDemand_lb", "EDemand_ub")]
   Adj.EDemand$LABAREA <- gsub(",", ".", Adj.EDemand$LABAREA)
   Adj.EDemand$LABAREA <- gsub(" ", ".", Adj.EDemand$LABAREA)

# establish which country balances by year fall outside the plausible feed range and how much
# "flexibility" there is between existing feed supply and residual to adjust

   outDF <- CntryRanges[CntryRanges$Feed.use > CntryRanges$EDemand_ub | CntryRanges$Feed.use < CntryRanges$EDemand_lb, ]
   outAreas <- unique(outDF$AreaCode)
   
   xFBS.FE.CON <- xFBS[xFBS$AreaCode %in% outAreas & xFBS$Feed > 0, ]
   xFBS.FE.CON <- xFBS.FE.CON[order(xFBS.FE.CON$AreaCode), ]
   xFBS.FE.CON["Flex"] <- xFBS.FE.CON$Feed.use - xFBS.FE.CON$Stock.change

# calculate the adjustment needed to bring feed supply with feed demand range
  colnames(Adj.EDemand)[colnames(Adj.EDemand) == "AREA"] <- "AreaCode"
  colnames(Adj.EDemand)[colnames(Adj.EDemand) == "LABAREA"] <- "AreaName"
  xFBS.FE.DISEQ <- merge(xFBS.FE.CON[xFBS.FE.CON$Commodity == "GRAND TOTAL", ], 
                         Adj.EDemand, 
						 by = c("AreaCode", "Year"))
						 
# establish midpoint of feed range to be inserted in FBS
  xFBS.FE.DISEQ["Adj.mult"] <- 1
  xFBS.FE.DISEQ["Adj.mult"] <- with(xFBS.FE.DISEQ, 
            ifelse(xFBS.FE.DISEQ$Feed.use > xFBS.FE.DISEQ$EDemand_ub, 
			      (xFBS.FE.DISEQ$EDemand_ub + xFBS.FE.DISEQ$EDemand_lb) * 0.5 / xFBS.FE.DISEQ$Feed.use, 
				  (xFBS.FE.DISEQ$EDemand_lb + xFBS.FE.DISEQ$EDemand_ub) * 0.5 / xFBS.FE.DISEQ$Feed.use))

  xFBS.FE.DISEQ["Adj"] <- (1 - xFBS.FE.DISEQ$Adj.mult) * xFBS.FE.DISEQ$Feed.use	
  xFBS.FE.DISEQ <- xFBS.FE.DISEQ[order(xFBS.FE.DISEQ$AreaCode), ]				
  xFBS.FE.CON <- merge(xFBS.FE.CON, xFBS.FE.DISEQ[, c("AreaCode", "Year", "Adj")])
  xFBS.FE.CON <- xFBS.FE.CON[order(xFBS.FE.CON$AreaCode), ]

# split data.frame by country and year and apply the pro-rata adjustment factor
  spdf <- split(xFBS.FE.CON, xFBS.FE.CON[, c("AreaCode", "Year")])

  for(i in 1:length(spdf)){
   spdf[[i]]["FeedWt"] <-spdf[[i]]$Feed.use / spdf[[i]][spdf[[i]]$Commodity == "GRAND TOTAL", "Feed.use"]
  }

# re-assemble data.frame and estbalish "new feed" and "new stock change"
  xFBS.FE.CON <- do.call("rbind", spdf)
  xFBS.FE.CON["Feed.Adj"] <- xFBS.FE.CON$Adj * xFBS.FE.CON$FeedWt
  xFBS.FE.CON["New.Feed.use"] <- xFBS.FE.CON$Feed.use - xFBS.FE.CON$Feed.Adj   
  xFBS.FE.CON["New.Stock.change"] <- xFBS.FE.CON$Stock.change + xFBS.FE.CON$Feed.Adj
  
  xFBS.FE.CON$Feed.use <- xFBS.FE.CON$Stock.change <- NULL
  xFBS.FE.CON$Flex <- xFBS.FE.CON$Adj <- xFBS.FE.CON$FeedWt <- xFBS.FE.CON$Feed.Adj <- NULL
  xFBS.FE.CON <- xFBS.FE.CON[-(which(xFBS.FE.CON$Commodity == "GRAND TOTAL")), ]
  colnames(xFBS.FE.CON)[colnames(xFBS.FE.CON) == "New.Feed.use"] <- "Feed.use"
  colnames(xFBS.FE.CON)[colnames(xFBS.FE.CON) == "New.Stock.change"] <- "Stock.change"

# remove the country balances from the original balance that were subject to adjustment and 
# then add the feed-adjusted numbers

  ConTabDrop <- which(ConTab$AreaCode %in% outAreas & ConTab$Feed > 0)
  ConTab <- ConTab[-ConTabDrop, ]
  ConTab <- rbind(ConTab, xFBS.FE.CON)
  ConTab$Year <-as.numeric(as.character(ConTab$Year))
  ConTab <- ConTab[order(ConTab$AreaCode, -ConTab$Year, ConTab$Group, ConTab$Commodity ), ]

# write to a data.frame with new "grand totals"
  FBSgt <- ddply(ConTab,.(AreaName, AreaCode, Year), 
             function (x) colSums(x[, -c(1:5)], na.rm = TRUE))
  FBSgt["Group"] <- "GRAND TOTAL"
  FBSgt["Commodity"] <- "GRAND TOTAL"

  FBS <- rbind(ConTab, FBSgt)

# return adjusted balances, with "ConTab" sent for balancing subject to constraints. "FBS" 
# for internal use.
  return(list(Adj.EDemand, xFBS.FE.CON, ConTab, FBS))
}