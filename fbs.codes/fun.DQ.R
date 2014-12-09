#' Function to assess the degree of imputed caloric supply in FBS
#'
#' This function compiles primary domestic supply data (production and net primary trade, i.e.
#' no standardized processed products) necessary for the calculation of feed availability. 
#' The logic is we don't feed animals with imported bread but we do with imported wheat grain.
#' @param QP - FBS-processed production 
#' @param QPdq - FBS-processed production subject to imputation
#' @param Mdq - FBS-processed trade (imports) subject to imputation
#' @param M - FBS-processed trade (imports)

#' @keywords data quality
#' @export
#' fun.DQ()

				  
fun.DQ = function(QP,
                  QPdq,
				  Mdq,
				  M) {

        QP$Year <- as.numeric(as.character(QP$Year))
        QPdq$Year <- as.numeric(as.character(QPdq$Year))
        QP[is.na(QP)] <- 0
        QPdq[is.na(QPdq)] <- 0

# HOW MANY CALORIES ARE IMPUTED AT THE WORLD-YEAR LEVEL?

        impDS <- aggregate(QPdq$QPe, by = list(QPdq$Year), FUN = sum)  +
                 aggregate(Mdq$Me, by = list(Mdq$Year), FUN = sum) 

        DS <- aggregate(QP$QPe, by = list(QP$Year), FUN = sum) +
              aggregate(M$Me, by = list(M$Year), FUN = sum)

        wldIMP <- impDS/DS

# HOW MANY CALORIES ARE IMPUTED AT THE NATIONAL-YEAR-COMM LEVEL?

# production
        q <- aggregate(QPdq$QPe, 
                by = list(Year=QPdq$Year, 
                          AreaCode=QPdq$AreaCode, 
						  Commodity=QPdq$Commodity), 
			    FUN = sum)
# imports
        m <- aggregate(Mdq$Me, 
		        by = list(Year=Mdq$Year,
				          AreaCode=Mdq$AreaCode,
				          Commodity=Mdq$Commodity), 
			    FUN = sum)

# calculate total imputed supplies				
	    mq <- merge(m, q, by=c("Year", "AreaCode", "Commodity"), all=TRUE)
        mq[is.na(mq)] <- 0	
	    mq["sum"] <- mq[4] + mq[5]
		mq <- mq[, c(1:3, 6)]
	    impDS <- mq

# aggregate to FBS commodity list to compare
        q <- aggregate(QP$QPe, 
                by = list(Year=QP$Year, 
                          AreaCode=QP$AreaCode, 
						  Commodity=QP$Commodity), 
			    FUN = sum)  
        m <- aggregate(M$Me, 
		        by = list(Year=M$Year,
				          AreaCode=M$AreaCode,
				          Commodity=M$Commodity), 
			    FUN = sum)

# calculate ratio of imputed to total 				
	    mq <- merge(m, q, by=c("Year", "AreaCode", "Commodity"), all=TRUE)
        mq[is.na(mq)] <- 0	
	    mq["sum"] <- mq[4] + mq[5]
		mq <- mq[, c(1:3, 6)]
	    DS <- mq

		natIMP <- merge(impDS, DS, by=c("Year", "AreaCode", "Commodity"), all=TRUE)
        natIMP["perc"] <- natIMP$sum.x/natIMP$sum.y

return(list(wldIMP, natIMP))
}