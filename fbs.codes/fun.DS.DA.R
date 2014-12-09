#' Function to establish FBS primary domestic availability
#'
#' This function establishes FBS primary domestic availability (in KCAL) for the purposes of 
#' the standardization of highly complex (D2D3) trade involving multiple parents
#' @param lQP - data.frame of FBS-processed production
#' @param lPM - data.frame of balanced primary imports (lPM)

#' @keywords domestic availability
#' @export
#' @examples
#' fun.DS.DA()


fun.DS.DA = function (lQP,
					  lPM) {
					  
       lDA <- merge(lQP[, c(Refa, "QPe")],
                    lPM[, c(Refa, "Mpe")],
                    by = c(Refa), all = TRUE)
      
       lDA[is.na(lDA)] <- 0
       lDA["DA"] <- lDA$QPe + lDA$Mpe
       lDA$Year <- paste0("DA", lDA$Year)
       lDA <- dcast(lDA, AreaCode + Group + Commodity  ~ Year, 
             value.var = "DA") 

return(lDA)
}