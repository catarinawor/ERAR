#=======================================================
#ERA function GetPSCFisheries()
#Translated from VB ERA CIS code
#November 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)




#' @title GetPSCFisheries
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#'
#' @details
#'
#' @return D: A list containing all initial settings contained in M and the folowing objects NumberPSCFisheries (double),
#' PSCFisheryNumber (vector), PSCFisheryName (vector), PSCFisheryGear (vector).
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetPSCFisheries <- function(M){




    dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
    
    #'Get name,number,gear of PSCFisheries
    
    ERASQL2 = "SELECT ID,Name,Gear from ERA_PSCFishery"
    df1 <-  RODBC::sqlQuery( dta , query = ERASQL2)
    names(df1) <- c("PSCFisheryNumber","PSCFisheryName","PSCFisheryGear")
            
    NumberPSCFisheries <- nrow(df1)    
    

    D <- list(NumberPSCFisheries=NumberPSCFisheries,
              PSCFisheryNumber = df1$PSCFisheryNumber,
              PSCFisheryName = df1$PSCFisheryName,
              PSCFisheryGear = df1$PSCFisheryGear)
	
    return(D)

} 





