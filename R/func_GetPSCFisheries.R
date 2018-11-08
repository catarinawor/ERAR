#=======================================================
#ERA function GetPSCFisheries()
#Translated from VB ERA CIS code
#November 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


GetPSCFisheries <- function(M){




    dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
    #ERASQL <- "SELECT COUNT(ID) from ERA_PSCFishery"

    #NumberPSCFisheries <-  RODBC::sqlQuery( dta , query = ERASQL )[1,1]


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





