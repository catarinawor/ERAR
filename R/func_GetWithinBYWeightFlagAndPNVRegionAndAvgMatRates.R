#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)




#' @title GetWithinBYWeightFlagAndPNVRegionAndAvgMatRate
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#'
#' @details
#'
#' @return D: A list 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates <- function(D,M){

    dta <- RODBC::odbcConnectAccess2007(M$datbse)      


    #  'determines whether Within BY Weighting occurs for a stock
    ERASQL = paste0("Select PNVRegion, WithinBYWeightFlag, AverageMatRateFlag, Age2AverageMatRate, Age3AverageMatRate, Age4AverageMatRate, Age5AverageMatRate from ERA_Stock Where ERAStock = '", D$CurrentStock , "'")

    df1 <- sqlQuery( dta , query = ERASQL )
        
    
    if(!df1$AverageMatRateFlag){ 
        return(list( OceanAge=D$OceanStartAge:(D$OceanStartAge+3),
            AverageMatRate=df1[,4:7],
            PNVRegion=df1$PNVRegion,
            WithinBYWeightFlag= df1$WithinBYWeightFlag,
            longerr=0))
    }else{
        nome<- "../logs/GetWithinBYWeightFlagAndPNVRegionAndAvgMatRate.log"
        sink(nome)
        cat(paste("You have selected the AverageMatRateFlag in the ERA_Stock table, but not all average maturation rates have been entered in the table. The program will stop. Error"))
        sink()
        return(list(longerr=1))

    }



        #original VB code
        #=================================================================================
        #'determines whether Within BY Weighting occurs for a stock
        #ERASQL = "Select PNVRegion, WithinBYWeightFlag, AverageMatRateFlag, Age2AverageMatRate, Age3AverageMatRate, Age4AverageMatRate, Age5AverageMatRate from ERA_Stock Where ERAStock = '" & CurrentStock & "'"
        #ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
        #CISDataReader = ERACommand.ExecuteReader()
        #CISDataReader.Read()
        #PNVRegion = CISDataReader(0)
        #withinBYWeightFlag = CISDataReader(1)
        #ReadAvgMatRteFlg = CISDataReader(2)
        #If ReadAvgMatRteFlg = True Then
        #    Try
        #        AverageMatRate(OceanStartAge) = CISDataReader(3)
        #        AverageMatRate(OceanStartAge + 1) = CISDataReader(4)
        #        AverageMatRate(OceanStartAge + 2) = CISDataReader(5)
        #        AverageMatRate(OceanStartAge + 3) = CISDataReader(6)
        #    Catch
        #        MsgBox("You have selected the AverageMatRateFlag in the ERA_Stock table, but not all average maturation rates have been entered in the table. " & vbCr & "The program will stop.", , "Error")
        #        End
        #    End Try
        #End If
        #CISDataReader.Close()
        
        
}




