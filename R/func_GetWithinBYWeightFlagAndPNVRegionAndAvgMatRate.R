#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


GetWithinBYWeightFlagAndPNVRegionAndAvgMatRate <- function(){

  #  'determines whether Within BY Weighting occurs for a stock
        ERASQL = "Select PNVRegion, WithinBYWeightFlag, AverageMatRateFlag, Age2AverageMatRate, Age3AverageMatRate, Age4AverageMatRate, Age5AverageMatRate from ERA_Stock Where ERAStock = '" & CurrentStock & "'"
        
        #read in form database#ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
        #ISDataReader = ERACommand.ExecuteReader()
        CISDataReader.Read()
        PNVRegion = CISDataReader(0)
        withinBYWeightFlag = CISDataReader(1)
        ReadAvgMatRteFlg = CISDataReader(2)
        if(ReadAvgMatRteFlg){
            AverageMatRate[OceanStartAge] = CISDataReader[3]
            AverageMatRate[OceanStartAge + 1] = CISDataReader[4]
            AverageMatRate[OceanStartAge + 2] = CISDataReader[5]
            AverageMatRate[OceanStartAge + 3] = CISDataReader[6]
        }

        if(is.na(sum(CISDataReader))){
            print(paste("You have selected the AverageMatRateFlag in the ERA_Stock table, but not all average maturation rates have been entered in the table. ", vbCr ,"The program will stop. Error"))
        }

        
        
}




