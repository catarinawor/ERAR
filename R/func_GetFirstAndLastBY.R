#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


GetFirstAndLastBY <- function(dbse,CASStockString,LastCalendarYear,OceanStartAge){


    dta <- RODBC::odbcConnectAccess2007(dbse)   #specifies the file path   

        df2 <- RODBC::sqlFetch(dta, "ERA_WireTagCode")
    ERASQL <- paste0("SELECT Min(BroodYear) AS MinOfBroodYear, Max(BroodYear) AS MaxOfBroodYear FROM ERA_WireTagCode WHERE CASStock IN ('", as.character(CASStockString[[1]]) ,"') AND NOT ExcludeTagCodeFromERA = (-1)")
    
    (df1 <- sqlQuery( dta , query = ERASQL ))
    
    erro<-0

    FirstBY<-df1[1,1]

    LastAvailableBY<-df1[1,2]
    


#

    FirstBY<-min(df2$BroodYear[df2$CASStock== as.character(CASStockString)&df2$ExcludeTagCodeFromERA!=-1])
    LastAvailableBY<-max(df2$BroodYear[df2$CASStock== as.character(CASStockString)&df2$ExcludeTagCodeFromERA!=-1])
    
    if(is.na( FirstBY)|is.na(LastAvailableBY)){
        print(paste("please open ERA_CASStockToERAStockMapping and see if " , CASStockString , " is missing in the CASStock field (column).  Program is going to stop"))
        erro<-1

    }



	#Get first and last brood years for the ERAstock
	#need to read these from the data
	#LastAvailableBY<- 
    #ERASQL = "SELECT Min(BroodYear) AS MinOfBroodYear, Max(BroodYear) AS MaxOfBroodYear FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ")" & " AND NOT ExcludeTagCodeFromERA = -1"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    CISDataReader.Read()
	
	# last brood to run based on user-selected LastCalendarYear.  The last year in the listbox is determined by the last year of IM data.
	LastPossibleBY<- LastCalendarYear - OceanStartAge
    
    #set LastBy to lesser of LastAvailableBY or LastPossibleBY
    if(LastAvailableBY < LastPossibleBY){
    	 LastBY <- LastAvailableBY
    }else{
    	LastBY <- LastPossibleBY
    } 
    
    #read from database
    #youngestAge<-
    #ERASQL2 = paste0("SELECT Min(age) FROM ERA_CWDBRecovery as r INNER JOIN ERA_WireTagCode as t ON r.TagCode = t.TagCode  WHERE CASStock IN (" ,CASStockString, ")")
    #df3 <- RODBC::sqlQuery( dta , query = ERASQL2 )

    ERA_CWDBRecovery<- RODBC::sqlFetch(dta, "ERA_CWDBRecovery")
    ERA_WireTagCode<- RODBC::sqlFetch(dta, "ERA_WireTagCode")

    df3<-merge( ERA_CWDBRecovery, ERA_WireTagCode, by = "TagCode")

    df4<-df3[df3$CASStock==as.character(CASStockString),]

    



    


    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    CISDataReader.Read()
    #    youngestAge = CISDataReader(0)
    #    CISDataReader.Close()

    if(youngestAge < OceanStartAge & !isCombineAge2And3[ERAStock]){
        if(youngestAge == 1){
            sink('Log_OlderThanMaxAge_ID.txt',append = TRUE,)
            cat( paste(ShakerMethod, youngestAge, "will not be used in Exploitation Rate Analysis.\n"))
            # Stop writing to the file
            sink()
           
        }else if(youngestAge > 1){
            print(paste("Youngest age =", youngestAge, "is less than OceanStartAge in SuperStock.  Did you forget to combine age 2 and 3?")  )
        }
    }
}




