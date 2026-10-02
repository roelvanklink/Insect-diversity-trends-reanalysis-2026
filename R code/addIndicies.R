
addIndicies <- function(myData){
  
  #year covariates
  myData$cYear <- myData$Year - floor(median(myData$Year))
  myData$iYear <- myData$Year - min(myData$Year) + 1
  myData$rYear <- myData$iYear
  myData$rYear2 <- myData$iYear
  
  #random intercept indices (these are nested)
  myData$Plot_ID_4INLA <- interaction(myData$Datasource_ID,myData$Plot_ID)
  myData$Plot_ID_4INLA <- as.numeric(factor(myData$Plot_ID_4INLA))   
  myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
  myData$Country_State_4INLA <- as.numeric(factor(myData$Country_State))
  
  # This is now a crossed random effect: accounting for datasets that were collected at the same location
  #  myData$Location[is.na(myData$Location)] <- 1#dummy value
  # myData$Location_4INLA <- interaction(myData$Datasource_ID,myData$Location) # this is not necessary anymore
  myData$Location_4INLA <- as.numeric(factor(myData$Location))
  
  myData$Period_4INLA <- interaction(myData$Datasource_ID,myData$Period)
  myData$Period_4INLA <- as.numeric(factor(myData$Period_4INLA))
  
  #random slope indices
  myData$Plot_ID_4INLAs <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
  myData$Datasource_ID_4INLAs <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
  myData$Location_4INLAs <- myData$Location_4INLA+max(myData$Location_4INLA)
  myData$Country_State_4INLAs <- myData$Country_State_4INLA+max(myData$Country_State_4INLA)
  
  
  
  # add interaction with unit, if Unit is present in data. porbably not used in any other analysis than the science paper
  
  if ("Unit" %in% names(myData)){
  # add indices for Inla. VEry important to have different indices for the biomass and abundance data in the same dataset! 
  # otherwise Inla thinks these different metrics are drawn from the same distribution! 
  myData$DSunit_4INLA <- interaction(myData$Datasource_ID,myData$Unit)
  myData$DSunit_4INLA <- as.numeric(factor(myData$DSunit_4INLA))
  myData$Locunit_4INLA <- interaction(myData$Location,myData$Unit)
  myData$Locunit_4INLA <- as.numeric(factor(myData$Locunit_4INLA))
  myData$Plotunit_4INLA <- interaction(myData$Plot_ID, myData$Unit)
  myData$Plotunit_4INLA <- as.numeric(factor(myData$Plotunit_4INLA))
  # random slopes
  myData$Plotunit_4INLAs <- myData$Plotunit_4INLA+max(myData$Plotunit_4INLA)
  myData$DSunit_4INLAs   <- myData$DSunit_4INLA+max(myData$DSunit_4INLA)
  myData$Locunit_4INLAs <- myData$Locunit_4INLA+max(myData$Locunit_4INLA)
  
  unique(myData$Unit)
  myData$Unit<- droplevels(myData$Unit)
  }
  
  
  
  return(myData)
}





addIndiciesPops <- function(myData){
	
	#year covariates
	myData$cYear <- myData$Year - floor(median(myData$Year))
	myData$iYear <- myData$Year - min(myData$Year) + 1
	myData$rYear <- myData$iYear
	myData$rYear2 <- myData$iYear
	
	#random intercept indices (these are nested)
	myData$Plot_ID_4INLA <- interaction(myData$Datasource_ID,myData$Plot_ID)
	myData$Plot_ID_4INLA <- as.numeric(factor(myData$Plot_ID_4INLA))   
	myData$Datasource_ID_4INLA <- as.numeric(factor(myData$Datasource_ID))
	myData$Country_State_4INLA <- as.numeric(factor(myData$Country_State))
	
	# This is now a crossed random effect: accounting for datasets that were collected at the same location
	#  myData$Location[is.na(myData$Location)] <- 1#dummy value
	# myData$Location_4INLA <- interaction(myData$Datasource_ID,myData$Location) # this is not necessary anymore
	myData$Location_4INLA <- as.numeric(factor(myData$Location))
	
#	myData$Period_4INLA <- interaction(myData$Datasource_ID,myData$Period)
#	myData$Period_4INLA <- as.numeric(factor(myData$Period_4INLA))
	
	#random slope indices
	myData$Plot_ID_4INLAs <- myData$Plot_ID_4INLA+max(myData$Plot_ID_4INLA)
	myData$Datasource_ID_4INLAs <- myData$Datasource_ID_4INLA+max(myData$Datasource_ID_4INLA)
	myData$Location_4INLAs <- myData$Location_4INLA+max(myData$Location_4INLA)
	myData$Country_State_4INLAs <- myData$Country_State_4INLA+max(myData$Country_State_4INLA)
	
	# add indices for Inla. VEry important to have different indices for the biomass and abundance data in the same dataset! 
	# otherwise Inla thinks these different metrics are drawn from the same distribution! 
	myData$DSunit_4INLA <- interaction(myData$Datasource_ID,myData$Unit)
	myData$DSunit_4INLA <- as.numeric(factor(myData$DSunit_4INLA))
	myData$Locunit_4INLA <- interaction(myData$Location,myData$Unit)
	myData$Locunit_4INLA <- as.numeric(factor(myData$Locunit_4INLA))
	myData$Plotunit_4INLA <- interaction(myData$Plot_ID, myData$Unit)
	myData$Plotunit_4INLA <- as.numeric(factor(myData$Plotunit_4INLA))
	# random slopes
	myData$Plotunit_4INLAs <- myData$Plotunit_4INLA+max(myData$Plotunit_4INLA)
	myData$DSunit_4INLAs   <- myData$DSunit_4INLA+max(myData$DSunit_4INLA)
	myData$Locunit_4INLAs <- myData$Locunit_4INLA+max(myData$Locunit_4INLA)
	
	unique(myData$Unit)
#	myData$Unit<- droplevels(myData$Unit)
	
	plot.id.values <- unique(sort(myData$Plot_ID))
	plot.id.inverse <- rep(NA, max(plot.id.values))
	plot.id.inverse[plot.id.values] <- length(plot.id.values):1
	myData$Plot_ID_rep <- plot.id.inverse[myData$Plot_ID]
	
	
	
	return(myData)
}
