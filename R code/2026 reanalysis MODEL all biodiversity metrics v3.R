 library (INLA)
#INLA:::inla.dynload.workaround()
setwd("") # for loading the data

# Libraries --------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(boot)
library(data.table)
library(moments)

# Data files -------------------------------------------------------------------

parameters<- read.csv("metricsCor.csv", stringsAsFactors = F)
load("completeData2023pure.RData")

# General params ---------------------------------------------------------------

args <- commandArgs(trailingOnly = T)


# Exclusions -------------------------------------------------------------------

# exclude freshwater
completeData2023pure<- subset(completeData2023pure, Realm != 'Freshwater'); dim(completeData2023pure)

# 2026 create dataframes to check outcome against old #########
completeDataOld<- subset(completeData2023pure,!Plot_ID %in% c(5, 	921, 922, 924,925, 	643, 644, 646, 647, 137, 138, 139 ) ) # excluded exp plots
completeDataOld<- subset(completeDataOld, !Datasource_ID %in% c(300, 1364, 1357,1387, 1410, 1402, 1353)) # excluded exp datasets
dim(completeDataOld)
cDrichnessOld <- subset(completeDataOld, Unit == "richness"); dim(cDrichnessOld)
cDabundOld    <- subset(completeDataOld, Unit == "abundance"); dim(cDabundOld)
#


exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139,  #brazil fragmentation experiment
							1727, # sweden experimental thinning added 2026
							522, 525, 527, # Tasmania burned plots. # added RvK June 2026 
							2667,	2668,	2669,	2670,2671,	2672, # planted plots in Uganda (were not in Nature paper)
							2673,	2674,	2675,	2676,	2678,	2679,	2680,	2681,	2682,	2683,	2684,	2685,	2686,
							2687,	2688,	2689,	2690,	2691,	2692,	2693,	2694,	2695,	2696,	2697,	2698,	2699,	2700,	2701,2702)
exptDatasources<- c(300, #Kellogg, 
										1357, #Luquillo CTE, 
										1364, #Cedar creek big bio, 
										1387, # some German grassland,
										1410, # austr spiders
										# addition 2026: exclude more 
										301,  # konza
										1396, # Germany springtails (revegetalisation of a rubble dump), 
										#1397 (artificial nesting sites) is not experimental  
										1460# russia rove beetles (agricultural practices) and 
										)

# exclude											
completeData2023pure<- completeData2023pure[!completeData2023pure$Datasource_ID %in% exptDatasources, ]
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% exptPlots, ]
dim(completeData2023pure)

# remove 1 Portal ant dataset and russia springtails
completeData2023pure<- subset(completeData2023pure, Datasource_ID != 1353 & Datasource_ID != 1402 ) 


# exclude plots with a trend in taxonomic resolution:
bad_tax<- 
	c(849L, 132L, 1442L, 1527L)

completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% bad_tax, ]
dim(completeData2023pure)


# Subset summary ---------------------------------------------------------------

length(unique(completeData2023pure$Datasource_ID)) # 103 remaining in 2026

unique(completeData2023pure[, c("Datasource_ID", "Datasource_name")]) # datasource_IDs
length(unique(completeData2023pure$Plot_ID))#903

completeData2023pure$pltYr<- paste0(completeData2023pure$Plot_ID, "_", completeData2023pure$Year)

startEndYears<- completeData2023pure%>% 
	group_by(Plot_ID) %>%
	summarise(
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T), 
		nYrs = length(unique(Year)),
	)
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)

# Rename -----------------------------------------------------------------------

# rename for simplicity
completeData2023<- completeData2023pure


# include pollinator data for newest dataframe---------------------
# load pollinator data for new analysis anno 2026
completeDataPollinators<- readRDS("C:/Dropbox/Insect Biomass Trends/csvs/completeDataPollinators.RDS"); dim(completeDataPollinators)
head(completeDataPollinators)
overlappingDatasets<- intersect(unique(completeDataPollinators$Datasource_ID) , unique(completeData2023pure$Datasource_ID))

setdiff(names(completeData2023), names(completeDataPollinators))
setdiff(names(completeDataPollinators), names(completeData2023))
matchingColumns<- intersect(names(completeDataPollinators), names(completeData2023))

completeData2026<- subset(completeData2023, !Datasource_ID %in% overlappingDatasets); dim(completeData2026)

completeData2026<- rbind(completeData2026[,matchingColumns], 
												 completeDataPollinators[, matchingColumns] ); dim(completeData2026)

completeData2026<- completeData2026[!completeData2026$Datasource_ID %in% exptDatasources, ]; dim(completeData2026)
completeData2026<- completeData2026[!completeData2026$Plot_ID %in% exptPlots, ]
dim(completeData2026)


unique(completeData2026$Unit)

cDabund2026<- subset(completeData2026, Unit == "abundance"); dim(cDabund2026)




# Model objects ----------------------------------------------------------------

# make objects needed for different models
# 2026: this actually is overkill, because we don't analyse all of them
cDrichness <- subset(completeData2023, Unit == "richness"); dim(cDrichness)
cDrarRichness <- subset(completeData2023, Unit == "rarefiedRichness"); dim(cDrarRichness)
cDabund    <- subset(completeData2023, Unit == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2023, Unit == "ENSPIE")   ;dim(cDenspie)
cDenspie$Number[is.infinite(cDenspie$Number)] <- 0
cDhorn     <- subset(completeData2023, Unit == "Horn"); dim(cDhorn)
cDhorn$dif <-  cDhorn$Number - cDhorn$ExpectedBeta
cDhorn$SES <- (cDhorn$Number - cDhorn$ExpectedBeta) / cDhorn$SDexpectedBeta
cDhorn<- cDhorn[!cDhorn$pltYr %in% startEndYears$pltStartYr, ] ;dim(cDhorn)
cDbray     <- subset(completeData2023, Unit == "Bray"); dim(cDbray)
cDbray$dif <-  cDbray$Number - cDbray$ExpectedBeta
cDbray$SES <- (cDbray$Number - cDbray$ExpectedBeta) / cDbray$SDexpectedBeta
cDbray<- cDbray[!cDbray$pltYr %in% startEndYears$pltStartYr, ]; dim(cDbray)
cDjaccard  <- subset(completeData2023, Unit == "Jaccard") ; dim(cDjaccard)
cDjaccard$Number[is.nan(cDjaccard$Number)] <- NA
cDjaccard$dif <-  cDjaccard$Number - cDjaccard$ExpectedBeta
cDjaccard$SES <- (cDjaccard$Number - cDjaccard$ExpectedBeta) / cDjaccard$SDexpectedBeta
cDjaccard<- cDjaccard[!cDjaccard$pltYr %in% startEndYears$pltStartYr, ] ; dim(cDjaccard)
cDlogNr10   <- subset(completeData2023, Unit == "logNr10"    ); dim(cDlogNr10)  
cDlogNr90   <- subset(completeData2023, Unit ==  "logNr90"   ); dim(cDlogNr90)  
cDlogNr020   <- subset(completeData2023, Unit ==  "logNr020"   ); dim(cDlogNr020)  
cDlogNr2040   <- subset(completeData2023, Unit ==  "logNr2040"   ); dim(cDlogNr2040)  
cDlogNr4060   <- subset(completeData2023, Unit ==  "logNr4060"   ); dim(cDlogNr4060)  
cDlogNr6080   <- subset(completeData2023, Unit ==  "logNr6080"   ); dim(cDlogNr6080)  
cDlogNr80100   <- subset(completeData2023, Unit ==  "logNr80100"   ); dim(cDlogNr80100)  
cDlogQ1   <- subset(completeData2023, Unit ==  "logNrQ1"   ); dim(cDlogQ1)  
cDlogQ2   <- subset(completeData2023, Unit ==  "logNrQ2"   ); dim(cDlogQ2)  
cDlogQ3   <- subset(completeData2023, Unit ==  "logNrQ3"   ); dim(cDlogQ3)  
cDlogQ4   <- subset(completeData2023, Unit ==  "logNrQ4"   ); dim(cDlogQ4)  
cDdom       <- subset(completeData2023, Unit == "dominanceRel"); dim(cDdom)  
cDshan  <- subset(completeData2023, Unit == "Shannon"); dim(cDshan)  
cDpielou <- subset(completeData2023, Unit == "Pielou"); dim(cDpielou)  
cDevar   <- subset(completeData2023, Unit == "Evar"); dim(cDevar)
cDmcN <- subset(completeData2023, Unit == "dominanceMcNaught"); dim(cDmcN )  
cdCoverageR7	<-subset(completeData2023, Unit == "coverageRichness.7"); dim(cdCoverageR7)
cdCoverageR8	<-subset(completeData2023, Unit == "coverageRichness.8"); dim(cdCoverageR8)
cdCoveragePIE7	<-subset(completeData2023, Unit == "coverageENSpie.7"); dim(cdCoveragePIE7)
cdCoveragePIE8	<-subset(completeData2023, Unit == "coverageENSpie.8"); dim(cdCoveragePIE8)

cDHillNrs <- subset(completeData2023, Unit == "ENSPIE"| Unit == "Shannon" | Unit == "richness")
cDHillNrs <- reshape2::dcast(cDHillNrs[, c(1:9,11:14, 18,37,40, 55, 57:74)], ...~Unit, value.var = "Number") # excluded the Inla indices for interactions between 
cDHillNrs$ShanEven <- exp(cDHillNrs$Shannon) / cDHillNrs$richness
cDHillNrs$ShanEven <- round(cDHillNrs$ShanEven , 6) # round to 6 decimals. for some reason some 1's are recognized as >1
cDHillNrs$ShanEven[cDHillNrs$ShanEven >1] <- NA
cDHillNrs$SimpEven <- (cDHillNrs$ENSPIE) / cDHillNrs$richness
cDHillNrs$ShanEven[is.infinite(cDHillNrs$ShanEven)] <- NA
cDHillNrs$SimpEven[is.infinite(cDHillNrs$SimpEven)] <- NA
# remove plots with only NA values: 
onlyNA<- subset( reshape2::dcast(cDHillNrs, Plot_ID ~ "nonNA", value.var = "SimpEven", function(x){sum(!is.na(x))}), nonNA ==0)
cDHillNrs<- subset(cDHillNrs, !Plot_ID %in% onlyNA$Plot_ID)

# Best selection ---------------------------------------------------------------

topNotch<- unique(cDlogNr020$Datasource_ID) # select only datasets with full underlying species data
length(topNotch)
length(unique(unique(cDlogNr020$Plot_ID)))

cDrichnessReduced<- subset(cDrichness, Datasource_ID %in% topNotch); dim(cDrichnessReduced)
cDabundReduced	<- subset(cDabund, Datasource_ID %in% topNotch); dim(cDabundReduced)
cDenspieReduced	<- subset(cDenspie, Datasource_ID %in% topNotch); dim(cDenspieReduced)
cDrarRichReduced	<- subset(cDrarRichness , Datasource_ID %in% topNotch); dim(cDrarRichReduced)

# Object list ------------------------------------------------------------------

# put everything in a list to select from
all.df<-list(			 cDabund2026 = cDabund2026,
	
							cDrichness = cDrichness,
						 cDrarRichness = cDrarRichness,
						 cDabund = cDabund,
						 cDenspie =   cDenspie,
						 cDhorn =   cDhorn,
						 cDbray =   cDbray,
						 cDjaccard =   cDjaccard,
						 cDshan=	cDshan, 
						 cDpielou=		cDpielou, 
						 cDevar  =   cDevar,
						 cDlogNr10=	cDlogNr10,
						 cDlogNr90=	cDlogNr90,
						 cDlogNr020=	cDlogNr020,
						 cDlogNr2040=	cDlogNr2040,
						 cDlogNr4060=	cDlogNr4060 ,
						 cDlogNr6080=	cDlogNr6080  ,
						 cDlogNr80100=	cDlogNr80100  ,
						 cDlogQ1	=	cDlogQ1   ,
						 cDlogQ2   =	cDlogQ2   ,
						 cDlogQ3   =	cDlogQ3   ,
						 cDlogQ4   =	cDlogQ4   ,
						 cDdom	=	cDdom, 
						 cDmcN	=	cDmcN, 
						 cDHillNrs = cDHillNrs,
						 cdCoverageR7 = cdCoverageR7,
						 cdCoverageR8 = cdCoverageR8,
						 cdCoveragePIE7 = cdCoveragePIE7,
						 cdCoveragePIE8 = cdCoveragePIE8,
						 cDrichnessReduced = cDrichnessReduced, 
						 cDabundReduced = cDabundReduced, 
						 cDenspieReduced = cDenspieReduced, 
						 cDrarRichReduced= cDrarRichReduced

)







# first run Tweedie models. this is to my current (2026) best knowledge the most fitting to our dataset, 
# as it handles both integer and non-nterger positive data

# Array job code ---------------------------------------------------------------
# LOOP STARTS HERE FOR TWEEDIE MODELS
output_dir<- ("/Tweedie models")
for(i in c(33:37,39:50,  65)){


parameters[i,]
taskID<- i

print("model name:")
print(parameters$model_name[taskID])

# select metric from the task file
metric<- parameters$model_name[taskID]
metric


# save start time to check total run time later
startTime<- Sys.time()

# grab needed dataframe from list 
dat<- all.df[[as.character(parameters$input_file[taskID])]] ; dim(dat)

# select realm (not necessary, because we only test terrestrial)
rlm<- parameters$subset[taskID]
dat<- subset(dat, Realm == rlm)
dim(dat)
#str(dat)


# add indices
source("C:\\Dropbox\\work\\2017 iDiv\\2018 insect biomass\\final-insect-diversity-trends2/addIndicies.r")
dat<- addIndicies(dat)




# set priors
vect<- dat[[parameters$yForPrior[taskID]]]

if(parameters$family[taskID] =="tweedie"){
	sd.res<-	3 * sd(log(vect+0.1), na.rm = T)
}
if(parameters$family[taskID] =="beta"){
	sd.res<-	3 *sd(boot::logit(vect)[boot::logit(vect) != Inf & boot::logit(vect) != -Inf ], na.rm = T)
}


prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
fam <-   parameters$family[taskID]

# replace 1 with .999999, otherwise beta regression will crash (only evenness models)
if(fam == "beta"){ 
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==1 & !is.na(dat[, parameters$y[taskID]])]<- 1-0.000001
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==0 & !is.na(dat[, parameters$y[taskID]])]<- 0+0.000001
} 

# Rue updated model ------------------------------------------------------------

# here's the magic addition from Harvard Rue, that will make it run in a fraction of the time:  

dat$E <- exp(8) # I don't know what this does 
dat$iYear.scale <- scale(dat$iYear)
val <- range(dat$iYear)

#Ah - yes the indices should be continuous. So after you dropped plots, you should have 
#recreated a new factor starting from plot ID 1, but I dont get the reverse thing!
plot.id.values <- unique(sort(dat$Plot_ID))
plot.id.inverse <- rep(NA, max(plot.id.values))
plot.id.inverse[plot.id.values] <- length(plot.id.values):1 # changed by Roel. This should acually make things faster
dat$Plot_ID_rep <- plot.id.inverse[dat$Plot_ID]





# set model formula using the task file
formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
												 "+f(Period_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(iYear, model = 'ou', 	
                    values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                    hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                    replicate = Plot_ID_rep)" 
))


#print(formul)



print(startTime)
model <- inla( formul,
							 family = fam,  
							 control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))),
							 
							 control.compute = list(config = FALSE, 
							 											 dic=TRUE,
							 											 waic=TRUE, 
							 											 openmp.strategy="huge", 
							 											 cpo = FALSE), 
							 # control.inla = list(#int.strategy="eb", 
							 # 										tolerance =  1e-08), 
							 control.predictor = list(link = 1) , 
							 #verbose = T, 
							 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
							 num.threads = 4,# 
							 data=dat)

beepr::beep()

fixed = model$summary.fixed
print(fixed)

print(parameters$model_name[taskID])
print("done after:")
print(Sys.time() - startTime)
#assign(as.character(parameters$model_name[i]), model)

# save whole model and summary of fixed effects
model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))
saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)



# extract posterior marginals and sample posteriors
n <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial",
								 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
marg<- n

marg$y80<- marg$y
marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
marg$y90<- marg$y
marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
marg$y95<- marg$y
marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA

# save marginal
marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
saveRDS (marg, file =  marg_file)


#pull out random intercepts and slopes:
RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])

#data source ID
intercepts     <- model$summary.random$Datasource_ID_4INLA
slopes         <- model$summary.random$Datasource_ID_4INLAs
slopes_Location<-model$summary.random$Location_4INLAs
slopes_plot    <-model$summary.random$Plot_ID_4INLAs
names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")

# add up fixed slope and random slopes
fx<-data.frame(Realm =  "Terrestrial", #
							 fixedSlp = model$summary.fixed$mean[2], 
							 fixedIntercept = (model$summary.fixed$mean[1]  ) )
RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  

rand_file <- file.path(output_dir, paste0(metric,"randomSlopes.rds"))

# save random effects
saveRDS (RandEfDataset, file =  rand_file)


}



# FOR BETA MODELS  ####

for(i in c(38, 51, 57,58, 61:64)){
	
	
	parameters[i,]
	taskID<- i
	
	print("model name:")
	print(parameters$model_name[taskID])
	
	# select metric from the task file
	metric<- parameters$model_name[taskID]
	metric
	
	
	# save start time to check total run time later
	startTime<- Sys.time()
	
	# grab needed dataframe from list 
	dat<- all.df[[as.character(parameters$input_file[taskID])]] ; dim(dat)
	
	# select realm (not necessary, because we only test terrestrial)
	rlm<- parameters$subset[taskID]
	dat<- subset(dat, Realm == rlm)
	dim(dat)
	#str(dat)
	
	
	# add indices
	source("C:\\Dropbox\\work\\2017 iDiv\\2018 insect biomass\\final-insect-diversity-trends2/addIndicies.r")
	dat<- addIndicies(dat)
	
	
	
	
	# set priors
	vect<- dat[[parameters$yForPrior[taskID]]]
	
	if(parameters$family[taskID] =="tweedie"){
		sd.res<-	3 * sd(log(vect+0.1), na.rm = T)
	}
	if(parameters$family[taskID] =="beta"){
		sd.res<-	3 *sd(boot::logit(vect)[boot::logit(vect) != Inf & boot::logit(vect) != -Inf ], na.rm = T)
	}
	
	
	prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
	fam <-   parameters$family[taskID]
	
	# replace 1 with .999999, otherwise beta regression will crash (only evenness models)
	if(fam == "beta"){ 
		dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==1 & !is.na(dat[, parameters$y[taskID]])]<- 1-0.000001
		dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==0 & !is.na(dat[, parameters$y[taskID]])]<- 0+0.000001
	} 
	
	# Rue updated model ------------------------------------------------------------
	
	# here's the magic addition from Harvard Rue, that will make it run in a fraction of the time:  
	
	dat$E <- exp(8) # I don't know what this does 
	dat$iYear.scale <- scale(dat$iYear)
	val <- range(dat$iYear)
	
	#Ah - yes the indices should be continuous. So after you dropped plots, you should have 
	#recreated a new factor starting from plot ID 1, but I dont get the reverse thing!
	plot.id.values <- unique(sort(dat$Plot_ID))
	plot.id.inverse <- rep(NA, max(plot.id.values))
	plot.id.inverse[plot.id.values] <- length(plot.id.values):1 # changed by Roel. This should acually make things faster
	dat$Plot_ID_rep <- plot.id.inverse[dat$Plot_ID]
	
	
	
	
	
	# set model formula using the task file
	formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
													 "+f(Period_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(iYear, model = 'ou', 	
                    values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                    hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                    replicate = Plot_ID_rep)" 
	))
	
	
	#print(formul)
	
	
	
	print(startTime)
	model <- inla( formul,
								 family = fam,  
							#	 control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))), # only used in Tweedie
								 
								 control.compute = list(config = FALSE, 
								 											 dic=TRUE,
								 											 waic=TRUE, 
								 											 openmp.strategy="huge", 
								 											 cpo = FALSE), 
								 control.inla = list(int.strategy="eb", 
								 										tolerance =  1e-08), 
								 control.predictor = list(link = 1) , 
								 #verbose = T, 
								 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
								 num.threads = 4,# 
								 data=dat)
	
	beepr::beep()
	
	fixed = model$summary.fixed
	print(fixed)
	
	print(parameters$model_name[taskID])
	print("done after:")
	print(Sys.time() - startTime)
	#assign(as.character(parameters$model_name[i]), model)
	
	# save whole model and summary of fixed effects
	model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
	fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))
	saveRDS (model, file =  model_file)
	saveRDS (fixed, file = fixed_file)
	
	
	
	# extract posterior marginals and sample posteriors
	n <-  data.frame(Metric = metric, 
									 Realm = "Terrestrial",
									 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
	marg<- n
	
	marg$y80<- marg$y
	marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
	marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
	marg$y90<- marg$y
	marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
	marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
	marg$y95<- marg$y
	marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
	marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA
	
	# save marginal
	marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	saveRDS (marg, file =  marg_file)
	
	
	#pull out random intercepts and slopes:
	RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])
	
	#data source ID
	intercepts     <- model$summary.random$Datasource_ID_4INLA
	slopes         <- model$summary.random$Datasource_ID_4INLAs
	slopes_Location<-model$summary.random$Location_4INLAs
	slopes_plot    <-model$summary.random$Plot_ID_4INLAs
	names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
	names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
	names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
	names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes
	
	# datasource level slopes for Fig 1
	RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
	RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")
	
	# add up fixed slope and random slopes
	fx<-data.frame(Realm =  "Terrestrial", #
								 fixedSlp = model$summary.fixed$mean[2], 
								 fixedIntercept = (model$summary.fixed$mean[1]  ) )
	RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
	RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
	
	rand_file <- file.path(output_dir, paste0(metric,"randomSlopes.rds"))
	
	# save random effects
	saveRDS (RandEfDataset, file =  rand_file)
	
	
}







# LOG 10(n+1) models #####
# these are the original models as published in Van Klink et al 2024
output_dir<- "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models"

for(i in c(1:5 ,7: 18)){
	
	
	parameters[i,]
	taskID<- i
	
	print("model name:")
	print(parameters$model_name[taskID])
	
	# select metric from the task file
	metric<- parameters$model_name[taskID]
	metric
	
	
	# save start time to check total run time later
	startTime<- Sys.time()
	
	# grab needed dataframe from list 
	dat<- all.df[[as.character(parameters$input_file[taskID])]] ; dim(dat)
	
	# select realm (not necessary, because we only test terrestrial)
	rlm<- parameters$subset[taskID]
	dat<- subset(dat, Realm == rlm)
	dim(dat)
	#str(dat)
	
	
	# add indices
	source("C:\\Dropbox\\work\\2017 iDiv\\2018 insect biomass\\final-insect-diversity-trends2/addIndicies.r")
	dat<- addIndicies(dat)
	
	
	
	
	# set priors
	vect<- dat[[parameters$yForPrior[taskID]]]
	
	if(parameters$family[taskID] =="gaussian"){
		sd.res<-	3 * sd(log(vect+0.1), na.rm = T)
	}
	if(parameters$family[taskID] =="beta"){
		sd.res<-	3 *sd(boot::logit(vect)[boot::logit(vect) != Inf & boot::logit(vect) != -Inf ], na.rm = T)
	}
	
	
	prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
	fam <-   parameters$family[taskID]
	
	# replace 1 with .999999, otherwise beta regression will crash (only evenness models)
	if(fam == "beta"){ 
		dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==1 & !is.na(dat[, parameters$y[taskID]])]<- 1-0.000001
		dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==0 & !is.na(dat[, parameters$y[taskID]])]<- 0+0.000001
	} 
	
	# Rue updated model ------------------------------------------------------------
	
	# here's the magic addition from Harvard Rue, that will make it run in a fraction of the time:  
	
	dat$E <- exp(8) # I don't know what this does 
	dat$iYear.scale <- scale(dat$iYear)
	val <- range(dat$iYear)
	
	#Ah - yes the indices should be continuous. So after you dropped plots, you should have 
	#recreated a new factor starting from plot ID 1, but I dont get the reverse thing!
	plot.id.values <- unique(sort(dat$Plot_ID))
	plot.id.inverse <- rep(NA, max(plot.id.values))
	plot.id.inverse[plot.id.values] <- length(plot.id.values):1 # changed by Roel. This should acually make things faster
	dat$Plot_ID_rep <- plot.id.inverse[dat$Plot_ID]
	
	
	
	
	
	# set model formula using the task file
	formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
													 "+f(Period_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(iYear, model = 'ou', 	
                    values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                    hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                    replicate = Plot_ID_rep)" 
	)) # reverse indexing on the autoregressive term ('Plot_ID_rep') introduced in 2026. this speeds up models, but does not affect outcome
	
	
	#print(formul)
	
	
	
	print(startTime)
	model <- inla( formul,
								 family = fam,  
								 control.compute = list(config = FALSE, 
								 											 dic=TRUE,
								 											 waic=TRUE, 
								 											 openmp.strategy="huge", 
								 											 cpo = FALSE), 
								 	 control.predictor = list(link = 1) , 
								 #verbose = T, 
								 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
								 num.threads = 4,# 
								 data=dat)
	
	beepr::beep()
	
	fixed = model$summary.fixed
	print(fixed)
	
	print(parameters$model_name[taskID])
	print("done after:")
	print(Sys.time() - startTime)
	#assign(as.character(parameters$model_name[i]), model)
	
	# save whole model and summary of fixed effects
	model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
	fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))
	saveRDS (model, file =  model_file)
	saveRDS (fixed, file = fixed_file)
	
	
	
	# extract posterior marginals and sample posteriors
	n <-  data.frame(Metric = metric, 
									 Realm = "Terrestrial",
									 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
	marg<- n
	
	marg$y80<- marg$y
	marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
	marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
	marg$y90<- marg$y
	marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
	marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
	marg$y95<- marg$y
	marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
	marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA
	
	# save marginal
	marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	saveRDS (marg, file =  marg_file)
	
	
	#pull out random intercepts and slopes:
	RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])
	
	#data source ID
	intercepts     <- model$summary.random$Datasource_ID_4INLA
	slopes         <- model$summary.random$Datasource_ID_4INLAs
	slopes_Location<-model$summary.random$Location_4INLAs
	slopes_plot    <-model$summary.random$Plot_ID_4INLAs
	names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
	names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
	names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
	names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes
	
	# datasource level slopes for Fig 1
	RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
	RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")
	
	# add up fixed slope and random slopes
	fx<-data.frame(Realm =  "Terrestrial", #
								 fixedSlp = model$summary.fixed$mean[2], 
								 fixedIntercept = (model$summary.fixed$mean[1]  ) )
	RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
	RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
	
	rand_file <- file.path(output_dir, paste0(metric,"randomSlopes.rds"))
	
	# save random effects
	saveRDS (RandEfDataset, file =  rand_file)
	
	
}










# TRASH #####
# Compare models ---------------------------------------------------------------

list.files(output_dir)
setwd(output_dir)
outputs <- list.files(output_dir, full.names = F) %>%
              set_names() %>%
              map_dfr(readRDS, .id="source") %>%
              separate(source, c( "model", "response","subset","type","ext")) %>%
              mutate(model = gsub("outputs/","", model)) %>%
              janitor::clean_names() 

ggplot(outputs[grepl("Year",row.names(outputs)),]) +
  geom_pointrange(aes(x=model, y=mean, ymin=x0_025quant, ymax=x0_975quant))+
  coord_flip()+
  facet_grid(subset~response, scales = 'free_x')+
	geom_hline(yintercept = 0)+
  theme_bw()

write.csv(outputs, "N and S model comparison 20260501.csv")

	# End --------------------------------------------------------------------------










# test code 
Year<- 1:30
year.effect = 0.2
start_abund = log(10)
log_abund <- start_abund + year.effect*Year
Number <- rpois(length(log_abund),log_abund)
mydata <- data.frame(Number, Year)
mydata

mydata<- subset(dat, Plot_ID == 1581)

lm1 <- inla(log10(Number) ~ Year , family="gaussian", data=mydata)
q_log10 <- lm1$summary.fixed["Year", c("mean","0.025quant", "0.975quant")]
(10^q_log10 - 1) * 100



lm2 <- inla((Number) ~ Year , family="tweedie", data=mydata)
q_tweedie <- lm2$summary.fixed["Year", c("mean","0.025quant", "0.975quant")]
(exp(q_tweedie) - 1) * 100

lm3 <- inla(round(Number) ~ Year, family="poisson", data=mydata)
q_poisson <- lm3$summary.fixed["Year", c("mean","0.025quant", "0.975quant")]
(exp(q_poisson) - 1) * 100

plot(mydata$Year , mydata$Number, pch = 20)
points(mydata$Year , 10^(lm1$summary.fitted.values$mean))
points(mydata$Year ,(lm2$summary.fitted.values$mean), col = "red")
points(mydata$Year ,(lm3$summary.fitted.values$mean), col = "blue") # very similar 

ggplot(mydata, aes(x = Year, y = Number))+ 
	geom_line()



