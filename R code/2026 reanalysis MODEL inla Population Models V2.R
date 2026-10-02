


 library (INLA)


setwd("C:/Dropbox/Insect Biomass Trends/csvs/")

output_dir <- "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/populationModels"

#library(INLA)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(data.table)

args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
threads <- 4
print(paste("threads: ", threads))



starttime<- Sys.time()

# load task file
parameters<- read.csv("C:/Dropbox/work/2017 iDiv/2018 insect biomass/final-insect-diversity-trends2/popModels.csv", stringsAsFactors = F) # models to be run
# load data
allPops<- readRDS(file = "allPopulations 2023.rds")# 
head(allPops); dim(allPops)

# might need these later 
plots<-fread( file = "PlotData 5.0.csv"); dim(plots)
plots<- as.data.frame(plots)
dim(plots)

studies<-read.csv(file = "studies 5.2.csv", header = T); dim(studies)

allPops <- merge(allPops, plots[, c('Plot_ID',    'Datasource_nameREDUNDANT'  ,  'Location' , "Plot_name")]); dim(allPops)
allPops   <- merge(allPops, studies[, c("Datasource_ID", "Datasource_name", "Abundance.Biomass", "Invertebrate_group", "Invertebrate_group_Scientific_name", "Realm", "Experimental_manipulation",
																				"Continent", "Climate_zone",  "Country_State", "Country", "Region")]); dim(allPops )

beepr::beep()



exptPlots<- c(5, # alaska
							921, 922, 924,925, #smes
							643, 644, 646, 647, # hemlock removal
							137, 138, 139,  #brazil fragmentation experiment
							1727 # sweden experimental thinning added 2026
)
exptDatasources<- c(300, 1364, 1357,1387, 1410, #Kellogg, Luquillo CTE, Cedar creek big bio, some German grassland, austr sipders
										# addition 2026 exclude more: 
										301,  # konza
										1396, # Germany springtails (revegetalisation of a rubble dump), 
										#1397 (artificial nesting sites) is not experimental  
										1460# russia rove beetles (agricultural practices) 
)

allPops<- allPops[!allPops$Datasource_ID %in% exptDatasources, ]
allPops<- allPops[!allPops$Plot_ID %in% exptPlots, ]

# exclude plots with questionable taxonomy (includes the two German springtail plots):
bad_tax<- 
	c(849L, 132L, 133L, 1442L, 1527L)

allPops<- allPops[!allPops$Plot_ID %in% bad_tax, ]
dim(allPops)

allPops<- subset(allPops, Datasource_ID != 1353 & Datasource_ID != 1402 ) 


# make selection of high quality (>15 yrs data) datasets for RtM estimation
library(reshape2)
nr_yrs<- reshape2::dcast(allPops, Plot_ID~ "yrs_data", value.var = "Year", function(x){length(unique(x))}) # number of years data per plot
min15_yrs<- subset(nr_yrs, yrs_data >=15) # select long plots
dim(min15_yrs)
hqPops<- subset(allPops, Plot_ID %in% min15_yrs$Plot_ID); dim(hqPops)

# not used in paper: 
min10_yrs<- subset(nr_yrs, yrs_data >=10)
dim(min10_yrs)
hqPops10<- subset(allPops, Plot_ID %in% min10_yrs$Plot_ID); dim(hqPops10)



# create list of needed objects
# commonness assigned in relation to highest value in whole timeseries (not used in analyses)
allPopsCG0<- subset(allPops, CGYr1 == 0);length(unique(allPopsCG0$Taxon)); dim(allPopsCG0)
allPopsCG1<- subset(allPops, CGYr1 == 1);length(unique(allPopsCG1$Taxon))
allPopsCG2<- subset(allPops, CGYr1 == 2);length(unique(allPopsCG2$Taxon))
allPopsCG3<- subset(allPops, CGYr1 == 3);length(unique(allPopsCG3$Taxon))
allPopsCG4<- subset(allPops, CGYr1 == 4);length(unique(allPopsCG4$Taxon))
allPopsCG5<- subset(allPops, CGYr1 == 5);length(unique(allPopsCG5$Taxon))

# only datasets with at least 15 years of data, based on first year
hqPopsCG0<- subset(hqPops, CGYr1.0 == 0);length(unique(hqPopsCG0$Taxon)); dim(hqPopsCG0)
hqPopsCG1<- subset(hqPops, CGYr1.0 == 1);length(unique(hqPopsCG1$Taxon))
hqPopsCG2<- subset(hqPops, CGYr1.0 == 2);length(unique(hqPopsCG2$Taxon))
hqPopsCG3<- subset(hqPops, CGYr1.0 == 3);length(unique(hqPopsCG3$Taxon))
hqPopsCG4<- subset(hqPops, CGYr1.0 == 4);length(unique(hqPopsCG4$Taxon))
hqPopsCG5<- subset(hqPops, CGYr1.0 == 5);length(unique(hqPopsCG5$Taxon))

# commonness for all data assigned in relation to highest value in year 1
allPopsCG0Yr1<- subset(allPops, CGYr1.0 == 0);length(unique(allPopsCG0Yr1$Taxon)); dim(allPopsCG0Yr1)
allPopsCG1Yr1<- subset(allPops, CGYr1.0 == 1);length(unique(allPopsCG1Yr1$Taxon))
allPopsCG2Yr1<- subset(allPops, CGYr1.0 == 2);length(unique(allPopsCG2Yr1$Taxon))
allPopsCG3Yr1<- subset(allPops, CGYr1.0 == 3);length(unique(allPopsCG3Yr1$Taxon))
allPopsCG4Yr1<- subset(allPops, CGYr1.0 == 4);length(unique(allPopsCG4Yr1$Taxon))
allPopsCG5Yr1<- subset(allPops, CGYr1.0 == 5);length(unique(allPopsCG5Yr1$Taxon))

# commonness assigned in relation to highest mean value in yr 1-2
allPopsCG0Yr1.2<- subset(allPops, CG1.2 == 0);length(unique(allPopsCG0Yr1.2$Taxon)); dim(allPopsCG0Yr1.2)
allPopsCG1Yr1.2<- subset(allPops, CG1.2 == 1);length(unique(allPopsCG1Yr1.2$Taxon))
allPopsCG2Yr1.2<- subset(allPops, CG1.2 == 2);length(unique(allPopsCG2Yr1.2$Taxon))
allPopsCG3Yr1.2<- subset(allPops, CG1.2 == 3);length(unique(allPopsCG3Yr1.2$Taxon))
allPopsCG4Yr1.2<- subset(allPops, CG1.2 == 4);length(unique(allPopsCG4Yr1.2$Taxon))
allPopsCG5Yr1.2<- subset(allPops, CG1.2 == 5);length(unique(allPopsCG5Yr1.2$Taxon))

# commonness assigned in relation to highest mean value in yr 1-5
allPopsCG0Yr1.5<- subset(allPops, CG1.5 == 0);length(unique(allPopsCG0Yr1.5$Taxon)); dim(allPopsCG0Yr1.5)
allPopsCG1Yr1.5<- subset(allPops, CG1.5 == 1);length(unique(allPopsCG1Yr1.5$Taxon))
allPopsCG2Yr1.5<- subset(allPops, CG1.5 == 2);length(unique(allPopsCG2Yr1.5$Taxon))
allPopsCG3Yr1.5<- subset(allPops, CG1.5 == 3);length(unique(allPopsCG3Yr1.5$Taxon))
allPopsCG4Yr1.5<- subset(allPops, CG1.5 == 4);length(unique(allPopsCG4Yr1.5$Taxon))
allPopsCG5Yr1.5<- subset(allPops, CG1.5 == 5);length(unique(allPopsCG5Yr1.5$Taxon))

# commonness assigned in relation to highest mean value over all years
allPopsCG0allYr<- subset(allPops, CGallYrs == 0);length(unique(allPopsCG0allYr$Taxon)); dim(allPopsCG0allYr)
allPopsCG1allYr<- subset(allPops, CGallYrs == 1);length(unique(allPopsCG1allYr$Taxon))
allPopsCG2allYr<- subset(allPops, CGallYrs == 2);length(unique(allPopsCG2allYr$Taxon))
allPopsCG3allYr<- subset(allPops, CGallYrs == 3);length(unique(allPopsCG3allYr$Taxon))
allPopsCG4allYr<- subset(allPops, CGallYrs == 4);length(unique(allPopsCG4allYr$Taxon))
allPopsCG5allYr<- subset(allPops, CGallYrs == 5);length(unique(allPopsCG5allYr$Taxon))

# high quality data with > 10 years of data (not used)
hqPops10yrCG5<- subset(hqPops10, CGYr1.0 == 5)
hqPops10yrCG4<- subset(hqPops10, CGYr1.0 == 4)
hqPops10yrCG3<- subset(hqPops10, CGYr1.0 == 3)
hqPops10yrCG2<- subset(hqPops10, CGYr1.0 == 2)
hqPops10yrCG1<- subset(hqPops10, CGYr1.0 == 1)
hqPops10yrCG0<- subset(hqPops10, CGYr1.0 == 0)



# put all in a alist 
all.data<-list(allPops = allPops,
							 allPopsCG0 = allPopsCG0,
							 allPopsCG1 = allPopsCG1,
							 allPopsCG2 = allPopsCG2,
							 allPopsCG3 = allPopsCG3,
							 allPopsCG4 = allPopsCG4,
							 allPopsCG5 = allPopsCG5, 
							 hqPops = hqPops,
							 hqPopsCG0 = hqPopsCG0,
							 hqPopsCG1 = hqPopsCG1,
							 hqPopsCG2 = hqPopsCG2,
							 hqPopsCG3 = hqPopsCG3,
							 hqPopsCG4 = hqPopsCG4,
							 hqPopsCG5 = hqPopsCG5 ,
							 allPopsCG0Yr1 = allPopsCG0Yr1,
							 allPopsCG1Yr1 = allPopsCG1Yr1,
							 allPopsCG2Yr1 = allPopsCG2Yr1,
							 allPopsCG3Yr1 = allPopsCG3Yr1,
							 allPopsCG4Yr1 = allPopsCG4Yr1,
							 allPopsCG5Yr1 = allPopsCG5Yr1, 
							 allPopsCG0Yr1.2 = allPopsCG0Yr1.2,
							 allPopsCG1Yr1.2 = allPopsCG1Yr1.2,
							 allPopsCG2Yr1.2 = allPopsCG2Yr1.2,
							 allPopsCG3Yr1.2 = allPopsCG3Yr1.2,
							 allPopsCG4Yr1.2 = allPopsCG4Yr1.2,
							 allPopsCG5Yr1.2 = allPopsCG5Yr1.2, 
							 allPopsCG0Yr1.5 = allPopsCG0Yr1.5,
							 allPopsCG1Yr1.5 = allPopsCG1Yr1.5,
							 allPopsCG2Yr1.5 = allPopsCG2Yr1.5,
							 allPopsCG3Yr1.5 = allPopsCG3Yr1.5,
							 allPopsCG4Yr1.5 = allPopsCG4Yr1.5,
							 allPopsCG5Yr1.5 = allPopsCG5Yr1.5, 
							 allPopsCG0allYr = allPopsCG0allYr,
							 allPopsCG1allYr = allPopsCG1allYr,
							 allPopsCG2allYr = allPopsCG2allYr,
							 allPopsCG3allYr = allPopsCG3allYr,
							 allPopsCG4allYr = allPopsCG4allYr,
							 allPopsCG5allYr = allPopsCG5allYr, 
							 hqPops10yrCG5 = hqPops10yrCG5,
							 hqPops10yrCG4 = hqPops10yrCG4,
							 hqPops10yrCG3 = hqPops10yrCG3,
							 hqPops10yrCG2 = hqPops10yrCG2,
							 hqPops10yrCG1 = hqPops10yrCG1,
							 hqPops10yrCG0 = hqPops10yrCG0
							 
)




# START OF LOOP 


for(i in 1 : 48){
	
	
	parameters[i,]
	taskID<- i
	
	print("model name:")
	print(parameters$model_name[taskID])
	
	
	# save start time to check total run time later
	startTime<- Sys.time()
	
	# grab needed dataframe from list 
	dat<- all.data[[as.character(parameters$input_file[taskID])]] ; dim(dat)
	
	# select realm (not necessary, because we only test terrestrial)
	rlm<- parameters$Realm[taskID]
	dat<- subset(dat, Realm == rlm)
	dim(dat)
	#str(dat)
	
	


source("C:\\Dropbox\\work\\2017 iDiv\\2018 insect biomass\\final-insect-diversity-trends2/addIndicies.r")
dat<- addIndiciesPops(dat)


# metric from task file
metric<- parameters$model_name[taskID]
metric


startTime<- Sys.time()

# select realm (not needed)
rlm<- parameters$Realm[taskID]
dat<- subset(dat, Realm == rlm)

# exclude censoring years according to subset 
cens<- parameters$subset[taskID]
dat<-  subset(dat, !indexYear %in% 0:cens)

dim(dat)








# exclude censoring years according to subset 
cens<- parameters$subset[taskID]
dat<-  subset(dat, !indexYear %in% 0:cens)

dim(dat)



#set priors
sd.res <- 3 * sd(log10(dat$Number+1), na.rm = T) 

vect<- dat$Number
hist(log(vect)) # we have non-integers and zeroes
sd.res<-	3 * sd(log10(vect+1), na.rm = T)

prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
fam <-  "tweedie" 

# add reverse indexing  
dat$E <- exp(8) # 
dat$iYear.scale <- scale(dat$iYear)
val <- range(dat$iYear)

plot.id.values <- unique(sort(dat$Plot_ID))
plot.id.inverse <- rep(NA, max(plot.id.values))
plot.id.inverse[plot.id.values] <- length(plot.id.values):1
dat$Plot_ID_rep <- plot.id.inverse[dat$Plot_ID]









# mode formula
formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
												"+f(TaxonPlot_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(TaxonPlot_4INLAs,iYear,model='iid', hyper = prior.prec)+ 
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
								 f(iYear, model = 'ou', 	
							 		values = seq(val[1], val[2]), 
							 		replicate = as.numeric(Plot_ID_rep), 
							 		hyper = list(theta1 = list(prior = 'pc.prec')))"
))
#print(formul)


# run INLA model with tweedie distribution 
model <- inla( formul,
							 family = 'tweedie',
							 control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))),
							 
							 control.compute = list(config = TRUE, 
							 											 dic=TRUE,
							 											 waic=TRUE, 
							 											 cpo = TRUE, 
							 											 openmp.strategy="huge"), 
							 control.inla = list( int.strategy="eb", # excluding this probably makes the model a bit faster, and more precise
							 											tolerance =  1e-08), 
							 control.predictor = list(link = 1) , verbose = F, 
							 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
							 num.threads = 4, #
							 data=dat)

fixed = model$summary.fixed
print(parameters$model_name[taskID])
print("done:")
print(Sys.time() - startTime)
print(fixed)

#assign(as.character(parameters$model_name[i]), model)

#save model file and summary
model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))
saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)



# extract posterior marginals and sample posteriors
n <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial",
								 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
marg<- n#rbind(m,n)

marg$y80<- marg$y
marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to everything below the 80% quantile 
marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
marg$y90<- marg$y
marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to everything below the 90% quantile 
marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
marg$y95<- marg$y
marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to everything below the 95% quantile 
marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA


marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))

saveRDS (marg, file =  marg_file)


#pull out dataset level random intercepts and slopes:
RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])

intercepts     <- model$summary.random$Datasource_ID_4INLA
slopes         <- model$summary.random$Datasource_ID_4INLAs
slopes_Location<-model$summary.random$Location_4INLAs
slopes_plot    <-model$summary.random$Plot_ID_4INLAs
names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for ED Fig 1 and 8
RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")

# add up fixed slope and random slopes

fx<-data.frame(Realm =  "Terrestrial", #
							 fixedSlp = model$summary.fixed$mean[2], 
							 fixedIntercept = (model$summary.fixed$mean[1]  ) )
RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  

rand_file <- file.path(output_dir, paste0(metric,"randomSlopes.rds"))

# save random effects for dataset level
saveRDS (RandEfDataset, file =  rand_file)




# pull out population level random slopes 
allRandEf <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm", "TaxonPlot_4INLAs",'Plot_ID_4INLAs', "Location_4INLAs" )])

slopes_study         <- model$summary.random$Datasource_ID_4INLAs
names(slopes_study)[2:ncol(slopes_study)] <- paste0("Study_Slope_", names(slopes_study)[2:ncol(slopes_study)])             # names for dataset slopes
allRandEf<- merge(allRandEf, slopes_study, by.x="Datasource_ID_4INLAs", by.y="ID", all = T )

slopes_Location<-model$summary.random$Location_4INLAs
names(slopes_Location)[2:ncol(slopes_Location)] <- paste0("Loc_slp_", names(slopes_Location)[2:ncol(slopes_Location)]) # names for Location slopes
allRandEf<- merge(allRandEf, slopes_Location, by.x="Location_4INLAs", by.y="ID", all = T )

slopes_plot    <-model$summary.random$Plot_ID_4INLAs
names(slopes_plot)[2:ncol(slopes_plot)]     <- paste0("Plot_slp_", names(slopes_plot)[2:ncol(slopes_plot)])        # names for plot slopes
allRandEf<- merge(allRandEf, slopes_plot, by.x="Plot_ID_4INLAs", by.y="ID", all = T )

slopes_spec <- model$summary.random$TaxonPlot_4INLAs
names(slopes_spec)[2:ncol(slopes_spec)] <- paste0("Spec_Slope_", names(slopes_spec)[2:ncol(slopes_spec)])             # names for dataset slopes
allRandEf<- merge(allRandEf, slopes_spec, by.x="TaxonPlot_4INLAs", by.y="ID", all = T )


All_rand_file <- file.path(output_dir, paste0(metric,"allRandomSlopes.rds"))

# save file
saveRDS (allRandEf, file =  All_rand_file)


}




# OLD POISSON MODELS #####
# not ideal for our data

output_dir <- "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/2026 Poisson reruns"

for(i in c(30,35, 36, 41,42, 47,48)){ # to do: .   1,2,13,14,19, 29,done. 20 failed (over 48h) nbinomial
	
	
	parameters[i,]
	taskID<- i
	
	print("model name:")
	print(parameters$model_name[taskID])
	
	
	
	# grab needed dataframe from list 
	dat<- all.data[[as.character(parameters$input_file[taskID])]] ; dim(dat)
	
	# select realm (not necessary, because we only test terrestrial)
	rlm<- parameters$Realm[taskID]
	dat<- subset(dat, Realm == rlm)
	dim(dat)
	#str(dat)
	
	
	
	
	source("C:\\Dropbox\\work\\2017 iDiv\\2018 insect biomass\\final-insect-diversity-trends2/addIndicies.r")
	
	dat<- addIndiciesPops(dat)
	
	
	# metric from task file
	metric<- parameters$model_name[taskID]
	metric
	
	
	startTime<- Sys.time()
	
	# select realm (not needed)
	rlm<- parameters$Realm[taskID]
	dat<- subset(dat, Realm == rlm)
	
	dim(dat)
	
	
	
	
	
	
	
	
	# exclude censoring years according to subset 
	cens<- parameters$subset[taskID]
	print(paste("sensoring:", cens))
	dat<-  subset(dat, !indexYear %in% 0:cens)
	
	dim(dat)
	
	
	
	#set priors for random effects
	sd.res <- 3 * sd(log(dat$Number+1), na.rm = T) 
	prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
	fam <-  "poisson" #parameters$family[taskID]
	
	# here's the magic addition from Harvard Rue, that will make it run in a fraction of the time:  
	
	dat$E <- exp(8) # 
	dat$iYear.scale <- scale(dat$iYear)
	val <- range(dat$iYear)
	
	plot.id.values <- unique(sort(dat$Plot_ID))
	plot.id.inverse <- rep(NA, max(plot.id.values))
	plot.id.inverse[plot.id.values] <- length(plot.id.values):1
	dat$Plot_ID_rep <- plot.id.inverse[dat$Plot_ID]
	
	
	
	
	
	
	
	
	
	# mode formula
	formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
													 "+f(TaxonPlot_4INLA,model='iid')+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(TaxonPlot_4INLAs,iYear,model='iid', hyper = prior.prec)+ 
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
								 f(iYear, model = 'ou', 	
							 		values = seq(val[1], val[2]), 
							 		replicate = as.numeric(Plot_ID_rep), 
							 		hyper = list(theta1 = list(prior = 'pc.prec')))"
	))
	#print(formul)
	
	
	# run INLA model
	model <- inla( formul,
								 family = 'Poisson',
								 #control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))),
									 offset = log(E),
							 
								 control.compute = list(config = TRUE, 
								 											 dic=TRUE,
								 											 waic=TRUE, 
								 											 cpo = TRUE, 
								 											 openmp.strategy="huge"), 
								 control.inla = list( int.strategy="eb", # shoudl be changed to default for final analysis
								 										 tolerance =  1e-08), 
								 control.predictor = list(link = 1) , verbose = F, 
								 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
								 num.threads = 4, #blas.num.threads = threads,
								  data=dat)
	
	fixed = model$summary.fixed
	print(parameters$model_name[taskID])
	print("done:")
	print(Sys.time() - startTime)
	print(fixed)
	
	#assign(as.character(parameters$model_name[i]), model)
	
	#save model file and summary
	model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
	fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))
	saveRDS (model, file =  model_file)
	saveRDS (fixed, file = fixed_file)
	
	
	
	# extract posterior marginals and sample posteriors
	n <-  data.frame(Metric = metric, 
									 Realm = "Terrestrial",
									 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
	marg<- n#rbind(m,n)
	
	marg$y80<- marg$y
	marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to everything below the 80% quantile 
	marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
	marg$y90<- marg$y
	marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to everything below the 90% quantile 
	marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
	marg$y95<- marg$y
	marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to everything below the 95% quantile 
	marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA
	
	
	marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	
	saveRDS (marg, file =  marg_file)
	
	
	#pull out dataset level random intercepts and slopes:
	RandEfDataset <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm")])
	
	intercepts     <- model$summary.random$Datasource_ID_4INLA
	slopes         <- model$summary.random$Datasource_ID_4INLAs
	slopes_Location<-model$summary.random$Location_4INLAs
	slopes_plot    <-model$summary.random$Plot_ID_4INLAs
	names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
	names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
	names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
	names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes
	
	# datasource level slopes for ED Fig 1 and 8
	RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
	RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_ID_4INLAs", by.y="ID")
	
	# add up fixed slope and random slopes
	
	fx<-data.frame(Realm =  "Terrestrial", #
								 fixedSlp = model$summary.fixed$mean[2], 
								 fixedIntercept = (model$summary.fixed$mean[1]  ) )
	RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
	RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
	
	rand_file <- file.path(output_dir, paste0(metric,"randomSlopes.rds"))
	
	# save random effects for dataset level
	saveRDS (RandEfDataset, file =  rand_file)
	
	
	
	
	# pull out population level random slopes 
	allRandEf <- 	unique(dat[,c("Datasource_ID", "Datasource_name", "Datasource_ID_4INLA", "Datasource_ID_4INLAs", "Realm", "TaxonPlot_4INLAs",'Plot_ID_4INLAs', "Location_4INLAs" )])
	
	slopes_study         <- model$summary.random$Datasource_ID_4INLAs
	names(slopes_study)[2:ncol(slopes_study)] <- paste0("Study_Slope_", names(slopes_study)[2:ncol(slopes_study)])             # names for dataset slopes
	allRandEf<- merge(allRandEf, slopes_study, by.x="Datasource_ID_4INLAs", by.y="ID", all = T )
	
	slopes_Location<-model$summary.random$Location_4INLAs
	names(slopes_Location)[2:ncol(slopes_Location)] <- paste0("Loc_slp_", names(slopes_Location)[2:ncol(slopes_Location)]) # names for Location slopes
	allRandEf<- merge(allRandEf, slopes_Location, by.x="Location_4INLAs", by.y="ID", all = T )
	
	slopes_plot    <-model$summary.random$Plot_ID_4INLAs
	names(slopes_plot)[2:ncol(slopes_plot)]     <- paste0("Plot_slp_", names(slopes_plot)[2:ncol(slopes_plot)])        # names for plot slopes
	allRandEf<- merge(allRandEf, slopes_plot, by.x="Plot_ID_4INLAs", by.y="ID", all = T )
	
	slopes_spec <- model$summary.random$TaxonPlot_4INLAs
	names(slopes_spec)[2:ncol(slopes_spec)] <- paste0("Spec_Slope_", names(slopes_spec)[2:ncol(slopes_spec)])             # names for dataset slopes
	allRandEf<- merge(allRandEf, slopes_spec, by.x="TaxonPlot_4INLAs", by.y="ID", all = T )
	
	
	All_rand_file <- file.path(output_dir, paste0(metric,"allRandomSlopes.rds"))
	
	# save file
	saveRDS (allRandEf, file =  All_rand_file)
	
	
}























#  TRASH  #####



# choose data
dat <- hqPopsCG5 ; dim(dat); subs <- "hqPopsCG5"
dat <- hqPopsCG4 ; dim(dat); subs <- "hqPopsCG4"
dat <- hqPopsCG3 ; dim(dat); subs <- "hqPopsCG3"
dat <- hqPopsCG2 ; dim(dat); subs <- "hqPopsCG2"

# for all data based on yr 1 (Fig 4 in paper)
dat<- allPopsCG5; dim(dat); subs<- "allPopsCG5"
dat<- allPopsCG4; dim(dat); subs<- "allPopsCG4"
dat<- allPopsCG3; dim(dat); subs<- "allPopsCG3"
dat<- allPopsCG2; dim(dat); subs<- "allPopsCG2"
dat<- allPopsCG1; dim(dat); subs<- "allPopsCG1"



# random<- sample(unique(dat$Taxon), 100)
# random
# for reproducibility take this sample. 10 randm plots 
#dat<- subset(dat, Taxon %in% random); dim(dat)

#poisson model used in paper #####
# timer
startTime<- Sys.time()
# improved model
modelPois <- inla( Number ~ cYear + 
									 	f(TaxonPlot_4INLA,             model = "iid", hyper = prior.prec) + 
									 	f(Plot_ID_4INLA,               model = "iid", hyper = prior.prec) + 
									 	f(Location_4INLA, 	           model = "iid", hyper = prior.prec) + 
									 	f(Datasource_ID_4INLA,         model = "iid", hyper = prior.prec) +
									 	f(TaxonPlot_4INLAs,     iYear, model = "iid", hyper = prior.prec) + 
									 	f(Plot_ID_4INLAs,	      iYear, model = "iid", hyper = prior.prec) + 
									 	f(Location_4INLAs,      iYear, model = "iid", hyper = prior.prec) + 
									 	f(Datasource_ID_4INLAs, iYear, model = "iid", hyper = prior.prec) + 
									 	f(iYear, model = "ou", 	
									 		values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou. it makes a small difference
									 		replicate = as.numeric(Plot_ID_rep), 
									 		hyper = list(theta1 = list(prior = "pc.prec"))),
									 family = 'Poisson',
									 offset = log(E),
									 control.compute = list(config = TRUE, 
									 											 dic=TRUE,
									 											 waic=TRUE, 
									 											 cpo = TRUE, 
									 											 openmp.strategy="huge"), 
									 control.inla = list( tolerance =  1e-08), 
									 control.predictor = list(link = 1) , 
									 verbose = T, 
									 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
									 num.threads = threads, 
									 #		 control.fixed = list(prec = 10, prec.intercept = 0.1), # prior added
									 #blas.num.threads = threads,
									 data=dat)

Sys.time()
print("done:")
print(Sys.time() - startTime)

modelPois$summary.fixed

parameters$model_name[taskID]

q_poisPop <- modelPois$summary.fixed#["cYear", c("mean","0.025quant", "0.975quant")]
q_poisPercPop <- (exp(q_poisPop) - 1) * 100

# save summary of fixed effects
fixed_file <- file.path(output_dir, paste("Poisson",subs,"SUMMARY.rds",sep="_"))
fixed_file
saveRDS(q_poisPop, file = fixed_file)



# ggplot(dat, aes (y = Number, x = Year, color = Taxon))+
# 	geom_line()+
# 	scale_y_log10()+
# 	facet_wrap(.~Plot_name)



#assign(as.character(parameters$model_name[i]), model)


startTime<- Sys.time()
# improved model
modelTwee <- inla( Number ~ cYear + 
									 	f(TaxonPlot_4INLA,             model = "iid", hyper = prior.prec) + 
									 	f(Plot_ID_4INLA,               model = "iid", hyper = prior.prec) + 
									 	f(Location_4INLA, 	           model = "iid", hyper = prior.prec) + 
									 	f(Datasource_ID_4INLA,         model = "iid", hyper = prior.prec) +
									 	f(TaxonPlot_4INLAs,     iYear, model = "iid", hyper = prior.prec) + 
									 	f(Plot_ID_4INLAs,	      iYear, model = "iid", hyper = prior.prec) + 
									 	f(Location_4INLAs,      iYear, model = "iid", hyper = prior.prec) + 
									 	f(Datasource_ID_4INLAs, iYear, model = "iid", hyper = prior.prec) + 
									 	f(iYear, model = "ou", 	
									 		values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou. it makes a small difference
									 		replicate = as.numeric(Plot_ID_rep), 
									 		hyper = list(theta1 = list(prior = "pc.prec"))),
									 family = 'tweedie',
									 control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))),
									 offset = log(E),   #Rue's addition , no clue wat it does
									 control.compute = list( 
									 	config = TRUE, 
									 	dic=TRUE,
									 	waic=TRUE, 
									 	cpo = TRUE ), 
									 control.predictor = list(link = 1) , 
									 verbose = F, 
									 quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
									 num.threads = 4, 
									 #		 	control.fixed = list(prec = 10, prec.intercept = 0.1), # the prior on the slope in combination with the scaled year are 
									 data=dat)


print("done:")
print(Sys.time() - startTime)

modelTwee$summary.fixed

beepr::beep()
q_tweediePop <- modelTwee$summary.fixed#["cYear", c("mean","0.025quant", "0.975quant")]
q_tweediePercPop <- (exp(q_tweediePop) - 1) * 100

# #these coefficient are in log, so scale coefficient to log10
# cols_to_scale <- c("mean", "sd", "0.001quant", "0.01quant",  "0.025quant",   "0.05quant",
# 									 "0.1quant", "0.3quant", "0.5quant",   "0.7quant", "0.9quant",   "0.95quant",  
# 									 "0.975quant",   "0.99quant",  "0.999quant", "mode")
# log10_scale <- (model4$summary.fixed)
# log10_scale[, cols_to_scale] <- log10_scale[, cols_to_scale]/log(10)
# (10^(log10_scale) - 1) * 100 # should be the same! but is not


# save summary of fixed effects
fixed_file <- file.path(output_dir, paste("tweedie",subs,"SUMMARY.rds",sep="_"))
fixed_file
saveRDS(q_tweediePop, file = fixed_file)




# compare to old results: 
setwd("C:\Dropbox\Insect Biomass Trends\2020 Taxonomic paper\model outputs richness paper")

OldPop2sum <- as.data.frame(readRDS("C:/Dropbox/Insect Biomass Trends/2020 Taxonomic paper/model outputs richness paper/inlaPop2T1SUMMARY.rds"))
OldPop2sum
tweediePop2sum<- as.data.frame(readRDS("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/populationModels/tweedie_allPopsCG2_SUMMARY.rds"))
tweediePop2sum

OldPop3sum <- as.data.frame(readRDS("C:/Dropbox/Insect Biomass Trends/2020 Taxonomic paper/model outputs richness paper/inlaPop3T1SUMMARY.rds"))
OldPop3sum
tweediePop3sum<- as.data.frame(readRDS("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/populationModels/tweedie_allPopsCG3_SUMMARY.rds"))
tweediePop3sum


OldPop4sum <- as.data.frame(readRDS("C:/Dropbox/Insect Biomass Trends/2020 Taxonomic paper/model outputs richness paper/inlaPop4T1SUMMARY.rds"))
OldPop4sum
tweediePop4sum<- as.data.frame(readRDS("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/populationModels/tweedie_allPopsCG4_SUMMARY.rds"))
tweediePop4sum

Oldpop5sum <- as.data.frame(readRDS("C:/Dropbox/Insect Biomass Trends/2020 Taxonomic paper/model outputs richness paper/inlaPop5T1SUMMARY.rds"))[ 2, ]
Oldpop5sum
tweediePop5sum<- as.data.frame(readRDS("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/populationModels/tweedie_allPopsCG4_SUMMARY.rds"))
tweediePop5sum
poissonPop5Sum <- 
	
	q_tweediePop
pop5Marg<- readRDS("inlaPop5T1Marginal.rds" )


