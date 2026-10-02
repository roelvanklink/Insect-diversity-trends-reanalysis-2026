suppressPackageStartupMessages( library (INLA))
INLA:::inla.dynload.workaround()



library(tidyverse)
library(reshape2)
parameters<- read.csv("C:/Dropbox/work/2017 iDiv/2018 insect biomass/final-insect-diversity-trends2/sensitivityContinent.csv", stringsAsFactors = F)


args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

load("C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2023pure.RData") 

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

completeData2023pure<- completeData2023pure[!completeData2023pure$Datasource_ID %in% exptDatasources, ]
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% exptPlots, ]

# exclude plots with a trend in taxonomic resolution: 
bad_tax<- 
	c(849L, 132L, 1442L, 1527L, 10001L, 10003L, 10005L, 10006L, 10007L, 10008L, 10009L, 10011L, 10012L, 10015L, 10016L, 10017L, 10018L, 
		10019L, 10021L, 10024L, 10025L, 10026L, 10028L, 10029L, 10032L, 10033L, 10034L, 10035L, 10036L, 10038L, 10039L, 10044L, 10048L, 
		10049L, 10051L, 10052L, 10056L, 10057L, 10058L, 10059L, 10063L, 10066L, 10067L, 10068L, 10070L, 10072L, 10075L, 10079L, 10080L, 
		10081L, 10082L, 10084L, 10085L, 10087L, 10092L, 10093L, 10097L, 10099L, 10106L, 10108L, 10113L, 10114L, 10115L, 10119L, 10124L, 
		10125L, 10127L, 10128L, 10130L, 10135L, 10137L, 10138L, 10145L, 10146L, 10147L, 10149L, 10158L, 10160L, 10163L, 10164L, 10165L, 
		10168L, 10169L, 10170L, 10171L, 10173L, 10175L, 10178L, 10180L, 10181L, 10182L, 10185L, 10187L, 10188L, 10189L, 10192L, 10193L, 
		10195L, 10196L, 10198L, 10349L, 10200L, 10201L, 10204L, 10206L, 10208L, 10212L, 10215L, 10216L, 10219L, 10221L, 10223L, 10224L, 
		10225L, 10226L, 10227L, 10229L, 10230L, 10233L, 10236L, 10249L, 10250L, 10252L, 10253L, 10254L, 10255L, 10256L, 10257L, 10258L, 
		10259L, 10277L, 10279L, 10280L, 10281L, 10282L, 10283L, 10284L, 10285L, 10286L, 10287L, 10289L, 10290L, 10291L, 10292L, 10294L, 
		10295L, 10297L, 10298L, 10299L, 10301L, 10302L, 10303L, 10304L, 10305L, 10306L, 10307L, 10308L, 10309L, 10311L, 10312L, 10313L, 
		10314L, 10315L, 10316L, 10317L, 10318L, 10320L, 10321L, 10322L, 10323L, 10325L, 10326L, 10327L, 10328L, 10329L, 10331L, 10332L, 
		10334L, 10335L, 10336L, 10337L, 10338L, 10339L, 10340L, 10342L, 10343L, 10344L, 10346L, 10348L, 10469L, 10350L, 10351L, 10352L, 
		10353L, 10354L, 10355L, 10356L, 10357L, 10361L, 10362L, 10364L, 10365L, 10366L, 10368L, 10369L, 10370L, 10371L, 10373L, 10374L, 
		10375L, 10376L, 10378L, 10379L, 10380L, 10381L, 10382L, 10383L, 10384L, 10385L, 10386L, 10387L, 10388L, 10389L, 10390L, 10391L, 
		10392L, 10393L, 10394L, 10395L, 10398L, 10399L, 10400L, 10401L, 10403L, 10404L, 10407L, 10408L, 10409L, 10410L, 10412L, 10413L, 
		10414L, 10415L, 10416L, 10417L, 10418L, 10419L, 10421L, 10422L, 10424L, 10425L, 10426L, 10427L, 10428L, 10429L, 10430L, 10431L, 
		10432L, 10433L, 10434L, 10435L, 10436L, 10437L, 10438L, 10439L, 10475L, 10440L, 10441L, 10442L, 10443L, 10444L, 10445L, 10446L, 
		10448L, 10449L, 10485L, 10452L, 10454L, 10455L, 10457L, 10458L, 10459L, 10460L, 10463L, 10464L, 10465L, 10467L, 10468L, 10470L, 
		10474L, 10499L, 10491L, 10490L, 10498L, 10489L, 10506L, 10503L, 10511L, 10504L, 10500L, 10501L)

completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% bad_tax, ]

dim(completeData2023pure)


completeData2023pure$pltYr<- paste0(completeData2023pure$Plot_ID, "_", completeData2023pure$Year) # unique ID for year/plot combination fo that we exclude some years 

startEndYears<- completeData2023pure%>% 
	group_by(Plot_ID) %>%
	summarise(
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T), 
		nYrs = length(unique(Year)),
	)
startEndYears$pltStartYr<- paste0(startEndYears$Plot_ID, "_", startEndYears$Start_year)


# rename 
completeData2023<- completeData2023pure
completeData2023$Unit[completeData2023$Unit == "richness"   & completeData2023$Datasource_ID == 1394  ] <- "rarefiedRichness"


# try removing Africa to prevent crash
#completeData2023<- subset(completeData2023, Continent != "Africa") # doesn't work 



# assign random slope and random intercept parameters # 2026: I don't think this is used
#completeData2023$Continent_4INLA <- as.numeric(factor(completeData2023$Continent))
#completeData2023$Continent_4INLAs <- completeData2023$Continent_4INLA + max(completeData2023$Continent_4INLA)

# next try (6.1.21) put in continent as fixed effect, and  
completeData2023$Continent2<- completeData2023$Continent
completeData2023$Continent2[completeData2023$Continent2 == "Oceania"] <- "Rest"
completeData2023$Continent2[completeData2023$Continent2 == "Asia"] <- "Rest"
completeData2023$Continent2[completeData2023$Continent2 == "Latin America"] <- "Rest"
completeData2023$Continent2[completeData2023$Continent2 == "Africa"] <- "Rest"

unique(completeData2026$Continent)
centr<- subset(completeData2026,Continent == "Latin America")
unique(centr$Datasource_name)
completeData2026$Continent[completeData2026$Continent == "Latin America"] <- "South America"
completeData2026$Continent[completeData2026$Datasource_name == "Panama homoptera Wolda"] <- "Central America"
completeData2026$Continent[completeData2026$Datasource_name ==  "LTER Luquillo canopy1"  ] <- "Central America"



cDabund2026 <- subset(completeData2026, Unit == "abundance"); dim(cDabund2026)
cDrichness <- subset(completeData2023, Unit == "richness"); dim(cDrichness)
cDabund    <- subset(completeData2023, Unit == "abundance"); dim(cDabund)
cDenspie   <- subset(completeData2023, Unit == "ENSPIE")   ;dim(cDenspie)
cDenspie$Number[is.infinite(cDenspie$Number)] <- 0





all.df<-list(cDrichness,
						 cDabund,
						 cDabund2026,
						 cDenspie
)
names(all.df)<- c("cDrichness",  "cDabund", "cDabund2026" , "cDenspie" )



taskID

for(i in c(1:6)){
	
	
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
	))
	
	
	#print(formul)
	
	
	
	print(startTime)
	model <- inla( formul,
								 family = fam,  
								 # control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))),
								 
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
	
	
	
	marg<-lapply(model$marginals.fixed, FUN = inla.smarginal,  factor = 50)
	
	
	
	
	marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	
	saveRDS (marg, file = marg_file)
	
	
	# extract posterior marginals and sample posteriors
	# n <-  data.frame(Metric = metric, 
	# 								 Realm = "Terrestrial",
	# 								 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
	# marg<- n
	# 
	# marg$y80<- marg$y
	# marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
	# marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
	# marg$y90<- marg$y
	# marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
	# marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
	# marg$y95<- marg$y
	# marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
	# marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA
	# 
	# # save marginal
	# marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	# saveRDS (marg, file =  marg_file)
	# 
	# 
	
}



# Tweedie model loop #####
output_dir  <- output_dir<- ("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/Tweedie models/")

for(i in c(14)){
	
	
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
	
	
	
	marg<-lapply(model$marginals.fixed, FUN = inla.smarginal,  factor = 50)
	
	
	
	
	marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	
	saveRDS (marg, file = marg_file)
	
	
	# extract posterior marginals and sample posteriors
	# n <-  data.frame(Metric = metric, 
	# 								 Realm = "Terrestrial",
	# 								 inla.smarginal(model$marginals.fixed$`cYear`, factor = 50))
	# marg<- n
	# 
	# marg$y80<- marg$y
	# marg$y80  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
	# marg$y80  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.9quant"] ]<- NA
	# marg$y90<- marg$y
	# marg$y90  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
	# marg$y90  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.95quant"] ]<- NA
	# marg$y95<- marg$y
	# marg$y95  [ marg$Realm == "Terrestrial" & marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
	# marg$y95  [ marg$Realm == "Terrestrial" & marg$x> fixed["cYear", "0.975quant"] ]<- NA
	# 
	# # save marginal
	# marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))
	# saveRDS (marg, file =  marg_file)
	# 
	# 
	
}


fixed[8:16,]
# make graph for 2026 data

# count datasets
datasets <-  aggregate(cDabund2026, Datasource_ID~Continent, function(x){length(unique(x))})
datasets[7,2]<- paste('number of datasets:\n', datasets[7,2])

fixed$Continent <- c("Africa", "Asia",  "Central America", "Europe",         
										 "North America" ,  "Oceania" , "South America")

brks<- c(-0.04, -0.03, -0.02, -0.01, 0,  0.01, 0.02, 0.03, 0.04)
perc<-(exp(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

contplot2026<- ggplot(fixed[8:14,])+
	geom_errorbar(aes(x=Continent,ymin=`0.025quant`,ymax=`0.975quant`), alpha = 0.5,
								linewidth = 1, width=0, position=position_dodge(width= 0.7), color = "grey50")+  
	 geom_errorbar(aes(x=Continent,ymin=`0.05quant`,ymax=`0.95quant`), alpha = 0.75,
	 							linewidth = 2,width=0, position=position_dodge(width= 0.7), color = "grey50")+  
	 geom_errorbar(aes(x=Continent,ymin=`0.1quant`,ymax= `0.9quant`),
	 							linewidth = 3, width=0, position=position_dodge(width= 0.7), color = "grey50")+  
	geom_point(aes(x=Continent,   y=mean), shape = 16, color = "black", fill = "black",  alpha=1,
						 size = 2.5, position=  position_dodge(width = 0.7))+
	coord_flip()+
	xlab ("")+ ylab("Trend slope  \n % change per year")+ #
	geom_hline(yintercept=0,linetype="dashed")+
	geom_text(aes(y = 0.1, x = Continent, label = Datasource_ID), data = datasets, size = 2.5)+
	#	geom_text(aes(x = X1 , y = 0.028, label = text), size = 3) +
	#	geom_text(aes(x = X1 , y = 0.037, label = ptxt), size = 2.5) +
	scale_y_continuous(breaks = brks,labels = l)+#, limits=c(-0.050,  0.08))+
theme_classic()+
	theme(#axis.text.y=element_blank(),
				#axis.ticks.y=element_blank(), 
				legend.key=element_blank(), 
				legend.position="none", 
				strip.text.y.left = element_text(size=10, angle=90, hjust = 1),
				strip.background = element_rect(colour = "white")
	)
contplot2026

datasets$percChange<- (exp(fixed$mean[8:14] )  *100) - 100
datasets$percChange10<- (exp(fixed$`0.1quant` [8:14] )  *100) - 100
datasets$percChange90<- (exp(fixed$`0.9quant` [8:14] )  *100) - 100


















# OLD #############
print("model name:")
metric<- parameters$model_name[taskID]
metric

startTime<- Sys.time()

dat<- all.df[[as.character(parameters$input_file[taskID])]] ; dim(dat)
rlm<- parameters$subset[taskID]
dat<- subset(dat, Realm == rlm)
dim(dat)
#str(dat)

# set priors
vect<- dat[[parameters$yForPrior[taskID]]]

if(parameters$family[taskID] =="gaussian"){
	sd.res<-	3 * sd(log10(vect+1), na.rm = T)
}
if(parameters$family[taskID] =="beta"){
	sd.res<-	3 *sd(boot::logit(vect)[boot::logit(vect) != Inf & boot::logit(vect) != -Inf ], na.rm = T)
}


prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1
fam <-   parameters$family[taskID]

# replace 1 with .999999, otherwise beta regression will crash 
if(fam == "beta"){ 
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==1 & !is.na(dat[, parameters$y[taskID]])]<- 1-0.000001
	dat[, parameters$y[taskID]][dat[, parameters$y[taskID]] ==0 & !is.na(dat[, parameters$y[taskID]])]<- 0+0.000001
} 




formul<-as.formula(paste(parameters$y[taskID], " ~ ", parameters$model_formula[taskID]   ,
												 "+f(Period_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Location_4INLA,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Location_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iYear,model='iid', hyper = prior.prec)+
                         f(iYear, model='ou',  replicate=as.numeric(Plot_ID_4INLA), 
			hyper = list(theta1 = list(prior='pc.prec')))" 
))


print(formul)

model <- inla( formul,
							 family = fam,							 
							 control.compute = list(config = TRUE, 
							 											 dic=TRUE,
							 											 waic=TRUE, 
							 											 cpo = TRUE), 
							 #control.inla = list(h = 0.484761), 
							 control.predictor = list(link = 1) , verbose = F, 
							 quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,    
							 data=dat)

fixed <- model$summary.fixed


parameters$model_name[taskID]
print("done after:")
Sys.time() - startTime
#assign(as.character(parameters$model_name[i]), model)

model_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"TEST.rds"))
fixed_file <- file.path(output_dir, paste0(as.character(parameters$model_name[taskID]),"SUMMARY.rds"))



saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)





# extract posterior marginals and sample posteriors


marg<-lapply(model$marginals.fixed, FUN = inla.smarginal,  factor = 50)




marg_file <- file.path(output_dir, paste0(metric,"Marginal.rds"))

saveRDS (marg, file = marg_file)

