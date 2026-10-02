library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(data.table)
library(reshape2)
# set theme and color scheme for figures	
theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
																	 panel.grid.minor = element_blank(),
																	 panel.background = element_blank(), 
																	 axis.line = element_line(colour = "black") , 
																	 legend.key=element_blank())

shps<- c("Freshwater" = 24, "Terrestrial" = 21 )#, "Both realms" = 22)
col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #
sz = 0.5
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
col.scheme.realm2 <- c(  "Freshwater"  = "dodgerblue2", "Freshwater2"  = "dodgerblue2",  "Terrestrial" = "chocolate4", "Terrestrial2" = "chocolate4")
lntps<-  c("Terrestrial" = 2,  "Freshwater" = 3)

source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate metrics.R")
setwd("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models") # work
taxa<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/taxa5.2.csv"); dim(taxa)

load("C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2023pure.Rdata"); dim(completeData2023pure) # 

# exclude plots with a trend in taxonomic resolution: 

dim(completeData2023pure)

# exclude experimental data: 
# exclude experimental sites
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

completeData2023pure<- subset(completeData2023pure, Datasource_ID != 1353 & Datasource_ID != 1402 ) 
# remove 1 Portal ant dataset and russia springtails

# exclude plots with a trend in taxonomic resolution:
bad_tax<- 
	c(849L, 132L, 1442L, 1527L)

completeData2023pure<- completeData2023pure[!completeData2023pure$Datasource_ID %in% exptDatasources, ]
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% exptPlots, ]; dim(completeData2023pure)
completeData2023pure<- completeData2023pure[!completeData2023pure$Plot_ID %in% bad_tax, ]; dim(completeData2023pure)


plots<-as.data.frame(fread( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv")); dim(plots)
UKfwPlots<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots); dim(plots)

figure_path<- "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models/figures"
completeData2023<- completeData2023pure




# Making needed objects to test influence of short and long timeseries #######

# cut everything down to last 10 years, exclude everything that can't be cut 

# what's the cutoff year for each plot? 
metadata_per_plotZ<- completeData2023pure %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_ID = unique(Plot_ID),
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Realm = unique(Realm),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		cutoffYear = max(Year, na.rm = T)-10
	)

completeData2023pure<-   merge(completeData2023pure, metadata_per_plotZ )

completeData2023pureShort <-subset(completeData2023pure, Year> cutoffYear); dim(completeData2023pureShort)
# 
metadata_per_plotZ1<- completeData2023pureShort %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_ID = unique(Plot_ID),
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Realm = unique(Realm),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T),
		cutoffYear = max(Year, na.rm = T)-10
	) # correct 

metadata_per_plotZ1<- subset(completeData2023pureShort, !is.na(Number)) %>% 
	group_by(Plot_ID) %>%
	summarise(
		Plot_ID = unique(Plot_ID),
		Datasource_ID = unique(Datasource_ID),
		Datasource_name = unique(Datasource_name), 
		Realm = unique(Realm),
		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		Number_of_years_data = length(unique(Year)), 
		Start_year = min(Year, na.rm = T),
		End_year = max(Year, na.rm = T)
	) # correct 


# make the needed datasets
metadata_per_plotZ2<- subset(metadata_per_plotZ1, Duration >=10)
dim(metadata_per_plotZ2) # 1010 plots
length(unique(metadata_per_plotZ2$Datasource_ID)) # 107 datasets

# now exclude all datasets that are not long enough
completeData2023Short <-subset(completeData2023pure, Plot_ID %in% metadata_per_plotZ2$Plot_ID)
saveRDS(completeData2023Short, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2023short.rds")



# Long: keep only 20+ years data #
metadata_per_plotZ3<- subset(metadata_per_plotZ, Duration > 20); dim(metadata_per_plotZ3)#549
length(unique(metadata_per_plotZ3$Datasource_ID)) # 66 datasets

#hypothetically 30 yr cutoff (not used)
metadata_per_plotZ4<- subset(metadata_per_plotZ, Duration > 30); dim(metadata_per_plotZ4)#148
length(unique(metadata_per_plotZ4$Datasource_ID)) # 29 datasets



completeData2023long <-subset(completeData2023pure, Plot_ID %in% metadata_per_plotZ3$Plot_ID); dim(completeData2023long)
saveRDS(completeData2023long, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/completeData2023long.rds")


# After analysis in an HPC, this will create figure ED2
# ED Fig 2 short & long####
inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
richMarg<- readRDS("inlaRichnessTMarginal.rds")
richMarg$Metric<- "Richness"
richMarg$Data<- "All" 

inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
abMarg<- readRDS("inlaAbunTMarginal.rds" )
abMarg$Metric <- "Abundance"
abMarg$Data<- "All"

inlapieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
pieMarg<- readRDS("inlaENSPIETMarginal.rds" )
pieMarg$Metric<- "Simpson diversity (ENS)"
pieMarg$Data<- "All"

inlaRichLongSum<- as.data.frame(readRDS("inlaRichLongTSUMMARY.rds"))
inlaRichLongMarg<- readRDS("inlaRichLongTMarginal.rds" )
inlaRichLongMarg$Metric<- "Richness"
inlaRichLongMarg$Data<- "20 years"

inlaAbLongSum<- as.data.frame(readRDS("inlaAbundLongTSUMMARY.rds"))
inlaAbLongMarg<- readRDS("inlaAbundLongTMarginal.rds" )
inlaAbLongMarg$Metric<- "Abundance"
inlaAbLongMarg$Data<- "20 years"

inlaPieLongSum<- as.data.frame(readRDS("inlaENSPIELongTSUMMARY.rds"))
inlaPieLongMarg<- readRDS("inlaENSPIELongTMarginal.rds" )
inlaPieLongMarg$Metric<- "Simpson diversity (ENS)"
inlaPieLongMarg$Data<- "20 years"

inlaRichShortSum<- as.data.frame(readRDS("inlaRichShortTSUMMARY.rds"))
inlaRichShortMarg<- readRDS("inlaRichShortTMarginal.rds" )
inlaRichShortMarg$Metric<- "Richness"
inlaRichShortMarg$Data<- "10 years"

inlaAbShortSum<- as.data.frame(readRDS("inlaAbundShortTSUMMARY.rds"))
inlaAbShortMarg<- readRDS("inlaAbundShortTMarginal.rds" )
inlaAbShortMarg$Metric<- "Abundance"
inlaAbShortMarg$Data<- "10 years"

inlaPieShortSum<- as.data.frame(readRDS("inlaENSPIEShortTSUMMARY.rds"))
inlaPieShortMarg<- readRDS("inlaENSPIEShortTMarginal.rds" )
inlaPieShortMarg$Metric<- "Simpson diversity (ENS)"
inlaPieShortMarg$Data<- "10 years"

# bind needed objects together
univar<- rbind(abMarg, richMarg, pieMarg,  inlaAbLongMarg, inlaRichLongMarg, inlaPieLongMarg, 
							 inlaAbShortMarg, inlaRichShortMarg, inlaPieShortMarg)
univar$Metric<- factor(univar$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
univar$Data<- factor(univar$Data, levels = c ("All", "20 years", "10 years" ))

# remove very low values (which lead to very long tails)
univar<- subset(univar, y> 1)

brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

# new labels
data_labs<- c( "All",  ">19 years data", "Last 10 years data" )
names(data_labs)<- c("All", "20 years", "10 years")

mean <- univar %>%
	group_by(Realm, Metric ,Data) %>%
	filter(y == max(y, na.rm=TRUE))

# make fig ED 2
ggplot(univar, aes(x = x, y = y))+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	geom_vline(data= subset(mean, Realm == 'Terrestrial'), aes(xintercept = x), linetype= 'dotted')+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	facet_grid(cols = vars(Metric), rows = vars(Data), scales = "free_x", 
						 labeller = labeller(Data = data_labs))+ #, switch = "y"
	ylab ("Density")+  xlab("Trend slope  \n % change per year")+
	scale_linetype_manual(values = lntps)+
	scale_fill_manual(values = "grey50")+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.02,  0.012))+
	
	theme_classic()+
	theme(axis.text.y=element_blank(),
				axis.ticks.y=element_blank(), 
				legend.key=element_blank(), 
				legend.position="none")

ggsave(filename = "Van Klink ED Fig 2 timeseries length.jpg" , path = figure_path, width = 15, height = 15,  units = "cm",dpi = 300, device = "jpg")
ggsave(filename = "Van Klink ED Fig 2 timeseries length.pdf" , path = figure_path, width = 15, height = 15,  units = "cm",dpi = 300, device = "pdf")
ggsave(filename = "Van Klink ED Fig 2 timeseries length.png" , path = figure_path, width = 15, height = 15,  units = "cm",dpi = 300, device = "png")








#  Create needed objects to test effect of large datasets (large number of plots):  #####


metadata_per_dataset<-  completeData2023pure %>% 
	     group_by(Datasource_ID) %>%
	     summarise(
		         Datasource_ID = unique(Datasource_ID),
		         Datasource_name = unique(Datasource_name), 
		         Number_of_metrics = length(unique(Unit)),
		         Realm = unique(Realm),
		         Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
		         Number_of_years_data = length(unique(Year)), 
		         Start_year = min(Year, na.rm = T),
		         End_year = max(Year, na.rm = T),
		         Number_of_plots =  length(unique(Plot_ID)), # should be 1
		         Number_of_samples = length(unique(paste(Year, Period))),
		         Continent = unique(Continent), 
		         Country_State = unique(Country_State),
		         Country = unique(Country),
		         Region = unique(Region),
		         Realm = unique(Realm)
		 	     )
 ggplot(metadata_per_dataset, aes(x = Datasource_name , y = Number_of_plots))+
	          geom_point()+ coord_flip()+ facet_wrap(.~Realm)
 hist(metadata_per_dataset$Number_of_plots)
 median(metadata_per_dataset$Number_of_plots)#
 mean(metadata_per_dataset$Number_of_plots) # 

 #visualize how many plots there are in datasets 
 ggplot(subset(metadata_per_dataset, Number_of_plots >5),
 			 aes(x = Datasource_name , y = Number_of_plots))+
 	geom_point()+ coord_flip()+ facet_wrap(.~Realm)+
 scale_y_log10()+
 		geom_hline(yintercept = 20)+# 
 geom_hline(yintercept = 50)+
 geom_hline(yintercept = 10)
 
 
 nrow(subset(metadata_per_dataset, Number_of_plots >10)) # 
 nrow(subset(metadata_per_dataset, Number_of_plots >50)) # 
 
 metadata_per_plot<-  completeData2023pure %>% 
 	group_by(Plot_ID) %>%
 	summarise(
 		Datasource_ID = unique(Datasource_ID),
 		Datasource_name = unique(Datasource_name), 
 		nPlot_names = length(unique(Plot_name)), 
 		Realm = unique(Realm),
 		Taxonomic_scope =  (unique(Invertebrate_group)),
 		Taxonomic_precision =  paste(unique(Taxonomic_precision), collapse = " "), 
 		Source_of_values = unique(source),
 		Start_year = min(Year, na.rm = T),
 		End_year = max(Year, na.rm = T),
 		Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
 		Country_State = unique(Country_State),
 		Country = unique(Country),
 		Region = unique(Region),
 		#Stratum = length(unique(Stratum)),
 		Longitude = unique(Longitude),
 		Latitude = unique(Latitude),
 		AbundanceBiomass = unique(Abundance.Biomass),
 		Number_of_samples = length(unique(paste(Year, Period))),
 		Number_of_years = length(unique(Year))
 	)
 dim(metadata_per_plot) #
 str(metadata_per_plot)
 length(unique(completeData2023pure$Plot_ID))# same
 subset(metadata_per_plot, Datasource_ID == '79')
 subset(metadata_per_plot, Duration <10)
 
 
 
# loop for selecting 10, 20 or 50 plots from each dataset
 
 for ( q in c(1:3)){
		  maxNrPlots<- 	c(10,20,50)[q]
		badDatasets<-  (subset(metadata_per_dataset, Number_of_plots > maxNrPlots)) # 36
		 
		 
		# split dataset in data to be used as is, and data that needs to be made smaller 
		completeData2023Good<- subset(completeData2023pure, !Datasource_ID %in% badDatasets$Datasource_ID) 
		completeData2023bad<- subset(completeData2023pure, Datasource_ID %in% badDatasets$Datasource_ID) 
		
		badDatasets<- badDatasets$Datasource_ID 
		badDatasets<- badDatasets[badDatasets %in% completeData2023pure$Datasource_ID]
		 
		
		# for each dataset with over maxNrPlots: 
		selectedPlots<- NULL
		
		for(i in 1:length(badDatasets)){
		 # 1) get number of locations 
			
		smpPltsPerDataset<- NULL
			
		dataset<- badDatasets[i]
		
		
		dat1<- subset(completeData2023bad, Datasource_ID == dataset)
		#get number of locations: 
		length(unique(dat1$Plot_ID))
		nLocs<- length(unique(dat1$Location))
 
			# if al plots are in 1 location (= close together) take maxNrPlots longest plots 
			if (nLocs  == 1 ){
			
					metadat<-  subset(dat1, !is.na(Number)) %>% 
					group_by(Plot_ID) %>%
					summarise(
						Plot_ID = unique(Plot_ID),
						Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
						Number_of_years_data = length(unique(Year)), 
						Number_of_samples = length(unique(paste(Year, Period))),
						total_N = sum(Number[Unit == "abundance"], na.rm = T),
						richness = max(Number[Unit == "richness"], na.rm = T)
							)
								metadat<- arrange(metadat, -Number_of_years_data, total_N)	
									
								smpPlot<- metadat$Plot_ID[1:maxNrPlots]	
								
								smpPltsPerDataset<- c(smpPltsPerDataset, smpPlot)
								
								selectedPlots<- c(selectedPlots,  smpPlot ) # stitch together all selected plots 
							
	cat(paste( "Dataset",  badDatasets[i], unique(dat1$Datasource_name), ":",  nLocs, "Locs:",length(smpPltsPerDataset), "plots" , "\r\n"))
									}
							
# try to take the spatial spread of the data into account: take one site per location if possible. 

				if (nLocs != 1 & nLocs< maxNrPlots ){
		  	# take the n (maxNrPlots / nLocations) plots with longest timeseries and the highest total N from each loc (probably correlated)
					metadat<-  subset(dat1, !is.na(Number)) %>% 
							group_by(Plot_ID) %>%
							summarise(
								Plot_ID = unique(Plot_ID),
								Location = unique(Location),
								Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
								Number_of_years_data = length(unique(Year)), 
								Number_of_samples = length(unique(paste(Year, Period))),
								total_N = sum(Number[Unit == "abundance"], na.rm = T),
								richness = max(Number[Unit == "richness"], na.rm = T)
							)
					metadat<- arrange(metadat, Location, -Number_of_years_data, -total_N)					

					idealNrPlotsPerLoc<- maxNrPlots / nLocs
					proportional<- table(metadat$Location)/ nrow(metadat)
					proportional <- as.data.frame(	proportional*maxNrPlots)	# proportial dividion of plots oiver locations 
				# but what if there are not enough plots in each Location? 
				
				# take proportionally to how they're divided over the plots, but enforce represenation of all locations
			locs<- unique(metadat$Location)	
			
			for(k in 1:nLocs){
				
				meta1<- subset(metadat, Location == locs[k])
				# for locations with fewer (less than half) plots, take nr of plots rounded up
				if(proportional$Freq[proportional$Var1 == locs[k] ] <=median(proportional$Freq))
				nPlots<- ceiling(proportional[proportional$Var1 == locs[k], ]$Freq)
				
				# for locations with many plots (upper half) 
				if(proportional$Freq[proportional$Var1 == locs[k] ] > median(proportional$Freq))
					nPlots<- floor(proportional[proportional$Var1 == locs[k], ]$Freq)
				
				
				smpPlot<- meta1$Plot_ID[1:nPlots]	
				
				smpPltsPerDataset<- c(smpPltsPerDataset, smpPlot)
				
				
				selectedPlots<- c(selectedPlots,  smpPlot ) # stitch together all selected plots 
				
				
			}
					cat(paste( "Dataset",  badDatasets[i], unique(dat1$Datasource_name), ":",  nLocs, "Locs:",length(smpPltsPerDataset), "plots" , "\r\n"))
					}
					
	#
	
	
 
				 if (nLocs > maxNrPlots){
		  	
				 	
				 	# We want to keep some geographical spread, so we look for the major axis of variation
				 	#remove Location column 
				 	dat1<- dat1[, -(which(names(dat1) == 'Location'))]
				 	
				 	
						#dat1<- merge(dat1, plots[, c("Plot_ID" , "Latitude", "Longitude")])
						dat1$Latitude[dat1$Latitude == 0]<- NA # rmove any 0's for missing data
						dat1$Longitude[dat1$Longitude == 0]<- NA
						
							#take major axis of variation in space (lat or long or both and divide data into equally sized groups
							long<- max(dat1$Longitude, na.rm = T) - min (dat1$Longitude, na.rm = T)			
							lat <- max(dat1$Latitude, na.rm = T) - min (dat1$Latitude, na.rm = T)			
							
							# if major axis of variation is latitude
							if(lat>long){
							lat<- unique(dat1$Latitude)
							lat<-data.frame(Latitude =  lat, 
												 Group = as.numeric(cut_number(lat, maxNrPlots))) # divide plots in equal numbers over geographical gradient
							table(lat$Group)
							dat1<- merge(dat1, lat); dim(dat1)
							
							}else{
								long<- unique(dat1$Longitude)
								long<-data.frame(Longitude =  long, 
																Group = as.numeric(cut_number(long, maxNrPlots))) # divide plots in equal numbers over geographical gradient
								table(long$Group)
								dat1<- merge(dat1, long); dim(dat1)}
							
							
							
							dat1%>% group_by(Group) %>%
								summarise(
									Plot_IDs = length(unique(Plot_ID)))
							
							# for each group get an overview of the quality of the plots 
												selectedPlots<- NULL
														for (j in 1: maxNrPlots){
											dat2<- subset(dat1, Group == j); dim(dat2)
											
											metadat<-  subset(dat2, !is.na(Number)) %>% 
												group_by(Plot_ID) %>%
												summarise(
													Plot_ID = unique(Plot_ID),
													Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
													Number_of_years_data = length(unique(Year)), 
													Start_year = min(Year, na.rm = T),
													End_year = max(Year, na.rm = T),
													Number_of_years_data = length(unique(Year)), 
													Number_of_samples = length(unique(paste(Year, Period))),
													total_N = sum(Number[Unit == "abundance"], na.rm = T),
													richness = max(Number[Unit == "richness"], na.rm = T)
													)
											
											maxYr<- max(metadat$Number_of_years_data)
			
			# if there's only 1 sample with highest number of years, take it
			if(sum(metadat$Number_of_years_data == maxYr) == 1){
			
			smpPlot<- metadat$Plot_ID[metadat$Number_of_years_data == maxYr]}
			
			#if there's multiple , take the one with highest N
			if(sum(metadat$Number_of_years_data == maxYr) > 1){
				
			smpPlot<- metadat[metadat$Number_of_years_data == maxYr, ]
			smpPlot<- smpPlot$Plot_ID[smpPlot$total_N  == max(smpPlot$total_N)]
			}
			
			smpPltsPerDataset<- c(smpPltsPerDataset, smpPlot)
											
			selectedPlots<- c(selectedPlots,  smpPlot )
			
														
				}												
											cat(paste( "Dataset",  badDatasets[i], unique(dat1$Datasource_name),  length(smpPltsPerDataset), "plots" , "\r\n"))
											}

												}

	# close loop ====================================================================											

			length(selectedPlots) # should be nr Datasets * maxNrPlots
			length(unique(selectedPlots))# should be same 
			length(badDatasets)*maxNrPlots

			# select these from DF 
 
			completeData2023badselected<- subset(completeData2023bad, Plot_ID %in% selectedPlots) 
			
			#stitch back together and assign name
				cDfixed<- rbind(completeData2023Good, completeData2023badselected) 
				path<- "C:\\Dropbox\\Insect Biomass Trends/csvs/"
				dfname<- paste0("completeData2023max", maxNrPlots, "plots"); dfname
				
				#assign(dfname, cDfixed)
				
				saveRDS(cDfixed, file = paste0(path, dfname, ".rds"))
				
}				
				dim(cDfixed) #works

# after analysis, plot figure S3
# ED Fig 3 number of sites####
#	get standard model outputs: Abundance, richness, pie. 

	inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
	richMarg<- readRDS("inlaRichnessTMarginal.rds")
	richMarg$Metric<- "Richness"
	richMarg$Data<- "All data" 
	
	inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
	abMarg<- readRDS("inlaAbunTMarginal.rds" )
	abMarg$Metric <- "Abundance"
	abMarg$Data<- "All data"
	
	inlaPieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
	pieMarg<- readRDS("inlaENSPIETMarginal.rds" )
	pieMarg$Metric <- "Simpson diversity (ENS)"
	pieMarg$Data<- "All data"
	
	
	# get model outputs of sensitivity checks:
	# model summary:
	inlaRich50Sum<- as.data.frame(readRDS("inlaRichness50plotsTSUMMARY.rds"))
	# marginals:
	inlaRich50Marg<- readRDS("inlaRichness50plotsTMarginal.rds" )
	# add info for faceting: 
	inlaRich50Marg$Metric<- "Richness"
	inlaRich50Marg$Data<- "Max 50 plots per dataset"
	
	inlaAb50Sum<- as.data.frame(readRDS("inlaAbundance50plotsTSUMMARY.rds"))
	inlaAb50Marg<- readRDS("inlaAbundance50plotsTMarginal.rds" )
	inlaAb50Marg$Metric<- "Abundance"
	inlaAb50Marg$Data<- "Max 50 plots per dataset"
	
	inlaPIE50Sum<- as.data.frame(readRDS("inlaENSPIE50plotsTSUMMARY.rds"))
	inlaPIE50Marg<- readRDS("inlaENSPIE50plotsTMarginal.rds" )
	inlaPIE50Marg$Metric<- "Simpson diversity (ENS)"
	inlaPIE50Marg$Data<- "Max 50 plots per dataset"
	
	inlaRich20Sum<- as.data.frame(readRDS("inlaRichness20plotsTSUMMARY.rds"))
	inlaRich20Marg<- readRDS("inlaRichness20plotsTMarginal.rds" )
	inlaRich20Marg$Metric<- "Richness"
	inlaRich20Marg$Data<- "Max 20 plots per dataset"
	
	inlaAb20Sum<- as.data.frame(readRDS("inlaAbundance20plotsTSUMMARY.rds"))
	inlaAb20Marg<- readRDS("inlaAbundance20plotsTMarginal.rds" )
	inlaAb20Marg$Metric<- "Abundance"
	inlaAb20Marg$Data<- "Max 20 plots per dataset"
	
	inlaPIE20Sum<- as.data.frame(readRDS("inlaENSPIE20plotsTSUMMARY.rds"))
	inlaPIE20Marg<- readRDS("inlaENSPIE20plotsTMarginal.rds" )
	inlaPIE20Marg$Metric<- "Simpson diversity (ENS)"
	inlaPIE20Marg$Data<- "Max 20 plots per dataset"
	
	inlaRich10Sum<- as.data.frame(readRDS("inlaRichness10plotsTSUMMARY.rds"))
	inlaRich10Marg<- readRDS("inlaRichness10plotsTMarginal.rds" )
	inlaRich10Marg$Metric<- "Richness"
	inlaRich10Marg$Data<- "Max 10 plots per dataset"
	
	inlaAb10Sum<- as.data.frame(readRDS("inlaAbundance10plotsTSUMMARY.rds"))
	inlaAb10Marg<- readRDS("inlaAbundance10plotsTMarginal.rds" )
	inlaAb10Marg$Metric<- "Abundance"
	inlaAb10Marg$Data<- "Max 10 plots per dataset"
	
	inlaPIE10Sum<- as.data.frame(readRDS("inlaENSPIE10plotsTSUMMARY.rds"))
	inlaPIE10Marg<- readRDS("inlaENSPIE10plotsTMarginal.rds" )
	inlaPIE10Marg$Metric<- "Simpson diversity (ENS)"
	inlaPIE10Marg$Data<- "Max 10 plots per dataset"
	
# bind together: 
	univarPlts<- rbind(abMarg, richMarg, pieMarg, inlaAb10Marg, inlaRich10Marg, inlaPIE10Marg, inlaAb20Marg, 
										 inlaRich20Marg, inlaPIE20Marg , inlaAb50Marg, inlaRich50Marg, inlaPIE50Marg)
	univarPlts$Metric<- factor(univarPlts$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
	univarPlts$Data<- factor(univarPlts$Data, levels = c ("All data", "Max 50 plots per dataset" , "Max 20 plots per dataset"  ,  "Max 10 plots per dataset" ))
	
	mean <- univarPlts %>%
		group_by(Realm, Metric ,Data) %>%
		filter(y == max(y, na.rm=TRUE))
	
	
	univarPlts<- subset(univarPlts, y> 1) # this is only to remove white space from the plots
	
		
	brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
	perc<-(10^(brks )  *100) - 100
	l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
	
	ggplot(univarPlts, aes(x = x, y = y))+
		#	geom_line( )+
		geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
		geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
		geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
		geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
		geom_vline(data= subset(mean, Realm == 'Terrestrial'), aes(xintercept = x), linetype='dotted')+
		geom_vline(xintercept = 0, linetype = 'dashed')+
		facet_grid(cols = vars(Metric), rows = vars(Data), scales = 'free_x')+ #, switch = "y"
		ylab ("Density")+  xlab("Trend slope  \n % change per year")+
		scale_fill_manual(values = "grey50")+
		scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.012,  0.014))+
		scale_linetype_manual(values = lntps)+
		theme_classic()+
		theme(axis.text.y=element_blank(),
					axis.ticks.y=element_blank(), 
					legend.key=element_blank(), 
					legend.position="none"
		)
	
	
	ggsave(filename = "Fig S3 number of plots.png" , path = figure_path, width = 15, height = 17,  units = "cm",dpi = 300, device = "png")
	ggsave(filename = "Fig S3 number of plots.pdf" , path = figure_path, width = 15, height = 17,  units = "cm",dpi = 300, device = "pdf")
	
	
	
				
# Plot ED Fig 5 quartiles #####
# Quartiles based on distribution of abundance values (0.25, median, 0.75)
sads<- subset(completeData2023pure, Unit ==  "logNrQ1" |  Unit ==   "logNrQ2"|  Unit == "logNrQ3" |  Unit == "logNrQ4")
piv<- reshape2::dcast(subset(sads, !is.na(Number)), Plot_ID+ Year ~ Unit, value.var = "Number", mean)
pivot_wider(sads, id_cols = Plot_ID, from = Unit, values_from = Number, values_fn = mean)

#load output from models
quart1<- as.data.frame(readRDS("quartile1TSUMMARY.rds"))
q1Marg<- readRDS("quartile1TMarginal.rds" )
q1Marg$Metric<- "Quartile 1"

quart2<- as.data.frame(readRDS("quartile2TSUMMARY.rds"))
q2Marg<- readRDS("quartile2TMarginal.rds" )
q2Marg$Metric<- "Quartile 2"

quart3<- as.data.frame(readRDS("quartile3TSUMMARY.rds"))
q3Marg<- readRDS("quartile3TMarginal.rds" )
q3Marg$Metric<- "Quartile 3"

quart4<- as.data.frame(readRDS("quartile4TSUMMARY.rds"))
q4Marg<- readRDS("quartile4TMarginal.rds" )
q4Marg$Metric<- "Quartile 4"



quartilesData<- rbind(q1Marg, q2Marg,q3Marg, q4Marg)
quartilesData$Metric<- as.factor(quartilesData$Metric)

quartilesDataNeg<- quartilesData
#quantilesDataNeg$x<- quantilesDataNeg$x+0.000000001
quartilesDataNeg[, 4:7] <- -quartilesDataNeg[, 4:7]
quartilesDataNeg$Realm[quartilesDataNeg$Realm == "Terrestrial"]<- "Terrestrial2"
quartilesDataNeg$Realm[quartilesDataNeg$Realm == "Freshwater"]<- "Freshwater2"

quartilesData<- rbind(quartilesData, quartilesDataNeg)

# cut off tails 
quartilesData <- quartilesData[quartilesData$x <0.01 & quartilesData$x >-0.005 , ]


brks<- c(-0.02, -0.01, -0.005, -0.0025, 0, 0.0025, 0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
col.scheme.realm2<- c(Terrestrial = "grey50",  Terrestrial2 = "grey50")

ggplot(subset(quartilesData, y>1 | y< -1 ), aes(x = x, y = y))+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
	geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
	coord_flip()+
	geom_vline(xintercept = 0, linetype = 'dashed')+
	facet_grid(cols = vars(Metric), switch = "x")+
	ylab ("SAD quartile")+  xlab("Trend slope  \n % change per year")+
	scale_fill_manual(values = col.scheme.realm2)+
	scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.01))+
	theme_classic()+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank(), 
				legend.key=element_blank(), 
				legend.position="none", 
				strip.background = element_rect(colour = "white")
	)

ggsave(filename = "Fig S5 quartiles.png" , path = figure_path, width = 8.9, height = 8.9,  units = "cm",dpi = 300, device = "png")
ggsave(filename = "Fig S5 quartiles.pdf" , path = figure_path, width = 8.9, height = 8.9,  units = "cm",dpi = 300, device = "pdf")


				
				
						
				
				
				
				
				
				
				
 # Create data needed to test effect of bias towards Europe and North America #####
			
			metadata_per_datasetX<- subset(completeData2023pure, Unit == "richness") %>% 
				group_by(Datasource_ID) %>%
				summarise(
					Datasource_ID = unique(Datasource_ID),
					Datasource_name = unique(Datasource_name), 
					Realm = unique(Realm),
					Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
					Number_of_years_data = length(unique(Year)), 
					Start_year = min(Year, na.rm = T),
					End_year = max(Year, na.rm = T),
					Number_of_plots =  length(unique(Plot_ID)), # should be 1
					Number_of_samples = length(unique(paste(Year, Period))),
					Continent = unique(Continent), 
					Country_State = unique(Country_State),
					Country = unique(Country),
					Region = unique(Region)
				)
			

			
datasets<- as.data.frame(table(metadata_per_datasetX$Continent, metadata_per_datasetX$Realm )	 )
 
median(subset(datasets, Var2 == "Freshwater")$Freq )
 




#   
completeData2023$Continent2<- completeData2023$Continent
completeData2023$Continent2[completeData2023$Continent2 == "Oceania"] <- "Rest"
completeData2023$Continent2[completeData2023$Continent2 == "Asia"] <- "Rest"
completeData2023$Continent2[completeData2023$Continent2 == "Latin America"] <- "Rest"
completeData2023$Continent2[completeData2023$Continent2 == "Africa"] <- "Rest"
# sorry people
metadata_per_cont<- completeData2023 %>% 
	group_by(Continent2, Realm, Unit ) %>%
	summarise(
		Datasets = length(unique(Datasource_ID)),
		Number_of_plots =  length(unique(Plot_ID))	)

metadata_per_cont<- subset(metadata_per_cont, Unit %in% c("richness", "abundance", "ENSPIE"))
metadata_per_cont$Metric <- NA
metadata_per_cont$Metric[metadata_per_cont$Unit == "richness"]<- "Richness"
metadata_per_cont$Metric[metadata_per_cont$Unit == "abundance"]<- "Abundance"
metadata_per_cont$Metric[metadata_per_cont$Unit == "ENSPIE"]<- "Simpson diversity (ENS)"
names(metadata_per_cont)[names(metadata_per_cont) == "Continent2"]<- "Continent"
metadata_per_cont$text<- paste(  metadata_per_cont$Datasets, "|", metadata_per_cont$Number_of_plots)
metadata_per_cont$Metric<- factor(metadata_per_cont$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
metadata_per_cont$x<- NA
metadata_per_cont$x[metadata_per_cont$Metric == "Simpson diversity (ENS)" & metadata_per_cont$Realm == "Terrestrial"] <- -0.0015
metadata_per_cont$x[metadata_per_cont$Metric == "Simpson diversity (ENS)" & metadata_per_cont$Realm == "Freshwater"] <- 0.008
metadata_per_cont$x[metadata_per_cont$Metric == "Richness" & metadata_per_cont$Realm == "Terrestrial"] <- -0.005
metadata_per_cont$x[metadata_per_cont$Metric == "Richness" & metadata_per_cont$Realm == "Freshwater"] <- 0.005
metadata_per_cont$x[metadata_per_cont$Metric == "Abundance" & metadata_per_cont$Realm == "Terrestrial"] <- -0.01
metadata_per_cont$x[metadata_per_cont$Metric == "Abundance" & metadata_per_cont$Realm == "Freshwater"] <- 0.01
metadata_per_cont$y<- NA
metadata_per_cont$y[metadata_per_cont$Continent == "Europe"] <- 400 
metadata_per_cont$y[metadata_per_cont$Continent == "North America"] <- 300 
metadata_per_cont$y[metadata_per_cont$Continent == "Rest"] <- 200 

metadata_per_cont$Continent<- factor(metadata_per_cont$Continent, levels = c ("Rest", "Europe", "North America"   ))


# used
cont2PIESum<- as.data.frame(readRDS("inlaENSPIEFixCont2TSUMMARY.rds"))
cont2AbSum<- as.data.frame(readRDS("inlaAbundFixCont2TSUMMARY.rds"))
cont2RichSum<- as.data.frame(readRDS("inlaRichFixCont2TSUMMARY.rds"))
cont2PIEmarg<- (readRDS("inlaENSPIEFixCont2TMarginal.rds"))
cont2Abmarg<-   (readRDS("inlaAbundFixCont2TMarginal.rds"))
cont2Richmarg<-   (readRDS("inlaRichFixCont2TMarginal.rds"))



# Plot ED Fig 9 geographical bias #####

metrics <- c("Abundance", "Richness", "ENSPIE" )	
marginals<- list(cont2Abmarg, cont2Richmarg, cont2PIEmarg)
summaries<-  list(cont2AbSum, cont2RichSum, cont2PIESum)


#select correct list objects for slopes
elements_to_take<- c(4:6)


ContMargs<- NULL
for ( j in 1:3){

# take each list 
obj<- marginals [[j]]
sum<- summaries [[j]]	
metric<- metrics[j]

obj<- obj [elements_to_take]
sum<- sum[elements_to_take, ]



vars<- names(obj)


for(i in 1: length(vars)){
# for each object  loop through variables


var<-vars[i]	
#Realm<- strsplit(var, ":")[[1]][1]
Continent<- strsplit(var, ":")[[1]][1]
#Realm<-gsub("Realm", "", Realm)
Continent<-gsub("Continent2", "", Continent)


estName<- sum[rownames(sum) == var, ]

marg<- obj[[var]]

marg <-  data.frame(Metric = metric, 
								 Realm = "Terrestrial", 
								 Continent = Continent, 
								 x = marg$x, 
								 y = marg$y)
marg$y80<- marg$y
marg$y90<- marg$y
marg$y95<- marg$y

	marg$y80  [marg$x< estName$`0.1quant`] <- NA # allocate NA to evrything below the 80% quantile 
	marg$y80  [marg$x> estName$`0.9quant` ]<- NA
	marg$y90  [marg$x< estName$`0.05quant`]<- NA # allocate 0 to evrything below the 90% quantile 
	marg$y90  [marg$x> estName$`0.95quant` ]<- NA
	marg$y95  [marg$x< estName$`0.025quant`]<- NA # allocate 0 to evrything below the 95% quantile 
	marg$y95  [marg$x> estName$`0.975quant`]<- NA


	ContMargs<- rbind(ContMargs, marg)

}
	}
dim(ContMargs)	
table(ContMargs$Continent, ContMargs$Metric)
	
ContMargs$y[ContMargs$y < 1 ]<- NA	
	
ContMargs$Metric[ContMargs$Metric == "ENSPIE"]<- "Simpson diversity (ENS)"
#ContMargs$Metric<- factor(ContMargs$Metric, levels = c ("Abundance", "Richness", "Simpson diversity (ENS)" ))
ContMargs$Continent<- factor(ContMargs$Continent, levels = c ("Rest", "Europe", "North America"   ))
	

	
	brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.02, 0.03, 0.04)
	perc<-(10^(brks )  *100) - 100
	l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
	
	ggplot(ContMargs, aes(x = x, y = y))+
		geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8)+
		geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6)+
		geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3)+
		geom_area( aes(x = x, y = y, fill = Realm), alpha = 0.3)+
		#geom_vline(data= mean, aes(xintercept = x, linetype=Realm))+
		geom_vline(xintercept = 0, linetype = 'dashed')+
		facet_grid(cols = vars(Metric), rows = vars(Continent), switch = "y", scales = 'free')+ #
		ylab ("Density")+  xlab("Trend slope")+
		scale_linetype_manual(values = lntps)+
		scale_fill_manual(values = 'grey50')+
		scale_x_continuous(breaks = brks)+ #,  limits=c(-0.02,  0.022)
		geom_text(aes(x = x, y = y, label = text), data = subset(metadata_per_cont, Realm == "Terrestrial" ), size = 3)+
	#	geom_text(aes(x = x, y = y, label = text), data = subset(metadata_per_cont, Realm == "Freshwater" ), size = 3)+
		theme_classic()+
		theme(axis.text.y=element_blank(),
					axis.ticks.y=element_blank(), 
					legend.key=element_blank(), 
					legend.position="none", 
					strip.text.y.left = element_text(size=10, angle=90, hjust = 1),
					strip.background = element_rect(colour = "white")
		)
 
	ggsave(filename = "Fig S8 Geography.png" , path = figure_path, width = 18, height = 12,  units = "cm",dpi = 300, device = "png")
	ggsave(filename = "Fig S8 Geography.pdf" , path = figure_path, width = 18, height = 12,  units = "cm",dpi = 300, device = "pdf")
	
	
# get slope estimates for continents 	
cont2AbSum<- as.data.frame(readRDS("inlaAbundFixCont2TSUMMARY.rds"))
cont2AbSum$percSlope<-  (exp(cont2AbSum$mean )-1)   *100

contAbSum<- as.data.frame(readRDS("inlaAbundFixContTSUMMARY.rds"))
contAbSum$percSlope<-  (exp(contAbSum$mean )-1)   *100


cont2RichSum<- as.data.frame(readRDS("inlaRichFixCont2TSUMMARY.rds"))
cont2RichSum$percSlope<-  (exp(cont2RichSum$mean )-1)   *100

contRichSum<- as.data.frame(readRDS("inlaRichFixContTSUMMARY.rds"))
contRichSum$percSlope<-  (exp(contRichSum$mean )-1)   *100





# ED fig 10 

setwd("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/Tweedie models") # new tweedie results + beta 

# Richness 
inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
richRandom<- readRDS("inlaRichnessTrandomSlopes.rds")
richMarg<- readRDS("inlaRichnessTMarginal.rds")
richMarg$Metric<- "Richness"

inlaRarRichSum<- as.data.frame(readRDS("inlaRarRichTSUMMARY.rds"))
rarRichRandom<- readRDS("inlaRarRichTrandomSlopes.rds")
rarRichMarg<- readRDS("inlaRarRichTMarginal.rds")
rarRichMarg$Metric<- "Rarefied\nrichness"

inlaCovSum<- as.data.frame(readRDS("inlaCoverageR8TSUMMARY.rds"))
CovRandom<- readRDS("inlaCoverageR8TrandomSlopes.rds")
CovMarg<- readRDS("inlaCoverageR8TMarginal.rds")
CovMarg$Metric<- "Coverage\nrichness"

inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
abRandom<- readRDS("inlaAbunTrandomSlopes.rds")
abMarg<- readRDS("inlaAbunTMarginal.rds" )
abMarg$Metric<- "Abundance"

inlapieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
pieRandom<- readRDS("inlaENSPIETrandomSlopes.rds")
pieMarg<- readRDS("inlaENSPIETMarginal.rds" )
pieMarg$Metric<- "Diversity\n(Simpson)"

# load marginal for reduced dataset for richness and abundance 
redRichSum<- as.data.frame(readRDS("inlaReducedRichnessTSUMMARY.rds"))
redRichMarg<- readRDS("inlaReducedRichnessTMarginal.rds")
redRichMarg$Metric<- "Richness"

redAbSum<- as.data.frame(readRDS("inlaReducedAbunTSUMMARY.rds"))
redAbMarg<- readRDS("inlaReducedAbunTMarginal.rds" )
redAbMarg$Metric<- "Abundance"

reduced<- rbind(redRichMarg, redAbMarg) 
reduced$Metric<- factor(reduced$Metric, levels = c ("Abundance", "Richness", "Rarefied richness", "Coverage richness", "ENS-PIE" ))#, "Evenness (Pielou)", "Evenness (Shannon)" ))
reduced$dat <- "55 datasets with full \ncommunity data only"

inlaShanSum<- as.data.frame(readRDS("inlaShanTSUMMARY.rds"))
shanMarg<- readRDS("InlaShanTMarginal.rds")
shanMarg$Metric<- "Diversity\n(Shannon)"

inlaShannonevenness<- as.data.frame(readRDS("inlaShannonevennessTSUMMARY.rds"))
ShannonevennessMarg<- readRDS("inlaShannonevennessTMarginal.rds" )
ShannonevennessMarg$Metric <- "Evenness"

inlaSimpsonevenness<- as.data.frame(readRDS("inlaSimpsonevennessTSUMMARY.rds"))
SimpsonevennessMarg<- readRDS("inlaSimpsonevennessTMarginal.rds" )
SimpsonevennessMarg$Metric <- "Evenness"




# make Fig 2 biodiversity metrics ######

univar<- rbind(abMarg, richMarg, rarRichMarg, CovMarg,  pieMarg)
univar<- rbind(abMarg, richMarg, rarRichMarg, CovMarg,  pieMarg, shanMarg,  SimpsonevennessMarg)
univar$Metric<- factor(univar$Metric, levels = c ("Abundance", "Richness", "Rarefied\nrichness", "Coverage\nrichness",
																									"Diversity\n(Shannon)", "Diversity\n(Simpson)", "Evenness"  )) # right order


figure_path <- "/figures"

# make axis labels:
brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.03, 0.04)
perc<-(exp(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
ltyp<- c("55 datasets with full \ncommunity data only" = 'dotted')

ggplot(subset(univar, y>1)  , aes(x = x, y = y))+
	geom_vline(xintercept = -0.03 )+
	geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8, stat = "identity")+
	geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6, stat = "identity")+
	geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3, stat = "identity")+
	geom_area(  aes(x = x, y = y,   fill = Realm), alpha = 0.3, stat = "identity")+
	geom_line(data = subset(reduced, y>1), aes(x =x , y = y, linetype = dat ), color = 'black')+ # line for reduced dataset of only data with all raw data available
	geom_vline(xintercept = 0, linetype = 'dashed')+
	facet_grid(rows = vars(Metric), switch = "y")+
	ylab ("")+  xlab("Trend slope  \n % change per year")+
	scale_fill_manual(values = "grey50" )+ 
	scale_linetype_manual (values = ltyp)+
	scale_x_continuous(breaks = brks, labels = l, limits=c(-0.03, 0.03))+
#	geom_text(aes(x = 0.017, y = y, label = text), data = metadata_metrics2, size = 2.5)+
	theme_classic()+
	theme(legend.key=element_blank(), 
				legend.position="none", 
				text = element_text(size =7),
				axis.title=element_text(size=9),
				axis.text.y=element_blank(),
				axis.ticks.y=element_blank(), 
				strip.text.y.left = element_text(size=9, angle=0, hjust = 1),
				strip.background = element_rect(colour = "white"))

ggsave(filename = "Fig ED10 univar.png" , path =  paste0(getwd(), figure_path), width = 8.9, height = 10,  units = "cm",dpi = 600, device = "png")
ggsave(filename = "Fig ED10 univar.pdf" , path =  paste0(getwd(), figure_path), width = 8.9, height = 10,  units = "cm",dpi = 300, device = "pdf")

	