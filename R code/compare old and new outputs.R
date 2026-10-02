library(ggplot2)



# these files are created in the script 'graphs models main text...'

univarOLD<- readRDS( file = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/all univariate results ORIGINAL log10.rds")
univarOLD$run <- "Original"
univarNEWlog10<- readRDS( file = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/all univariate results corrected log10.rds")
univarNEWlog10$run <- "Corrected"
univarNEWtweedie<- readRDS( file = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/all univariate results corrected tweedie.rds")
univarNEWtweedie$run <- "Tweedie"


head(univarOLD)
head(univarNEWlog10)
head(univarNEWtweedie)





# MAKE COMPARISON GRAPHS FOR UPDATE 28-5-2026
OLDandNEW<- rbind(univarOLD, univarNEWlog10)

# New vs old data selection, same model (but optimized)

brks<- c(-0.02, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
ltyp<- c("55 datasets with full \ncommunity data only" = 'dotted')


ggplot(subset(OLDandNEW, run != "Tweedie" ), aes(x = x, y = y))+
  geom_area(  aes(x = x, y = y80, fill = run), alpha = 0.8, stat = "identity")+
     geom_area(  aes(x = x, y = y90, fill = run), alpha = 0.6, stat = "identity")+
    geom_area(  aes(x = x, y = y95, fill = run), alpha = 0.3, stat = "identity")+
  geom_area(  aes(x = x, y = y, fill = run), alpha = 0.5, stat = "identity")+
  #    geom_line(data = subset(reduced, y>1), aes(x =x , y = y, linetype = dat ), color = 'black')+ # line for reduced dataset of only data with all raw data available
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_grid(rows = vars(Metric), switch = "y")+
  ylab ("")+  xlab("Trend slope  \n % change per year")+
  #  scale_fill_manual(values = "grey50" )+ 
  # scale_linetype_manual (values = ltyp)+
  scale_x_continuous(breaks = brks, labels = l, limits=c(-0.012, 0.02))+
  #scale_x_continuous( limits=c(-3, 5))+
  #  geom_text(aes(x = 0.017, y = y, label = text), data = metadata_metrics2, size = 2.5)+
  theme_classic()+
  theme(legend.key=element_blank(), 
        # legend.position="none", 
        text = element_text(size =7),
        axis.title=element_text(size=9),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        strip.text.y.left = element_text(size=9, angle=0, hjust = 1),
        strip.background = element_rect(colour = "white"))


ggsave(filename = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models/figures/Fig 2 oldnew comparison.png", 
       width = 13, height = 10,  units = "cm",dpi = 600, device = "png")



# convert both to % change for comparability

univarOLDconv<- univarOLD
univarOLDconv[, 3]<-  (10^(univarOLDconv[, 3])  *100) - 100
univarNEWlogconv<- univarNEWlog10
univarNEWlogconv[, 3]<-  (10^(univarNEWlog10[, 3])  *100) - 100
univarNEWtweedieconv<- univarNEWtweedie
univarNEWtweedieconv[, 3]<-  (exp(univarNEWtweedieconv[, 3])  *100) - 100

OLDandNEWconv<- rbind(univarOLDconv, univarNEWlogconv, univarNEWtweedieconv)

head(OLDandNEWconv)
unique(OLDandNEWconv$Metric) # shoudl have no NAs

ggplot(OLDandNEW , aes(x = x, y = y))+
  geom_vline(xintercept = -0.012 )+
  # geom_area(  aes(x = x, y = y80, fill = Realm), alpha = 0.8, stat = "identity")+
  # geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6, stat = "identity")+
  # geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3, stat = "identity")+
  geom_area(  aes(x = x, y = y, fill = run), alpha = 0.5, stat = "identity")+
  #  geom_line(data = subset(reduced, y>1), aes(x =x , y = y, linetype = dat ), color = 'black')+ # line for reduced dataset of only data with all raw data available
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_grid(rows = vars(Metric), switch = "y")+
  ylab ("")+  xlab("Trend slope  \n % change per year")+
  #  scale_fill_manual(values = "grey50" )+ 
  # scale_linetype_manual (values = ltyp)+
  scale_x_continuous( limits=c(-3, 5))+
  #  geom_text(aes(x = 0.017, y = y, label = text), data = metadata_metrics2, size = 2.5)+
  theme_classic()+
  theme(legend.key=element_blank(), 
        # legend.position="none", 
        text = element_text(size =7),
        axis.title=element_text(size=9),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        strip.text.y.left = element_text(size=9, angle=0, hjust = 1),
        strip.background = element_rect(colour = "white"))

ggsave(filename = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models/figures/Fig 2 oldnew comparison.png", 
       width = 13, height = 10,  units = "cm",dpi = 600, device = "png")



# Tweedie only 
ggplot(subset(OLDandNEW, run == "Tweedie" ), aes(x = x, y = y))+
  geom_area(  aes(x = x, y = y80), fill = "grey50", alpha = 0.8, stat = "identity")+
  geom_area(  aes(x = x, y = y90), fill = "grey50", alpha = 0.6, stat = "identity")+
  geom_area(  aes(x = x, y = y95), fill = "grey50", alpha = 0.3, stat = "identity")+
  geom_area(  aes(x = x, y = y), fill = "grey50", alpha = 0.5, stat = "identity")+
  #    geom_line(data = subset(reduced, y>1), aes(x =x , y = y, linetype = dat ), color = 'black')+ # line for reduced dataset of only data with all raw data available
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_grid(rows = vars(Metric), switch = "y")+
  ylab ("")+  xlab("Trend slope  \n % change per year")+
  #  scale_fill_manual(values = "grey50" )+ 
  # scale_linetype_manual (values = ltyp)+
  scale_x_continuous( limits=c(-3, 5))+
  #  geom_text(aes(x = 0.017, y = y, label = text), data = metadata_metrics2, size = 2.5)+
  theme_classic()+
  theme(legend.key=element_blank(), 
        # legend.position="none", 
        text = element_text(size =7),
        axis.title=element_text(size=9),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        strip.text.y.left = element_text(size=9, angle=0, hjust = 1),
        strip.background = element_rect(colour = "white"))

ggsave(filename = "Fig ED10 Tweedie.png" , path =   "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/tweedie models/figures", 
       width = 8.9, height = 10,  units = "cm",dpi = 600, device = "png")











# QUANTILES 
quantilesOLD<- readRDS(file =  "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/quantiles results ORIGINAL log10.rds")
quantilesOLD$run <- "Original"
quantilesNEWlog10<- readRDS( file = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/quantiles results corrected log10.rds")
quantilesNEWlog10$run <- "Corrected"
quantilesNEWtweedie<- readRDS( file = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/Tweedie models/quantiles results corrected tweedie.rds")
quantilesNEWtweedie$run <- "Tweedie"

quantilesData<- rbind(quantilesOLD, quantilesNEWtweedie, quantilesNEWlog10)

# old vs new quantiles 

brks<- c(-0.02, -0.01, -0.005, -0.0025,  0, 0.0025, 0.005, 0.01, 0.02, 0.03, 0.04) #old log 10 models
#brks<- c(-0.03, -0.02, -0.01, -0.005,   0,  0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
col.scheme.realm2<- c(Terrestrial = "grey50",  Terrestrial2 = "grey50")



ggplot(subset(quantilesData, run != "Tweedie" & y>0  ), aes(x = xn, y = y))+ #quantilesData
  #geom_line( )+
  geom_area(  aes(x = x, y = y80, fill = run), alpha = 0.8,  position = "identity")+
  geom_area(  aes(x = x, y = y90, fill = run), alpha = 0.6,  position = "identity")+
  geom_area(  aes(x = x, y = y95, fill = run), alpha = 0.3,  position = "identity")+
  geom_area(  aes(x = x, y = y, fill = run), alpha = 0.3, position = "identity")+
  facet_grid(cols = vars(Metric), switch = "x")+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  coord_flip()+
  ylab ("SAD interval")+  xlab("Trend slope of number of species")+
  #scale_fill_manual(values = col.scheme.realm2)+
  scale_x_continuous(breaks = brks,labels = l, limits=c(-0.005,  0.0028))+
  #ggtitle("Trend in number of species per SAD interval")+
  theme_classic()+
  theme(#legend.key=element_blank(), 
    #legend.position="none", 
    text = element_text(size =8),
    axis.text.x=element_blank(),
    axis.title=element_text(size=9),
    axis.ticks.x=element_blank(), 
    strip.background = element_rect(colour = "white")
  )

ggsave(filename = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models/figures/Fig 3 oldnew comparison.png", 
       width = 13, height = 10,  units = "cm",dpi = 600, device = "png")


quantilesNEWtweedieNeg<- quantilesNEWtweedie
quantilesNEWtweedieNeg[, 4:7] <- -quantilesNEWtweedieNeg[, 4:7]
quantilesNEWtweedieNeg$Realm[quantilesNEWtweedieNeg$Realm == "Terrestrial"]<- "Terrestrial2"

quantilesNEWtweedie<- rbind(quantilesNEWtweedie, quantilesNEWtweedieNeg)


brks<- c(-0.04, -0.02, -0.01, -0.005,   0,  0.005, 0.01, 0.02, 0.03, 0.04) #old log 10 models
#brks<- c(-0.03, -0.02, -0.01, -0.005,   0,  0.005, 0.01, 0.02, 0.03, 0.04)
perc<-(exp(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
col.scheme.realm2<- c(Terrestrial = "grey50",  Terrestrial2 = "grey50")


#Tweedie quantiles 
ggplot(subset(quantilesNEWtweedie, y>1 | y< -1 ), aes(x = x, y = y))+ #
  #	geom_line( )+
  geom_area(  aes(x = x, y = y80, fill = Realm),  alpha = 0.8,  position = "identity")+
  geom_area(  aes(x = x, y = y90, fill = Realm), alpha = 0.6,  position = "identity")+
  geom_area(  aes(x = x, y = y95, fill = Realm), alpha = 0.3,  position = "identity")+
  geom_area(   aes(x = x, y = y, fill = Realm), alpha = 0.3,  position = "identity")+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  coord_flip()+
  facet_grid(cols = vars(Metric), switch = "x")+
  ylab ("SAD interval")+  xlab("Trend slope of number of species  \n % change per year")+
  scale_fill_manual(values = col.scheme.realm2)+
  scale_x_continuous(breaks = brks,labels = l)+#, limits=c(-0.005,  0.003))+
  ggtitle("b) Trend in number of species per SAD interval")+
  theme_classic()+
  theme(legend.key=element_blank(), 
        legend.position="none", 
        text = element_text(size =8),
        axis.text.x=element_blank(),
        axis.title=element_text(size=9),
        axis.ticks.x=element_blank(), 
        strip.background = element_rect(colour = "white")
  )

ggsave(filename = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/tweedie models/figures/ED Fig 10b tweedie SAD changes.png", 
       width = 10, height = 10,  units = "cm",dpi = 600, device = "png")






# convert to same scale 
quantilesNEWtweedie$xn<- quantilesNEWtweedie$x /2.303 
quantilesNEWtweedie$xn
quantilesOLD$xn<- quantilesOLD$x
quantilesNEWlog10$xn<- quantilesNEWlog10$x

#  % conversion. not to be taken serious, but it works 
quantilesOLD$xperc<- (10^(quantilesOLD$x)-1)
quantilesNEWlog10$xperc<- (10^(quantilesNEWlog10$x)-1)
quantilesNEWtweedie$xperc<- exp(quantilesNEWtweedie$x)-1
# this works for sure

quantilesData<- rbind(quantilesOLD, quantilesNEWtweedie, quantilesNEWlog10)




ggplot(subset(quantilesData, y>0  ), aes(x = x, y = y))+ #quantilesData
  #geom_line( )+
   geom_area(  aes(x = x, y = y80, fill = run), alpha = 0.8,  position = "identity")+
   geom_area(  aes(x = x, y = y90, fill = run), alpha = 0.6,  position = "identity")+
   geom_area(  aes(x = x, y = y95, fill = run), alpha = 0.3,  position = "identity")+
  geom_area( aes(x = x, y = y, fill = run), alpha = 0.3, position = "identity")+
  facet_grid(cols = vars(Metric), switch = "x")+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  coord_flip()+
  ylab ("SAD interval")+  xlab("Trend slope of number of species")+
  #scale_fill_manual(values = col.scheme.realm2)+
  #scale_x_continuous(breaks = brks,labels = l, limits=c(-0.005,  0.0028))+
  #ggtitle("Number of species per SAD interval")+
  theme_classic()+
  theme(#legend.key=element_blank(), 
        #legend.position="none", 
        text = element_text(size =8),
        axis.text.x=element_blank(),
        axis.title=element_text(size=9),
        axis.ticks.x=element_blank(), 
        strip.background = element_rect(colour = "white")
  )

setwd("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models/") # 

ggsave(filename = "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models/figures/Fig 3 oldnew comparison.png" , 
       width = 8.9, height = 10,  units = "cm",dpi = 600, device = "png")







# Quantitative outputs:  #####
# univariate 

# Richness 

# load tweedie models
setwd("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/Tweedie models") # new tweedie results + beta 

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

quantile80100Sum<- as.data.frame(readRDS("quantile80100TSUMMARY.rds"))
quantile6080Sum<- as.data.frame(readRDS("quantile6080TSUMMARY.rds"))
quantile4060Sum<- as.data.frame(readRDS("quantile4060TSUMMARY.rds"))
quantile2040Sum<- as.data.frame(readRDS("quantile2040TSUMMARY.rds"))
quantile020Sum<- as.data.frame(readRDS("quantile020TSUMMARY.rds"))


Realms<- c( "Terrestrial" )
tweedieMnChangeEsts<- rbind(
  cbind(Metric = "Abundance",         Realms, inlaAbSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Richness",          Realms, inlaRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Rarefied richness", Realms, inlaRarRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "ENS-Shannon",       Realms, inlaShanSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "ENS-PIE",           Realms, inlapieSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Abundance (55 datasets)", Realms, redAbSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Richness (55 datasets)",  Realms, redRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "0-20% Quantile",  Realms, quantile020Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "20-40% Quantile",  Realms, quantile2040Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "40-60% Quantile",  Realms, quantile4060Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "60-80% Quantile",  Realms, quantile6080Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "80-100% Quantile",  Realms, quantile80100Sum[ 2, c(1,2,5,6,7, 11,12,13)] )
  
  )

tweedieMnChangeEsts$lower2.5Perc10Yr <- (exp(tweedieMnChangeEsts$`0.025quant`*10 )-1)   *100
tweedieMnChangeEsts$meanPerc10Yr <- (exp(tweedieMnChangeEsts$mean*10 )-1)   *100
tweedieMnChangeEsts$meanPerc30Yr <- (exp(tweedieMnChangeEsts$mean*30 )-1)   *100
tweedieMnChangeEsts$upper97.5Perc10Yr <- (exp(tweedieMnChangeEsts$`0.975quant`*10 )-1)   *100

tweedieMnChangeEsts$lower2.5PercYr <- (exp(tweedieMnChangeEsts$`0.025quant` )-1)   *100
tweedieMnChangeEsts$lower5PercYr <- (exp(tweedieMnChangeEsts$`0.05quant` )-1)   *100
tweedieMnChangeEsts$lower10PercYr <- (exp(tweedieMnChangeEsts$`0.1quant` )-1)   *100
tweedieMnChangeEsts$meanPercYr <- (exp(tweedieMnChangeEsts$mean )-1)   *100
tweedieMnChangeEsts$upper90PercYr <- (exp(tweedieMnChangeEsts$`0.9quant` )-1)   *100
tweedieMnChangeEsts$upper95PercYr <- (exp(tweedieMnChangeEsts$`0.95quant` )-1)   *100
tweedieMnChangeEsts$upper97.5PercYr <- (exp(tweedieMnChangeEsts$`0.975quant` )-1)   *100
tweedieMnChangeEsts$Realm2<- tweedieMnChangeEsts$Realms
tweedieMnChangeEsts$Metric2<- tweedieMnChangeEsts$Metric
tweedieMnChangeEsts



# load log10 models 
setwd("C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/log10 (n+1) models") # new tweedie results + beta 

inlaRichSum<- as.data.frame(readRDS("inlaRichnessTSUMMARY.rds"))
inlaRarRichSum<- as.data.frame(readRDS("inlaRarRichTSUMMARY.rds"))
inlaCovSum<- as.data.frame(readRDS("inlaCoverageR8TSUMMARY.rds"))
inlaAbSum<- as.data.frame(readRDS("inlaAbunTSUMMARY.rds"))
inlapieSum<- as.data.frame(readRDS("inlaENSPIETSUMMARY.rds"))
# load marginal for reduced dataset for richness and abundance 
redRichSum<- as.data.frame(readRDS("inlaReducedRichnessTSUMMARY.rds"))
redAbSum<- as.data.frame(readRDS("inlaReducedAbunTSUMMARY.rds"))

reduced<- rbind(redRichMarg, redAbMarg) 
reduced$Metric<- factor(reduced$Metric, levels = c ("Abundance", "Richness", "Rarefied richness", "Coverage richness", "ENS-PIE" ))#, "Evenness (Pielou)", "Evenness (Shannon)" ))
reduced$dat <- "55 datasets with full \ncommunity data only"

inlaShanSum<- as.data.frame(readRDS("inlaShanTSUMMARY.rds"))
inlaShannonevenness<- as.data.frame(readRDS("inlaShannonevennessTSUMMARY.rds"))
inlaSimpsonevenness<- as.data.frame(readRDS("inlaSimpsonevennessTSUMMARY.rds"))
quantile80100Sum<- as.data.frame(readRDS("quantile80100TSUMMARY.rds"))
quantile6080Sum<- as.data.frame(readRDS("quantile6080TSUMMARY.rds"))
quantile4060Sum<- as.data.frame(readRDS("quantile4060TSUMMARY.rds"))
quantile2040Sum<- as.data.frame(readRDS("quantile2040TSUMMARY.rds"))
quantile020Sum<- as.data.frame(readRDS("quantile020TSUMMARY.rds"))

Realms<- c( "Terrestrial" )

log10MnChangeEsts<- rbind(
  cbind(Metric = "Abundance",         Realms, inlaAbSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Richness",          Realms, inlaRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Rarefied richness", Realms, inlaRarRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "ENS-Shannon",       Realms, inlaShanSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "ENS-PIE",           Realms, inlapieSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Abundance (55 datasets)", Realms, redAbSum[ 2, c(1,2,5,6,7, 11,12,13)] ), 
  cbind(Metric = "Richness (55 datasets)",  Realms, redRichSum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "0-20% Quantile",  Realms, quantile020Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "20-40% Quantile",  Realms, quantile2040Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "40-60% Quantile",  Realms, quantile4060Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "60-80% Quantile",  Realms, quantile6080Sum[ 2, c(1,2,5,6,7, 11,12,13)] ),
  cbind(Metric = "80-100% Quantile",  Realms, quantile80100Sum[ 2, c(1,2,5,6,7, 11,12,13)] )
  
)

log10MnChangeEsts$lower2.5Perc10Yr <- (10^(log10MnChangeEsts$`0.025quant`*10 )-1)   *100
log10MnChangeEsts$meanPerc10Yr <- (10^(log10MnChangeEsts$mean*10 )-1)   *100
log10MnChangeEsts$meanPerc30Yr <- (10^(log10MnChangeEsts$mean*30 )-1)   *100
log10MnChangeEsts$upper97.5Perc10Yr <- (10^(log10MnChangeEsts$`0.975quant`*10 )-1)   *100

log10MnChangeEsts$lower2.5PercYr <- (10^(log10MnChangeEsts$`0.025quant` )-1)   *100
log10MnChangeEsts$lower5PercYr <- (10^(log10MnChangeEsts$`0.05quant` )-1)   *100
log10MnChangeEsts$lower10PercYr <- (10^(log10MnChangeEsts$`0.1quant` )-1)   *100
log10MnChangeEsts$meanPercYr <- (10^(log10MnChangeEsts$mean )-1)   *100
log10MnChangeEsts$upper90PercYr <- (10^(log10MnChangeEsts$`0.9quant` )-1)   *100
log10MnChangeEsts$upper95PercYr <- (10^(log10MnChangeEsts$`0.95quant` )-1)   *100
log10MnChangeEsts$upper97.5PercYr <- (10^(log10MnChangeEsts$`0.975quant` )-1)   *100
log10MnChangeEsts$Realm2<- log10MnChangeEsts$Realms
log10MnChangeEsts$Metric2<- log10MnChangeEsts$Metric



comparisonTable<- 
  data.frame(
    Metric = log10MnChangeEsts$Metric, 
    meanlog10 = log10MnChangeEsts$meanPercYr ,
    log10_2.5 = log10MnChangeEsts$lower2.5PercYr,
    log10_5 = log10MnChangeEsts$lower5PercYr,
    log10_10 = log10MnChangeEsts$lower10PercYr,
    log10_90 = log10MnChangeEsts$upper90PercYr,
    log10_95 = log10MnChangeEsts$upper95PercYr,
    log10_97.5 = log10MnChangeEsts$upper97.5PercYr,
    meanTweedie = tweedieMnChangeEsts$meanPercYr,
    tweedie_2.5 = tweedieMnChangeEsts$lower2.5PercYr,
    tweedie_5 = tweedieMnChangeEsts$lower5PercYr,
    tweedie_10 = tweedieMnChangeEsts$lower10PercYr,
    tweedie_90 = tweedieMnChangeEsts$upper90PercYr,
    tweedie_95 = tweedieMnChangeEsts$upper95PercYr,
    tweedie_97.5 = tweedieMnChangeEsts$upper97.5PercYr
  )
comparisonTable

comparisonTable$log10evidence[comparisonTable$log10_90 <0] <- "weak"
comparisonTable$log10evidence[comparisonTable$log10_95 <0] <- "moderate"
comparisonTable$log10evidence[comparisonTable$log10_97.5 <0] <- "strong"

comparisonTable$tweedieevidence[comparisonTable$tweedie_90 <0] <- "weak"
comparisonTable$tweedieevidence[comparisonTable$tweedie_95 <0] <- "moderate"
comparisonTable$tweedieevidence[comparisonTable$tweedie_97.5 <0] <- "strong"


comparisonTableFinal<-data.frame(
  Figure = c(rep("Fig 2", 7), rep("Fig 3", 5)),
  Metric = comparisonTable$Metric, 
  meanlog10 = round(comparisonTable$meanlog10, 2) ,
  log10evidence = comparisonTable$log10evidence,
  meanTweedie = round(comparisonTable$meanTweedie, 2),
  tweedieEvidence = comparisonTable$tweedieevidence)
  


comparisonTableFinal[is.na(comparisonTableFinal)] <- ""

comparisonTableFinal

library(openxlsx)

write.xlsx(comparisonTableFinal, "C:/Dropbox/work/2017 iDiv/2018 insect biomass/insect-richness-trends correction/comparisonTweedieLog10 outputs 20260622.xlsx")

