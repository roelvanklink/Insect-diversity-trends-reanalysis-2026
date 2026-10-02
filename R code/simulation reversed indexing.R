##############################################################################
# To show that the reverse indexing of the auto-regressive term has no effect 
# on the outcome, we first simulate a dataset of animal counts across multiple 
# sites, with each site having multiple plots. We also simulate some missing time
# points, as is common in such time series. This is a relatively good approximation
# of the data analysed in Van Klink et al 2024, though much more structured and 
# far less complex. It will serve the purpose of showing the sensitivity of the 
# choices of the statistical modeling approach.


# ############################################################################
# PART 1) Simulate hierarchical animal count time series  ####
#
# Structure:
#   Location (e.g. region) 
#     -> Site (nested within location)
#         -> Year (repeated survey, some years missing per site)
#
# Random intercepts AND random slopes (trend) at BOTH the location level
# and the site level (site nested within location), on top of a single
# global declining trend. Counts are generated from a Negative Binomial
# to give realistic overdispersion, and missing timepoints are introduced
# both as scattered "surveys not done" and as truncated site records
# (start late / end early), which is typical of real monitoring data.
#
# The true parameter values used to generate the data are returned
# alongside the observed data frame, so you can later score how well
# different models. 
#
# use this to show that reverse indexing has no effect on the outcome
# also use this to check for effects of family chosen (Poisson, Negative binomial and Tweedie), 
# and of the choice of the prior
##############################################################################

simulate_animal_data <- function(
    n_locations           = 6,     # number of locations
    sites_per_location    = 5,     # scalar or vector (length n_locations) of site counts
    n_years               = 20,    # number of survey years
    start_year            = 2000,
    
    global_intercept      = log(300),  # mean abundance at year 1, log scale
    global_slope          = -0.04,    # overall declining trend, log scale per year
    
    location_intercept_sd = 0.5,   # SD of location-level random intercepts
    location_slope_sd     = 0.02,  # SD of location-level random slopes
    site_intercept_sd     = 0.35,  # SD of site-level random intercepts (within location)
    site_slope_sd         = 0.015, # SD of site-level random slopes (within location)
    
    nb_size               = 4,     # NB dispersion (smaller = more overdispersed)
    
    p_missing_scattered   = 0.15,  # probability any given site-year is missing (not surveyed)
    p_site_truncated      = 0.3,   # probability a site has a truncated (late start / early end) record
    max_truncate_years    = 5,     # max years trimmed from either end when truncated
    
    seed                  = 1
) {
  set.seed(seed)
  
  # allow sites_per_location to be a scalar or a per-location vector
  if (length(sites_per_location) == 1) {
    sites_per_location <- rep(sites_per_location, n_locations)
  }
  stopifnot(length(sites_per_location) == n_locations)
  
  years <- start_year + 0:(n_years - 1)
  year_c <- (years - mean(years)) / 1  # centered year, keeps intercept = mean-year abundance
  
  # ---- random effects -------------------------------------------------
  loc_ids <- seq_len(n_locations)
  loc_re <- data.frame(
    location        = paste0("Loc_", sprintf("%02d", loc_ids)),
    loc_intercept   = rnorm(n_locations, 0, location_intercept_sd),
    loc_slope       = rnorm(n_locations, 0, location_slope_sd)
  )
  
  site_list <- vector("list", n_locations)
  for (i in loc_ids) {
    n_s <- sites_per_location[i]
    site_list[[i]] <- data.frame(
      location         = loc_re$location[i],
      site             = paste0(loc_re$location[i], "_Site_", sprintf("%02d", seq_len(n_s))),
      site_intercept   = rnorm(n_s, 0, site_intercept_sd),
      site_slope       = rnorm(n_s, 0, site_slope_sd)
    )
  }
  site_re <- do.call(rbind, site_list)
  
  # ---- build full location x site x year grid --------------------------
  meta <- merge(site_re, loc_re, by = "location")
  grid <- merge(meta, data.frame(year = years, year_c = year_c), by = NULL)
  
  # ---- linear predictor and NB counts -----------------------------------
  grid$linpred <- with(grid,
                       global_intercept + loc_intercept + site_intercept +
                         (global_slope + loc_slope + site_slope) * year_c)
  grid$mu <- exp(grid$linpred)
  grid$count_true <- rnbinom(nrow(grid), mu = grid$mu, size = nb_size)
  
  # ---- introduce missingness --------------------------------------------
  # (a) scattered missing surveys
  is_scattered_na <- rbinom(nrow(grid), 1, p_missing_scattered) == 1
  
  # (b) truncated site records (monitoring started late / stopped early)
  site_names <- unique(grid$site)
  truncated_sites <- sample(site_names, size = round(p_site_truncated * length(site_names)))
  trunc_df <- data.frame(
    site       = truncated_sites,
    trim_start = sample(0:max_truncate_years, length(truncated_sites), replace = TRUE),
    trim_end   = sample(0:max_truncate_years, length(truncated_sites), replace = TRUE)
  )
  grid <- merge(grid, trunc_df, by = "site", all.x = TRUE)
  grid$trim_start[is.na(grid$trim_start)] <- 0
  grid$trim_end[is.na(grid$trim_end)] <- 0
  
  yr_rank <- ave(grid$year, grid$site, FUN = function(y) rank(y))
  yr_from_end <- ave(grid$year, grid$site, FUN = function(y) rank(-y))
  is_truncated_na <- (yr_rank <= grid$trim_start) | (yr_from_end <= grid$trim_end)
  
  grid$missing <- is_scattered_na | is_truncated_na
  
  # observed data: count is NA (or you can drop rows entirely) where missing
  grid$count <- ifelse(grid$missing, NA_integer_, grid$count_true)
  
  observed <- grid[, c("location", "site", "year", "year_c", "count")]
  observed <- observed[order(observed$location, observed$site, observed$year), ]
  rownames(observed) <- NULL
  
  # a version with missing rows dropped entirely (true "gappy" time series,
  # rather than explicit NAs) -- often more realistic for survey data
  observed_dropped <- observed[!is.na(observed$count), ]
  
  list(
    data              = observed,          # full grid, NA where missing
    data_dropped_na   = observed_dropped,  # missing rows removed entirely
    truth = list(
      global_intercept = global_intercept,
      global_slope     = global_slope,
      location_effects = loc_re,
      site_effects     = site_re,
      nb_size          = nb_size
    )
  )
}

##############################################################################

sim <- simulate_animal_data(
  n_locations        = 6,
  sites_per_location = c(4, 5, 5, 3, 6, 4),  # unequal sites per location
  n_years            = 20,
  start_year         = 2005,
  global_slope       = -0.04,
  seed               = 1
)

df <- sim$data_dropped_na   # use this for fitting models (gappy time series)
head(df)
str(df)

# quick sanity check plot
if (interactive()) {
  library(ggplot2)
  ggplot(df, aes(year, count, group = site, color = location)) +
    geom_line(alpha = 0.5) +
    geom_point(alpha = 0.5) +
    facet_wrap(~location) +
    theme_minimal() +
    labs(title = "Simulated animal counts by site, grouped by location",
         y = "Count", x = "Year")
}

# PART 2) run the models used in Van Klink et al 2024 on the simulated data. ####
# here we focus on the reverse indexing of the autoregressive term, which
# we found to massively speed up model convergence for the full dataset. 

# INLA has specific needs for the indexing of the random effects. 
# Here we add indices according to the model used in Van Klink et al 2024
df$cyear <- df$year - floor(median(df$year))
df$iyear <- df$year - min(df$year) + 1
df$iYear.scale <- scale(df$iyear)
df$ryear <- df$iyear
df$ryear2 <- df$iyear

#random intercept indices (these are nested)
df$Plot_ID_4INLA <- interaction(df$location,df$site)
df$Plot_ID_4INLA <- as.numeric(factor(df$Plot_ID_4INLA))   
df$Datasource_ID_4INLA <- as.numeric(factor(df$location))

#random slope indices
df$Plot_ID_4INLAs <- df$Plot_ID_4INLA+max(df$Plot_ID_4INLA)
df$Datasource_ID_4INLAs <- df$Datasource_ID_4INLA+max(df$Datasource_ID_4INLA)

# add reverse index 
df$loc.numeric <- as.numeric(as.factor((sort(df$site))))
plot.id.values <- sort(unique(df$loc.numeric))
plot.id.inverse <- rep(NA, max(plot.id.values))
plot.id.inverse[plot.id.values] <- length(plot.id.values):1 #
df$Plot_ID_rev <- plot.id.inverse[df$loc.numeric]

# not present in the current dataset
# df$Period_4INLA <- interaction(df$Datasource_ID,df$Period)
# df$Period_4INLA <- as.numeric(factor(df$Period_4INLA))



# Because INLA is a Bayesian method, the outcomes are not exactly equal each run. 
# To show that the forward and reverse indexing of the auto-regressive term give the same result, we thus 
# need to run the model a number of times (here n=50) and test for differences in the mean estimates. 


library(INLA)
inla.setOption(num.threads = "1:1")

n_runs   <- 50
model_names <- c("modelRev", "modelForw")   

# make an object to store the outcomes
results <- data.frame(
  run             = rep(seq_len(n_runs), each = length(model_names)),
  model           = rep(model_names, times = n_runs),
  intercept_mean  = NA_real_,
  intercept_sd    = NA_real_,
  intercept_lower = NA_real_,   # e.g. 0.025 quantile
  intercept_upper = NA_real_,   # e.g. 0.975 quantile
  slope_mean      = NA_real_,
  slope_sd         = NA_real_,
  slope_lower     = NA_real_,
  slope_upper     = NA_real_,
  elapsed_sec     = NA_real_,
  stringsAsFactors = FALSE
)


fam <- "tweedie"

sd.res<-	3 * sd(log(df$count), na.rm = T)
prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 3* log sd
fam <-   "tweedie"
df$E <- exp(8) # I don't know what this does 
val <- range(df$iyear)

for (r in 1:n_runs){
model<- "modelRev"



timing<- system.time({
modelRev<- inla( count ~ cyear +
        f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
        f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec)+
        f(Plot_ID_4INLAs,      iyear,model='iid', hyper = prior.prec)+
        f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
        f(iyear, model = 'ou', 	
          hyper = list(theta1 = list(prior = 'pc.prec')), #
          replicate = Plot_ID_rev),
      family = fam,  
      control.family = list(hyper = list(p = list(fixed = TRUE, initial = 1.5))), # this is helpful for finding the empirical mean-variance scaling
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
      data=df)
})


row_i <- which(results$run == r & results$model == model)
fixed <- modelRev$summary.fixed 

results$intercept_mean[row_i]  <- fixed["(Intercept)", "mean"]
results$intercept_sd[row_i]    <- fixed["(Intercept)", "sd"]
results$intercept_lower[row_i] <- fixed["(Intercept)", "0.025quant"]
results$intercept_upper[row_i] <- fixed["(Intercept)", "0.975quant"]

results$slope_mean[row_i]  <- fixed["cyear", "mean"]
results$slope_sd[row_i]    <- fixed["cyear", "sd"]
results$slope_lower[row_i] <- fixed["cyear", "0.025quant"]
results$slope_upper[row_i] <- fixed["cyear", "0.975quant"]

results$elapsed_sec[row_i] <- timing[3]




model<- "modelForw"
timing <- system.time({
  modelForw <- inla( count ~ cyear +
          f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
          f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec)+
          f(Plot_ID_4INLAs,      iyear,model='iid', hyper = prior.prec)+
          f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
          f(iyear, model = 'ou', 	
            hyper = list(theta1 = list(prior = 'pc.prec')), 
            replicate = Plot_ID_4INLAs ), # this is the difference between the models
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
        data=df)
  
})
timing

row_i <- which(results$run == r & results$model == model)
fixed <- modelForw$summary.fixed # rows: (Intercept), year_c

results$intercept_mean[row_i]  <- fixed["(Intercept)", "mean"]
results$intercept_sd[row_i]    <- fixed["(Intercept)", "sd"]
results$intercept_lower[row_i] <- fixed["(Intercept)", "0.025quant"]
results$intercept_upper[row_i] <- fixed["(Intercept)", "0.975quant"]

results$slope_mean[row_i]  <- fixed["cyear", "mean"]
results$slope_sd[row_i]    <- fixed["cyear", "sd"]
results$slope_lower[row_i] <- fixed["cyear", "0.025quant"]
results$slope_upper[row_i] <- fixed["cyear", "0.975quant"]

results$elapsed_sec[row_i] <- timing[3]

print(r)
print(timing[3])
}
results 


# plot the outcomes
library(ggplot2)
ggplot(results, aes(x = slope_mean, fill = model)) +
  geom_histogram(alpha = 0.7, bins = 15, position = "identity", binwidth= 0.000005 ) +
   coord_cartesian(xlim = c(min(results$slope_mean )-0.00002, max(results$slope_mean)+0.00002)) +
  theme_minimal()


ggplot(results, aes(x = slope_mean, fill = model, color = model)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  geom_vline(xintercept = sim$truth$global_slope,
             linetype = "dashed", color = "black", linewidth = 0.6) +
  coord_cartesian(xlim = c(min(results$slope_mean )-0.0005, max(results$slope_mean)+0.0005)) +
    labs(
    title = "Distribution of estimated slopes across 40 runs",
    subtitle = "Dashed line = true global slope",
    x = "Estimated slope (mean posterior)",
    y = "Density",
      fill = "Model",
    color = "Model"
  ) +
  theme_minimal()

# test for a difference in means:
t.test(results$slope_mean[results$model == "modelForw"], results$slope_mean[results$model == "modelRev"])
# no evidence for a difference in outcomes! 






# PART 3: an additional test to check whether setting a wider or narrower prior on the random effects #####
# meaningfully impacts the range of the random effect estimates in the results. 

# run with the 3 different priors #####
# priors 
sd.res<-	3 * sd(log(df$count), na.rm = T) # normal prior
sd.resWide<-	6 * sd(log(df$count), na.rm = T) # double wide prior 
sd.resNarrow<-	1 * sd(log(df$count), na.rm = T) # very narrow prior of 1 sd



library(INLA)
inla.setOption(num.threads = "1:1")

n_runs= 50
model_names <- c("modelNormalPrior", "modelWidePrior", "modelNarrowPrior")   

# this results table includes extra columns for the random effects
resultsPriors <- data.frame(
  run             = rep(seq_len(n_runs), each = length(model_names)),
  model           = rep(model_names, times = n_runs),
  intercept_mean  = NA_real_,
  intercept_sd    = NA_real_,
  intercept_lower = NA_real_,   # e.g. 0.025 quantile
  intercept_upper = NA_real_,   # e.g. 0.975 quantile
  slope_mean      = NA_real_,
  slope_sd         = NA_real_,
  slope_lower     = NA_real_,
  slope_upper     = NA_real_,
  #elapsed_sec     = NA_real_,
  stringsAsFactors= FALSE, 
  randomInterceptMin = NA_real_,
  randomInterceptMax = NA_real_,
  randomSlopesMin = NA_real_,
  randomSlopesMax = NA_real_, 
  randomSlopesRange = NA_real_,
  plotSlopesMean = NA_real_,
  plotSlopesMax = NA_real_,
  plotSlopesMin = NA_real_,
  plotSlopesRange = NA_real_
)
dim(resultsPriors)


# run models

for (r in 1:n_runs){

  
modelNormalPrior <- inla( count ~ cyear +
                        f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
                        f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec)+
                        f(Plot_ID_4INLAs,      iyear,model='iid', hyper = prior.prec)+
                        f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
                        f(iyear, model = 'ou', 	
                          #  values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                          hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                          replicate = Plot_ID_rev),
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
                      data=df)
  
  row_i <- which(resultsPriors$run == r & resultsPriors$model == "modelNormalPrior")
  fixed <- modelNormalPrior$summary.fixed # rows: (Intercept), year_c
  
  resultsPriors$intercept_mean[row_i]  <- fixed["(Intercept)", "mean"]
  resultsPriors$intercept_sd[row_i]    <- fixed["(Intercept)", "sd"]
  resultsPriors$intercept_lower[row_i] <- fixed["(Intercept)", "0.025quant"]
  resultsPriors$intercept_upper[row_i] <- fixed["(Intercept)", "0.975quant"]
  
  resultsPriors$slope_mean[row_i]  <- fixed["cyear", "mean"]
  resultsPriors$slope_sd[row_i]    <- fixed["cyear", "sd"]
  resultsPriors$slope_lower[row_i] <- fixed["cyear", "0.025quant"]
  resultsPriors$slope_upper[row_i] <- fixed["cyear", "0.975quant"]
  
  # get random effects at location level 
  intercepts     <- modelNormalPrior$summary.random$Datasource_ID_4INLA
  slopes         <- modelNormalPrior$summary.random$Datasource_ID_4INLAs
  slopes_plot    <-modelNormalPrior$summary.random$Plot_ID_4INLAs
  
  
  resultsPriors$randomInterceptMin[row_i] <- min(intercepts$mean)
  resultsPriors$randomInterceptMax[row_i] <- max(intercepts$mean)
  resultsPriors$randomSlopesMin[row_i]  <- min(slopes$mean)
  resultsPriors$randomSlopesMax[row_i]  <- max(slopes$mean)
  resultsPriors$randomSlopesRange[row_i]  <- max(slopes$mean)- min(slopes$mean)
  
  resultsPriors$plotSlopesMean[row_i] <- mean(slopes_plot$mean)
  resultsPriors$plotSlopesMax[row_i] <- max(slopes_plot$mean)
  resultsPriors$plotSlopesMin[row_i] <- min(slopes_plot$mean)
  resultsPriors$plotSlopesRange[row_i] <- max(slopes_plot$mean)-
    min(slopes_plot$mean)
  
  print(row_i)
  

sd.res<-	3 * sd(log(df$count), na.rm = T)
sd.resWide<-	6 * sd(log(df$count), na.rm = T)
sd.resNarrow<-	1 * sd(log(df$count), na.rm = T)
  
  
  prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.resWide, 0.01))) #
  fam <-   "tweedie"
  
  
  
  modelWidePrior <- inla( count ~ cyear +
                              f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
                              f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec)+
                              f(Plot_ID_4INLAs,      iyear,model='iid', hyper = prior.prec)+
                              f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
                              f(iyear, model = 'ou', 	
                                #  values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                                hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                                replicate = Plot_ID_rev),
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
                            data=df)

    
  row_i <- which(resultsPriors$run == r & resultsPriors$model == "modelWidePrior")
  fixed <- modelWidePrior$summary.fixed # rows: (Intercept), year_c
  
  resultsPriors$intercept_mean[row_i]  <- fixed["(Intercept)", "mean"]
  resultsPriors$intercept_sd[row_i]    <- fixed["(Intercept)", "sd"]
  resultsPriors$intercept_lower[row_i] <- fixed["(Intercept)", "0.025quant"]
  resultsPriors$intercept_upper[row_i] <- fixed["(Intercept)", "0.975quant"]
  
  resultsPriors$slope_mean[row_i]  <- fixed["cyear", "mean"]
  resultsPriors$slope_sd[row_i]    <- fixed["cyear", "sd"]
  resultsPriors$slope_lower[row_i] <- fixed["cyear", "0.025quant"]
  resultsPriors$slope_upper[row_i] <- fixed["cyear", "0.975quant"]
  
  # get random effects at location level 
  intercepts     <- modelWidePrior$summary.random$Datasource_ID_4INLA
  slopes         <- modelWidePrior$summary.random$Datasource_ID_4INLAs
  slopes_plot    <- modelWidePrior$summary.random$Plot_ID_4INLAs
  
  
  resultsPriors$randomInterceptMin[row_i] <- min(intercepts$mean)
  resultsPriors$randomInterceptMax[row_i] <- max(intercepts$mean)
  resultsPriors$randomSlopesMin[row_i]  <- min(slopes$mean)
  resultsPriors$randomSlopesMax[row_i]  <- max(slopes$mean)
  resultsPriors$randomSlopesRange[row_i]  <- max(slopes$mean)- min(slopes$mean)
  
  resultsPriors$plotSlopesMean[row_i] <- mean(slopes_plot$mean)
  resultsPriors$plotSlopesMax[row_i] <- max(slopes_plot$mean)
  resultsPriors$plotSlopesMin[row_i] <- min(slopes_plot$mean)
  resultsPriors$plotSlopesRange[row_i] <- max(slopes_plot$mean)-
                                          min(slopes_plot$mean)
  
  print(row_i)
  
  
  
  
  prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.resNarrow, 0.01))) #
  fam <-   "tweedie"
  
  
  
  modelNarrowPrior <- inla( count ~ cyear +
                            f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
                            f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec)+
                            f(Plot_ID_4INLAs,      iyear,model='iid', hyper = prior.prec)+
                            f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
                            f(iyear, model = 'ou', 	
                              #  values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                              hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                              replicate = Plot_ID_rev),
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
                          data=df)
  
  
  row_i <- which(resultsPriors$run == r & resultsPriors$model == "modelNarrowPrior")
  fixed <- modelNarrowPrior$summary.fixed # rows: (Intercept), year_c
  
  resultsPriors$intercept_mean[row_i]  <- fixed["(Intercept)", "mean"]
  resultsPriors$intercept_sd[row_i]    <- fixed["(Intercept)", "sd"]
  resultsPriors$intercept_lower[row_i] <- fixed["(Intercept)", "0.025quant"]
  resultsPriors$intercept_upper[row_i] <- fixed["(Intercept)", "0.975quant"]
  
  resultsPriors$slope_mean[row_i]  <- fixed["cyear", "mean"]
  resultsPriors$slope_sd[row_i]    <- fixed["cyear", "sd"]
  resultsPriors$slope_lower[row_i] <- fixed["cyear", "0.025quant"]
  resultsPriors$slope_upper[row_i] <- fixed["cyear", "0.975quant"]
  resultsPriors
  # get random effects at location level 
  intercepts     <- modelNarrowPrior$summary.random$Datasource_ID_4INLA
  slopes         <- modelNarrowPrior$summary.random$Datasource_ID_4INLAs
  slopes_plot    <-modelNarrowPrior$summary.random$Plot_ID_4INLAs
  
  
  resultsPriors$randomInterceptMin[row_i] <- min(intercepts$mean)
  resultsPriors$randomInterceptMax[row_i] <- max(intercepts$mean)
  resultsPriors$randomSlopesMin[row_i]  <- min(slopes$mean)
  resultsPriors$randomSlopesMax[row_i]  <- max(slopes$mean)
  resultsPriors$randomSlopesRange[row_i]  <- max(slopes$mean)- min(slopes$mean)
  
  resultsPriors$plotSlopesMean[row_i] <- mean(slopes_plot$mean)
  resultsPriors$plotSlopesMax[row_i] <- max(slopes_plot$mean)
  resultsPriors$plotSlopesMin[row_i] <- min(slopes_plot$mean)
  resultsPriors$plotSlopesRange[row_i] <- max(slopes_plot$mean)-
    min(slopes_plot$mean)
  
  
  print(row_i)

    
  
  
  
  }
resultsPriors
  

ggplot(resultsPriors, aes(x = slope_mean, fill = model)) +
  geom_histogram(alpha = 0.7, bins = 15, position = "identity", binwidth= 0.000005 ) +
  #coord_cartesian(xlim = c(min(results$slope_mean )-0.00002, max(results$slope_mean)+0.00002)) +
  theme_minimal()
# there is a significant, but small (4th decimal place) difference in slope mean estimates dependingon the prior width. 
# this is unexpected, but given the small absolute size of the difference doesn't change any interpretation


  # there are probably differences between prior models. 
  # melt df for plotting 
  library(tidyverse)
  library(dplyr)
  long <- resultsPriors %>% 
    pivot_longer(
      cols = `intercept_mean`:`plotSlopesRange`, 
      names_to = "variable",
      values_to = "value"
    )  
  long
  
  ggplot(long, aes(x = value, y = model))+ 
           geom_point()+
           facet_wrap(.~variable, scales = "free_x")
  # this is rather uninterpretable 

  
# make some graphs showing the mean upper and lower estimates    
# first the fixed intercept and slope estimates 
interceptEstimates<- subset(long, variable %in% c("intercept_mean", 
                                                  "intercept_lower", 
                                                  "intercept_upper"))  
  
ggplot(interceptEstimates, aes(x = value, y = model, color = variable))+ 
  geom_point()

slopeEstimates <- subset(long, variable %in% c("slope_mean", 
                                               "slope_lower", 
                                               "slope_upper"))

ggplot(slopeEstimates, aes(x = value, y = model, color = variable))+ 
  geom_point()
# conclusion: slope credible interval is only slighty wider with wide prior. no meaningful difference


# look at random slopes at location level
randomSlopes<- subset(long, variable %in% c("randomSlopesMin",  # min estimated random slope
                                                  "randomSlopesMax"))  # max ranodm slope
ggplot(randomSlopes, aes(x = value, y = model, color = variable))+ 
  geom_point()

# check plot level
slopesPlotLevel<- subset(long, variable %in% c("plotSlopesMax", 
                                                       "plotSlopesMean", 
                                                       "plotSlopesMin"))



ggplot(slopesPlotLevel, aes(x = value, y = model, color = variable))+ 
  geom_point()+
  ggtitle("mean random plot slopes")
# practically no difference 

ggplot(subset(long, variable == "plotSlopesRange"), aes(x = value, y = model, color = variable))+ 
  geom_point()+
  ggtitle("Range of random plot slopes")

  # conclusion: the effect of the prior width exists, but is negligible 
  


  # some tests with the fixed and ranodm priors to see why I'm not recovering my true slope
  
  # normal prior width but with decline in fixed effect 
  prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) # back to normal prior on random effects
  
  # add prior on fixed effects. Very tight prior (1 mln) gives the true slope exactly. loose prior (100) reverts back to -0.016
  modelFixedPrior <- inla( count ~ cyear +
                             f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
                             f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec)+
                             f(Plot_ID_4INLAs,      iyear,model='iid', hyper = prior.prec)+
                             f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
                             f(iyear, model = 'ou', 	
                               #  values = seq(val[1], val[2]), # Rue's addition, no idea what this does or if it's useful in ou
                               hyper = list(theta1 = list(prior = 'pc.prec')), # this was our original prior
                               replicate = Plot_ID_rev),
                           family = fam,  
                           
                           control.fixed = list(
                             mean = list(cyear = -0.04, default = 0),   # centered near the true slope
                             prec = list(cyear = 100,   default = 0.001)), 
                           
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
                           data=df)
  
  
  row_i <- which(resultsPriors$run == r & resultsPriors$model == "modelFixedPrior")
  fixed <- modelFixedPrior$summary.fixed # rows: (Intercept), year_c
  fixed
  
  
  
  
  # check in how far the true slope is eaten by the ranodm slopes 
  modelNoRandomSlopesFlatPrior <- inla( count ~ cyear +
                             f(Plot_ID_4INLA,               model='iid', hyper = prior.prec )+
                             f(Datasource_ID_4INLA,       model='iid', hyper = prior.prec),
                           family = fam,  
                           
                           control.fixed = list(
                             mean = list(cyear = 0, default = 0),   # centered near the true slope
                             prec = list(cyear = 0.001,   default = 0.001)), 
                           
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
                           data=df)
  
 modelNoRandomSlopesFlatPrior$summary.fixed # rows: (Intercept), year_c
  # still -0.017
 
 
 fit_nb <- inla(count ~ cyear + f(Plot_ID_4INLA, model='iid', hyper=prior.prec) +
                  f(Datasource_ID_4INLA, model='iid', hyper=prior.prec),
                data = df, family = "nbinomial")
 fit_nb$summary.fixed["cyear", ]
 
 summary(fit_nb)
 
summary( glm(count ~ cyear, data = df) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  resultsPriors$intercept_mean[row_i]  <- fixed["(Intercept)", "mean"]
  resultsPriors$intercept_sd[row_i]    <- fixed["(Intercept)", "sd"]
  resultsPriors$intercept_lower[row_i] <- fixed["(Intercept)", "0.025quant"]
  resultsPriors$intercept_upper[row_i] <- fixed["(Intercept)", "0.975quant"]
  
  resultsPriors$slope_mean[row_i]  <- fixed["cyear", "mean"]
  resultsPriors$slope_sd[row_i]    <- fixed["cyear", "sd"]
  resultsPriors$slope_lower[row_i] <- fixed["cyear", "0.025quant"]
  resultsPriors$slope_upper[row_i] <- fixed["cyear", "0.975quant"]
  resultsPriors
  # get random effects at location level 
  intercepts     <- modelNarrowPrior$summary.random$Datasource_ID_4INLA
  slopes         <- modelNarrowPrior$summary.random$Datasource_ID_4INLAs
  slopes_plot    <-modelNarrowPrior$summary.random$Plot_ID_4INLAs
  
  
  resultsPriors$randomInterceptMin[row_i] <- min(intercepts$mean)
  resultsPriors$randomInterceptMax[row_i] <- max(intercepts$mean)
  resultsPriors$randomSlopesMin[row_i]  <- min(slopes$mean)
  resultsPriors$randomSlopesMax[row_i]  <- max(slopes$mean)
  resultsPriors$randomSlopesRange[row_i]  <- max(slopes$mean)- min(slopes$mean)
  
  resultsPriors$plotSlopesMean[row_i] <- mean(slopes_plot$mean)
  resultsPriors$plotSlopesRange[row_i] <- max(slopes_plot$mean)-
    min(slopes_plot$mean)
  
  
  
  
  
  
  
  
  
  # trash 
  # add up fixed slope and random slopes
  
  fx<-data.frame(Realm =  "Terrestrial", #
                 fixedSlp = model$summary.fixed$mean[2], 
                 fixedIntercept = (model$summary.fixed$mean[1]  ) )
  RandEfDataset<- merge(RandEfDataset, fx, by = "Realm" )
  RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
  
  
  
  
  
  
  
  
  
  formul.rev <- as.formula("count ~ cyear+
                         f(Plot_ID_4INLA,model='iid', hyper = prior.prec )+
                         f(Datasource_ID_4INLA,model='iid', hyper = prior.prec)+
                         f(Plot_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
                         f(Datasource_ID_4INLAs,iyear,model='iid', hyper = prior.prec)+
                         f(iYear, model = 'ou', 	
                  #  values = seq(val[1], val[2]), 
                    hyper = list(theta1 = list(prior = 'pc.prec')), 
                    replicate = Plot_ID_rev)" )
  
  