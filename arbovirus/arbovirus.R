# BING-F-432 - Epidemiologie - Cartographie de risque Arbovirus 

# 1. Visualiser les données (TP1)


tab = read.csv("Arbovirus_risk_mapping_simulated_dataset_1-1.csv", head=T)

head(tab)

str(tab)

if (!require(sp)) install.packages("sp") # to install the package if not installed
if (!require(sf)) install.packages("sf")
if (!require(raster)) install.packages("raster")
if (!require(colorspace)) install.packages("colorspace")
if (!require(MetBrewer)) install.packages("MetBrewer")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(blockCV)) install.packages("blockCV")
if (!require(dismo)) install.packages("dismo")
if (!require(gbm)) install.packages("gbm")
if (!require(ncf)) install.packages("ncf")
library(sp); library(sf); library(raster); library(colorspace); library(MetBrewer); library(RColorBrewer)
library(blockCV); library(dismo); library(gbm); library(ncf)

coast_study_area = shapefile("Coasts_study_area_shapefile/Coasts_study_area_shapefile.shp")
envVariableNames = c("croplands_all_categories","human_pop_density_log10","managed_pasture_and_rangeland",
                     "precipitation_inFall","precipitation_spring", "precipitation_summer", "precipitation_winter",
                     "primary_forest_areas","primary_non.forest_areas", "relative_humidity_inFall", "relative_humidity_spring",
                     "relative_humidity_summer", "relative_humidity_winter", "secondary_forest_areas", "secondary_non.forest_areas",
                     "temperature_inFall", "temperature_spring", "temperature_summer", "temperature_winter")
envVariables = list()
for (i in 1:length(envVariableNames)) {
  fileName = paste0("Environmental_raster_files/Raster_",envVariableNames[i],".asc")
  envVariables[[i]] = raster(fileName, overwrite=T)
  names(envVariables[[i]]) = envVariableNames[i]
}

# simple visualisation des cas

plot(coast_study_area, main = "Spatial distributions of arbovirus cases")
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.4, lwd=0.7, col="black")
}

# visualisation des variables environnementales

cols = colorRampPalette(brewer.pal(9,"Greens"))(150)[1:100]
plot(envVariables[[1]], col=cols, main="Croplands all categories")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"BuPu"))(150)[1:100]
plot(envVariables[[2]], col=cols, main="Human population density (log10)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"YlOrBr"))(150)[1:100]
plot(envVariables[[3]], col=cols, main="Managed pasture and rangeland")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Blues"))(150)[1:100]
plot(envVariables[[4]], col=cols, main="Precipitation (fall)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Blues"))(150)[1:100]
plot(envVariables[[5]], col=cols, main="Precipitation (spring)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Blues"))(150)[1:100]
plot(envVariables[[6]], col=cols, main="Precipitation (summer)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Blues"))(150)[1:100]
plot(envVariables[[7]], col=cols, main="Precipitation (winter)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Greens"))(150)[1:100]
plot(envVariables[[8]], col=cols, main="Primary forest areas")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Greens"))(150)[1:100]
plot(envVariables[[9]], col=cols, main="Primary non-forest areas")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"GnBu"))(150)[1:100]
plot(envVariables[[10]], col=cols, main="Relative humidity (fall)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7) 
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"GnBu"))(150)[1:100]
plot(envVariables[[11]], col=cols, main="Relative humidity (spring)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"GnBu"))(150)[1:100]
plot(envVariables[[12]], col=cols, main="Relative humidity (summer)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"GnBu"))(150)[1:100]
plot(envVariables[[13]], col=cols, main="Relative humidity (winter)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"BuGn"))(150)[1:100]
plot(envVariables[[14]], col=cols, main="Secondary forest areas")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"BuGn"))(150)[1:100]
plot(envVariables[[15]], col=cols, main="Secondary non-forest areas")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Oranges"))(150)[1:100]
plot(envVariables[[16]], col=cols, main="Temperature (fall)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Oranges"))(150)[1:100]
plot(envVariables[[17]], col=cols, main="Temperature (spring)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Oranges"))(150)[1:100]
plot(envVariables[[18]], col=cols, main="Temperature (summer)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

cols = colorRampPalette(brewer.pal(9,"Oranges"))(150)[1:100]
plot(envVariables[[19]], col=cols, main="Temperature (winter)")
plot(coast_study_area, add=T)
for (i in dim(tab)[1]:1) {
  points(tab[i,c("longitude","latitude")], pch=16, cex=0.7)
  points(tab[i,c("longitude","latitude")], pch=1, cex=0.7, lwd=0.4, col="black")
}

# Comment mettre les variables saisonnières (précipitations, humidité, température) sur une 
# même échelle entre les saisons ?

# loading the dataset of geo-referenced occurrence records

template = envVariables[[1]]; template[!is.na(template[])] = 1 # just to generate a raster covering
# the study area (with inland accessible raster cell with a value of "1" and NA otherwise)
cells_arbovirus = unique(raster::extract(template, tab, cellnumbers=T))[,"cells"]
background = template # we start from the template raster to generate the background one
background[(1:length(background[]))%in%cells_arbovirus] = NA # to assign a NA value and thus to
# "discard" cells associated with at least one confirmed occurrence record for arbovirus
cells_background = which(!is.na(background[])) # to retrieve the raster cell IDs of all the cells
# of the resulting background raster that do not contain a NA value. The pseudo-absence
# cells will be randomly sampled among those cells (see step 3 below)

# visualisation des cas d'arbovirus

plot(coast_study_area, cols=NA, border=NA)
cols= colorRampPalette(brewer.pal(9,"YlOrRd"))(150)[1:100]
points(tab$longitude, tab$latitude, col=cols, ann=F, legend=F, axes=F, box=F)
plot(coast_study_area, add=T, border="gray50", lwd=0.5)
mtext("Arbovirus observations", side=3, line=0.3, cex=0.65, col="gray30")

# generating the data frames for the replicate BRT analyses

number_of_replicates = 30 # arbitrary (for your analyses, you can e.g. conduct up to 100 replicates)
dataframes = list() # the list object in each the data frame for each BRT analysis will be stored
for (i in 1:number_of_replicates) {
  presences = xyFromCell(template, cells_arbovirus) # to retrieve the centroid coordinates of the
  # raster cells treated as "presence" locations (with at least one occurrence record)
  cells_pseudo_absence = sample(cells_background, length(cells_arbovirus), replace=F) # random
  # selection of raster cells that will be treated as pseudo-absence in this specific
  # BRT replicate analysis. To this end, we here use the function "sample()" to
  # sample, without replacement ("replace=F"), a number of raster cell IDs within the
  # set of background cell IDs (see above); this number of pseudo-absence cells to
  # sample being equal to the number of raster cells treated as presence locations
  pseudo_absences = xyFromCell(template, cells_pseudo_absence) # to retrieve the centroid
  # coordinates of the raster cells treated as pseudo-absence locations
  data = rbind(cbind(rep(1,dim(presences)[1]),presences),
               cbind(rep(0,dim(pseudo_absences)[1]),pseudo_absences)) # to combine the centroid
  # coordinates of both presence cells and selected pseudo-absence ones
  colnames(data) = c("response", "longitude", "latitude") # names of three first columns
  data = cbind(data, raster::extract(stack(envVariables), data[,c(2:3)])) # completing the data
  # frame with the environmental values extracted at all those cell positions
  dataframes[[i]] = as.data.frame(data) # to convert the resulting table in a data frame object
  # and to store it at the ith position within the "dataframes" list object
  if (i== 1) print(str(dataframes[[i]])) # to display the structure of the first data frame
}

# investigating the spatial autocorrelation for the spatial cross-validation

par(oma=c(0,0,0,0), mar=c(2,2.2,0,0))
for (i in 1:number_of_replicates) {
  data = dataframes[[i]]
  correlogram = ncf::correlog(data[,"longitude"], data[,"latitude"], data[,"response"],
                              na.rm=T, increment=100, resamp=0, latlon=T)
  if (i== 1) { # to only generate an empty plot once (for the first data frame); plot on which
    # we will then add the correlogram curves estimated for the other data frames
    plot(correlogram$mean.of.class, correlogram$correlation, ann=F, axes=F,
         lwd=0.2, cex=0.5, col=NA, ylim=c(-1,1.03), xlim=c(250,5700))
    abline(h=0, lwd=0.5, col=rgb(222,67,39,255,maxColorValue=255), lty=2)
    axis(side=1, lwd.tick=0.2, cex.axis=0.7, lwd=0.2, tck=-0.03,
         col.axis="gray30", mgp=c(0,0.00,0), at=seq(0,9000,1000))
    axis(side=2, lwd.tick=0.2, cex.axis=0.7, lwd=0.2, tck=-0.03,
         col.axis="gray30", mgp=c(0,0.25,0), at=seq(-1.5,3,0.5))
    title(xlab="Distance (km)", cex.lab=0.9, mgp=c(0.9,0,0), col.lab="gray30")
    title(ylab="Correlation", cex.lab=0.9, mgp=c(1.3,0,0), col.lab="gray30")
  }
  lines(correlogram$mean.of.class[-1], correlogram$correlation[-1], lwd=0.1, col="gray60")
  # to add on the plot the correlogram curves estimated for each data frames
}

# training the ecological niche models

theRanges = c(2000,2000)*1000 # distance in meters defined from the spatial correlograms
gbm.x = envVariableNames # names of the predictor variables within the data frames
gbm.y = "response" # name of the response variable (column) within the data frames
offset = NULL; tree.complexity = 5 # number of nodes in the trees ("tc" parameter)
learning.rate = 0.005 # contribution of each tree to the growing model ("lr" parameter)
bag.fraction = 0.80 # proportion of observations used in selecting variables
site.weights = rep(1, dim(data)[1]) # weight assigned to each location (here uniform)
var.monotone = rep(0, length(gbm.x)) # allows forcing the relationship between a predictor and the
# response to be strictly increasing or strictly decreasing, thereby incorporating prior
# ecological knowledge into the model (here set to "0" for all the predictor variable to
# set no constraint/prior knowledge)
n.folds = 5 # number of spatial folds to generate and consider in the cross-validation
prev.stratify = TRUE # set but not considered here as we define the folds prior to and outside the
# "gbm.step()" function. In a standard (default) cross-validation procedure, "prev.stratify"
# set to "TRUE" would lead to each fold being stratified, meaning presences and absences are
# distributed evenly across folds
family = "bernoulli" # statistical distribution used to model the response variable (see above)
n.trees = 10 # initial number of trees (to be re-set before launching a new algorithm)
step.size = 5 # interval at which the predictive deviance is computed and logged (at each interval,
# the folds are successively used as test dataset and the remaining folds as training
# datasets to compute the deviance). Note: you can run some preliminary analyses to evaluate
# the potential impact of the "tree.complexity", "learning rate", and "step size" parameters.
# See also the comprehensive study of Elith et al. (2008) for a detailed discussion about
# the impact of those three parameters
max.trees = 10000 # maximum number of trees that will be considered by the BRT algorithm
tolerance.method = "auto" # method used to decide when to stop the algorithm (see below)
tolerance = 0.001 # tolerance value used to stop the algorithm. When "tolerance.method" is set to
# "auto" (automatic), the total mean deviance is multiplied by this value to determine the
# threshold value for the reduction in deviance: when the reduction in deviance becomes
# lower than this threshold the "gbm.step" functions stops ("gbm.step" calculates the
# the average residual deviance at each step of the cross-validation process and continues
# to build trees as long as the reduction in deviance is greater than this threshold)
plot.main = TRUE # to plot the mean residual deviance across the number of trees grown during the
# cross-validation process
plot.folds = FALSE # to plot the considered folds (not used here as we use an external spatial
# cross-validation procedure as implemented in the function "cv_spatial")
verbose = TRUE # to conduct some screen recording during the BRT algorithm
silent = FALSE # to activate the silent mode, which suppresses the printing of running messages
# and cross-validation results to the screen
keep.fold.models = FALSE # to conserve the fold-specific models from the cross-validation
keep.fold.vector = FALSE # to conserve the repartition of locations among the folds (not needed
# here as we specify the spatial fold a priori using the "cv_spatial" function and thus
# already have this piece of information anyway)
keep.fold.fit = FALSE # to keep the predicted values from the cross-validation for observations

brt_models = list() # list object in which all the trained ecological niche models will be stored
predictions = list() # list object in which we will store all the predictions of the ecological
# suitability across the study area. Each element of the list will be one raster object
AUCs = matrix(nrow=number_of_replicates, ncol=1) # a one-column matrix in which we will store
# the AUC metrics of each trained model. AUC = Area Under the Curve (AUC) of the Receiver
# Operating Characteristic (ROC) plot, here averaged across the cross-validation folds
# (see below for further detail on assessing the predictive performance of the models)
colnames(AUCs) = c("AUC")
for (i in 1:number_of_replicates) {
  data = dataframes[[i]] # as detailed above, each replicate analysis is based on a distinct
  # data frame (involving a distinct set of pseudo-absence locations). The "cv_spatial"
  # function requires as input a "SpatialPointsDataFrame", which can be generated as follows:
  spdf = SpatialPointsDataFrame(data[c("longitude","latitude")],
                                data[,c("response",envVariableNames)],
                                proj4string=crs(template))
  myblocks = cv_spatial(spdf, column="response", k=n.folds, size=theRanges[1], selection="random")
  fold.vector = myblocks$folds_ids # to retrieve the ID of the hexagonal spatial fold assigned to
  # each entry of the data frame (so for both presence and pseudo-absence entries/rows)
  n.trees = 10 # the initial number of trees has to re-set (here to 10) before re-launching the
  # BRT algorithm implemented in the "gbm.step" function
  brt_models[[i]] = gbm.step(data, gbm.x, gbm.y, offset, fold.vector, tree.complexity,
                             learning.rate, bag.fraction, site.weights, var.monotone,
                             n.folds, prev.stratify, family, n.trees, step.size,
                             max.trees, tolerance.method, tolerance, plot.main,
                             plot.folds, verbose, silent, keep.fold.models,
                             keep.fold.vector, keep.fold.fit) # to launch the "gbm.step" function
  AUCs[i,"AUC"] = brt_models[[i]]$cv.statistics$discrimination.mean # to retrieve the average AUC
  # Now that we have trained the ecological niche model for this replicate analysis, we can
  # use it to predict the ecological niche suitability across the study area (i.e. to predict
  # one ecological suitability value per raster cell), which can be done as follows:
  dataframe = as.data.frame(stack(envVariables)) # to store the environmental raster cell values
  # in a data frame format; data frame from which we will then remove all rows with NA values:
  dataframe = dataframe[which(!is.na(rowMeans(dataframe))),] # data frame for the prediction step
  n.trees = brt_models[[i]]$gbm.call$best.trees # final number of trees fitted by the algorithm
  type = "response"; single.tree = FALSE # parameters to set for the "predict.gbm" function:
  prediction = predict.gbm(brt_models[[i]], dataframe, n.trees, type, single.tree)
  buffer = envVariables[[1]]; buffer[!is.na(buffer[])] = prediction; predictions[[i]] = buffer
  # this last line of code allows to re-put the predicted values in a raster format
}
write.csv(AUCs, "AUC_value_replicates.csv", row.names=F, quote=F) # to save all AUC estimates
saveRDS(brt_models, "BRT_model_replicates.rds") # to save all the trained models in a ".rds" file
saveRDS(predictions, "BRT_model_predictions.rds") # the same but for all the prediction rasters

# maps showing the distribution of presence and pseudo-absence locations for the seven first data frames

par(mfrow=c(1,7), oma=c(0,0,1.5,0), mar=c(0.75,0,0,0))
for (i in 1:7) {
  df = subset(dataframes[[i]][,c(2,3)])
  presences = df[dataframes[[i]]$response==1,]
  absences = df[dataframes[[i]]$response==0,]
  plot(coast_study_area, cols=NA, border="gray30", lwd=0.5)
  points(presences, pch=16, col="red", cex=0.2, add=T)
  points(absences, pch=16, col="blue", cex=0.2, add=T)
  mtext("presence - pseudo-absence", side=3, line=0.3, cex=0.65, col="gray30")
}

# generating risk maps from the ecological niche models

predictions = readRDS("BRT_model_predictions.rds")
par(mfrow=c(1,7), oma=c(0,0,1.5,0), mar=c(0.75,0,0,0), lwd=0.2, col="gray30",
    col.axis="gray30", fg="gray30") # general graphical parameters
cols = rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(131)[11:121]) # a frequently used
# "diverging" colour scale (from "RColorBrewer") to colour risk mapping outcome
for (i in 1:7) { # for the illustration, we here only plot the results for seven first replicates
  plot(predictions[[i]], col=cols, ann=F, legend=F, axes=F, box=F) # to plot the prediction raster
  plot(coast_study_area, add=T, border="gray50", lwd=0.5) # to add the contour of western Europe
  mtext("Ecological suitability", side=3, line=0.3, cex=0.65, col="gray30") # map title (1° part)
  mtext(paste0("(BRT replicate ",i,")"), side=3, line=-0.7, cex=0.65, col="gray30") # (2° part)
  plot(predictions[[i]], col=cols, legend.only=T, add=T, legend.width=0.5,
       legend.shrink=0.3, smallplot=c(0.10,0.80,0.09,0.12), adj=3,
       axis.args=list(cex.axis=0.65, lwd=0, lwd.tick=0.2, col.tick="gray30",
                      tck=-0.9, col="gray30", col.axis="gray30", line=0,
                      mgp=c(0,0.1,0)), alpha=1, side=3, horizontal=T) # to add the colour legend
}

# maps for mean and standard deviation

par(mfrow=c(1,2), oma=c(0,0,1.5,0), mar=c(0.75,0,0,0), lwd=0.2, col="gray30")
Stack1 <- stack(predictions)
predictions_mean <- calc(Stack1, fun=mean)
plot(predictions_mean, col=cols, ann=F, legend=F, axes=F, box=F)
plot(coast_study_area, add=T, border="gray50", lwd=0.5)
mtext("Predictions mean", side=3,line=0.3, cex=0.65, col="gray30")
predictions_sd <- calc(Stack1, fun=sd)
plot(predictions_mean, col=cols, ann=F, legend=F, axes=F, box=F)
plot(coast_study_area, add=T, border="gray50", lwd=0.5)
mtext("Predictions standard deviation", side=3,line=0.3, cex=0.65, col="gray30")

# assessing the predictive performance of the ecological niche models

AUCs = read.csv("AUC_value_replicates.csv", head=T)
cat("Mean AUC = ",round(mean(AUCs[,1]),2),
    ", AUC range = [",round(min(AUCs),2),", ",round(max(AUCs),2),"]","\n",sep="")

brt_models = readRDS("BRT_model_replicates.rds") # to load the replicate ecological niche models
SI_ppcs = rep(NA, length(brt_models)) # the vector in which we will store the SIppc values
thresholds = rep(NA, length(brt_models)) # the vector in which we will the corresponding threshold
# value maximising the SIppc for each ecological niche model
for (i in 1:length(brt_models)) {
  tmp = matrix(nrow=101, ncol=2) # temporary ("tmp") matrix with two columns, one for the threshold
  # value ranging from 0 to 1 with a 0.01 step increment, and 2° column for the SIppc values
  colnames(tmp) = c("threshold","SIppc"); tmp[,"threshold"] = seq(0,1,0.01)
  dataframe = brt_models[[i]]$gbm.call$dataframe
  responses = dataframe$response; data = dataframe[,4:dim(dataframe)[2]]
  n.trees = brt_models[[i]]$gbm.call$best.trees; type = "response"; single.tree = FALSE
  prediction = predict.gbm(brt_models[[i]], data, n.trees, type, single.tree)
  P = sum(responses==1) # number of presence locations (raster cells)
  A = sum(responses==0) #number of pseudo-absence locations (raster cells)
  prev = P/(P+A) # proportion of recorded sites where the species is present
  x = (P/A)*((1-prev)/prev); SI_ppc = 0 # (see abobe for the formula of "x")
  for (threshold in seq(0,1,0.01)) {
    TP = length(which((responses==1)&(prediction>=threshold))) # true positives
    FN = length(which((responses==1)&(prediction<threshold))) # false negatives
    FP_pa = length(which((responses==0)&(prediction>=threshold))) # false positives
    SI_ppc_tmp = (2*TP)/((2*TP)+(x*FP_pa)+(FN)) # (see above for the formula of SIppc)
    tmp[which(tmp[,"threshold"]==threshold),"SIppc"] = SI_ppc_tmp
    if (SI_ppc < SI_ppc_tmp) { # to save the threshold value maximising SIppc once reached
      SI_ppc = SI_ppc_tmp; optimised_threshold = threshold
    }
  }
  SI_ppcs[i] = SI_ppc; thresholds[i] = optimised_threshold # SIppc and associated threshold for
  # this replicate BRT analysis (this replicate ecological niche model)
}
mean_SIppc = round(mean(SI_ppcs),2); mean_thres = round(mean(thresholds),2) # mean SIppc
min_SIppc = round(min(SI_ppcs),2); min_thres = round(min(thresholds),2) # minimum SIppc
max_SIppc = round(max(SI_ppcs),2); max_thres = round(max(thresholds),2) # maximum SIppc
cat("Mean SIppc = ",mean_SIppc,", SIppc range = [",min_SIppc,", ",max_SIppc,"]","\n",sep="")

# retrieving the relative importance of the environmental variables

relative_importances = matrix(nrow=length(brt_models), ncol=length(envVariables))
colnames(relative_importances) = envVariableNames
brt_models = readRDS("BRT_model_replicates.rds")
for (i in 1:length(brt_models)) {
  for (j in 1:length(envVariables)) {
    relative_importances[i,j] = summary(brt_models[[i]])[names(envVariables[[j]]),"rel.inf"]
  }
}
write.table(relative_importances, "Relative_importances.csv", row.names=F, quote=F, sep=",")
relative_importances = read.csv("Relative_importances.csv", head=T)
RI_summary = matrix(nrow=length(envVariables), ncol=1)
rownames(RI_summary) = envVariableNames; colnames(RI_summary) = c("mean RI [95% CI]")
for (i in 1:dim(relative_importances)[2]) {
  mean_RI = round(mean(relative_importances[,i]),1)
  ci95_RI = round(t.test(relative_importances[,i])$conf.int[1:2],1)
  RI_summary[i,1] = paste0(mean_RI," [",ci95_RI[1],", ",ci95_RI[2],"]")
}
RI_summary

# visualising the response curves associated with each variable

brt_models = readRDS("BRT_model_replicates.rds") # to load all the BRT models in the ".rds" file
sp = SpatialPoints(dataframes[[1]][which(dataframes[[1]][,"response"]==1),2:3])
envVariableValues = matrix(nrow=3, ncol=length(envVariables)) # to initiate the matrix
colnames(envVariableValues) = envVariableNames
row.names(envVariableValues) = c("median","minV","maxV")
for (i in 1:length(envVariables)) {
  points = rasterize(sp, envVariables[[i]])
  rast = envVariables[[i]]; rast[is.na(points)] = NA
  minV = min(rast[], na.rm=T); maxV = max(rast[], na.rm=T)
  envVariableValues[,i] = cbind(median(rast[],na.rm=T), minV, maxV)
}
par(mfrow=c(1,7), oma=c(0,0,0,0), mar=c(2,1.2,0.5,0.5), lwd=0.2, col="gray30")
envVariableTitles = c("Croplands","Human Population Density (log10)","Managed Pasture and Rangeland",
                     "Precipitation (fall)","Precipitation (spring)","Precipitation (summer)","Precipitation (winter)",
                     "Primary Forest Areas","Primary non-forest Areas", "Relative Humidity (fall)","Relative Humidity (spring)",
                     "Relative_Humidity (summer)", "Relative Humidity (winter)", "Secondary Forest Areas","Secondary non-forest Areas",
                     "Temperature (fall)", "Temperature (spring)","Temperature (summer)","Temperature (winter)")
for (i in 1:19) { # to get a plot per environmental variable in a specific order
  valuesInterval = (envVariableValues["maxV",i]-envVariableValues["minV",i])/100 # to estimate
  # the curves based on 100 values ranging from the minimum and maximum values recorded
  # for the considered environmental factor at all raster cells treated as presence locations
  dataframe = data.frame(matrix(nrow=length(seq(envVariableValues["minV",i],
                                                envVariableValues["maxV",i],valuesInterval)),
                                ncol=length(envVariables))) # this data frame will gather, for
  # each environmental factor (columns), 100 values: the aforementioned range of 100 values
  # for the target environmental factor, and 100 times the median value for the other factors
  colnames(dataframe) = envVariableNames
  for (j in 1:length(envVariables)) {
    interval = (envVariableValues["maxV",j]-envVariableValues["minV",j])/100
    if (i== j) {
      dataframe[,envVariableNames[j]] = seq(envVariableValues["minV",j],
                                            envVariableValues["maxV",j],interval)
    } else {
      dataframe[,envVariableNames[j]] = rep(envVariableValues["median",j],
                                            dim(dataframe)[1])
    }
  }
  predictions = list() # this list will store the predicted values for the ecological suitability
  # when considering the values of the successive rows of the "dataframe" generated above
  for (j in 1:length(brt_models)) { # a loop for to generate one curve per BRT analysis replicate
    n.trees = brt_models[[j]]$gbm.call$best.trees; type = "response"; single.tree = FALSE
    prediction = predict.gbm(brt_models[[j]], newdata=dataframe, n.trees, type, single.tree)
    if (j== 1) { # to following lines of code are for saving minimum and maximum x-axis values
      minX = min(dataframe[,envVariableNames[i]])
      maxX = max(dataframe[,envVariableNames[i]])
    } else {
      if (minX > min(dataframe[,envVariableNames[i]])) {
        minX = min(dataframe[,envVariableNames[i]])
      }
      if (maxX < max(dataframe[,envVariableNames[i]])) {
        maxX = max(ddataframe[,envVariableNames[i]])
      }
    }
    predictions[[j]] = prediction
  }
  col = rgb(222,67,39,255,maxColorValue=255) # red
  for (j in 1:length(brt_models)) {
    if (j== 1) {
      plot(dataframe[,envVariableNames[i]], predictions[[j]], col=col, ann=F,
           axes=F, lwd=0.2, type="l", xlim=c(minX,maxX), ylim=c(0,1))
    } else {
      lines(dataframe[,envVariableNames[i]], predictions[[j]], col=col, lwd=0.2)
    }
  }
  axis(side=1, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.15,0))
  axis(side=2, lwd.tick=0.2, cex.axis=0.7, lwd=0, tck=-0.040, col.axis="gray30", mgp=c(0,0.4,0))
  title(xlab=envVariableTitles[i], cex.lab=0.9, mgp=c(1.0,0,0), col.lab="gray30")
  box(lwd=0.2, col="gray30")
}

