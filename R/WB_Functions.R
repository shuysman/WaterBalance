###################################################################################
## WB_functions.R
## Functions to calculate water balance variables from input climate data. 
## Based on Dave Thoma's water balance Excel spreadsheet model.
## Created 10/30/2019 by ARC, Updated 4/14/21 by MGS
## v1.1.0
###################################################################################

#' Temperature threshold using Jennings et al., 2018 to partition rain and snow
#'
#' Extracts the rain-snow temperature threshold from a raster.
#' @param Lat Latitude of the site (degrees).
#' @param Lon Longitude of the site (degrees).
#' @export
#' get_jtemp()

get_jtemp = function(Lat, Lon){
  j.raster = raster::raster(system.file("extdata","merged_jennings2.tif", package="WaterBalance")) 
  projection = sp::CRS("+init=epsg:4326")
  coords = cbind(Lat, Lon)
  sp = sp::SpatialPoints(coords, proj4string = projection)
  jtemp = raster::extract(j.raster, sp)
  return(jtemp)
}

#' Freeze factor using Jennings et al., 2018 thresholds to partition rain and snow
#'
#' Calculates a freeze factor from 0-1 based on a temperature threshold from Jennings et al., 2018 and average temperature.
#' @param jtemp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @export
#' get_freeze()

get_freeze = function(jtemp, tmean) {
    low_temp_threshold <- jtemp - 3
    high_temp_threshold <- jtemp + 3
    freeze <- dplyr::case_when(
        tmean <= low_temp_threshold ~ 0,
        tmean >= high_temp_threshold ~ 1,
        TRUE ~ (1 / (high_temp_threshold - low_temp_threshold)) * (tmean - low_temp_threshold)
    )
    return(freeze)
}

#' Rain
#'
#' Calculates rainfall totals based on precipitation and freeze factor.
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from average temperature and Jennings et al., 2018. Values are 0-1.
#' @export
#' get_rain()

get_rain = function(ppt, freeze){
  rain = ppt*freeze
  return(rain)
}

#' Snow
#'
#' Calculates snowfall totals based on precipitation and freeze factor.
#' @param ppt A vector of precipitation values.
#' @param freeze A vector of freeze factor values, calculated from average temperature and Jennings et al., 2018. Values are 0-1.
#' @export
#' get_snow()

get_snow = function(ppt, freeze){
  snow = (1 - freeze)*ppt
  return(snow)
}

#' Melt
#'
#' Calculates the amount of snowmelt at time steps from snowpack, temperature, and Hock melt factor.
#' @param tmean A vector of daily mean temperatures (deg C).
#' @param jtemp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param hock A melt factor of daily snowmelt when warm enough to melt.
#' @param snow A time series vector of snowfall values.
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_melt()

get_melt = function(tmean, jtemp, hock, snow, sp.0=NULL){
  sp.0 = ifelse(!is.null(sp.0), sp.0, 0)
  melt <- vector()
  melt[1] = ifelse(tmean[1] < (jtemp[1]-3)||sp.0==0, 0,
                   ifelse((tmean[1]-(jtemp[1]-3))*hock>sp.0, 
                          sp.0, (tmean[1]-(jtemp[1]-3))*hock))
  snowpack <- vector()
  snowpack[1] = sp.0 + snow[1] - melt[1]
  for(i in 2:length(tmean)){
    melt[i] = ifelse(tmean[i]<(jtemp[i]-3) | snowpack[i-1]==0, 0, 
                     ifelse((tmean[i]-(jtemp[i]-3))*hock>snowpack[i-1], 
                            snowpack[i-1], (tmean[i]-(jtemp[i]-3))*hock))
    snowpack[i] = snowpack[i-1]+snow[i]-melt[i]
  }
  return(melt)
}

#' Snowpack
#'
#' Calculates snowpack accumulation at time steps, from a time series of snowfall and melt.
#' @param jtemp the Jennings temperature extracted from the raster based on latitude and longitude.
#' @param snow A time series vector of snowfall values.
#' @param melt A time series vector of snowmelt.
#' @param sp.0 (optional) Initial snowpack value. Default is 0.
#' @export
#' get_snowpack()

get_snowpack = function(jtemp, snow, melt, sp.0=NULL){
  low_thresh_temp = jtemp - 3
  sp.i = ifelse(!is.null(sp.0), sp.0, 0)
  snowpack <- vector()
  for(i in 1:length(melt)){
    snowpack[i] = sp.i+snow[i]-melt[i]
    sp.i=snowpack[i]
  }
  return(snowpack)
}

#' Modify PET
#'
#' Modifies PET by heat load according to method by Lutz et al. (2010).
#' @param pet A time series vector of PET values.
#' @param slope Slope of the site (in degrees).
#' @param aspect Aspect of the site (in degrees).
#' @param lat Latitude of the site (in degrees).
#' @param freeze A vector of freeze factor values, calculated from Tmean. Values are 0-1.
#' @param shade.coeff (optional) A shade coefficient from 0-1. Default is 1.
#' @export
#' modify_PET()

modify_PET = function(pet, slope, aspect, lat, freeze, shade.coeff=NULL){
  f.aspect = abs(180 - abs(aspect - 225))
  lat.rad = ifelse(lat > 66.7, (66.7/180)*pi, (lat/180)*pi)
  slope.rad = (slope/180)*pi
  aspect.rad = (f.aspect/180)*pi
  heat.load = 0.339+0.808*cos(lat.rad)*cos(slope.rad) - 0.196*sin(lat.rad)*sin(slope.rad) - 0.482*cos(aspect.rad)*sin(slope.rad)
  sc = ifelse(!is.null(shade.coeff), shade.coeff, 1)
  freeze = ifelse(freeze == 0,0,1)
  PET.Lutz = pet*heat.load*sc*freeze
  return(PET.Lutz)
}

#' Water reaching soil surface as snow plus rain
#'
#' Calculates water reaching soil surface using rain and snowmelt. 
#' @param rain A vector of daily rain.
#' @param melt A vector of daily snowmelt.
#' @export
#' get_w()

get_w = function(rain, melt){
  w = (melt+rain)
  return(w)
}

#' Water reaching soil surface minus PET
#'
#' Calculates water reaching soil surface minues the PET.
#' @param w A time series vector of water reaching soil surface as snow plus rain.
#' @param pet A time series vector of PET values.
#' @export
#' get_w_pet()

get_w_pet = function(w, pet){
  w_pet = (w-pet)
  return(w_pet)
}

#' Soil Water Content (SWC)
#'
#' Calculates soil water content from available water (rain + snowmelt), PET, max. water-holding capacity, and initial SWC.
#' @param w  A time series vector of available water for soil charging (rain + snowmelt).
#' @param pet A time series vector of PET.
#' @param w_pet A time series vector of the difference between w and pet.
#' @param swc.max The maximum soil water-holding capacity of the soil layer being assessed.
#' @param swc.0 (optional) The initial soil water content value. Default is 0.
#' @export
#' get_soil()

get_soil = function(w, swc.0=NULL, pet, w_pet, swc.max){
  swc.i = ifelse(!is.null(swc.0), swc.0,0)
  soil=c()
  for(i in 1:length(pet)){
    soil[i] = ifelse(w[i]>pet[i], min((w_pet[i]+swc.i),swc.max), swc.i-swc.i*(1-exp(-(pet[i]-w[i])/swc.max)))
    swc.i=soil[i]
  }
  return(soil)  
}

#' Daily change in Soil Water Content (SWC)
#'
#' Calculates daily change in soil water content.
#' @param swc A time series vector of soil water content.
#' @param swc.0 (optional) The initial soil water content value. Default is 0.
#' @export
#' get_d_soil()

get_d_soil=function(swc, swc.0=NULL){
  swc.0 = ifelse(!is.null(swc.0), swc.0, 0)
  d_soil = swc - lag(swc, default=swc.0)
  return(d_soil)
}

#' Actual Evapotranspiration (AET)
#'
#' Calculates actual evapotranspiration (AET) from available water, PET, and soil water.
#' @param w  A time series vector of available water for soil charging (rain + snowmelt).
#' @param pet A time series vector of PET.
#' @param swc A time series vector of soil water content.
#' @param swc.0 (optional) The initial soil water content value. Default is 0.
#' @export
#' get_AET()

get_AET = function(w, pet, swc, swc.0=NULL){
  swc.i = ifelse(!is.null(swc.0), swc.0, 0)
  AET = c()
  for(i in 1:length(w)){
    AET[i] = ifelse(w[i] > pet[i], pet[i], w[i]+swc.i-swc[i])
    swc.i = swc[i]
  }
  return(AET)
}

#' Runoff or excess input greater than soil water holding capacity
#' 
#' Calculates runoff at daily timesteps based on water reaching soil surface, AET, change in soil moisture, and a runoff coefficient
#' @param ppt A vector of precipitation values.
#' @param w A time series vector of available water for soil charging (rain + snowmelt).
#' @param d_soil A time series vector of change in soil moisture from previous day.
#' @param AET A time series vector of actual evapotranspiration.
#' @param R.coeff A fraction of precpitation that can be shunted to direct runoff.
#' @export
#' get_runoff()

get_runoff=function(ppt, w, d_soil, AET, R.coeff=NULL){
  R.coeff = ifelse(!is.null(R.coeff), R.coeff, 0)
  DRO = ppt*(R.coeff/100)
  runoff = w-d_soil-AET+DRO
  return(runoff)
}

#' Climatic water deficit
#' 
#' Calculates daily climatic water deficit, which is PET - AET.
#' @param pet A time series vector of PET.
#' @param AET A time series vector of actual evapotranspiration.
#' @export
#' get_deficit()

get_deficit=function(pet, AET){
  deficit = pet-AET
  return(deficit)
}

#' Growing Degree-Days
#'
#' Calculates growing degree-days at daily time steps based on mean temperature and a threshold temperature.
#' @param tmean A time series vector of daily mean temperatures (deg C).
#' @param tbase (optional) A threshold temperature, above which growing degree-days are calculated. Default is 0.
#' @export
#' get_GDD()

get_GDD = function(tmean, tbase=NULL){
  tb = ifelse(!is.null(tbase), tbase, 0)
  GDD = ifelse(tmean < tb, 0, tmean - tb)
  return(GDD)
}
