library(dplyr)
library(magrittr)

frog <- read.csv("../FrogRock-inputforR.csv")
correct <- read.csv("../CorrectVars.csv")

test <- correct %>%
  select(Date)

frog <- frog %>%
  mutate(tmean = (tmin_degC+tmax_degC)/2)

Lat <- 44.95354
Lon <- -110.54083

test_acc <- 0.00005

## Test functions from WB v2 package

frog$jtemp <- as.numeric(get_jtemp(Lon, Lat))

test_that("jtemp_correct", {
    expect_equal(frog$Jennings.factor, frog$jtemp)
    })

frog$freeze <- get_freeze(frog$jtemp, frog$tmean)
test$freeze <- frog$freeze - correct$F

test_that("freeze_correct", {
    expect_true(max(test$freeze) <= test_acc)
})

frog$rain <- get_rain(frog$precip_mmday,frog$freeze)
test$RAIN <- frog$rain - correct$RAIN

test_that("rain_correct", {
    expect_true(max(test$RAIN) <= test_acc)
})

frog$snow <- get_snow(frog$precip_mmday, frog$freeze)
test$SNOW <- frog$snow - correct$SNOW

test_that("snow_correct", {
    expect_true(max(test$SNOW) <= test_acc)
})

frog$melt <- get_melt(frog$tmean, frog$jtemp, frog$Hock, frog$snow) #tested to make sure it works with init pack values other than 0
test$MELT <- frog$melt - correct$MELT

test_that("melt_correct", {
    expect_true(max(test$MELT) <= test_acc)
})

frog$snowpack <- get_snowpack(frog$jtemp, frog$snow, frog$melt) #tested to make sure it works with init pack values other than 0
test$PACK <- frog$snowpack - correct$PACK

test_that("pack_correct", {
    expect_true(max(test$PACK) <= test_acc)
})

frog$W <- get_w(frog$rain, frog$melt)
test$W <- frog$W - correct$W

test_that("W_correct", {
    expect_true(max(test$W) <= test_acc)
})

frog$PET <- get_OudinPET(frog$yday, Lat, frog$snowpack, frog$tmean, slope=2, frog$aspect, shade.coeff = 1)
test$PET <- frog$PET - correct$PET

test_that("PET_correct", {
    expect_true(max(test$PET) <= test_acc)
})


