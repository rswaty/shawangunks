

## try to get vdep chart per fireshed for 4fri area per top 2 BpSs

# use rlandfire package?  

# how to visualize/quantify?

# dependencies ----

library(foreign)
library(raster)
library(rlandfire)
library(scales)
library(sf)
library(terra)
library(tidyverse)

# shapefile of firesheds that intersect 4FRI
shp <- st_read("inputs/firesheds.shp") %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

# check the shape
vect(shp)
# plot the shape
plot(shp)

# read in attribute tables (I had LANDFIRE ones hand.  They do not come with downloaded tif files.  Firesheds came from .gdb posted by Kerry Metlen)
bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")
scls2020_conus_atts <- read.csv("inputs/LF20_SCla_220.csv")
scls2022_conus_atts <- read.csv("inputs/LF22_SCla_230.csv")
firesheds_atts <- read.csv("inputs/firesheds_atts.csv")
ref_con <- read_csv("inputs/ref_con_long.csv")



## get LANDFIRE data: bps, 2020 scls and 2022 scls ----

# ran on October 3, 2024 then commented out
          # aoi <- getAOI(shp)
          # 
          # 
          # # LFPS Parameters
          # products <-  c("200BPS", "220SCLASS", "230SCLASS")
          # projection <- 5070
          # resolution <- 30
          # 
          # 
          # # R specific arguments
          # save_file <- tempfile(fileext = ".zip")
          # 
          # # call API
          # ncal <- landfireAPI(products, 
          #                     aoi, 
          #                     projection, 
          #                     resolution, 
          #                     path = save_file)

# took just over 2 minutes

# extract data to inputs folder and split rasters  ----

# get location of downloaded file, then manually cut paste to inputs dir
          # tempdir()

# process datasets ----

bps_scl <- rast("inputs/j36c84271027d4778a93253e07c2fc593.tif")

# "split" downloaded raster into separate layers
for(lyr in names(bps_scl)) assign(lyr, bps_scl[[lyr]])


# bps 

bps_4fri_firesheds <- US_200BPS %>%
  crop(shp) %>%
  mask(shp)

plot(bps_4fri_firesheds)


bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")

# bps_aoi <-  bps_conus_r %>%
#   crop(shp) %>%
#   mask(shp)

levels(bps_4fri_firesheds)[[1]] <- bps_conus_atts
activeCat(bps_4fri_firesheds) <- "VALUE"


bps_4fri_firesheds_atts <- values(bps_4fri_firesheds, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_4fri_firesheds)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(bps_4fri_firesheds, "outputs/bps_4fri_firesheds.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(bps_4fri_firesheds_atts, "outputs/bps_4fri_firesheds.tif.vat.dbf")

## write csv for fun
write.csv(bps_4fri_firesheds_atts, "outputs/bps_4fri_firesheds_attributes.csv")


bps10_4fri_firesheds <- bps_4fri_firesheds_atts %>%
  group_by(BPS_NAME) %>%
  summarize(ACRES = sum(ACRES),
            REL_PERCENT = sum(REL_PERCENT)) %>%
  arrange(desc(REL_PERCENT)) %>%
  subset(BPS_NAME != "Open Water" & BPS_NAME != "Barren-Rock/Sand/Clay") %>%
  distinct(BPS_NAME, .keep_all = TRUE) %>%
  top_n(n = 10, wt = REL_PERCENT) 


# 2020 Succession Class ----


sclas2020_4fri_firesheds <- US_200SCLASS %>%
  crop(shp) %>%
  mask(shp)

scls2020_conus_atts <- read.csv("inputs/LF20_SCla_220.csv")

levels(sclas2020_4fri_firesheds)[[1]] <- scls2020_conus_atts
activeCat(sclas2020_4fri_firesheds) <- "VALUE"


scls2020_4fri_firesheds_atts <- values(sclas2020_4fri_firesheds, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(sclas2020_4fri_firesheds)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(sclas2020_4fri_firesheds, "outputs/sclas2020_4fri_firesheds.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(scls2020_4fri_firesheds_atts, "outputs/sclas2020_4fri_firesheds.tif.vat.dbf")

## write csv for fun
write.csv(scls2020_4fri_firesheds_atts, "outputs/sclas2020_4fri_firesheds.csv")


# 2022 Succession Class ----


sclas2022_4fri_firesheds <- US_230SCLASS %>%
  crop(shp) %>%
  mask(shp)


scls2022_conus_atts <- read.csv("inputs/LF22_SCla_230.csv")



levels(sclas2022_4fri_firesheds)[[1]] <- scls2022_conus_atts
activeCat(sclas2022_4fri_firesheds) <- "VALUE"


scls2022_4fri_firesheds_atts <- values(sclas2022_4fri_firesheds, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(sclas2022_4fri_firesheds)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(sclas2022_4fri_firesheds, "outputs/sclas2022_4fri_firesheds.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(scls2022_4fri_firesheds_atts, "outputs/sclas2022_4fri_firesheds.tif.vat.dbf")

## write csv for fun
write.csv(scls2022_4fri_firesheds_atts, "outputs/sclas2022_4fri_firesheds.csv")



# try to extract combined BpS/Slc raster that was downloaded ----

bps_scl_fs_2020 <- terra::extract(bps_scl, shp, df = TRUE, ID = TRUE)   %>%
    group_by(ID, US_200BPS, US_200SCLASS) %>%
    summarize(count = n()) 

bps_scl_fs_2022 <- terra::extract(bps_scl, shp, df = TRUE, ID = TRUE)   %>%
  group_by(ID, US_200BPS, US_230SCLASS) %>%
  summarize(count = n()) 


# wrangle BPS-SCLASS data for 2020 and 2022; merge together for charts ----


# 2020
bps_scl_fs_2020_full <- bps_scl_fs_2020 %>%
  left_join(scls2020_conus_atts %>% 
              dplyr::select(VALUE, LABEL), 
            by = c("US_200SCLASS" = "VALUE")) %>%
  left_join(bps_conus_atts %>%
              dplyr::select(VALUE, BPS_MODEL, BPS_NAME),
            by = c("US_200BPS" = "VALUE")) %>%
  group_by(BPS_MODEL) %>%
  mutate(total_count = sum(count)) %>%
  mutate(currentPercent = as.integer((count/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"), remove = FALSE)

# 2022
bps_scl_fs_2022_full <- bps_scl_fs_2022 %>%
  left_join(scls2022_conus_atts %>% 
              dplyr::select(VALUE, LABEL), 
            by = c("US_230SCLASS" = "VALUE")) %>%
  left_join(bps_conus_atts %>%
              dplyr::select(VALUE, BPS_MODEL, BPS_NAME),
            by = c("US_200BPS" = "VALUE")) %>%
  group_by(BPS_MODEL) %>%
  mutate(total_count = sum(count)) %>%
  mutate(currentPercent = as.integer((count/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"), remove = FALSE)


