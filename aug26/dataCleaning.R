# Code to clean and export data for 'Interactive Textile Circulation Explorer' App

#Loading Libraries
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
library(shiny)

#Read in the data
joined.data.original <- read_csv("joined.csv")
# Making a second copy
joined.data.original1 <- read_csv("joined.csv")

hist_geo <- readRDS(file.choose())


#Data Cleaning - changed all the NA into 0 and "N/A"
joined.data.original1[["orig_yr"]][is.na(joined.data.original1["orig_yr"])]=0
joined.data.original1[["dest_yr"]][is.na(joined.data.original1["dest_yr"])]=0
joined.data.original1[["textile_quantity"]][is.na(joined.data.original1["textile_quantity"])]=0
joined.data.original1[["quant_ells"]][is.na(joined.data.original1["quant_ells"])]=0
joined.data.original1[["units_ells"]][is.na(joined.data.original1["units_ells"])]=0
joined.data.original1[["value_per_piece"]][is.na(joined.data.original1["value_per_piece"])]=0
joined.data.original1[is.na(joined.data.original1)]="N/A"

#changing textile names
joined.data.original1$textile_name[joined.data.original1$textile_name=="Kannekijns"]<-"Kannekyns"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Carroots"]<-"Corroots"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Pattamaroepoe"]<-"Pattamaraphoe"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Nicanees"]<-"Nickanees"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Deken"]<-"Dekens"
joined.data.original1$textile_name[joined.data.original1$textile_name=="Tannyzijde"]<-"Tannazijde"


#changing textile quality (eng)
joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="grof (rough, coarse?)"]<-"coarse"
joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="gemeen (common), grof (rough, coarse?)"]<-"common, coarse"

joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="fijn (fine)"]<-"fine"
joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="geraffineerd (refined)"]<-"fine"

joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="dicht geweven (thick/dense woven?)"]<-"heavy"
joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="dicht (thick/dense woven?), fijn (fine)"]<-"heavy, fine"
joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="zwaar (heavy/thick)"]<-"heavy"

joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="gemeen (common)"]<-"ordinary"
joined.data.original1$textile_quality_eng[joined.data.original1$textile_quality_eng=="ordinaris (ordinary)"]<-"ordinary"



#taking out the guessing columns
joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="grof (rough, coarse?)"]<-"grof"
joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="gemeen (common), grof (rough, coarse?)"]<-"gemeen, grof"

joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="fijn (fine)"]<-"fijn"
joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="geraffineerd (refined)"]<-"geraffineerd"

joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="dicht geweven (thick/dense woven?)"]<-"dicht geweven"
joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="dicht (thick/dense woven?), fijn (fine)"]<-"dicht, fijn"
joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="zwaar (heavy/thick)"]<-"zwaar"

joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="gemeen (common)"]<-"gemeen"
joined.data.original1$textile_quality_arch[joined.data.original1$textile_quality_arch=="ordinaris (ordinary)"]<-"ordinaris"



# Creating new origin and destination columns for names to join to historical regions on the map

joined.data.original1 <- joined.data.original1 %>%
  mutate(join_col_orig = ifelse(orig_loc_region_modern == 'Netherlands', 'DutchRep',
                                ifelse(orig_loc_region_modern == 'Southeast coast of India', 'Coromandel',
                                ifelse(orig_loc_region_modern == 'Northwest coast of Java, Indonesia', 'JavaNW',
                                ifelse(orig_loc_region_modern == 'Northwest India', 'Surat',
                                ifelse(orig_loc_region_modern == 'Malabar, Southwest coast of India', 'Malabar',
                                ifelse(orig_loc_region_modern == 'Sri Lanka', 'Ceylon', 
                                ifelse(orig_loc_region_modern == 'Sumatra, Indonesia', 'WSumatra',
                                ifelse(orig_loc_region_modern == 'Maluku, Indonesia', 'SpiceIslands', 
                                ifelse(orig_loc_region_modern == 'Iran', 'Persia',
                                ifelse(orig_loc_region_modern == 'Malacca, Southwest Malaysia', 'Malacca',
                                ifelse(orig_loc_region_modern == 'Bengal, India and Bangladesh', 'BayBengal', 
                                ifelse(orig_loc_region_modern == 'West Bengal, India', 'BayBengal', 
                                ifelse(orig_loc_region_modern == 'South Africa', 'CapeGoodHope', 
                                ifelse(orig_loc_region_modern == 'Thailand', 'Siam', 
                                ifelse(orig_loc_region_modern == 'South Sulawesi, Indonesia', 'Makassar', 
                                ifelse(orig_loc_region_modern == 'Japan', 'Japan', 
                                ifelse(orig_loc_region_modern == 'Northeast coast of Java, Indonesia', 'JavaNE',
                                ifelse(orig_loc_region_modern == 'Java, Indonesia', 'JavaNE', NA)))))))))))))))))),
         join_col_dest = ifelse(dest_loc_region == 'Netherlands', 'DutchRep',
                                ifelse(dest_loc_region == 'Southeast coast of India', 'Coromandel',
                                ifelse(dest_loc_region == 'Northwest coast of Java, Indonesia', 'JavaNW',
                                ifelse(dest_loc_region == 'Northwest India', 'Surat',
                                ifelse(dest_loc_region == 'Malabar, Southwest coast of India', 'Malabar',
                                ifelse(dest_loc_region == 'Sri Lanka', 'Ceylon', 
                                ifelse(dest_loc_region == 'Sumatra, Indonesia', 'WSumatra',
                                ifelse(dest_loc_region == 'Maluku, Indonesia', 'SpiceIslands', 
                                ifelse(dest_loc_region == 'Iran', 'Persia',
                                ifelse(dest_loc_region == 'Malacca, Southwest Malaysia', 'Malacca',
                                ifelse(dest_loc_region == 'Bengal, India and Bangladesh', 'BayBengal', 
                                ifelse(dest_loc_region == 'West Bengal, India', 'BayBengal', 
                                ifelse(dest_loc_region == 'South Africa', 'CapeGoodHope', 
                                ifelse(dest_loc_region == 'Thailand', 'Siam', 
                                ifelse(dest_loc_region == 'South Sulawesi, Indonesia', 'Makassar', 
                                ifelse(dest_loc_region == 'Japan', 'Japan', 
                                ifelse(dest_loc_region == 'Northeast coast of Java, Indonesia', 'JavaNE',
                                ifelse(dest_loc_region == 'Java, Indonesia', 'JavaNE',
                                ifelse(dest_loc_region == 'Angola', '',
                                ifelse(dest_loc_region == 'Benin', 'Ardra',
                                ifelse(dest_loc_region == 'Ghana', 'Elmina',
                                ifelse(dest_loc_region == 'West Timor, Indonesia', 'Timor',
                                ifelse(dest_loc_region == 'Banda Islands, Maluku, Indonesia', 'SpiceIslands',
                                ifelse(dest_loc_region == 'Yemen', 'Mokka', NA)))))))))))))))))))))))))
  #%>%
  #mutate(join_col_dest = )

#rows <- unique(joined.data.original1[c("dest_loc_region")])






#joined.data.original <- joined.data.original1

# Exporting data 
#write.csv(joined.data.original, "textileData.csv")




