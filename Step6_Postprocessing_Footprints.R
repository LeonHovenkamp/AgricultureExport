# Load necessary libraries if not already loaded
# install.packages("R.matlab")
# install.packages("dplyr")
library(R.matlab)
library(dplyr)
library(data.table)

setwd("X:/My Documents/Research project/Gloria/test")

#setting up the index
regions <- read.csv("Regions.csv.", sep = ";")
sectors <- read.csv("Sectors.csv", sep = ";")
nrreg <- nrow(regions)
nrcom <- nrow(sectors)
index <- data.table(code = rep(regions$area_code, each = nrcom),
                    iso3c = rep(regions$Region_acronyms, each = nrcom),
                    country = rep(regions$Region_names, each = nrcom),
                    item = rep(sectors$Sector_names, nrreg))
rm(nrcom, nrreg)

# Load your regions and sectors data (assuming they are loaded elsewhere in your script)

#----------------------------------------------------------------
#-------------------Value added (total)------------------------
#----------------------------------------------------------------

Value_added <- readMat("FP_VA_agriculture.mat") #prepared in matlab step 5
#there is the option to insert "_plant" or "_animal" to analyse the plant-based and animal-based value added
Value_added <- Value_added$FP.export
Value_added[is.na(Value_added)] <- 0

rows_per_group <- 120

num_groups <- nrow(Value_added) / rows_per_group

Value_added_per_country <- matrix(0, nrow = num_groups, ncol = ncol(Value_added), dimnames = list(NULL, colnames(Value_added)))

for (group_index in 1:num_groups) {
  start_row <- (group_index - 1) * rows_per_group + 1
  end_row <- group_index * rows_per_group
  Value_added_per_country[group_index, ] <- colSums(Value_added[start_row:end_row, , drop = FALSE])
}

rownames(Value_added_per_country) <- regions$Region_names
colnames(Value_added_per_country) <- sectors$Sector_names

countries_to_remove <- c("Burundi", "Brunei Darussalam", "Buthan", "Eritrea", "DR Yemen (Aden)","Djibouti", 
                         "Gambia", "Liberia", "Rest of Americas", "Rest of Europe", "Rest of Africa", "Rest of Asia-Pacific",
                         "North Korea", "Moldova", "Turkmenistan", "Equatorial Guinea", "Belarus", "South Sudan",
                         "Syria", "Serbia", "Palestine", "Hong Kong", "Bahamas", "United Arab Emirates", "Luxembourg", "Malta", "Kuwait", "Singapore",
                         "Zimbabwe", "Venezuela")
Value_added_per_country <- as.data.frame(cbind(regions$Region_names, Value_added_per_country))
Value_added_per_country <- Value_added_per_country[!(Value_added_per_country$V1 %in% countries_to_remove), ] #remove smaller regions (economies)



#Postprocessing the Footprints as prepared in step 4 (matlab)
#------------------------------------------------------------
stressors <- c("Land", "Bio", "GHG", "Blue", "NH3")

for (stressor in stressors) {
FP_data <- readMat(paste0("FP_", stressor, "_agriculture.mat")) #there is the option to insert "_plant" or "_animal" to analyse the plant-based and animal-based footprints
FP_data <- FP_data$FP.export
  
rows_per_group <- 120
num_groups <- nrow(FP_data) / rows_per_group
FP_per_country <- matrix(0, nrow = num_groups, ncol = ncol(FP_data), dimnames = list(NULL, colnames(FP_data)))
  
for (group_index in 1:num_groups) {
start_row <- (group_index - 1) * rows_per_group + 1
end_row <- group_index * rows_per_group
FP_per_country[group_index, ] <- colSums(FP_data[start_row:end_row, , drop = FALSE])
  }
  
rownames(FP_per_country) <- regions$Region_names
FP_per_country_agg <- as.matrix(rowSums(FP_per_country))
  
  
# Calculating the ratios
FP_per_country_agg <- as.data.frame(cbind(regions$Region_names, FP_per_country))
FP_per_country_agg <- FP_per_country_agg[!(FP_per_country_agg$V1 %in% countries_to_remove), ] #remove smaller regions (economies)
FP_per_country_agg <- FP_per_country_agg[ , -1]
FP_per_country_agg <- as.data.frame(lapply(FP_per_country_agg, as.numeric))
   
#Here the value added ratio (country value added/ total value added) is calculated
Value_added_per_country <- Value_added_per_country[ , -1]
Value_added_per_country <- as.data.frame(lapply(Value_added_per_country, as.numeric))
Value_added_per_country_agg <- as.matrix(rowSums(Value_added_per_country))
Value_added_share <- Value_added_per_country_agg / colSums(Value_added_per_country_agg)

#Here the ratio country FP/ total FP is calculated
FP_per_country_agg <- as.matrix(rowSums(FP_per_country_agg))
FP_share <- FP_per_country_agg / colSums(FP_per_country_agg)
  
Ratio_FP_VA <- FP_share / Value_added_share #this is the EV ratio
  
FP_per_country <- as.data.frame(cbind(regions$Region_names, FP_per_country))
FP_per_country <- FP_per_country[!(FP_per_country$V1 %in% countries_to_remove), ]
  
#Exporting output
output_folder <- "./Results/"
write.csv(Ratio_FP_VA, paste0(output_folder, "EV_", stressor, "agriculture.csv"), row.names = FALSE)
write.csv(FP_per_country_agg, paste0(output_folder, "FP_per_country_", stressor, "agriculture.csv"))
write.csv(FP_per_country, paste0(output_folder, "FP_per_country_total_", stressor, "agriculture.csv"))
}

write.csv(Value_added_per_country_agg, paste0("./Results/", "VA_per_country_agriculture.csv"), row.names = FALSE)