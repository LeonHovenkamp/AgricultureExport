#----------------------------------------------------------------------------------------
#----------------------- Final demand setup (step 2)------------------------------------------
#----------------------------------------------------------------------------------------

library(Matrix)
library(data.table)
library(R.matlab)

setwd("X:/My Documents/Research project/Gloria/test")

# read region classification-----------------
regions <- read.csv("Regions.csv.", sep = ";")
# read commodity classification---------------
sectors <- read.csv("Sectors.csv", sep = ";")
nrreg <- nrow(regions)
nrcom <- nrow(sectors)
index <- data.table(code = rep(regions$area_code, each = nrcom),
                    iso3c = rep(regions$Region_acronyms, each = nrcom),
                    country = rep(regions$Region_names, each = nrcom),
                    item = rep(sectors$Sector_names, nrreg))
#write.csv(index, "./index.csv", row.names = FALSE)


Y <- readMat("Y_2019.mat")
Y <- do.call(rbind, Y)

#----------Creating FD by selecting dutch export and excluding dutch consumption------------------
Y_codes <- read.csv("Y_codes.csv", sep = ";") #downloaded from GLORIA database
Y_codes <- (Y_codes[, "Code"])
Y_codes_binary <- ifelse(Y_codes == "NLD", 0, 1) #excluding dutch domestic demand
Y_NL <- t(t(Y) * Y_codes_binary) #this is the FD demand table without dutch domestic demand check!!!
NL_binary <- ifelse(index[, 1] == "NLD", 1, 0) #matrix with 1s for NL and 0 for nonNL
Y_NL_tot <- rowSums(Y_NL) * NL_binary


#----------------------introducing the intermediate export (prepared in matlab)----------------------------------------

Sectors_binary <- read.csv("Sectors_binary.csv", sep = ";")


#plant_based_
Plant_based_binary <- Sectors_binary[, 2]
Y_plant_interm_export <- as.matrix(unlist(readMat("Z_NL_plant.mat"))) #as prepared in Matlab, see step 2
Y_total_plant <- (Y_NL_tot * Plant_based_binary) + Y_plant_interm_export
write.csv(Y_total_plant, "Y_total_plant.csv", row.names = FALSE)


#animal_based_
Animal_based_binary <- Sectors_binary[, 3] 
Y_animal_interm_export <- as.matrix(unlist(readMat("Z_NL_animal.mat"))) #as prepared in Matlab see, step 2
Y_total_animal <- (Y_NL_tot * Animal_based_binary) + Y_animal_interm_export
write.csv(Y_total_animal, "Y_total_animal.csv", row.names = FALSE)

#totalexport_based
Totalexport_based_binary <-Sectors_binary[, 4] 
Y_Totalexport_interm_export <- as.matrix(unlist(readMat("Z_NL_Totalexport.mat"))) #as prepared in Matlab, see step 2
Y_total_Totalexport <- (Y_NL_tot * Totalexport_based_binary) + Y_Totalexport_interm_export
write.csv(Y_total_Totalexport, "Y_total_Totalexport.csv", row.names = FALSE)


#agriculture
Agriculture_based_binary <- Sectors_binary[, 1]
Y_NL_interm_export <- as.matrix(unlist(readMat("Z_NL_agriculture.mat"))) #as prepared in Matlab, see step 2
Y_total_NL <- (Y_NL_tot * Agriculture_based_binary) + Y_NL_interm_export
write.csv(Y_total_NL, "Y_NL.csv", row.names = FALSE)
