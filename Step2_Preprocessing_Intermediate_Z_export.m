clc
clear

cd('X:/My Documents/Research project/Gloria/test');
Z = load('Z_2019.mat');
Z_total = cat(1, Z.Z_up, Z.Z_low);

Sector_binary = readmatrix("Sectors_binary.csv");
NL_binary = Sector_binary(:, 5); %NL=0 to capture only export and not intermediate trade within NL
NL_binary_t = transpose(NL_binary);

Z_NL_export = Z_total .* NL_binary_t; 

sectors = readmatrix("sectors.csv");

Agriculture_based_binary = Sector_binary(:, 1);
Plant_based_binary = Sector_binary(:, 2); %binary vectors were only Dutch sectors are selected
Animal_based_binary = Sector_binary(:, 3);
Totalexport_based_binary = Sector_binary(:, 4);

Z_NL_export_plant_based = Z_NL_export .* Plant_based_binary;
Z_NL_export_plant_based = sum(Z_NL_export_plant_based, 2);

Z_NL_export_animal_based = Z_NL_export .* Animal_based_binary; 
Z_NL_export_animal_based = sum(Z_NL_export_animal_based, 2);

Z_NL_export_totalexport_based = Z_NL_export .* Totalexport_based_binary;
Z_NL_export_totalexport_based = sum(Z_NL_export_totalexport_based, 2);

Z_NL_export_agriculture = Z_NL_export .* Agriculture_based_binary;
Z_NL_export_agriculture = sum(Z_NL_export_agriculture, 2);

save('Z_NL_plant', 'Z_NL_export_plant_based');
save('Z_NL_animal', 'Z_NL_export_animal_based');
save('Z_NL_agriculture', 'Z_NL_export_agriculture');
save('Z_NL_totalexport', 'Z_NL_export_totalexport_based');

clear 