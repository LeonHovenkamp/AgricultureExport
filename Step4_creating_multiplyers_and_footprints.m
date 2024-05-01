clear
clc
cd('X:/My Documents/Research project/Gloria/test');


EM = load("E_allCountries_selected.mat");

X = load("X_2019.mat"); %as prepared in step 3
X.X(X.X == 0) = 1;
X = X.X;

L = load('L_2019.mat'); %as prepared in step 3
L_total = cat(1, L.L_up, L.L_low);

%% Create footprints (FP) %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%selecting stressors based on GLORIA_readme_057_satellites
stressors_info = {
    'Land', [68, 69, 70, 71, 72];
    'Bio', [80, 81, 82, 83, 84, 85];
    'GHG', 2864;
    'Blue', [88, 89];
    'NH3', 2280;
    'Water Stress', [86, 87];
};

Y_diags = {
    {'agriculture', 'Y_NL.csv'};
    {'plant', 'Y_total_plant.csv'};
    {'animal', 'Y_total_animal.csv'};
    {'total_export', 'Y_total_Totalexport.csv'};
};

for y = 1:numel(Y_diags)
    y_type = Y_diags{y}{1};
    y_file = Y_diags{y}{2};
    
    
    Y_diag = diag(readmatrix(y_file));

    for i = 1:size(stressors_info, 1)
        stressor_name = stressors_info{i, 1};
        stressor_columns = stressors_info{i, 2};

        currentStressor = sum(EM.EM(:, stressor_columns), 2);

        E_coef_diag = diag(currentStressor) / diag(X);

        MP = E_coef_diag * L_total;
        FP2 = MP * Y_diag;

        FP_export = FP2(:, 13561:13680);

        save(['FP_' stressor_name '_' y_type '.mat'], 'FP_export');

        clear currentStressor E_coef_diag_land MP FP2 FP_export;
        i
    end
end