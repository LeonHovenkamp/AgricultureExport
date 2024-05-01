%this is similar to the emission matrix multiplication
clc
clear
cd('X:/My Documents/Research project/Gloria/test');


%% calculating value added

Y_diags = {
    {'agriculture', 'Y_NL.csv'};
    {'totalexport', 'Y_total_Totalexport.csv'};
    {'plant', 'Y_total_plant.csv'};
    {'animal', 'Y_total_animal.csv'};
};

X = load("X_2019.mat"); %as prepared in step 3
X.X(X.X == 0) = 1;
X = X.X;

L = load('L_2019.mat'); % Load L matrix
L_total = cat(1, L.L_up, L.L_low);
clear L


V_total = load("V_2019_total.mat"); %as prepared in step 1
i
E_coef_diag = diag(V_total.V_total) / diag(X);

MP = E_coef_diag * L_total;

for y = 1:numel(Y_diags)
    y_type = Y_diags{y}{1};
    y_file = Y_diags{y}{2};

    Y_diag = diag(readmatrix(y_file));
    
    FP2 = MP * Y_diag;
    FP_export = FP2(:, 13561:13680);

    save(['FP_VA_' y_type '.mat'], 'FP_export');

    clear FP2 FP_export;
    
    i
end

