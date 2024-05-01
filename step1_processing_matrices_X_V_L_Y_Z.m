clc
clear
cd('X:/My Documents/Research project/Gloria/');

%%%%%%%%GET Z_matrix%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read 2019's Z, and store them as matlab array
% 164 regions and 120 sectors
data = readtable('20230314_120secMother_AllCountries_002_T-Results_2019_057_Markup001(full).csv');
savefile='Z_2019';
Z=zeros(19680,19680);
for i=1:164
    r0=i*240-119;
    r1=i*240; 
    rr0=i*120-119;
    rr1=i*120; 
    for j=1:164
    c0=240*j-239;
    c1=240*j-120;    
    c00=j*120-119;
    c11=j*120;   
    data_block=table2array(data(r0:r1,c0:c1));    
    Z(rr0:rr1,c00:c11)=data_block;   
    end
end
Z_up=Z(1:9840,:);
Z_low=Z(9841:19680,:);
save(savefile,'Z_up','Z_low')
clear

%%%%%%Get V-matrix %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%all data can be downloaded at https://ielab.info/resources/gloria/about

% read each year's V, and store them as matlab array
% 164 regions and 120 sectors
clear
clc
data =readtable('20230315_120secMother_AllCountries_002_V-Results_2019_057_Markup001(full).csv');
savefile='V_2019';
% check=data(1:200,1:960);
V=zeros(6,19680);
 for i=1:164
    r0=i*6-5;
    r1=i*6;  
    
    c0=i*240-239;
    c1=i*240-120;
    
    data_block=table2array(data(r0:r1,c0:c1));
    
    cc0=i*120-119;
    cc1=i*120;    
    V(:,cc0:cc1)=data_block;   
 end

save(savefile,'V_2019_total')

%%%%%%%Get Y_matrix %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data = readtable('20230314_120secMother_AllCountries_002_Y-Results_2019_057_Markup001(full).csv');
Y=zeros(19680,984);
for i=1:164
    r0=i*240-119;
    r1=i*240;       
    data_block=table2array(data(r0:r1,:));
    
    rr0=i*120-119;
    rr1=i*120;    
    Y(rr0:rr1,:)=data_block;   
end
save('Y_2019','Y')
clear


%% %%%%%Creating X-matrix%%
%%%%%%%%%%%%%%%%%%%%%%
Z = load("Z_2019.mat");
Y = load("Y_2019.mat");

Z_total = cat(1, Z.Z_up, Z.Z_low);
Z_sum = sum(Z_total, 2);
Y_total = cat(1, Y.Y);
Y_sum = sum(Y_total, 2);

X = Z_sum + Y_sum;

save("X_2019.mat", "X");

%% %%%% Creating L-matrix %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X=(load('X_2019.mat').X);
Z = load("Z_2019.mat");
Z_total = cat(1, Z.Z_up, Z.Z_low);
clear Z

c=X<=0;
X=X+c*0.001;
A=Z_total./(X');   

I=eye(19680);
L=inv(I-A);  
L_up=L(1:9840,:);
L_low=L(9841:19680,:);     

save("L_2019.mat",'L_up','L_low')  
clear L_up L_low I A X Z_total c L

%% loading the value added
clc

X = load("X_2019.mat");

V = load("V_2019.mat");
V_total = sum(V.V, 1);

V_coeff = diag((V_total ./ transpose(X.X)));

save("V_2019_total", "V_total")

