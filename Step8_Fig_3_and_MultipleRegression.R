library(tibble)
library(ggplot2)
library(ggbreak)
library(dplyr)
library(ggbreak)
library(cowplot)
library(gridExtra)
#set color vector
color_vec_10 <- c("#EBCC2A","#5BBCD6","#F98400","#02401B","#F21A00", "#9986A5","#00A08A","#046C9A",  "#79402E","#808080")
labels_vec <- c("land use", "NH3","Blue water", "GHG emission","Biodiversity")
#here all labels can be named.


#first remove all regions
regions <- read.csv("Regions.csv.", sep = ";")
countries_to_remove <- c("Burundi", "Brunei Darussalam", "Buthan", "Eritrea", "DR Yemen (Aden)","Djibouti", 
                         "Gambia", "Liberia", "Rest of Americas", "Rest of Europe", "Rest of Africa", "Rest of Asia-Pacific",
                         "North Korea", "Moldova", "Turkmenistan", "Equatorial Guinea", "Belarus", "South Sudan",
                         "Syria", "Serbia", "Palestine", "Hong Kong", "Bahamas", "United Arab Emirates", "Luxembourg", "Malta", "Kuwait", "Singapore",
                         "Zimbabwe", "Venezuela")

Regions2 <-regions[!(regions$Region_names %in% countries_to_remove), ] 

#-------------------------------------------------------------------------------
#-------------------------------Creating EV-figure Fig. 3 and Fig. S6-----------
#-------------------------------------------------------------------------------


#load data
GHG_EV <- read.csv("./Results/EV_GHGagriculture.csv")
Bio_EV <- read.csv("./Results/EV_Bioagriculture.csv")
Land_EV <- read.csv("./Results/EV_Landagriculture.csv")
Blue_EV <- read.csv("./Results/EV_Blueagriculture.csv")
NH3_EV <- read.csv("./Results/EV_NH3agriculture.csv")


#GHG_EV <- read.csv("./Results/EV_GHGtotalexport.csv")
#Bio_EV <- read.csv("./Results/EV_Biototalexport.csv")
#Land_EV <- read.csv("./Results/EV_Landtotalexport.csv")
#Blue_EV <- read.csv("./Results/EV_Bluetotalexport.csv")
#NH3_EV <- read.csv("./Results/EV_NH3totalexport.csv")


rm(df_EV, df_GHG, df_EV3, df_Land, df_EV5)
df_GHG_a <- as.data.frame(GHG_EV)
df_GHG<- rownames_to_column(as_tibble(df_GHG_a), var = "RowNames")
df_GHG <- cbind(Regions2$Region_names, df_GHG$V1)
colnames(df_GHG) <- c("region", "EV") 
df_GHG <- as.data.frame(df_GHG)
df_GHG$EV <- as.numeric(df_GHG$EV)
df_GHG$log_Value <- log(df_GHG$EV)

rm(df_Land)
df_Land_a <- as.data.frame(Land_EV)
df_Land<- rownames_to_column(as_tibble(df_Land_a), var = "RowNames")
df_Land <- cbind(Regions2$Region_names, df_Land$V1)
colnames(df_Land) <- c("region", "EV") 
df_Land <- as.data.frame(df_Land)
df_Land$EV <- as.numeric(df_Land$EV)
df_Land$log_Value <- log(df_Land$EV)

df_Bio_a <- as.data.frame(Bio_EV)
df_Bio<- rownames_to_column(as_tibble(df_Bio_a), var = "RowNames")
df_Bio <- cbind(Regions2$Region_names, df_Bio$V1)
colnames(df_Bio) <- c("region", "EV") 
df_Bio <- as.data.frame(df_Bio)
df_Bio$EV <- as.numeric(df_Bio$EV)
df_Bio$log_Value <- log(df_Bio$EV)

df_Blue_a <- as.data.frame(Blue_EV)
df_Blue<- rownames_to_column(as_tibble(df_Blue_a), var = "RowNames")
df_Blue <- cbind(Regions2$Region_names, df_Blue$V1)
colnames(df_Blue) <- c("region", "EV") 
df_Blue <- as.data.frame(df_Blue)
df_Blue$EV <- as.numeric(df_Blue$EV)
df_Blue$log_Value <- log(df_Blue$EV)

df_NH3_a <- as.data.frame(NH3_EV)
df_NH3<- rownames_to_column(as_tibble(df_NH3_a), var = "RowNames")
df_NH3 <- cbind(Regions2$Region_names, df_NH3$V1)
colnames(df_NH3) <- c("region", "EV") 
df_NH3 <- as.data.frame(df_NH3)
df_NH3$EV <- as.numeric(df_NH3$EV)
df_NH3$log_Value <- log(df_NH3$EV)


rm(df_N2O_a, df_Blue_a, df_Bio_a, df_Land_a, df_GHG_a, df_NH3_a)
rm(Bio_EV, Blue_EV, GHG_EV, Land_EV, N2O_EV)




rm(df_EV5)
df_EV5 <- cbind(Regions2$Region_acronyms, df_Land, df_Bio$EV, df_Bio$log_Value, df_Blue$EV, 
                df_Blue$log_Value, df_GHG$EV, df_GHG$log_Value, df_NH3$EV, df_NH3$log_Value)
colnames(df_EV5) <- c("Iso3", "region", "EV_land", "Log_land","EV_Bio", "Log_Bio", "EV_Blue", "Log_Blue", "EV_CO2", 
                      "Log_CO2","EV_NH3", "Log_NH3")

df_EV5$Average <- (df_EV5$Log_land + df_EV5$Log_Blue + df_EV5$Log_Bio + df_EV5$Log_CO2 + df_EV5$Log_NH3)/ 5 #depends on nr of stressors



#-------------------------------------------------------------------------------
#--------------------------Fig 3 and 6 ranking and creating---------------------
#-------------------------------------------------------------------------------


#ranked by average stress
df_EV5 <- arrange(df_EV5, desc(Average))

#give north-south color------------
Global_north <- read.csv("./Regions - Emerging non emerging.csv", sep = ";")
df_EV5$Latitude <- Global_north$X[match(df_EV5$Iso3, Global_north$Region_acronyms)]
df_EV5$Global_north <- ifelse(df_EV5$Latitude == "Global South", "black", "red")
df_EV5$region <- factor(df_EV5$region, levels = unique(df_EV5$region))



#plot figure (3 subfigures and the combine in one figure)
P1 <- ggplot(df_EV5, aes(x= region, y=df_EV5$Log_land)) +
  geom_hline(yintercept = 0) +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_land), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_Blue), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_Bio), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_CO2), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_NH3), size = 0.17, color="skyblue") +
  geom_point(aes(x = region, y = Log_land, color = "Land"), size = 1.6, alpha = 1) + #add here geom plot order to create different colors for the points
  geom_point(aes(x = region, y = Log_Blue, color = "Blue"), size = 1.6, alpha = 1) +  
  geom_point(aes(x = region, y = Log_Bio, color = "Bio"), size = 1.6, alpha = 1) + 
  geom_point(aes(x = region, y = Log_CO2, color = "CO2"), size = 1.6, alpha = 1) + 
  geom_point(aes(x = region, y = Log_NH3, color = "NH3"), size = 1.6, alpha = 1) + 
  scale_y_continuous(limits=c(log(0.00005),log(2000)),
                     breaks = c(log(0.0005),log(0.05),log(0.1),log(0.2),log(0.5),log(1),log(2),log(5),log(10), log(20),log(250)),
                     labels = c("0.0003","0.05","0.1","0.2","0.5","1","2","5","10","20","250"))+
  scale_colour_manual(name = "Pressures", 
                     values = color_vec_10,
                     labels = c("Bio", "Water","GHG" ,"Land" , "NH3")) + 
  ylab("Ranked by average EV-ratio (share of pressure / share of VA)") +
  theme(
    axis.text.y = element_text(color= df_EV5$Global_north, size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size=6),
    text = element_text(size=7),
    panel.grid.minor = element_line(color = "#FFFFFF",
                                    size = 0.1),
    panel.grid.major = element_line(color = "#FFFFFF",
                                    size = 0.15),
    axis.ticks = element_line(colour = "black", size = 0.5),
    axis.ticks.length=unit(0.05, "cm"),
    axis.text.x=element_text(colour = "black", size=6),
        legend.key.size = unit(0.01, 'cm'),
    legend.text =  element_text(size = 10),
    legend.title =  element_text(size = 10),
    legend.box="vertical",
    legend.direction="horizontal",
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
  ) +
  guides(color = guide_legend(nrow=2,override.aes = list(size = 2) ,title.position = "left") )  +
  coord_flip()

#P1 <- P1 + scale_y_break(c(log (0.00020), log(0.05), log(60), log(220))) 

P1


#ranked by GDP per capita------------------------------------------------------------------------
GDP_per_capita <- read.csv("./GPD_per_capita.csv")
GDP_per_capita$X2019..YR2019. <- as.numeric(GDP_per_capita$X2019..YR2019.)
GDP_per_capita$Country.Name <- reorder(GDP_per_capita$Country.Name, GDP_per_capita$X2019..YR2019., decreasing = FALSE)

df_EV5$GDP <- GDP_per_capita$X2019..YR2019.[match(df_EV5$Iso3, GDP_per_capita$Country.Code)]
df_EV5$region <- reorder(df_EV5$region, df_EV5$GDP, decreasing = FALSE) #reorder by average GDP
#df_EV5 <- df_EV5[complete.cases(df_EV5[, "GDP"]), ]
df_EV5 <- arrange(df_EV5, GDP)


#give north-south color------------
Global_north <- read.csv("./Regions - Emerging non emerging.csv", sep = ";")
df_EV5$Latitude <- Global_north$X[match(df_EV5$Iso3, Global_north$Region_acronyms)]
df_EV5$Global_north <- ifelse(df_EV5$Latitude == "Global South", "black", "red")
df_EV5$region <- factor(df_EV5$region, levels = unique(df_EV5$region))



GDP <- ggplot(df_EV5, aes(x= region, y=df_EV5$Log_land)) +
  geom_hline(yintercept = 0) +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_land), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_Blue), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_Bio), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_CO2), size = 0.17, color="skyblue") +
  geom_segment( aes(x=region, xend=region, y=0, yend=Log_NH3), size = 0.17, color="skyblue") +
  geom_point(aes(x = region, y = Log_land, color = "Land"), size = 1.6, alpha = 1) + #add here geom plot order to create different colors for the points
  geom_point(aes(x = region, y = Log_Blue, color = "Blue"), size = 1.6, alpha = 1) +  
  geom_point(aes(x = region, y = Log_Bio, color = "Bio"), size = 1.6, alpha = 1) + 
  geom_point(aes(x = region, y = Log_CO2, color = "CO2"), size = 1.6, alpha = 1) + 
  geom_point(aes(x = region, y = Log_NH3, color = "NH3"), size = 1.6, alpha = 1) + 
  scale_y_continuous(limits=c(log(0.00005),log(2000)),
                     breaks = c(log(0.0005),log(0.05),log(0.1),log(0.2),log(0.5),log(1),log(2),log(5),log(10), log(20),log(250)),
                     labels = c("0.0003","0.05","0.1","0.2","0.5","1","2","5","10","20","250"))+
  scale_colour_manual(name = "Pressures", 
                      values = color_vec_10) + 
  ylab("Ranked by GDP per capita") +
  theme(
    axis.text.y = element_text(color= df_EV5$Global_north, size = 7),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size=6),
    text = element_text(size=7),
    panel.grid.minor = element_line(color = "#FFFFFF",
                                    size = 0.1),
    panel.grid.major = element_line(color = "#FFFFFF",
                                    size = 0.15),
    axis.ticks = element_line(colour = "black", size = 0.5),
    axis.ticks.length=unit(0.05, "cm"),
    axis.text.x=element_text(colour = "black", size=6),
    legend.key.size = unit(0.01, 'cm'),
    legend.text =  element_text(size = 5),
    legend.title =  element_text(size = 6),
    legend.box="vertical",
    legend.direction="horizontal",
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
  ) +
  guides(color = guide_legend(nrow=2,override.aes = list(size = 1) ,title.position = "left") )  +
  coord_flip()
GDP <- GDP + scale_y_break(c(log (0.00020), log(0.05), log(60), log(220)))
#GDP


#combining the plots-----------------------------------------

Legend_EV <- get_legend(P1)

GDP <- GDP + theme(legend.position = "none")
P2 <- P1 + theme(legend.position = "none")

plots_row1 <- grid.arrange(P2, GDP, ncol = 2, widths = c(0.95, 0.95))

EV_plots <- grid.arrange(plots_row1, Legend_EV, ncol = 1, heights = c(8, 0.5))



ggsave("FigureS6.PNG", plot = EV_plots, width = 0.7 * 17, height = 16, units = "in") #18.9
#ggsave("EV_plots.PDF", plot = EV_plots, width = 0.6 * 17, height = 11, units = "in") #18.9


#-------------------------------------------------------------------------------
#--------------------Multiple regression analysis-------------------------------
#-------------------------------------------------------------------------------

#ranked by GDP per capita
GDP_per_capita <- read.csv("./GPD_per_capita.csv")
GDP_per_capita$X2019..YR2019. <- as.numeric(GDP_per_capita$X2019..YR2019.)
df_EV5$GDP <- GDP_per_capita$X2019..YR2019.[match(df_EV5$Iso3, GDP_per_capita$Country.Code)]

#ranked by NorhtSouth
Global_north <- read.csv("./Regions - Emerging non emerging.csv", sep = ";")
df_EV5$Global_north <- Global_north$X[match(df_EV5$Iso3, Global_north$Region_acronyms)]

#Data_for_regression <- df_EV5[, -c(3:14)]
Data_for_regression <- df_EV5
Data_for_regression$GDP <- Data_for_regression$GDP/10000



#inEUorNOt
EU <- as.data.frame(read.csv("./EU_or_not.csv", sep = ";"))
Data_for_regression$EU <- ifelse(Data_for_regression$region %in% EU$Country, 1, 0) #1 = EU
#south_north
Data_for_regression$Global_north <- ifelse(Data_for_regression$Global_north == "Global North", 1, 0) #1 = global north


Data_for_regression$GDP_EU <- Data_for_regression$GDP*Data_for_regression$EU
Data_for_regression$GDP_Global_north <- Data_for_regression$GDP*Data_for_regression$Global_north


# Multiple Linear Regression Models for 2019
## Model 0: combining three predictors of coreness (Coreness), GDP per capital wealth (PGDP_sm), normalized outdegree centrality (nOutdeg), 
## and EU membership (in(EU)):

fit.0 <- lm(Average ~ GDP + EU + Global_north + GDP_EU + GDP_Global_north, data = Data_for_regression)
summary(fit.0)



cor_matrix <- cor(Data_for_regression[, c("GDP", "EU", "Global_north")],  method="spearman")

# Print the correlation matrix
print(cor_matrix)


