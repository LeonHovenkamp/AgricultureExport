library(ggplot2)
library(R.matlab)
library(patchwork)
library(pals)
library(dplyr)
library(cowplot)
library(knitr)
library(gridExtra)
library(maps)
library(mapproj)

library(tidyr)
library(wesanderson)
library(tibble)


color_vec_10 <- c("#EBCC2A", "#F98400","#F21A00", "#9986A5","#046C9A","#5BBCD6","#00A08A","#02401B",  "#79402E","#808080")
common_color_palette <- c("#EBCC2A", "#02401B", "#F98400","#F21A00", "#9986A5","#046C9A","#5BBCD6","#00A08A","#79402E","#808080", 
                          "#FFFF66", "#CC0066", "#f4cccc", "#b45f06", "#000033", "#a64d79", "#ACAC25", "#FF9999")
setwd("X:/My Documents/Research project/Gloria/test")

#-----------------------------------------------------


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

countries_to_remove <- c("Burundi", "Brunei Darussalam", "Buthan", "Eritrea", "DR Yemen (Aden)","Djibouti", 
                         "Gambia", "Liberia", "Rest of Americas", "Rest of Europe", "Rest of Africa", "Rest of Asia-Pacific",
                         "North Korea", "Moldova", "Turkmenistan", "Equatorial Guinea", "Belarus", "South Sudan",
                         "Syria", "Serbia", "Palestine", "Hong Kong", "Bahamas", "United Arab Emirates", "Luxembourg", "Malta", "Kuwait", "Singapore",
                         "Zimbabwe", "Venezuela")
Regions2 <- regions[!(regions$Region_names %in% countries_to_remove), ]



#-------------------------------------------------------------------------------
#----------------------Preparation data figure 1 and 2--------------------------------
#-------------------------------------------------------------------------------

  GHG <- readMat("FP_GHG_agriculture.mat") #this is a specific name from matlab
  GHG <- as.data.frame(GHG$FP.export)
  Land <- readMat("FP_Land_agriculture.mat") #this is a specific name from matlab
  Land <- as.data.frame(Land$FP.export)
  Blue <- readMat("FP_Blue_agriculture.mat") #this is a specific name from matlab
  Blue <- as.data.frame(Blue$FP.export)
  Bio <- readMat("FP_BIO_agriculture.mat") #this is a specific name from matlab
  Bio <- as.data.frame(Bio$FP.export)
  NH3 <- readMat("FP_NH3_agriculture.mat")
  NH3 <- as.data.frame(NH3$FP.export)
  
  Stressors <- list(Bio, Land, Blue, GHG, NH3)
  names(Stressors) <- c("Bio", "Land", "Blue", "GHG", "NH3")
  
  rm(Sectorial)
  
  Sectorial <- data.frame(matrix(nrow = nrow(Stressors[[1]]), ncol = 0))
  for (i in seq_along(Stressors)) {
    current_sum <- rowSums(Stressors[[i]])
    col_name <- names(Stressors)[i]  
    Sectorial[, col_name] <- current_sum
  }


rm(Stressors)
rm(Bio, GHG, Land, Blue, NO2, NH3)
  Sectorial <- cbind(index$iso3c, index$item, Sectorial) #this gives the total pressure per sector per country
  colnames(Sectorial) <- c("iso3c", "item", "Bio", "Land", "Blue", "GHG", "NH3")

#-------------------------------------------------------------------------------
#----------------------------Creating the bar plots of Fig. 2 ------------------

Land_use_aggr <- read.csv("./Results/FP_per_country_total_Landagriculture.csv")
Land_use_aggr <- Land_use_aggr[, -1]
Bio_aggr <- read.csv("./Results/FP_per_country_total_Bioagriculture.csv")
Bio_aggr <- Bio_aggr[, -1]
Blue_aggr <- read.csv("./Results/FP_per_country_total_Blueagriculture.csv")
Blue_aggr <- Blue_aggr[, -1]
GHG_agg <- read.csv("./Results/FP_per_country_total_GHGagriculture.csv")
GHG_agg <- GHG_agg[, -1]
NH3_agg <- read.csv("./Results/FP_per_country_total_NH3agriculture.csv")
NH3_agg <- NH3_agg[, -1]
  
#---------------------------creating a top-5------------------------------------

  my_list <- list(Bio_aggr, Land_use_aggr, Blue_aggr, GHG_agg, NH3_agg)
  names(my_list) <- c("Bio", "Land", "Blue", "GHG", "NH3")
  

result_df <- data.frame(matrix(nrow = nrow(my_list[[1]]), ncol = 0))
  for (i in seq_along(my_list)) {
    current_sum <- rowSums(my_list[[i]][, -1, drop = FALSE])
    col_name <- names(my_list)[i]  # Use the name of the list element as the column name
    result_df[, col_name] <- current_sum / sum(current_sum)
  }
  
result_df <- as.data.frame(cbind(Regions2$Region_acronyms, result_df))

  
List_to_order <- list(result_df$Bio, result_df$Land, result_df$Blue, result_df$GHG, result_df$NH3)
  names(List_to_order) <- c("Bio", "Land", "Blue", "GHG", "NH3")

  Top_5 <- data.frame(matrix(nrow = 135), ncol = 0)
  for (i in seq_along(List_to_order)) {
    if (is.numeric(List_to_order[[i]]) && length(List_to_order[[i]]) > 0) {
      ordering <- result_df[order(-List_to_order[[i]]), 1]
      Top_5 <- cbind(Top_5, ordering)
    }
  }
  rm(List_to_order)
  Top_5 <- Top_5[, -c(1, 2)]
  Top_5 <- as.data.frame(head(Top_5, 5))  #select number of top countries or specific country
  colnames(Top_5) <- c("Bio", "Land", "Blue", "GHG", "NH3")


#creating plots of top countries on shares with an 'others'  sector--------------

  create_aggregated_df <- function(data, value_column, top_items, n_groups) {
    result <- data %>%
      group_by(item) %>%
      summarize(total_number = sum({{ value_column }})) %>%
      arrange(desc(total_number))
    
    top_items <- head(result$item, n_groups)
    
    data$Aggregated_Column <- ifelse(data$item %in% top_items, data$item, "other sectors")
    
    aggregated_df <- data %>%
      group_by(iso3c, Aggregated_Column) %>%
      summarize(value = sum({{ value_column }}))
    
    return(aggregated_df)
  }
  
  global_top_items <- character(0)
  
  create_aggregated_df <- function(data, value_column, n_groups) {
    result <- data %>%
      group_by(item) %>%
      summarize(total_number = sum({{ value_column }})) %>%
      arrange(desc(total_number))
    
    top_items <- head(result$item, n_groups)
    global_top_items <<- union(global_top_items, top_items)
    
    data$Aggregated_Column <- ifelse(data$item %in% global_top_items, data$item, "other sectors")
    
    aggregated_df <- data %>%
      group_by(iso3c, Aggregated_Column) %>%
      summarize(value = sum({{ value_column }}))
    
    return(aggregated_df)
  }

  Bio <- Sectorial %>%
    filter(iso3c %in% Top_5$Bio & Bio != 0) %>%
    create_aggregated_df(value_column = Bio, n_groups = 5)
  colnames(Bio) <- c("iso3c", "sector", "Bio")

  GHG <- Sectorial %>%
    filter(iso3c %in% Top_5$GHG & GHG != 0) %>%
    create_aggregated_df(value_column = GHG, n_groups = 5)
  colnames(GHG) <- c("iso3c", "sector", "GHG")
  
  Land <- Sectorial %>%
    filter(iso3c %in% Top_5$Land & Land != 0) %>%
    create_aggregated_df(value_column = Land, n_groups = 5)
  colnames(Land) <-c("iso3c", "sector", "Land")

  Blue <- Sectorial %>% 
    filter(iso3c %in% Top_5$Blue & Blue != 0) %>%
    create_aggregated_df(value_column = Blue, n_groups = 5)
  colnames(Blue) <-c("iso3c", "sector", "Blue")

  NH3 <- Sectorial %>% 
    filter(iso3c %in% Top_5$NH3 & NH3 != 0) %>%
    create_aggregated_df(value_column = NH3, n_groups = 5)
  colnames(NH3) <-c("iso3c", "sector", "NH3")
  
  rm(Bio_aggr, Blue_aggr, Land_use_aggr, result_df, GHG_agg, HN3_agg)

datasets <- list(Land, Bio, Blue, GHG, NH3)

all_categories <- unique(unlist(lapply(datasets, function(df) unique(df$sector))))

common_color_palette <- c("#EBCC2A", "#F98400","#F21A00", "#9986A5","#046C9A","#5BBCD6","#00A08A","#02401B",  "#79402E","#ABD3A6", 
                          "#FFFF66", "#CC0066", "#f4cccc", "#b45f06","#808080", "#a64d79", "#ACAC25", "#FF9999","#000033")
common_fill_scale <- scale_fill_manual(values = setNames(common_color_palette, all_categories))

datasets <- list(Land, Bio, Blue, GHG, NH3)
titles <- c("Land", "Bio", "Blue", "GHG", "NH3")
rm(combined_absoluut_plots, combined_absoluut_plots2, combined_legend, 
   absoluut_plots, share_plots, plot_absoluut, plot_share, legends, final_plot, common_legend)

legends <- list()
share_plots <- list()

for (i in seq_along(datasets)) {

    plot_share <- ggplot(data = datasets[[i]], aes(x = iso3c, y = .data[[titles[i]]], fill = sector)) +
    geom_bar(position = "fill", stat = "identity") +
    labs(title = NULL,
         y = "Percentage") + 
    scale_y_continuous(
      name = FALSE,
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1"),
      position = "right"
    ) +
    scale_fill_manual(values = common_color_palette) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    common_fill_scale + 
    guides(fill = FALSE) 

  
  legend <- plot_share + theme(legend.position = "bottom") + guides(fill = guide_legend(ncol = 2))
  legends[[i]] <- get_legend(legend)
  
  share_plots[[i]] <- plot_share
}

combined_share_plots <- wrap_plots(share_plots, ncol = 3)
dummy_data <- data.frame(x = rep(1, length(all_categories)), y = rep(1, length(all_categories)), category = all_categories)
dummy_plot <- ggplot(dummy_data, aes(x, y, fill = category)) +
  geom_bar(stat = "identity") +
  common_fill_scale +
  theme_void()

# Create the legend
legend <- get_legend(dummy_plot +
                       guides(fill = guide_legend(ncol = 2, title = NULL))  # Set the number of columns in the legend
)

Share <- combined_share_plots + legend

share_plots[[5]] + legend

#-------------------------------------------------------------------------------
#-----------------------Adding the map to create Figure 2-----------------------
#-------------------------------------------------------------------------------

col_6 <- c(as.character(wes_palette("Zissou1", type = "continuous")),"#550307")

stressor_files <- c("FP_per_country_Landagriculture.csv", "FP_per_country_Bioagriculture.csv", "FP_per_country_Blueagriculture.csv", 
                    "FP_per_country_GHGagriculture.csv", "FP_per_country_NH3agriculture.csv")
stressor_names <- c( "Land use", "Biodiversity loss", "Blue water use", "GHG", "NH3")
stressor_factors <- c(1000, 0.00001, 1000, 1000,1)  #this is to change the unit of the stressor
#land use: 1000 ha -> Mha (f = 1000)
#Biodiversity: potentially disapeared fraction -> potentially disapeared fraction * 100.000 (f = 0.00001)
#Blue water stress: Million M3 -> Billion M3 (f = 1000)
#GHG emission: Kilotonnes CO2 -> Million Tonnes CO2 (f = 1000)
#N20 emission Kilotonnes N2O -> Kilotonnes N2O (f = 1)
#NH3 emission Kilotonnes NH3 -> Kilotonnes NH3 (f=1)

rm(world_EU_map_str_imp, world_map_totalEXP) 


# List of scale_fill_gradientn specifications for each stressor
scale_specs <- list(
  stressor1 = list(                         ######land
    limits = c(0,0.8),
    colours = col_6, 
    breaks = c(0,0.15,0.30,0.45,0.60,0.75),
    labels = c("0","0.15","0.30","0.45","0.60",">0.75"),
    values = c(0, 0.005/0.26,0.01/0.26,0.05/0.26,0.1/0.26,1)),
  stressor2 = list(                         ######Biodiversity
    limits = c(0,0.86),
    colours = col_6, 
    breaks = c(0,0.15,0.30,0.45,0.6,0.75),
    labels = c("0","0.15","0.30","0.45","0.60",">0.75"),
    values = c(0, 0.005/0.26,0.01/0.26,0.05/0.26,0.1/0.26,1)),
  stressor3 = list(                         #####Blue water consumption
    limits = c(0,0.6),
    colours = col_6, 
    breaks = c(0,0.1,0.2,0.3,0.4,0.5),
    labels = c("0","0.1","0.2","0.3","0.4",">0.5"),
    values = c(0, 0.005/0.26,0.01/0.26,0.05/0.26,0.1/0.26,1)),
  stressor4 = list(                         #####GHG
    limits = c(0,8),
    colours = col_6, 
    breaks = c(0,1.5,3,4.5,6,7.5),
    labels = c("0","1.5","3.0","4.5","6.0",">7.5"),
    values = c(0, 0.005/0.26,0.01/0.26,0.05/0.26,0.1/0.26,1)),
  stressor5 = list(                         #####NH3
    limits = c(0,93),
    colours = col_6, 
    breaks = c(0,10,20,30,40,90),
    labels = c("0","10","20","30","40",">90"),
    values = c(0, 0.005/0.26,0.01/0.26,0.05/0.26,0.1/0.26,1)))

#stressor3 = list(                         #####Blue water stress
#  limits = c(0,13.2),
#  colours = col_6, 
#  breaks = c(0,2,4,6,8,10,12),
#  labels = c("0","2","4","6","8","10", ">12"),
#  values = c(0, 0.005/0.26,0.01/0.26,0.05/0.26,0.1/0.26,1)


rm(plot_list_maps)
plot_list_maps <- list()

for (i in 1:length(stressor_files)) {
  stressor_per_country_agg <- as.data.frame(read.csv(paste0("./Results/", stressor_files[i])))
  stressor_per_country_agg[, "V1"] <- as.numeric(stressor_per_country_agg[, "V1"])
  
  Share_country_totalEXP = stressor_per_country_agg$V1 / stressor_factors[i]
  Share_country_totalEXP <- rownames_to_column(as_tibble(Share_country_totalEXP), var = "RowNames")
  Share_country_totalEXP <- cbind(Regions2$Region_names, Share_country_totalEXP$value)
  colnames(Share_country_totalEXP) <- c("region", "stressor")
  Share_country_totalEXP <- as.data.frame(Share_country_totalEXP)
  Share_country_totalEXP$stressor <- as.numeric(Share_country_totalEXP$stressor)
  
  world_map <- map_data("world")
  
  world_map[world_map$region=="USA","region"] <- "United States of America"
  world_map[which(world_map$region=="China" & world_map$subregion=="Hong Kong"),"region"] <- "Hong Kong"
  world_map[world_map$region=="Russia","region"] <- "Russian Federation"
  world_map[world_map$region=="Vietnam","region"] <- "Viet Nam"
  world_map[world_map$region=="Brunei","region"] <- "Brunei Darussalam"
  world_map[world_map$region=="UK","region"] <- "United Kingdom"
  world_map[world_map$region=="North Macedonia","region"] <- "Macedonia"
  
  world_map[world_map$region=="Vatican","region"] <- "Holy See"
  world_map[world_map$region=="South Georgia","region"] <- "South Georgia and the South Sandwich Islands"
  world_map[world_map$region=="South Sandwich Islands","region"] <- "South Georgia and the South Sandwich Islands"
  world_map[world_map$region=="Azores","region"] <- "Portugal"
  world_map[world_map$region=="Madeira Islands","region"] <- "Portugal"
  world_map[world_map$region=="Chagos Archipelago","region"] <- "British Indian Ocean Territory"
  world_map[world_map$region=="Heard Island","region"] <- "Heard and McDonald Islands"
  world_map[world_map$region=="Canary Islands","region"] <- "Spain"
  world_map[world_map$region=="Curacao","region"] <- "Netherlands Antilles (former)" 
  world_map[world_map$region=="Saba","region"] <- "Netherlands Antilles (former)" 
  world_map[world_map$region=="Sint Eustatius","region"] <- "Netherlands Antilles (former)" 
  world_map[world_map$region=="Sint Maarten","region"] <- "Netherlands Antilles (former)" 
  world_map[world_map$region=="Bonaire","region"] <- "Netherlands Antilles (former)" 
  world_map[world_map$region=="French Southern and Antarctic Lands","region"] <- "French Southern and Antarctic Territories" 
  world_map[world_map$region=="Antigua","region"] <- "Antigua and Barbuda"
  world_map[world_map$region=="Barbuda","region"] <- "Antigua and Barbuda"
  world_map[world_map$region=="Occupied Palestinian Territory","region"] <- "Palestine"
  world_map[world_map$region=="Ivory Coast","region"] <- "Cote d'Ivoire"
  
  world_map_totalEXP <- left_join(world_map, Share_country_totalEXP, by = "region")
  
  map_Total <- ggplot(world_map_totalEXP, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = stressor), color = "black") +
    scale_fill_gradientn(
      name = "stressor",
      limits = scale_specs[[paste0("stressor", i)]]$limits,
      colours = scale_specs[[paste0("stressor", i)]]$colours,
      breaks = scale_specs[[paste0("stressor", i)]]$breaks,
      labels = scale_specs[[paste0("stressor", i)]]$labels,
      values = scale_specs[[paste0("stressor", i)]]$values,
      space = "Lab",
      guide = guide_colorbar(
        ticks = TRUE,
        ticks.colour = "white",
        ticks.linewidth = 2,
        barheight = 0.5 )) +  # Adjust the height as needed
    geom_rect(aes(xmin = -11, xmax = 35.7, ymin = 34.1, ymax = 72), fill = NA, colour = "black", size = 0.3) +
    scale_y_continuous(limits = c(-58, 100)) +
    coord_equal(1.3) +
    labs(fill = "stressor") +
    theme_void() +
    theme(
      legend.position = c(0.6, -0.1),
      legend.direction = "horizontal",
      text = element_text(size = 10),
      legend.key.width = unit(1.5, 'cm')
    )
  
  EU_str_imp <- ggplot(data = world_map_totalEXP, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = stressor), color = "black") +
    scale_fill_gradientn(
      name = "stressor",
      limits = scale_specs[[paste0("stressor", i)]]$limits,
      colours = scale_specs[[paste0("stressor", i)]]$colours,
      breaks = scale_specs[[paste0("stressor", i)]]$breaks,
      labels = scale_specs[[paste0("stressor", i)]]$labels,
      values = scale_specs[[paste0("stressor", i)]]$values,
      space = "Lab",
      guide = guide_colorbar(
        ticks = TRUE,
        ticks.colour = "white",
        ticks.linewidth = 2
      )
    ) +
    scale_x_continuous(limits = c(-11, 35)) +
    scale_y_continuous(limits = c(36, 70)) +
    theme_void() +
    theme(
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8)
    )
  
  world_EU_map_str_imp <- cowplot::ggdraw() +
    coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
    annotation_custom(ggplotGrob(map_Total), xmin = -2.5, xmax = 26, ymin = -12, ymax = 34) +
    annotation_custom(ggplotGrob(EU_str_imp), xmin = -3, xmax = 4, ymin = 2, ymax = 9) +
    draw_text(paste(stressor_names[i]), x = 10, y = 19, size = 13)
  
  plot_list_maps[[paste0("plot", i)]] <- world_EU_map_str_imp #add the plots to a list
}

#adding the share of each sector to the maps

legend.map.abs <- get_legend(plot_list_maps[[1]]) 
legend.bar <- get_legend(share_plots[[1]])    #for this use the other R file
plots <- grid.arrange(arrangeGrob(plot_list_maps[[1]] + theme(legend.position="none"), 
                                  share_plots[[1]] + theme(legend.position="none"),
                                  ncol=2, nrow=1, widths=c(0.40,0.145)),
                      arrangeGrob(plot_list_maps[[2]] + theme(legend.position="none"), 
                                  share_plots[[2]] + theme(legend.position="none"),
                                  ncol=2, nrow=1, widths=c(0.40,0.145)),
                      arrangeGrob(plot_list_maps[[3]] + theme(legend.position="none"), 
                                  share_plots[[3]] + theme(legend.position="none"),
                                  ncol=2, nrow=1, widths=c(0.40,0.145)),
                      arrangeGrob(plot_list_maps[[4]] + theme(legend.position="none"), 
                                  share_plots[[4]] + theme(legend.position="none"),
                                  ncol=2, nrow=1, widths=c(0.40,0.145)),
                      arrangeGrob(plot_list_maps[[5]] + theme(legend.position="none"), 
                                  share_plots[[5]] + theme(legend.position="none"),
                                  ncol=2, nrow=1, widths=c(0.40,0.145)), legend,
                      ncol=2, nrow=3, heights = c(4, 4, 4))  # Adjusted the heights here),

combined_plots <- grid.arrange(
  plots,
  legend,
  ncol = 1, nrow = 2, heights = c(8, 1), widths = 1)

ggsave("FigureS4_totalexport.png", plot = plots, width = 0.95 * 17, height = 13, units = "in") #18.9



#-------------------------------------------------------------------------------
#--------------------------------Figure 1---------------------------------------
#-------------------------------------------------------------------------------


#-------------------------Inside outside netherlands-------------------
#----------------------------------------------------------------------
my_list <- list(Bio_aggr, Land_use_aggr, Blue_aggr, GHG_agg, NH3_agg)
names(my_list) <- c("Bio", "Land", "Blue", "GHG", "NH3")

result_df2 <- data.frame(matrix(nrow = nrow(my_list[[1]]), ncol = 0))
for (i in seq_along(my_list)) {
  current_sum <- rowSums(my_list[[i]][, -1, drop = FALSE])
  col_name <- names(my_list)[i]  # Use the name of the list element as the column name
  result_df2[, col_name] <- current_sum / 1
}

result_df2 <- as.data.frame(cbind(Regions2$Region_acronyms, result_df2))

NL_inside <- result_df2[result_df2$`Regions2$Region_acronyms` == "NLD", ]
NL_inside <- NL_inside[, -1]
NL_outside <- result_df2[result_df2$`Regions2$Region_acronyms` != "NLD", ]
NL_outside <- NL_outside[, -1]

NL_outside <- colSums(NL_outside)
NL_inside_outside <- rbind(NL_inside, NL_outside)
NL_inside_outside$Location <- c("NLD", "R.O.W")



#and how is the value added devided?
value_added <- read.csv("./Results/VA_per_country_agriculture.csv")
value_added <- rowSums(value_added)
value_added <- as.data.frame(cbind(Regions2$Region_names, value_added))

value_added_NL <- value_added[value_added$V1 == "Netherlands", ]
value_added_NL <- as.data.frame(value_added_NL[, -1])
value_added_outside <- value_added[value_added$V1 != "Netherlands", ]
value_added_outside <- as.data.frame(value_added_outside[, -1])
value_added_outside <- as.data.frame(sapply(value_added_outside, as.numeric))

value_added_outside <- colSums(value_added_outside)
VA_inside_outside <- rbind(value_added_NL, value_added_outside)
VA_inside_outside$Location <- c("NLD", "R.O.W")
colnames(VA_inside_outside) <- c("VA", "Location")
VA_inside_outside$VA <- as.numeric(as.character(VA_inside_outside$VA))
NL_inside_outside <- cbind(NL_inside_outside, VA = VA_inside_outside$VA)


impact_categories <- c("Land", "Bio", "Blue", "GHG", "NH3", "VA")

Inside_outside_list <- list()

for (category in impact_categories) {
  if (category %in% colnames(NL_inside_outside)) {
    current_data <- NL_inside_outside[, c(category, "Location")]
    
    current_data <- tidyr::gather(current_data, key = "Impact", value = "Value", -Location)
    
    Inside_outside_list[[paste0(category, "_plot")]] <- ggplot(data = current_data, aes(x = Impact, y = Value, fill = Location)) +
      geom_bar(position = "fill", stat = "identity") +
      labs(title = NULL, y = NULL, x = NULL) +  # Remove x-axis label
      scale_y_continuous(labels = NULL) +  # Remove y-axis labels
      scale_fill_manual(values = common_color_palette) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 13))
    
  } else {
    cat("Column", category, "not found in the data frame.\n")
  }
}


combined_plot <- grid.arrange(grobs = Inside_outside_list, ncol = length(Inside_outside_list)) 

combined_plot <- combined_plot + theme(legend.position = "bottom")
combined_plot

ggsave("Figure1.png", plot = combined_plot)



#--------Tables_in_the_appendix-------------------------------------------------
#-------------------------------------------------------------------------------
NL_sectorial <- Sectorial[Sectorial[,1] == "NLD", ]
NL_sectorial <- NL_sectorial[, -1]
library(dplyr)
library(tidyr)
library(ggplot2)
numeric_vars <- NL_sectorial[, -c(1)] 
normalized_data <- as.data.frame(scale(numeric_vars))
df_normalized <- cbind(NL_sectorial["item"], normalized_data)
rm(aggregated_df)

NL_sectorial_perc <- NL_sectorial %>% #creating a table for NL
  mutate(across(where(is.numeric), ~ round(. / sum(.)*100, 2), .names = "Percent_{.col}"))
NL_sectorial_perc <- NL_sectorial_perc[, -c(2:7)]
kable(NL_sectorial_perc, format = "latex", caption = "Sectorial analysis NL")
NL_sectorial_perc <- head(NL_sectorial_perc, 20)
NL_long <- tidyr::gather(NL_sectorial, key = "Impact", value = "Value", -item)

create_aggregated_df <- function(data, value_column, n_groups) {
  result <- data %>%
    group_by(Impact, item) %>%
    summarize(total_value = sum(!!sym(value_column))) %>%
    arrange(Impact, desc(total_value))
  
  top_items <- result %>%
    group_by(Impact) %>%
    top_n(n_groups, total_value) %>%
    pull(item)
  
  data$Aggregated_Item <- ifelse(data$item %in% top_items, as.character(data$item), "Others")
  
  aggregated_df <- data %>%
    group_by(Impact, Aggregated_Item) %>%
    summarize(Percentage = sum(!!sym(value_column)) / sum(data[[value_column]]) * 100)
  
aggregated_df <- bind_rows(
    aggregated_df,
    data %>%
      filter(!item %in% top_items) %>%
      group_by(Impact) %>%
      summarize(Percentage = sum(!!sym(value_column)) / sum(data[[value_column]]) * 100) %>%
      mutate(Aggregated_Item = "Others")
  )
  
  return(aggregated_df)
}


aggregated_df <- create_aggregated_df(NL_long, value_column = "Value", n_groups = 5)
aggregated_df <- aggregated_df[1:(nrow(aggregated_df) - 5), ]


#creating table
NL_sectorial_perc <- tidyr::spread(aggregated_df, key = "Impact", value = "Percentage")
numeric_columns <- names(NL_sectorial_perc)[-1]
normalize_columns <- function(x) {
  x / sum(x, na.rm = TRUE) *100}
NL_sectorial_perc <- NL_sectorial_perc %>%
  mutate(across(all_of(numeric_columns), normalize_columns))


library(knitr)
NL_sectorial_perc <- NL_sectorial_perc %>%
  mutate(across(where(is.numeric), ~round(., digits = 0)))

kable(NL_sectorial_perc, format = "latex", caption = "Sectorial analysis NL")


#total impact
Total_impact <- Sectorial[, -c(1:2)]
Total_impact <- colSums(Total_impact)
Total_impact <- t(data.frame(Value = Total_impact))



kable(Total_impact, format = "latex", caption = "Total impact")
