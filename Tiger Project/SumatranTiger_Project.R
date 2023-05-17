#----------------------------Sumatra Tiger Project------------------------------

#-------------------------Install packages & libraries--------------------------

install.packages("tidyverse")
install.packages("stringr")
install.packages("dplyr")
install.packages("maps")
install.packages("mapdata")
install.packages("ggplot2")
install.packages("tmap")
install.packages("tmaptools")
install.packages("leaflet")
install.packages("devtools")
install.packages("geofacet")
install.packages("here")
install.packages("sf")
install.packages("terra")
install.packages("shiny")
install.packages("RColorBrewer")
install.packages("sp")
install.packages("hrbrthemes")
install.packages('tinytex')
install.packages("data.table")
install.packages("htmlwidgets")
install.packages("pandoc")
install.packages("widgetframe")
install.packages("leaflet.extras")

#Import the libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(maps)
library(mapdata)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(devtools)
library(geofacet)
library(here)
library(sf)
library(terra)
library(shiny)
library(RColorBrewer)
library(sp)
library(hrbrthemes)
library(tinytex)
library(data.table)
library(sp)
library(htmlwidgets)
library(pandoc)
library(widgetframe)
library(leaflet.extras)


#---------------------------Import the shapefile--------------------------------

#Import the shapefile and rename it
IDN_shapefile <- st_read(here("..", "Data", "gadm41_IDN_shp"))
#Load the layers of the shapefile
st_layers(here("..", "Data", "gadm41_IDN_shp"))

#----------------------------Import the data------------------------------------

#Import TigerCapturesSumatra dataframe
tiger_capture_data <- read.csv(here("..", "Data", "TigerCapturesSumatra.csv"))

#-------------------------Tidy the shapefile data-------------------------------

#Open and name each  layer of the shapefile
IDN_data_layer1 <- read_sf(
  dsn = "C:\\Users\\jadeh\\OneDrive\\Documents\\Masters Degree\\Data Visualisation Project\\Data\\gadm41_IDN_shp", layer = "gadm41_IDN_1")

IDN_data_layer2 <- read_sf(
  dsn = "C:\\Users\\jadeh\\OneDrive\\Documents\\Masters Degree\\Data Visualisation Project\\Data\\gadm41_IDN_shp", layer = "gadm41_IDN_2")

IDN_data_layer3 <- read_sf(
  dsn = "C:\\Users\\jadeh\\OneDrive\\Documents\\Masters Degree\\Data Visualisation Project\\Data\\gadm41_IDN_shp", layer = "gadm41_IDN_3")

IDN_data_layer4 <- read_sf(
  dsn = "C:\\Users\\jadeh\\OneDrive\\Documents\\Masters Degree\\Data Visualisation Project\\Data\\gadm41_IDN_shp", layer = "gadm41_IDN_4")

#View the contents of layer 2; contains 14 variables
IDN_data_layer2

#Create a new variable for the provinces 
IDN_provinces <- st_read(
  "C:\\Users\\jadeh\\OneDrive\\Documents\\Masters Degree\\Data Visualisation Project\\Data\\gadm41_IDN_shp", layer = "gadm41_IDN_2")


#Create a new dataframe called National_parks
#use MUTATE to combine province names into areas that make up the national parks
#either Gunung Leuser, Bukit Barisan Selatan, or Kerinci Seblat

National_parks <- IDN_provinces %>%
  mutate(National_Parks = case_when(
    NAME_2 %in% c("Nagan Raya", "Aceh Barat Daya", "Aceh Selatan",
                  
                  "Aceh Tenggara", "Subulussalam", "Aceh Singkil",
                  
                  "Gayo Lues", "Aceh Tamiang", "Aceh Timur",
                  
                  "Bener Meriah") ~ "Gunung Leuser",
    
    NAME_2 %in% c("Lampung Barat", " Tanggamus", "Bengkulu Selatan",
                  
                  "Kaur") ~ "Bukit Barisan Selatan",
    
    NAME_2 %in% c("Kerinci", "Solok","Sawahlunto", "Lebong",
                  
                  "Rejang Lebong", "Solok Selatan", "Bungo",
                  
                  "Merangin") ~ "Kerinci Seblat"))


#remove unnecessary rows from the data
#Create a new dataframe which filters out NA values from  National_Parks column
filtered_parks <- dplyr::filter(National_parks, National_Parks %in% c(
  "Gunung Leuser", "Bukit Barisan Selatan", "Kerinci Seblat"))


#-------------------------------Tidy the data-----------------------------------

#Remove irrelevant columns

#Remove column "SurveyID", "Side" and "StationID"
tiger_capture_tidy <- subset(tiger_capture_data,
                             select = -c(SurveyID, StationID, Side))


#Detangle date and time from the Date.Time column
#create a new column extracting only the date  from the Date.Time column
#Keep only the month and year values
#use MUTATE to rename the column name
tiger_capture_tidy_date <- tiger_capture_tidy %>%  
  mutate(date= str_sub(tiger_capture_tidy$Date.Time, start=1, end=7))

#Now remove the original Date.Time column leaving just the new "date" column
tiger_capture_tidy_date <- subset(tiger_capture_tidy_date,
                                  select = -c(Date.Time))

#create a column with just the animal location in it
#Extract location only from column AnimalID
#This column contains both Individual Code and the park they were sighted in

#Extract location from AnimalID column
#Keep location information only
#BBS (Bukit Barisan Selatan), LEU (Gunung Leuser), or KER (Kerinci Seblat)
#Rename the column to national_park
tiger_map_data <- tiger_capture_tidy_date %>%
  mutate(national_park= str_sub(
    tiger_capture_tidy_date$AnimalID, start=1, end=3))

#Remove the AnimalID column from the dataset
tiger_map_data <- subset(tiger_map_data, select = -c(AnimalID))

#--------------------Create colour palettes for visualisations------------------

#Create an RColBrewer palette to use for the national parks on the map
#Needs to contain different green hues to differentiate each national park
#Green was selected to reflect the natural colours of national parks

map_palette <- brewer.pal(n=3, "Greens")
#Select a green palette from RColBrewer
#n is the number of colors needed in the palette

map_palette #Shows the names of the three green hues to manually add to the map


#Create an orange palette to use on all graphs and as the basecolor of the map
#Orange was selected to incorporate an overall tiger theme
#Create a RColBrewer palette to use for the graphs with different hues
graph_palette <- brewer.pal(n=3, "Oranges")
#Select orange palette from RColBrewer
#n is the number of colors needed in the map
graph_palette


#--------------------------------Plot the Maps----------------------------------

#Plot a basic map of Indonesia with province borders outlined

map <- tm_shape(IDN_shapefile) + #maps the outline of the shapefile
  tm_polygons() +
  tm_shape(IDN_provinces) + # overlays the provinces dataframe
  tm_polygons("#FDAE6B", alpha = 0.9) +
  #color the map in line with a tiger theme, alpha adjusts transparency
  tm_borders("#A1D99B") + #Add a border to each province
  tm_layout(main.title = "Map of Indonesia", #Add the main title above the map
            fontface = "italic", #Italicize the text
            fontfamily = "serif") #Change font for text
print(map)

#save map
tmap_save(
  map, filename =
    "C:\\Users\\jadeh\\OneDrive\\Documents\\Coding Project\\Tmap Figures\\Indonesia.png")



#Plot a basic map of Indonesia with provinces and national parks
# Highlight the three national parks on the base map
#To do this overlay the filtered_data df
map_2 <- tm_shape(IDN_shapefile) + #Maps the outline of the shapefule
  tm_polygons() +
  tm_shape(IDN_provinces) + #Overlays each province of Indonesia
  tm_polygons("#FDAE6B", alpha = 0.9) + #set basecolor of the map
  #alpha changes the transparency
  tm_borders("#A1D99B") + #Add border to each province
  tm_shape(filtered_parks)+ #Highlight the national park df
  tm_fill(col = "National_Parks", #Select which column of the df to add
          palette = c("#E5F5E0", "#A1D99B", "#31A354"),
          #set colors of each national park as a hue from the RcolBrewer palette
          position= c("RIGHT", "BOTTOM")) + #Change the position
  tm_xlab("Longitude")+ #Add x axis label
  tm_ylab("Lattitude")+ #Add y axis label
  tm_compass(type = "4star", size = 0.3, position = c("RIGHT", "TOP")) +
  #Add a compass showing north, adjust its size and position on the map
  tm_scale_bar(width = 0.25, text.size = 0.3, position= c("LEFT", "BOTTOM")) +
  #Add a scale bar, change its width, size and position on the map
  tm_legend(position = c("RIGHT", "TOP"), #change legend position
            legend.outside = TRUE, #Move legend off the map
            legend.text.size = 1) + #Change size of legend text
  tm_credits("Data Source: GADM", #Add credits to source the map
             size= 0.4, align= "right", #Change size and alignment of credits
             position= c("RIGHT", "BOTTOM")) + #Change position of credits
  tm_layout(main.title = "Map of Indonesia Overlaid With National Parks",
            #Add a title to the map
            fontface = "italic", #Change the style of text
            fontfamily = "serif", #Change the font
            legend.width = 1, #Change legend width
            legend.height = 0.9, #Change legend height
            legend.title.size = 1, #Change legend title size
            legend.text.size = 0.6) #Change the text size of the legend



#Plot a basic map of Indonesia with national parks overlaid
#Add a dashed outline around Sumatra
#This will highlight where the national parks are localised

#To draw a boundary box around Sumatra we need to know the extent of Indonesia
#Find the extent of Indonesia
st_bbox(IDN_shapefile)
#reveals the x-lim, y-lim, x-max, and y-max of the current shapefile

# Use st_bbox to change the extent of the map and zoom into Sumatra
bbox_new <- st_bbox(IDN_shapefile)
bbox_new
bbox_new[3] <- 114
#change the x-max value to zoom into Sumatra, [3] represents the x-max value
bbox_new <- bbox_new %>%
  st_as_sfc()
#Set it as spatial data

map_3 <- tm_shape(IDN_shapefile) + #Maps the outline of the shapefile
  tm_polygons() +
  tm_shape(IDN_provinces) + #Overlays the provinces of Indonesia onto the map
  tm_polygons("#FDAE6B", alpha = 0.9) + #set the color and transparency of map
  tm_borders("#A1D99B") + #Add borders to the provinces
  tm_shape(filtered_parks)+ #Add the national park dataframe
  tm_fill(col = "National_Parks", #Select which column of the df to add
          palette = c("#E5F5E0", "#A1D99B", "#31A354"),
          #set colors of each national park as a hue from the RcolBrewer palette
          position= c("RIGHT", "BOTTOM")) + #change the position
  tm_xlab("Longitude")+ #label the x axis
  tm_ylab("Lattitude")+ #label the y axis
  tm_compass(type = "4star", size = 0.3, position = c("RIGHT", "TOP")) +
  #Add a compass showing north, adjust its size and position on the map
  tm_scale_bar(width = 0.25, text.size = 0.3, position= c("LEFT", "BOTTOM")) +
  #Add a scale bar, change its width, size and position on the map
  tm_legend(position = c("RIGHT", "TOP"), #change legend position
            legend.outside = TRUE, #Move legend of the map
            legend.text.size = 1) + #Change legend text size
  tm_credits("Data Source: GADM", #Add credits to source the map
             size= 0.4, align= "right", #Change size and alignment of credits
             position= c("RIGHT", "BOTTOM")) + #change position of credits
  tm_layout(main.title = "Map of Indonesia Overlaid With National Parks",
            #Add title to the map
            fontface = "italic", #Change the style of the text
            fontfamily = "serif", #Change the font
            legend.width = 1, #change the legend width
            legend.height = 0.9, #change the legend height
            legend.title.size = 1, #change the size of the legend title
            legend.text.size = 0.6) + #change the size of the legend text
  tm_shape(bbox_new)+ #Add the new x-max value to the map
  tm_borders(col= "red", lwd= 2, lty= "dashed")
#col determines color of the border, lwd is line width, and lty is line style


nationalpark <- tmap_arrange(
  map_2, map_3, #Group map_2 and map_3 into one view
  ncol = NA, #change number of columns
  nrow = NA, # change number of rows
  widths = NA, #change width of visualisation
  heights = NA, #change height of visualisation
  sync = FALSE,
  asp = 0, #Aspect ratio of visualisation
  outer.margins = 0.02 #Change the outer margins
)

print(nationalpark)

#save map
tmap_save(nationalpark,
          filename = 
            "C:\\Users\\jadeh\\OneDrive\\Documents\\Coding Project\\Tmap Figures\\National_Parks.png")



#Currently the tiger data is non-spatial
#Need to extract the mean coordinates for each national park
#This will allow us to overlay the tiger data onto these areas


#Remove unnecessary columns with unwanted data to tidy up the data set (should help to run quicker)
filtered_parks <- filtered_parks %>%
  select(GID_2, GID_1, NAME_1, NAME_2, TYPE_2, CC_2, HASC_2, geometry, National_Parks)

#Assign names to each national park
Parks <- c("Gunung Leuser", "Bukit Barisan Selatan", "Kerinci Seblat")


#Use filter to create a dataframe with just Gunung Leuser park

#filter only Gunung Leuser park multipolygons

parks_GL <- filtered_parks %>%
  
  filter(National_Parks == Parks[1]) #keep only Gunung Leuser Park rows


#use rep to create a list of values of 0

#This allows us to fill in the coordinate data from the loops

#use one variable for x and one for y

GL_mean_x <- rep(0, nrow(parks_GL))

GL_mean_y <- rep(0, nrow(parks_GL))


#loop through Gunung Leuser park rows to find the mean coordinates

for(i in 1:nrow(parks_GL)){ #for every row in parks_GL, do the following
  
  geom_GL <- st_geometry(parks_GL) #extract the geometry of the national park
  
  multiGL <- geom_GL[[i]] #Index the multipolgyon
  
  coordsGL <- multiGL[[1]]#get x,y coordinates
  
  df_coordsGL <- data.frame(coordsGL[1]) #turn it into a dataframe
  
  meansGL <- colMeans(df_coordsGL) #overall mean x and y coordinates
  
  GL_mean_x[i] <- meansGL[1] #[1]extracts the average x value
  
  GL_mean_y[i] <- meansGL[2] #[2]extracts the average y value
  
}


GL_x_coord <- mean(GL_mean_x) #create a variable of the mean x coordinate

GL_y_coord <- mean(GL_mean_y) #create a variable of the mean y coordinate



#Use filter to create a dataframe with just Bukit Barisan Selatan park

#filter only Bukit Barisan Selatan park multipolygons

parks_BBB <- filtered_parks %>%
  
  filter(National_Parks == Parks[2])

#keep only Bukit Barisan Selatan rows


#use rep to create a list of values of 0

#This allows us to fill in the coordinate data from the loops

#use one variable for x and one for y

BBB_mean_x <- rep(0, nrow(parks_BBB))

BBB_mean_y <- rep(0, nrow(parks_BBB))


for(j in 1:nrow(parks_BBB)){ #for every row in parks_BBB, do the following
  
  geom_BBB <- st_geometry(parks_BBB) #extract the geometry of the national park
  
  multiBBB <- geom_BBB[[j]] #Index the multipolgyon
  
  coordsBBB <- multiBBB[[1]]#get x,y coordinates
  
  df_coordsBBB <- data.frame(coordsBBB[1]) #turn it into a dataframe
  
  meansBBB <- colMeans(df_coordsBBB) #overall mean x and y coordinates
  
  BBB_mean_x[j] <- meansBBB[1] # [1]extracts the mean x coordinate
  
  BBB_mean_y[j] <- meansBBB[2]} #[2]extracts the mean y coordinate


BBB_x_coord <- mean(BBB_mean_x) #create a variable of the mean x coordinate

BBB_y_coord <- mean(BBB_mean_y) #create a variable of the mean y coordinate



#Use filter to create a dataframe with just Bukit Barisan Selatan park

#filter only Bukit Barisan Selatan park multipolygons

parks_KS <- filtered_parks %>%
  
  filter(National_Parks == Parks[3])

#keep only Bukit Barisan Selatan rows


#use rep to create a list of values of 0

#This allows us to fill in the coordinate data from the loops

#use one variable for x and one for y

KS_mean_x <- rep(0, nrow(parks_KS))

KS_mean_y <- rep(0, nrow(parks_KS))


for(k in 1:nrow(parks_KS)){ #for every row in parks_KS, do the following
  
  geom_KS <- st_geometry(parks_KS) #extract the geometry of the national park
  
  multi <- geom_KS[[k]] #Index the multipolgyon
  
  coords <- multi[[1]]#get x,y coordinates
  
  df_coords <- data.frame(coords[1]) #turn it into a dataframe
  
  means <- colMeans(df_coords) #get the average
  
  KS_mean_x[k] <- means[1] #[1]extracts the x value
  
  KS_mean_y[k] <- means[2]} #[2]extracts the y value


KS_x_coord <- mean(KS_mean_x) #create a variable of the mean x coordinate

KS_y_coord <- mean(KS_mean_y) #create a variable of the mean y coordinate



#create a variable for the three x and 3 coordinates for the map

longitude <- c(GL_x_coord, BBB_x_coord, KS_x_coord)

latitude <- c(GL_y_coord, BBB_y_coord, KS_y_coord)



geocode <- data.frame(longitude,latitude)

geocode2 <- st_as_sf(geocode, coords= c("longitude", "latitude"), crs= 4326)



#Now incorporate all the for loops into one loop to output the average points

# Making a list of zeros for the x coordinates to be added to
longitude = rep(0,length(Parks))
latitude = rep(0,length(Parks))

for(i in 1:length(Parks)){ # Looping through the different parks
  Specific_Park <- filtered_parks %>%
    filter(National_Parks == Parks[i]) # Selecting the specific park
  
  geom <- st_geometry(Specific_Park)
  
  mean_x <- rep(0, nrow(Specific_Park)) #This allows us to fill in the coordinate data from the loops
  mean_y <- rep(0, nrow(Specific_Park))
  
  for(j in 1:nrow(Specific_Park)){
    multi <- geom[[j]] #Index the multipolgyon
    
    coords <- multi[[1]] #get x,y coordinates
    
    df_coords <- data.frame(coords[1]) #turn it into a dataframe
    
    means <- colMeans(df_coords) #overall mean x and y coordinates
    
    mean_x[j] <- means[1] #[1]extracts the average x value  
    
    mean_y[j] <- means[2] #[2]extracts the average y value
  }
  longitude[i] <- mean(mean_x) #create a variable of the mean x coordinate
  
  latitude[i] <- mean(mean_y) #create a variable of the mean y coordinate
}  

#create new data frame of average longitude and latitude coordinates 
geocode <- data.frame(longitude, latitude)

geocode2 <- st_as_sf(geocode, coords= c("longitude", "latitude"), crs= 4326)
#make it compatible with spatial data using st_as_sf 



map_4 <- tm_shape(IDN_shapefile, bbox= bbox_new) + #add new boundary to the whole shapefile
  
  tm_polygons() + #add polygons
  
  tm_shape(IDN_provinces, bbox= bbox_new) + #add new boundary to the provinces df
  
  tm_polygons("#FDAE6B", alpha = 0.9) + #set colour of regions, change transparency with alpha
  
  tm_borders("#A1D99B", lwd = 2) + #colour of borders and width of border lines
  
  tm_shape(filtered_parks, bbox= bbox_new)+ #add national park regions and add new boundary to the national parks df
  
  tm_fill(col = "National_Parks", #overlay national parks
          
          palette = c("#E5F5E0", "#A1D99B", "#31A354"), #Use RColBrewer palette to colour the national parks with a green colourscheme
          
          position= c("RIGHT", "BOTTOM")) + #change the position
  
  tm_xlab("Longitude")+ #Label x-axis
  
  tm_ylab("Lattitude")+ #Label y-axis
  
  tm_compass(type = "4star", size = 0.3, #Add a compass to indicate direction of North
             
             position = c("RIGHT", "TOP")) + #Position where the compass goes on the map
  
  tm_scale_bar(width = 0.25, text.size = 0.3, #Add a scale bar and adjust the size of text and the width of the bar
               
               position= c("LEFT", "BOTTOM")) + #Choose where to place the scale bar
  
  tm_legend(position = c("RIGHT", "TOP"), legend.outside = TRUE, #Remove the legend off the map and position it to the right
            
            legend.text.size = 1) +
  
  tm_credits("Data Source: GADM", size= 0.4, align= "right", #Provide a data source for the map
             
             position= c("RIGHT", "BOTTOM")) +
  
  tm_layout(main.title = "Map of Indonesia Overlaid With National Parks", #Title the map
            
            fontface = "italic", #Set style of text to be italicized
            
            fontfamily = "Times New Roman", #Set font to Serif for aesthetics
            
            legend.width = 1, #change width of legend so national park names all fit and are consistent in size
            
            legend.height = 0.9, #Change the height of the legend bar
            
            legend.title.size = 1, #Change size of legend title
            
            legend.text.size = 0.6) + #change size of legend text size
  
  tm_shape(geocode2)+ #add the mean coordinates to the map
  
  tm_dots(col="red", size=0.3) #change the color and size of coordinate points


print(map_4) #print the map

#save map
tmap_save(map_4,
          filename = 
            "C:\\Users\\jadeh\\OneDrive\\Documents\\Coding Project\\Tmap Figures\\Close_Up_Sumatra.png")



#---------------------------Plot some graphs------------------------------------

#Create a stacked bar chart
#First create a new column with total number of tigers spotted based on gender
#We need this numeric data to be able to plot the graph and pie chart
#Use case_when to do this
tiger_sighted <- tiger_map_data %>%
  mutate(tigers_spotted = case_when(
    Sex == "Female" &
      national_park== "BBS" ~"5",
    Sex == "Male" &
      national_park== "BBS" ~"8",
    Sex== "Unknown" &
      national_park== "BBS" ~"4",
    Sex == "Female" &
      national_park== "Leu" ~ "2",
    Sex == "Male" &
      national_park== "Leu" ~ "3",
    Sex == "Female" &
      national_park== "Ker" ~ "1",
    Sex == "Unknown" &
      national_park== "Ker" ~ "1"))

#There were no spotted unknown tigers in Leu and no males spotted at Ker
#so they cannot be included in this mutation


#First remove unnecessary columns
tiger_bar <- subset(tiger_sighted, select = -c(IndividualCode, date))


#Now use unique to find out the total number of each gender tiger in each park
new_tiger_bar <- tiger_bar %>%
  unique()

#Change tigers_spotted from character to numeric data to plot as the y axis
new_tiger_bar$tigers_spotted <- as.numeric(new_tiger_bar$tigers_spotted)


#Create a stacked bar chart to show the number of male vs female tigers spotted
ggplot(data=new_tiger_bar, aes(x=national_park, y=tigers_spotted, fill=Sex)) +
  #input the new_tiger_bar data as x and y variables and fill with gender
  geom_bar(stat="identity",position="stack", width= 0.3)+
  #stat='identity displays the sum of genders in each bar
  #stack gender bars on top of each other per each park
  scale_y_continuous() + #set y axis to continuous scale
  scale_fill_manual(values=c("#FEE6CE", "#FDAE6B", "#A1D99b")) +
  #Add custom color palette to the bar chart
  labs(title =
         "Number of Male, Female, and Unknown Tigers sighted at National Parks,
       Feb-Sept, 2014",
       #Add a title to the map
       x="National Park", y="Number of Tigers Sighted")
#label x and y axis

#save bar chart 
ggsave(here("Ggplot Figures", "barchart.png"))


#Create a pie chart representing the percentage of each gender
pie_chart <- new_tiger_bar %>%
  group_by(Sex) %>%
  summarise(sum= sum(tigers_spotted)) #sum each gender across all parks


#Create the pie chart
ggplot(pie_chart, aes(x="", y= sum, fill=Sex)) +
  
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + #use coord_polar to turn ggplot into a pie chart
  labs(x = NULL, y = NULL, fill = NULL,
       title= "The proportion of tiger genders across all national parks",
       #remove all labels and add a title
       caption = "Raw data source: Luskin, M.S., Albert, W.R. & Tobler, M.W.
       Sumatran tiger survival threatened by deforestation despite increasing
       densities in parks.
       Nat Commun 8, 1783 (2017). https://doi.org/10.1038/s41467-017-01656-4",
       x="National Park", y="Number of Tigers Sighted")+
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values=c("#FEE6CE", "#FDAE6B", "#A1D99b")) +
  #select colour palette of pie chart
  geom_text(aes(label = paste(round(sum / sum(sum) * 100, 1), "%")),
            #add percentages of genders onto the pie chart
            position = position_stack(vjust = 0.5))

#save pie chart
ggsave(here("Ggplot Figures", "piechart.png"))



#----------------------Plot an interactive leaflet map--------------------------

# Making a list of zeros for the x coordinates to be added to
longitude = rep(0,length(Parks))
latitude = rep(0,length(Parks))

for(i in 1:length(Parks)){ # Looping through the different parks
  Specific_Park <- filtered_parks %>%
    filter(National_Parks == Parks[i]) # Selecting the specific park
  
  geom <- st_geometry(Specific_Park)
  
  mean_x <- rep(0, nrow(Specific_Park)) #This allows us to fill in the coordinate data from the loops
  mean_y <- rep(0, nrow(Specific_Park))
  
  for(j in 1:nrow(Specific_Park)){
    multi <- geom[[j]] #Index the multipolgyon
    
    coords <- multi[[1]] #get x,y coordinates
    
    df_coords <- data.frame(coords[1]) #turn it into a dataframe
    
    means <- colMeans(df_coords) #overall mean x and y coordinates
    
    mean_x[j] <- means[1] #[1]extracts the average x value  
    
    mean_y[j] <- means[2] #[2]extracts the average y value
  }
  longitude[i] <- mean(mean_x) #create a variable of the mean x coordinate
  
  latitude[i] <- mean(mean_y) #create a variable of the mean y coordinate
}  

geocode <- data.frame(longitude, latitude)

geocode2 <- st_as_sf(geocode, coords= c("longitude", "latitude"), crs= 4326)


# add a column of total_tigers to the geocode2 dataframe to create popups in the leaflet map 
geocode_map <- geocode2 %>%
  mutate(geocode, total_tigers = c(5, 17, 2))

tmap_mode("view") #set the mode to view instead of plot

map_4 <- tm_shape(IDN_shapefile, bbox= bbox_new) + #add new boundary to the whole shapefile
  
  tm_polygons() +
  
  tm_shape(IDN_provinces, bbox= bbox_new) + #add new boundary to the provinces df
  
  tm_polygons("#FDAE6B", alpha = 0.9) + #set colour of regions, change transparency with alpha
  
  tm_borders("#A1D99B", lwd = 2) + #colour of borders and width of border lines
  
  tm_shape(filtered_parks, bbox= bbox_new)+ #add national park regions and add new boundary to the national parks df
  
  tm_fill(col = "National_Parks",
          
          palette = c("#E5F5E0", "#A1D99B", "#31A354"), #Use RColBrewer palette to colour the national parks with a green colourscheme
          
          position= c("RIGHT", "BOTTOM")) +
  
  tm_scale_bar(width = 0.25, text.size = 0.3, #Add a scale bar and adjust the size of text and the width of the bar
               
               position= c("LEFT", "BOTTOM")) + #Choose where to place the scale bar
  
  tm_legend(position = c("RIGHT", "TOP"), legend.outside = TRUE, #Remove the legend off the map and position it to the right
            
            legend.text.size = 1) +
  
  tm_layout(main.title = "Map of Indonesia Overlaid With National Parks", #Title the map
            
            fontface = "italic", #Set style of text to be italicized
            
            fontfamily = "serif", #Set font to Serif for aesthetics
            
            legend.width = 1, #change width of legend so national park names all fit and are consistent in size
            
            legend.height = 0.9, #Change the height of the legend bar
            
            legend.title.size = 1, #Change size of legend title
            
            legend.text.size = 0.6) + #change size of legend text size
  
  tm_shape(geocode_map)+ #add average coordinates
  
  tm_markers(col="red", size=0.3) #change colour and size of coordinate points

#create a leaflet map for interactivity
map_leaflet <- tmap_leaflet(map_4) %>% #tmap_leaflet joins tmap to a leaflet map
  addTiles() %>%
  addCircleMarkers(data= geocode_map, #overlay coordinate points
                   color= "red", #change the colour of the coordinate points
                   radius= 0.3, #set the radius size
                   popup= paste0("<b>total_tigers:</b>",
                                 geocode_map$total_tigers, "<br>"))
setView(map_leaflet, lng = 101.302282, lat =0.113824 , zoom = 1)
#set the longitude and latitude coordinates to specify where the map focuses
setMaxBounds(map_leaflet, 
             lng1 = 88,
             lat1 = -11,
             lng2 = 135,
             lat2 = 16
             #set boundaries for how far around the focus area you can move in the interactive map
)


map_leaflet  #print the map














