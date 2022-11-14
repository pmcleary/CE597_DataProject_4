library("plyr")
library("dplyr")
library("sf")
library("tmap")
library("tmaptools")
library("raster")
library("RColorBrewer")
library("lwgeom")
library("tidyr")
library("Hmisc")
library("tidyverse")

# Read in the shapefile of building data
dataimport <- st_read('buildings_shpfile/buildings.shp')
dataimport <- subset(dataimport, is.na(YEAR_RAZED))
campus_bldgs <- dplyr::select(dataimport,Entity,BLDG_ABBR,BUILDING_N,GIS_BID,Shape_Leng,Shape_Area,geometry) 
# Project the data into UTM Zone 16N, WGS 84
campus_bldgs <- st_transform(campus_bldgs,32616)
# Check for empty geometries
check <- any(is.na(st_dimension(campus_bldgs$geometry)))
# Check for corrupt geometries
check2 <- any(is.na(st_is_valid(campus_bldgs$geometry)))
# Check for invalid geometries
check3 <- any(na.omit(st_is_valid(campus_bldgs$geometry)) == FALSE)
# I have empty, corrupt, and invalid geometries; use st_make_valid to correct them.
#Single polygons may become multi-geometries in case of self-intersections.
# Remove anything with empty geometries.
campus_bldgs <- campus_bldgs[!st_is_empty(campus_bldgs),]
campus_bldgs <- st_make_valid(campus_bldgs)
campus_bldgs <- st_collection_extract(campus_bldgs, "POLYGON") # Extract MultiPolygons from Geometry Collections
campus_bldgs <- st_cast(campus_bldgs, to = "POLYGON")

#------My Perimeter Function---------------------------
my_perimeter <- function(d){
  m <- as.matrix(d[[1]])
  x <- m[ ,1]
  y <- m[ ,2]
  vertices <- length(m)
  i <- 1
  j <- i + 1
  perimeter = 0
  for (val in x){
    len <- sqrt((y[j] - y[i])^2 + (x[j] - x[i])^2)
    j <- j + 1
    i <- i + 1
    perimeter = perimeter + len
    if(i == vertices/2) {j == 1}
    if (i == vertices/2)
      break
  }
  return(perimeter)
}
#------My Area Function---------------------------
my_area <- function(d){
  m <- as.matrix(d[[1]])
  x <- m[ ,1]
  y <- m[ ,2]
  vertices <- length(m)
  i <- 1
  j <- (vertices/2)
  area = 0
  for (val in x){
    area <- area + (x[j] - x[i])*(y[i] + y[j])
    j <- i
    i <- i + 1
    }
area <- (area/2) * -1
return(area)
}
#------My Centroid Function---------------------------
my_centroid <- function(d,A){
  m <- as.matrix(d[[1]])
  x <- m[ ,1] 
  y <- m[ ,2]
  temp <- nrow(m) -1
  m <- m[1:temp, ]
  x_cent <- 0
  y_cent <- 0
  for (i in 1:nrow(m)){
    x_cent[i] <- -(y[i+1] - y[i])*(x[i]^2 + (x[i]*x[i+1]) + x[i+1]^2)
    y_cent[i] <- -(x[i] - x[i+1])*(y[i]^2 + (y[i]*y[i+1]) + y[i+1]^2) 
  }
  x <- sum(x_cent)/(A*6)
  y <- sum(y_cent)/(A*6)
  return(c(x,y))
}
#---------------Use the functions to calculate results---------------------------
campus_bldgs["my_perimeter"] <- sapply(campus_bldgs$geometry, my_perimeter)
campus_bldgs["my_perimeter"] <- round(campus_bldgs$my_perimeter , digits = 3)
campus_bldgs["my_area"] <- sapply(campus_bldgs$geometry, my_area)
campus_bldgs["my_area"] <- round(campus_bldgs$my_area, digits = 3) 
cent_temp <- mapply(my_centroid, campus_bldgs$geometry, A = campus_bldgs$my_area)
# mapply returns a matrix, below I transpose it to column form then create 
# a data frame to round the values to 3 decimal places, next the x and y 
#columns are united into one column for display ini the final sf data frame  
cent_temp <- t(cent_temp)
cent_df <- data.frame(cent_temp)
cent_df <- round(cent_df, digits = 3)
cent_df1 <- unite(cent_df, newcol, c(X1, X2), remove=FALSE)
campus_bldgs["my_centroid"] <- cent_df$newcol
# Create Columns of perimeter, area, and centroid with r functions
# Perimeter and area are rounded to 3 decimal places
campus_bldgs["r_perimeter"] <- st_perimeter(campus_bldgs$geometry)
campus_bldgs["r_perimeter"] <- as.numeric(round(campus_bldgs$r_perimeter, digits = 3))
campus_bldgs["r_area"] <- st_area(campus_bldgs$geometry)
campus_bldgs["r_area"] <- as.numeric(round(campus_bldgs$r_area, digits = 3))
r_cent <- mapply(st_centroid,campus_bldgs$geometry) 
r_cent <- t(r_cent)
r_centdf <- data.frame(r_cent)
r_centdf <- round(r_centdf, digits = 3)
r_centdf1 <- unite(r_centdf, newcol, c(X1, X2), remove=FALSE)
campus_bldgs["r_centroid"] <- r_centdf$newcol
campus_bldgs["diff_perimeters"] <- campus_bldgs$my_perimeter - campus_bldgs$r_perimeter 
campus_bldgs["diff_areas"] <- campus_bldgs$my_area - campus_bldgs$r_area
# Calculate centroid distances
cent_diffs <- sqrt((cent_temp[ ,1] - r_cent[ ,1])^2 + (cent_temp[ ,2] - r_cent[ ,2])^2)
cent_diffs <- round(cent_diffs, digits = 3) 
campus_bldgs["diff_centroids"] <- cent_diffs 
# Calculate Statistics of differences between functions
#Centroid stats
max_cent_diff <- max(campus_bldgs$diff_centroids[is.finite(campus_bldgs$diff_centroids)])
mean_cent_diff <- mean((campus_bldgs$diff_centroids[is.finite(campus_bldgs$diff_centroids)]))
sd_cent_diff <- sd((campus_bldgs$diff_centroids[is.finite(campus_bldgs$diff_centroids)]))
min_cent_diff <- min(((campus_bldgs$diff_centroids[is.finite(campus_bldgs$diff_centroids)])))
hist(campus_bldgs$diff_centroids, col = "blue", xlab = "My Perimeter vs. R Perimeter",
     main = "Differences Between My Centroid Function and R's")
# Perimeter stats
max_perm_diff <- max(campus_bldgs$diff_perimeters)
mean_perm_diff <- mean(campus_bldgs$diff_perimeters)
sd_perm_diff <- sd(campus_bldgs$diff_perimeters)
min_perm_diff <- min(campus_bldgs$diff_perimeters)
# Area stats
max_area_diff <- max(campus_bldgs$diff_areas)
mean_area_diff <- mean(campus_bldgs$diff_areas)
sd_area_diff <- sd(campus_bldgs$diff_areas)
#---------------Begin section 2---------------------------------------
#Read in twitter data
Tweets2014 <- read.csv("pu2014.csv")
Tweets <- subset(Tweets2014, select = c("epoch","user_id","longitude","latitude"))
Tweets <- st_as_sf(Tweets, coords = c("longitude", "latitude"))
# First establish a geographic coord sys for the twitter data
st_crs(Tweets) <- 4326
# Now add a projected crs to the twitter data 
Tweets <- st_transform(Tweets, 32616)
# crop the geographic area of the twitter data to just campus for faster processing
cropped_tweets <- st_crop(Tweets, c(xmin=505161.53, xmax=507600.62, ymin=4474165.91, ymax=4476469.37))
# crop the geographic area of the campus buildings data to just campus for faster processing
cropped_campus_bldgs <- st_crop(campus_bldgs, c(xmin=505161.53, xmax=507600.62, ymin=4474165.91, ymax=4476469.37))
# Join Twitter data and Campus polygons, used 10 meters to reduce overlapping points
bldgs_tweets <- st_join(cropped_tweets,cropped_campus_bldgs['BUILDING_N'], join = st_is_within_distance, dist = 10)
# Count the # of tweets per building
t_per_bldg <- count(as_tibble(bldgs_tweets), BUILDING_N)
# Add tweets per building to the campus polygon data to create a thematic map 
t_per_bldg <- as.data.frame(t_per_bldg)
cropped_campus_bldgs$Tweets_per_Bldg <- t_per_bldg[match(cropped_campus_bldgs$BUILDING_N, t_per_bldg$BUILDING_N), "n"]
# Remove NAs from bulding data
cropped_campus_bldgs <- subset(cropped_campus_bldgs,! is.na(BUILDING_N))
# Conduct time analysis on twitter data and top tweeting buildings
bldgs_tweets$date_time <- lubridate::as_datetime(bldgs_tweets$epoch)
bldgs_tweets$day_week <- weekdays(bldgs_tweets$date_time)
corec <- filter(bldgs_tweets, BUILDING_N == "Cordova Recreational Sports Center")
corec_count <- count(as_tibble(corec), day_week)
row_order <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
corec_count <- corec_count %>% slice(match(row_order, day_week))
owen <- filter(bldgs_tweets, BUILDING_N == "Richard Owen Residence Hall")
owen_count <- count(as_tibble(owen), day_week)
owen_count <- owen_count %>% slice(match(row_order, day_week))
pmu <- filter(bldgs_tweets, BUILDING_N == "Purdue Memorial Union")
pmu_count <- count(as_tibble(pmu), day_week)
pmu_count <- pmu_count %>% slice(match(row_order, day_week))
barplot(corec_count$n,main = "Tweets at CoREC throughout the week",ylab = "Number of Tweets", col = "blue", names.arg = row_order, las = 2, cex.names = .7)
barplot(owen_count$n,main = "Tweets at Richard Owen Res Hall throughout the week",ylab = "Number of Tweets", col = "blue", names.arg = row_order, las = 2, cex.names = .7)
barplot(pmu_count$n,main = "Tweets at Purdue Memorial Union throughout the week",ylab = "Number of Tweets", col = "blue", names.arg = row_order, las = 2, cex.names = .7)
tmap_mode("view")
Thematic_Tweet_Map <- tm_shape(cropped_campus_bldgs) + tm_polygons("Tweets_per_Bldg", style = "jenks", palette = "YlOrRd", n = 5)
print(Thematic_Tweet_Map)
