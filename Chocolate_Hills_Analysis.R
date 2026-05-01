library(terra)
library(sf)
library(dplyr)
library(ggstatsplot)
library(ggplot2) 

# ELEVATION DISTRIBUTION ANALYSIS USING RASTER DATA (TIFF)

# Read the TIFF file
dem_bohol <- rast("D:/Wordpress/Bohol/Chocolate_Hills_Area.tif")

# Convert raster (pixels) to a dataframe for statistical processing
df_dem <- as.data.frame(dem_bohol, xy = TRUE)

# QGIS usually provides a default band column name (e.g., layer, Band_1, etc.)
# We rename it to be more intuitive
colnames(df_dem) <- c("Longitude", "Latitude", "Elevation")

# Clean data from anomalous values (e.g., empty pixels with negative values)
df_clean <- df_dem %>% 
  filter(Elevation > 0)

# Create statistical visualization with English labels
plot_raster <- gghistostats(
  data = df_clean,
  x = Elevation,
  title = "Elevation Distribution of the Chocolate Hills Karst Landscape",
  subtitle = "Geomorphological evidence of cockpit/cone karst (Bimodal Pattern)",
  xlab = "Elevation (Meters above sea level)",
  ylab = "Frequency (Pixel Count)",
  type = "parametric",
  normal.curve = TRUE, 
  results.subtitle = FALSE 
) + 
  theme(
    plot.title = element_text(hjust = 0.5),    # Center the main title
    plot.subtitle = element_text(hjust = 0.5)  # Center the subtitle
  )

# Display the plot
print(plot_raster)

# ANALYSIS USING CONTOUR DATA (SHAPEFILE)

# Read contour data directly from the .shp file
contour_bohol <- st_read("D:/Wordpress/Bohol/Chocolate_Hills_Contours.shp")

# Drop geometry data to make the plotting process lighter and faster
df_contour <- st_drop_geometry(contour_bohol)

# We can check the column names with: names(df_contour)
# QGIS usually names it "ELEV"

# Plot elevation distribution based on contour lines
plot_contour <- gghistostats(
  data = df_contour,
  x = ELEV, # Ensure this column name matches our data
  title = "Contour Interval Distribution of the Chocolate Hills",
  xlab = "Contour Elevation (Meters above sea level)",
  ylab = "Number of Contour Lines"
) + 
  theme(
    plot.title = element_text(hjust = 0.5),    # Center the main title
    plot.subtitle = element_text(hjust = 0.5)  # Just in case there is a statistical test/subtitle below the title
  )

# Display the plot
print(plot_contour)

# SLOPE ANALYSIS & DESCRIPTIVE STATISTICS

# Calculate slope angle (in degrees) from DEM data
slope_bohol <- terrain(dem_bohol, v = "slope", unit = "degrees")

# Convert to dataframe
df_slope <- as.data.frame(slope_bohol, xy = TRUE)
colnames(df_slope) <- c("Longitude", "Latitude", "Slope")

# Filter extreme/empty values
df_slope_clean <- df_slope %>% filter(Slope > 0)

# Create slope angle distribution histogram
plot_slope <- gghistostats(
  data = df_slope_clean,
  x = Slope,
  title = "Slope Angle Distribution of Chocolate Hills",
  subtitle = "Evidence of uniform geomorphological weathering",
  xlab = "Slope Angle (Degrees)",
  ylab = "Frequency",
  type = "parametric",
  normal.curve = TRUE,
  results.subtitle = FALSE
) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Display the plot
print(plot_slope)

# Create an elevation summary statistics table
elevation_summary <- df_clean %>%
  summarise(
    Minimum_Elevation = min(Elevation),
    Maximum_Elevation = max(Elevation),
    Average_Elevation = mean(Elevation),
    Median_Elevation = median(Elevation)
  )

# Display the summary
print(elevation_summary)
