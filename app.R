# Elevation Plotting Shiny App -----------------------------------
# This file implements a Shiny app using rayshader and elevatr to grab
# elevation data from a given geographical area and 3d plot it

# load necessary packages 
library(rayshader)
library(elevatr)
library(tidyverse)
library(raster)
library(imager)
library(png)

# center_lat (numeric) is the latitude coordinate of the center
# center_lon (numeric) is the longitude coordinate of the center
# radius (numeric) is the "radius" of the square, in miles
# quality (numeric) is a measure of resolution (as a function of radius)
#    higher quality means slower loading times
elev_matrix <- function(center_lat, center_lon, radius) {
  
  # this string tells elevatr what type of projection we want to use
  prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # calculate the maximum and minimum latitudes and longitudes
  max_lat <- center_lat + (radius / 69)
  min_lat <- center_lat - (radius / 69)
  max_lon <- center_lon + (radius / 69)
  min_lon <- center_lon - (radius / 69)
  
  # make a dataframe containing the geographical bounds
  # of the selected terrain
  geo_bounds <- data.frame(x = c(max_lon, 
                                 max_lon, 
                                 min_lon, 
                                 min_lon),
                           y = c(max_lat,
                                 min_lat,
                                 max_lat,
                                 min_lat))
  
  
  # get the elevations at many points within those bounds, setting the
  # zoom appropriately in relation to the chosen radius
  #zoom <- round(quality / radius / 69 * 700)
  
  #if (zoom < 9) {
  #  zoom <- 9
  #} else if (zoom > 14) {
  #  zoom <- 14
  #}
  
  elevation <- get_elev_raster(locations = geo_bounds,
                               prj = prj,
                               z = 14,
                               clip = "bbox",
                               verbose = FALSE)
  
  # coerce the RasterLayer to a spatialdataframe
  elevation_spatial_df <- as(elevation, 'SpatialGridDataFrame')
  
  # ...and then coerce that to a dataframe
  elevation_df <- as.data.frame(elevation_spatial_df)
  
  # find the desired dimensions of the matrix
  nrow <- length(unique(elevation_df$s1))
  
  # make the matrix
  elevation_matrix <- matrix(elevation_df$layer,
                             nrow = nrow)
  # and return it!
  elevation_matrix
}


# User Interface (Frontend) ------------------------------------------------
ui <- fluidPage(
  
  # Add a title
  titlePanel("3D Elevation Plotting Tool"),
  
  # This sidebar is where the user can make plot choices
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        # subsection of the sidebar: geo options (takes more time)
#        h3("Geographical Options"),
        # A numeric input box for the center latitude point
        numericInput(inputId = "center_lat",
                     label = "Center Latitude Point:",
                     value = 45.373601,
                     min = -90,
                     max = 90),
        # A numeric input box for the center latitude point
        numericInput(inputId = "center_lon",
                     label = "Center Longitude Point:",
                     value = 45.373601,
                     min = -90,
                     max = 90),
        # A slider input box for the plot radius
        sliderInput(inputId = "radius",
                    label = "Radius (in miles)",
                    value = 1,
                    min = .1,
                    max = 10),
        # Wait to regrab all the data based on the parameters above
        # until the user says they're ready--this rerender will
        # take longer than the one below
        actionButton(inputId = "go_button",
                     label = "Go!"),
        helpText("Press this button once you have all the numbers
                 above set to where you want them!"),
        # subsection of the sidebar: plotting options (takes less time)
#        h3("Plotting Options"),
        # adjust the zscale of the plot
#        sliderInput(inputId = "z_scale",
#                    label = "Height Scale",
#                    value = 40,
#                    min = .1,
#                    max = 100),
        # cite the elevatr data source
        helpText("Data Source: Mapzen AWS Terrain Tiles")),
      # The main panel of the page is the plot
      mainPanel(imageOutput("plot")))),
  theme = "style.css")



# Server (Backend) ----------------------------------------------------------
server <- function(input, output) {
  # These are the user inputs; default to mt. hood
  reactive_vals <- reactiveValues(height_scale = 40,
                                  center_lat = 45.373601, 
                                  center_lon = -121.695942, 
                                  radius = 1,
                                  elev_data = NULL)
 
  # Only react to input changes when the go button is pressed
  observeEvent(input$go_button, { 
    # In that case, render the plot!
    output$plot <- renderImage({
      withProgress(message = "One sec!",
                   detail = "Downloading data...", {
      
        # grab the elevation data
        reactive_vals$elev_data <- elev_matrix(center_lat = reactive_vals$center_lat,
                                               center_lon = reactive_vals$center_lon,
                                               radius = reactive_vals$radius)
        
        
        # plot the elevation data and save it as a png
        sphere_shade(reactive_vals$elev_data, 
                     texture = "imhof2", 
                     sunangle = 70) %>%
        # Would be great to make this an interactive 3d plot
        # but X11 doesn't seem to play nice with Shiny.
        #plot_3d(reactive_vals$elev_data, 
        #        zscale = reactive_vals$height_scale, 
        #        theta = 220, 
        #        phi = 20) %>%
        save_png(filename = "output_plot.png")
        
        # read in the plot
        #output_plot <- load.image("output_plot.png")
        
        # resize it to a square
        #output_plot <- resize(output_plot, 300, 300) %>%
        #               matrix()
        
        # save the plot again
        #writePNG(output_plot, "output_plot.png")
        
        
        # return a list with the filepath to the plot png
        list(src = "output_plot.png")
      
      # The end of the withProgress call
      })
    # The end of the renderImage call
    }, deleteFile = FALSE)
  # The end of the observeEvent call
  })
# The end of the server call
}

# Run the app
shinyApp(ui = ui, server = server)

