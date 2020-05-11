# Data Visualization with ggplot
# Geospatial Visualizations

library(tidyverse)
library(ggmap)
library(maps)
library(mapproj)

#Uses Google's API - we need to enable "static maps" and "geocoding" APIs specifically
#See console.cloud.google.com

#Register key
register_google(key="AIzaSyAvDjy7f4gUx3LROJadM9Fp1zx29uIrP2U") #DELETE KEY

#### Introduction to ggmap ####

#Create a map of New York
qmap("New York, NY", zoom=10) #zoom=10 is a good overview for a metropolitan area. zoom=20 is a very specific view. zoom=5 shows a regional view.

#Save the map of New York - need to use get_map
nyc_map<-get_map("New York, NY", zoom=10)

#Plot the saved map
ggmap(nyc_map)

#### Geocoding ####

#Save the coordinates of New York
nyc<-geocode("New York, NY")
nyc

#Save the coordinates of a company (Lynda.com)
lynda<-geocode("Lynda.com")
lynda

#Save the coordinates of a landmark (the White House)
whitehouse<-geocode("White House")
whitehouse

#Show a map of New York using coordinates
nyc_map<-get_map(nyc)
ggmap(nyc_map)

#Show Lynda.com on a map using one line of code
ggmap(get_map(lynda))

#Save a map of the White House using one line of code
whitehousemap<-ggmap(get_map("whitehouse", zoom=18))
whitehousemap

#### Map Types ####

# The default map type is a terrain map (shows water, mountains, etc.)

# Show a terrain map
ggmap(get_map(nyc, maptype = "terrain"))

#Show a roadmap - emphasis on roads (e.g. shows highways)
ggmap(get_map(nyc, maptype = "roadmap"))

#Shows only the labels
ggmap(get_map(nyc, maptype = "terrain-labels"))

#Shows only the terrain lines
ggmap(get_map(nyc, maptype = "terrain-lines"))

#Shows satellite photo
ggmap(get_map(nyc, maptype = "satellite"))

#Shows hybrid map (satellite + terrain)
ggmap(get_map(nyc, maptype = "hybrid"))

#Show black and white map
ggmap(get_map(nyc, maptype = "toner"))

#Show a lighter black and white map
ggmap(get_map(nyc, maptype = "toner-lite"))

#Show a black and white background map
ggmap(get_map(nyc, maptype = "toner-background"))

#Show a watercolour palette map (artistic)
ggmap(get_map(nyc, maptype = "watercolor"))

#### Plotting ####

# Geocode the US
usa <- geocode("United States")

#Map the US
ggmap(get_map(usa, zoom=4)) #zoom=4 shows a large country such as the US

#Add a point for NYC 
ggmap(get_map(usa, zoom=4))+
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=nyc) #nyc is saved as a data frame with variables "lon" and "lat"

#Obtain the coordinates for multiple places
placenames<-c("New York, NY", "White House", "Lynda.com", "Mt. Rushmore", "The Alamo")
locations<-geocode(placenames)

#Save coordinates with names
places<-tibble(name=placenames, lat=locations$lat, lon=locations$lon)

#Plot the places
ggmap(get_map(usa, zoom=4))+
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=places)

#Add labels to map
ggmap(get_map(usa, zoom=4))+
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=places)+
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", data=places) #The labels overwrite the points...

#Add labels to map
ggmap(get_map(usa, zoom=4))+
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=places)+
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", data=places, nudge_y=1) #Nudge moves the value of a mapping by a small amount

#Make the map clearer by changing the map type
ggmap(get_map(usa, zoom=4, maptype = "toner-background"))+ #Much more readable
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=places)+
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", data=places, nudge_y=1)
ggmap(get_map(usa, zoom=4, maptype = "watercolor"))+ #Artistic map!
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=places)+
  geom_text(mapping=aes(x=lon, y=lat, label=name), color="red", data=places, nudge_y=1)

#### Building Maps Manually ####

#Suppose we want to create a blank map of the US with outlines for the states (e.g. to create a choropleth)

#Save the data for the state borders
states<-map_data("state") #This data can be used to create polygons that represent the state borders

#Create blank map using the polygon geometry
ggplot(data=states, mapping=aes(x=long, y=lat))+
  geom_polygon() #Created a rough map of the US but we forgot to "group" the polygons, meaning each dot is trying to connect with each others.

#Group the states, creating 48 polygons
ggplot(data=states, mapping=aes(x=long, y=lat, group=group))+ #The "states" tibble has a variable called "group" belonging to each state.
  geom_polygon() #The map is distorted, because the coordinates are on a globe

#Map the points in 2D
ggplot(data=states, mapping=aes(x=long, y=lat, group=group))+ #The "states" tibble has a variable called "group" belonging to each state.
  geom_polygon()+
  coord_map() #Specifies we are creating a map with the points

#Change the theme (remove latitude and longitude axes + gridlines)
ggplot(data=states, mapping=aes(x=long, y=lat, group=group))+ #The "states" tibble has a variable called "group" belonging to each state.
  geom_polygon()+
  coord_map()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title =  element_blank(),
        panel.background = element_blank())

#### Choropleths ####

#Load the "college" dataset
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#We want to colour states by number of schools

#Count the number of schools in each state
college_summary<-college %>%
  group_by(state) %>%
  summarize(schools=n()) #n() counts the number of schools in each group

#The "college_summary" tibble uses two letter state abbreviations and stores them as factors. "states", meanwhile, uses the states' full names (in all lowecases) and stores them as strings.

#Overwrite state names to have a consistent naming scheme
college_summary <- college_summary %>% 
  mutate(region=as.character(setNames(str_to_lower(state.name), state.abb)[as.character(state)])) #"state.name" and "state.abb" are preloaded R vectors
#DC is not a state, hence it does not appear in "state.name" and "state.abb"

#Overwrite DC
college_summary <- college_summary %>% 
  mutate(region=ifelse(as.character(state)=="DC", "district of columbia", region))

college_summary #The data is ready for mapping

#Merge the data into one set
mapdata<-merge(states, college_summary, by="region")

#Plot a basic map of the US
ggplot(data=mapdata, mapping=aes(x=long, y=lat, group=group))+ 
  geom_polygon()+
  coord_map()+
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title =  element_blank(),  plot.background=element_blank(), panel.background = element_blank())

#Create the choropleth by changing the fill according to "schools"
ggplot(data=mapdata, mapping=aes(x=long, y=lat, group=group, fill=schools))+ 
  geom_polygon()+
  coord_map()+
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title =  element_blank(),  plot.background=element_blank(), panel.background = element_blank())
#This fill is not optimal. For instance, lighter colours indicate more schools, which is counterintuitive.

#Create the choropleth using a gradient
ggplot(data=mapdata, mapping=aes(x=long, y=lat, group=group, fill=schools))+ 
  geom_polygon()+
  coord_map()+
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title =  element_blank(),  plot.background=element_blank(), panel.background = element_blank())+
  scale_fill_gradient(low="beige", high="red")
