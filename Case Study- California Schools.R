# Data Visualization with ggplot
# Case Study: Schools in California
# Goal: create a map of schools in California that also shows undergraduate enrollment and whether the school is public or private
# I wrote most of this code by myself as a challenge

#### Preamble (provided) ####

# Load the tidyverse libraries
library(tidyverse)
library(ggmap)

# Load the college dataset
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Load state information
states <- map_data("state")

# Plot a basic US map
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank()) 

#### Mapping ####

#Plot the schools
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=college, mapping=aes(x=lon, y=lat)) #Notice that the map also shows schools in Alaska and Hawaii

#Plot schools in the continential US
college<-filter(college, state != "AK" & state != "HI")
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=college, mapping=aes(x=lon, y=lat))

#### Institution Size and Control + Cleaning Up the Legends ####

#Modify the size of points based on undergraduate enrollment
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=college, mapping=aes(x=lon, y=lat, size=undergrads), alpha=0.5) #50% transparency looks good

#Modify the colour of the points based on type of institution
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=college, mapping=aes(x=lon, y=lat, size=undergrads, colour=control), alpha=0.5) #Default colours look fine

#Change the legend titles
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=college, mapping=aes(x=lon, y=lat, size=undergrads, colour=control), alpha=0.5)+
  scale_color_discrete(guide_legend(title="Institutional Control"))+
  scale_size(guide_legend(title="Undergraduate Population"))

#### Zooming in on California ####

#Filter to keep only points in California
california<-filter(states, region=="california")
california_colleges<-filter(college, state=="CA")

#Map schools in California
ggplot(california) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=california_colleges, mapping=aes(x=lon, y=lat, size=undergrads, colour=control), alpha=0.5)+
  scale_color_discrete(guide_legend(title="Institutional Control"))+
  scale_size(guide_legend(title="Undergraduate Population")) #The map is fairly uninformative - there are no lines or city

#Import US cities from the "maps" package
CA_cities<-filter(us.cities, country.etc=="CA")

#Keep selected cities
cities<-c("Los Angeles", "San Diego", "San Jose", "San Francisco", "Fresno", "Sacramento")
CA_cities<-subset(CA_cities, CA_cities$name %in% paste(cities, "CA"))

#Remove the last three character of city names (i.e. the " CA")
CA_cities$name<-str_remove(CA_cities$name, " CA")

#Add the city names on the map
ggplot(california) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=california_colleges, mapping=aes(x=lon, y=lat, size=undergrads, colour=control), alpha=0.5)+
  scale_color_discrete(guide_legend(title="Institutional Control"))+
  scale_size(guide_legend(title="Undergraduate Population"))+
  geom_text(mapping=aes(x=long, y=lat, label=name), data=CA_cities) #Names appear to overlay the dots in the origin visualization, so that's fine.

#Import county boundaries
county<-map_data("county", region="california")

#Add the county boundaries on the map
ggplot(county) + #Turns out the "california" map is not even necessary!
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=california_colleges, mapping=aes(x=lon, y=lat, size=undergrads, colour=control), alpha=0.5)+
  scale_color_discrete(guide_legend(title="Institutional Control"))+
  scale_size(guide_legend(title="Undergraduate Population"))+
  geom_text(mapping=aes(x=long, y=lat, label=name), data=CA_cities) 

#### Adding a title and subtitle ####

#Add a title and substitle
ggplot(county) + #Turns out the "california" map is not even necessary!
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())+
  geom_point(data=california_colleges, mapping=aes(x=lon, y=lat, size=undergrads, colour=control), alpha=0.5)+
  scale_color_discrete(guide_legend(title="Institutional Control"))+
  scale_size(guide_legend(title="Undergraduate Population"))+
  geom_text(mapping=aes(x=long, y=lat, label=name), data=CA_cities)+
  ggtitle("Most California Colleges are Located in Large Cities", subtitle = "Source: U.S. Department of Education")
