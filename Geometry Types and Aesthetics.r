# Data Visualization with ggplot

# Load the dataset
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#### Scatterplots ####

#Create simple scatterplot using the point geometry.
#Necessary aesthetics: coordinates
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg)) 

#Suppose we now want to separate public and private institutions ("control")

#Separate by shape aesthetic
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, shape=control)) #Hard to differentiate

#Separate by colour aesthetics
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control)) #Much easier to differentiate

#Suppose we want to show a fourth variable (# of undergrads). 

#Show number of undergrads using the size of the points (size aesthetic)
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads)) #Hard to see because of overlap

#Add a bit of transparency to increase visibility (alpha aesthetic)
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1) #Outside the mapping because we want all the points to have a transparency of alpha=1
#Alpha=1 is the default (opaque)
#Alpha=0.01 is 1% opaque
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=0.01) #Too transparent
#Set to 50% transparency
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=0.5) #Much better


#### Lines and smoothers ####

#Line charts use the line geometry
#The aesthetics are pretty similar to scatterplots.
ggplot(data=college) +
  geom_line(mapping=aes(x=tuition, y=sat_avg, color=control))

#Overlap the points
ggplot(data=college) +
  geom_line(mapping=aes(x=tuition, y=sat_avg, color=control))+
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control)) #The line is connecting each point

#The aesthetics are the same, so it's possible to make the code cleaner by changing the location of the mapping
ggplot(data=college, mapping=aes(x=tuition, y=sat_avg, color=control)) +
  geom_line()+
  geom_point()

#Instead of having a line connecting the points, it would be better to fit a line using smoothers.
#Smoothers use the smooth geometry
ggplot(data=college, mapping=aes(x=tuition, y=sat_avg, color=control)) +
  geom_smooth()+
  geom_point() #The points are making the lines hard to see

#Increase point transparency to make the lines more visible
ggplot(data=college, mapping=aes(x=tuition, y=sat_avg, color=control)) +
  geom_smooth()+
  geom_point(alpha=0.04) #The grey area is the standard error. 

#Remove standard error
ggplot(data=college, mapping=aes(x=tuition, y=sat_avg, color=control)) +
  geom_smooth(se=FALSE)+
  geom_point(alpha=0.04)


#### Bars and Columns ####

#How many schools are in each of the four regions

#Make a bar graph (bar geometry)
ggplot(college)+
  geom_bar(mapping=aes(region))

#Separate public and private schools using a stacked bar
#What about using the color aesthetic?
ggplot(college)+
  geom_bar(mapping=aes(region, color=control)) #Not good - only shows an outline

#Create a stacked bar using the filled aesthetic
ggplot(college)+
  geom_bar(mapping=aes(region, fill=control))

##Create a tible for the mean tuition by region
college %>% group_by(region) %>% summarize(average_tuition=mean(tuition))

#Show the mean tuition by region
college %>% group_by(region) %>% summarize(average_tuition=mean(tuition)) %>%
  ggplot()+ #No need to specify the dataset because of the pipe operator
    geom_bar(mapping=aes(x=region, y=average_tuition)) #Error: can't specify the y value in a bar plot

#We thus need to use a column chart (col geometry)
#Show the mean tuition by region
college %>% group_by(region) %>% summarize(average_tuition=mean(tuition)) %>%
  ggplot()+ 
  geom_col(mapping=aes(x=region, y=average_tuition))

#### Histograms ####

#Suppose we want to see the distribution of undergrads per college

#Create a bar graph
ggplot(college)+
  geom_bar(mapping=aes(undergrads)) #Not good - there are thousands of a bars!

#Use a histogram instead (histogram geometry)
ggplot(college)+
  geom_histogram(mapping=aes(undergrads)) #Got a message saying that R chose 30 bins

#The histogram starts <0. This doesn't make sense
#Start the histogram at 0
ggplot(college)+
  geom_histogram(mapping=aes(undergrads), origin=0)

#Suppose we only want 10 bins
ggplot(college)+
  geom_histogram(mapping=aes(undergrads), origin=0, bins=10)

#Suppose we want to specify the width of each bin, and not necessarily the number of bins
ggplot(college)+
  geom_histogram(mapping=aes(undergrads), origin=0, bins=10, binwidth = 10000) #The data is divided into batches of 10,000 undergrads

#### Boxplots ####

#How does tuition vary for public vs. private colleges?

#Use a scatterplot
ggplot(college)+
  geom_point(mapping=aes(x=control, y=tuition)) #Tuition is a continuous variable and control is discrete. The visualization is unhelpful as we can't see the density of the tuition

#Could use a jitter plot (which adds some noise to the x variable)
#jitter geometry
ggplot(college)+
  geom_jitter(mapping=aes(x=control, y=tuition)) #This is also unhelpful for seeing the density of tuition

#Use a boxplot (boxplot geometry)
ggplot(college)+
  geom_boxplot(mapping=aes(x=control, y=tuition)) #As we would expect, private schools tend to have a higher tuition.

