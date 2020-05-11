# Data Visualization with ggplot
# Beautifying Your Visualizations

# Load the dataset
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#### Theme (Background) ####

#Create a basic bar plot
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))

#Change the plot background (which is the defaultly white space outside of the actual chart) to purple
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(plot.background = element_rect(fill="purple")) #element_rect: we want the background to be a rectangle

#Change the panel background (where the chart appears) to purple
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_rect(fill="purple"))

# Remove all backgrounds for a minimalist aesthetic
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) #Element_blank removes a background object
#However, the gridlines are gone

#Add gridlines
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major = element_line(colour="grey")) #The x-axis gridlines are fairly useless

#Add gridlines only on the y axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey"))

#### Axes ####

#The default "count" y-axis value is fairly uninformative
#Replace it to "Number of schools"
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey"))+
  ylab("Number of Schools")

#Capitalize the x axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey"))+
  ylab("Number of Schools")+
  xlab("Region")

#The bar for "South" exceeds the maximum value of the y-axis
#Change the limit to 0-500
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey"))+
  ylab("Number of Schools")+
  xlab("Region")+
  ylim(0, 500)

#Note: if the limits are such that an entire bar segment are exceed them, then that segment is not shown. 
#Example:
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey"))+
  ylab("Number of Schools")+
  xlab("Region")+
  ylim(50, 500) #The "Public" values all disappear! (Since the count starts at 0)

#### Scales ####

#Scales map data onto the visualization. Examples: axes, colours for different factors, etc.
#Scales follow a scale_name_datatype convention. E.g. scale_x_discrete

#Capitalize the x axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")

#Replace the name of the y axis to "Number of schools"
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools")

#Change the limits of the y axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500)) #Include in the same call as the namechange!

#Change the fill colours; fill based on a variable is a scale
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue")) #Manuel because we are forcing colours. scale_fill_discrete would return a default colour scheme. 

#### Legends ####
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue")) #Notice that R understands that a legend is necessary for interpretation

#Control is not an informative name
#Change the legend title
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide_legend(title="Institution Type")) #guide_legend is used to change legends
#Here, since we want to change the colour legend, it goes into the call for scale_fill_manual

#What if we had not specified the colours? That's what scale_fill_discrete is for!
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_discrete(guide_legend(title="Institution Type"))

#Make the legend 1X2 instead of 2X1
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type", nrow=1)) #guide= is necessary when >1 arguments are passed

#Make the label appear underneath the colour box (label.position)
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type",
                                                                   nrow=1,
                                                                   label.position = "bottom"))

#Change the colour box width (keywidth)
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type",
                                                                   nrow=1,
                                                                   label.position = "bottom",
                                                                   keywidth = 2.5)) #Default is 1

#Change legend position
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type",nrow=1,label.position = "bottom",keywidth = 2.5))+
  theme(legend.position = "bottom") #It's a change to the theme, not to the legend's contents

#### Anotation #####

#Create scatterplot
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2)

#Annotate the competitive and expensive schools as "Elite Privates"
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2)+
  annotate("text", label="Elite Privates", x=45000, y=1450) #"text" is the type of annotation

#Add a line at the mean of "sat_avg"
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2)+
  annotate("text", label="Elite Privates", x=45000, y=1450)+
  geom_hline(yintercept=mean(college$sat_avg)) #hline = horizontal line

#Add an anotation for the line
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2)+
  annotate("text", label="Elite Privates", x=45000, y=1450)+
  geom_hline(yintercept=mean(college$sat_avg))+
  annotate("text", label="Mean SAT", x=47500, y=mean(college$sat_avg)-15)

#Add a line at the mean tuition (with an annotation)
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2)+
  annotate("text", label="Elite Privates", x=45000, y=1450)+
  geom_hline(yintercept=mean(college$sat_avg))+
  annotate("text", label="Mean SAT", x=47500, y=mean(college$sat_avg)-15)+
  geom_vline(xintercept = mean(college$tuition))+
  annotate("text", label="Mean Tuition", x=mean(college$tuition)+7500, y=700)

#Final esthetic changes
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2)+
  annotate("text", label="Elite Privates", x=45000, y=1450)+
  geom_hline(yintercept=mean(college$sat_avg))+
  annotate("text", label="Mean SAT", x=47500, y=mean(college$sat_avg)-15)+
  geom_vline(xintercept = mean(college$tuition))+
  annotate("text", label="Mean Tuition", x=mean(college$tuition)+7500, y=700)+
  theme(panel.background = element_blank(), legend.key = element_blank())+ #legend.key is the background colour of the legend boxes
  scale_color_discrete(name="Institution Type")+
  scale_size_continuous(name="Undergraduates")

#Only private schools have average SAT scores and tuitions above the national averages.

#### Titles ####

#Based on the ggtitle() command.

#Create bar chart
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type",nrow=1,label.position = "bottom",keywidth = 2.5))+
  theme(legend.position = "bottom")

#Add a title
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type",nrow=1,label.position = "bottom",keywidth = 2.5))+
  theme(legend.position = "bottom")+
  ggtitle("More Colleges Are in the Southern United States Than Any Region")

#Add subtitle
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank())+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Number of Schools", limits=c(0, 500))+
  scale_fill_manual(values=c("Orange", "Blue"), guide=guide_legend(title="Institution Type",nrow=1,label.position = "bottom",keywidth = 2.5))+
  theme(legend.position = "bottom")+
  ggtitle("More Colleges Are in the Southern United States Than Any Region", subtitle = "Source: U.S. Dept. of Education")

#### Using Themes ####

#ggplot and other libraries includes some predefined themes.

#Create a basic bar plot
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) #Notice that there is a default theme!

#Change to black and white theme (white background with black/grey lines)
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme_bw()

#Minimalist theme - only light gridlines
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme_minimal()

#Void theme - almost everything, not even axes
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme_void()

#Dark theme - dark panel
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  theme_minimal()

#The "ggthemes" package includes some other themes
library(ggthemes)

#Solarized theme - yellowish backgrounds
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  ggthemes::theme_solarized()

#Excel theme - meant to replicate the look of Excel charts
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  ggthemes::theme_excel()

#WSJ theme - meant to replicate the look of charts published in the Wall Street Journal
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  ggthemes::theme_wsj()

#Economist theme - meant to replicate the look of charts published in the Economist
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  ggthemes::theme_economist()

#538 theme - meant to replicate the look of charts published on fivethirtyeight.com
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))+
  ggthemes::theme_fivethirtyeight()
