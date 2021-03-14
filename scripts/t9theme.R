# File Description --------------------------------------------------------
## This file sets up our visualization theme.


# Load Packages  ----------------------------------------------------------

library(extrafont)


# Import non-system fonts if you have not ---------------------------------

font_import()


# Put fonts into data frame to view new fonts  -----------------------------

fonts <- fonttable()



# Theme – to add to ggplot simply type + t9theme after plot ---------------

t9theme <- theme(text = element_text(family = "Times New Roman"), legend.title = element_blank(), panel.grid = element_line(size = .3, colour = "black"), 
                 panel.background = element_rect(fill = "white"), plot.subtitle = element_text(size = 10), plot.title = element_text(size = 16)
                 , axis.title = element_text(size = 10), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                 panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(linetype = "dotted"), axis.ticks = element_blank(), 
                 legend.key = element_blank())
