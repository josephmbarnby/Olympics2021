#Loads libraries
setwd('/Users/josephbarnby/My Drive/Dropbox/Olympics2021')


library(tidyverse)
library(ggExtra)
library(ggrepel)
library(ggpubr)
library(ggforce)
libload <- c("tidyverse", "colorspace", "corrr",  "cowplot",
                   "ggdark", "ggforce", "ggrepel", "ggridges", "ggsci",
                   "ggtext", "ggthemes", "grid", "gridExtra", "patchwork",
                   "rcartocolor", "scico", "showtext", "shiny",
                   "plotly", "highcharter", "echarts4r")
lapply(libload, library, character.only = TRUE)
library(png)
library(grid)
library(extrafont)
library(ggflags)
library(ggtext)


# Clean data function -----------------------------------------------------

# manipulate
cleanOlympic <- function(X){
  long_data <- X %>% # turn regular data into long data
    plyr::join(Abrv, by = 'Country') %>%
    mutate(Name = ifelse(Name == 'Daiz', 'Daisy',
                         ifelse(Name == 'G', 'Georgie', Name))) %>%
    group_by(Name, Year, Medal) %>%
    mutate(MedSumPP = sum(Count)) %>%
    group_by(Country, Year) %>%
    mutate(MedSumPC = sum(Count)) %>%
    group_by(Name) %>%
    distinct()
  long_data

  wide_data <- long_data %>% # turn long data into wide data for summaries
    pivot_wider(id_cols = c(Name, Country, Abrv, Abrv2, Medal), names_from = 'Year', values_from = 'Count') %>%
    mutate(RawNoChange = `2021`-`2016`,
           PercChange  = (RawNoChange/`2016`)*100) %>%
    group_by(Name, Country) %>%
    mutate(TotalMed2016 = sum(`2016`),
           TotalMed2021 = sum(`2021`),
           RawNoChangeTotal = TotalMed2021-TotalMed2016,
           PercChangeTotal = (RawNoChangeTotal/TotalMed2016)*100) %>%
    group_by(Name) %>%
    mutate(TotalMedal2016 = sum(`2016`),
           TotalMedal2021 = sum(`2021`),
           TotalRawNoTotal = TotalMedal2021-TotalMedal2016,
           TotalPercTotal = (TotalRawNoTotal/TotalMedal2016)*100,
           Highest = ifelse(PercChangeTotal == max(PercChangeTotal), 'yes', 'no'))
  return(wide_data)
}

# load data
#content <- read_html("https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/medal-standings.htm")
#tables <- content %>% html_table(fill = TRUE)
#tables <- tables[[1]]
#tables <- tables %>%
#  rename(Country = 2, G = 3, S = 4, B = 5) %>%
#  mutate(Country = ifelse(Country == 'United States of America', 'United States',
#                          ifelse(Country == "People's Republic of China", 'China',
#                                 ifelse(Country == 'ROC', 'Russia',
#                                        ifelse(Country == 'Republic of Korea', 'South Korea', Country)))),
#         Year = 2021) %>%
#  add_row(Country = 'Jamaica',    G = 0, S = 0, B = 0, NOCCode = 'JAM') %>%
#  add_row(Country = 'Cuba',       G = 0, S = 0, B = 0, NOCCode = 'CUB') %>%
#  add_row(Country = 'Brazil',     G = 0, S = 0, B = 0, NOCCode = 'BRA') %>%
#  add_row(Country = 'Sweden',     G = 0, S = 0, B = 0, NOCCode = 'SWE') %>%
#  add_row(Country = 'Kenya',      G = 0, S = 0, B = 0, NOCCode = 'KEN') %>%
#  pivot_longer(3:5, 'Medal', values_to = 'Count') %>%
#  dplyr::select(2, 7, 8, 6)
#


# Load data ---------------------------------------------------------------

Odata <- read_csv('Sweepstake Logic Example - Sheet1.csv')
#tables <- tables %>%
#  plyr::join(Odata %>% dplyr::select(Name, Country), by = 'Country') %>%
#  na.omit() %>%
#  distinct() %>%
#  dplyr::select(Name, 1,2,3,4)

#Odata <- Odata %>% filter(Year == '2016') %>% rbind(tables)
Abrv   <- data.frame(Abrv = wide_data$Abrv,
                     Country = wide_data$Country,
                     Abrv2 = wide_data$Abrv2)
#Summaries
wide_data <- cleanOlympic(Odata)
wide_data %>%
  summarise(totalPer = mean(TotalPercTotal))

# Total % Change in Medals Per Country ------------------------------------
require(magick)

logo <- image_read("hpolympics.png", 'none')
gold <- image_read("GoldOlympics.png", 'none')
silv <- image_read("SilverOlympics.png", 'none')
bron <- image_read("BronzeOlympics.png", 'none')
#g <- rasterGrob(img, interpolate=TRUE)

x <- wide_data %>%
  summarise(totalPer = mean(TotalPercTotal)) %>%
  arrange(-totalPer) %>%
  slice(1:3)

y <- ggplot(wide_data %>% filter(Medal == 'G'),
            aes(y=reorder(Abrv, PercChangeTotal))) +
  #background_image(logo)+
  geom_segment( aes(yend=reorder(Abrv, PercChangeTotal), x=0, xend=PercChangeTotal), color="grey")+
  geom_vline(xintercept = 0, color = '#0085C7')+
  geom_point( aes(x=PercChangeTotal), color= '#DF0024', size=5 ) +
  geom_point(data = wide_data %>% filter(PercChangeTotal == max(PercChangeTotal), PercChangeTotal != -100),
             aes(x=PercChangeTotal), color= '#F4C300', size=5 ) +
  geom_flag(aes(x=-145, country = Abrv2)) +
  facet_wrap(~reorder(Name, -TotalPercTotal), scales = 'free_y', nrow = 2)+
  theme_void(base_family = "Poppins")+
  coord_cartesian(xlim = c(-150,150), clip = 'off')+
  scale_x_continuous(breaks = c(-100, -50, 0, 50, 100), labels = c(-100, "", 0, "", 100))+
  geom_text_repel(aes(x = PercChangeTotal, y = Abrv,
                       label = ifelse(PercChangeTotal == max(PercChangeTotal), 'Best relative
performance', NA)),
                   size = 3, arrow = arrow(type = 'closed', length = unit(0.1, 'cm')),
                   box.padding = unit(0.35, "lines"), point.padding = unit(0.5, "lines"),
                   segment.color = 'black', family = 'Poppins', max.overlaps = Inf,
                   nudge_x = 400,
                   nudge_y = -0.5,
                   )+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_textbox_highlight(
      size = 12,
      fill = "white", box.color = "black", color = "black",
      halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(2, "npc"),
      padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
      #hi.labels = x[1], hi.fill = "#F4C300",
      hi.box.col = "black"
    ),
    plot.caption = element_markdown(face = 'italic', margin = margin(20, 5, 5, 5)),
    plot.title = element_markdown(face = 'bold'),
    plot.subtitle = element_text(margin = margin(10, 10, 20, 10)),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
  )+
  labs(title = "Percentage change in total medals won between <br> the <span style='color:#0085C7'>2016</span> and <span style='color:#DF0024'>2021</span> Olympics",
       subtitle = 'Data from Olympic Committee 2021',
       caption = "<span style='color:#0085C7'>(C) J.M.Barnby, 2021</span>")+
  xlab("Total Number of Medals") +
  ylab("")
#009F3D olympic green
y
grid.raster(logo, x = 0.89, 0.89, width = unit(2.7, 'cm'), height = unit(2.8, 'cm'))

grid.raster(gold, x = 0.235, 0.757, width = unit(0.6, 'cm'), height = unit(0.6, 'cm'))
grid.raster(silv, x = 0.48, 0.757, width = unit(0.5, 'cm'), height = unit(0.5, 'cm'))
grid.raster(bron, x = 0.725,  0.757, width = unit(0.5, 'cm'), height = unit(0.5, 'cm'))

# Total Percentages of Bronze Medals --------------------------------------

x2 <- wide_data %>%
  group_by(Name) %>%
  filter(Medal == 'B') %>%
  mutate(
    totBronze = sum(`2021`),
    totalPer = (totBronze/TotalMedal2021)*100,
    totalPer = ifelse(is.na(totalPer), 0, totalPer)) %>%
  ungroup() %>%
  dplyr::select(Name, totalPer) %>%
  distinct()

y2 <- ggplot(wide_data %>%
               filter(Medal == 'B') %>%
               plyr::join(x2, by = 'Name') %>%
               mutate(xend = (`2021`/TotalMed2021)*100,
                      xend = ifelse(is.na(xend), 0, xend))) +
  geom_segment(aes(y=Abrv, yend=reorder(Abrv, xend), x=0, xend=xend), color="grey")+
  geom_vline(xintercept = 0, color = '#0085C7')+
  geom_point(aes(x=xend, y=reorder(Abrv, xend)), color='#6A3805', size=5 ) +
  geom_flag(aes(x=xend, y=reorder(Abrv, xend), country = Abrv2)) +
  facet_wrap(~reorder(Name, -xend), scales = 'free_y', nrow = 2)+
  theme_void(base_family = "Poppins")+
  coord_cartesian(xlim = c(0,100), clip = 'off')+
  #geom_label(x = 90, y = 3,
  #           aes(label = round(totalPer,1)))+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_textbox_highlight(
      size = 12,
      fill = "white", box.color = "black", color = "black",
      halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(2, "npc"),
      padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
      #hi.labels = x2 %>% filter(totalPer == max(totalPer)) %>% slice(1),
      #hi.fill = "#F4C300",
      hi.box.col = "black"
    ),
    plot.caption = element_markdown(face = 'italic', margin = margin(20, 1, 1, 1)),
    plot.title = element_markdown(face = 'bold'),
    plot.subtitle = element_text(margin = margin(10, 10, 20, 10)),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
  )+
  labs(title = "Percentages of total<span style='color:#6A3805'>bronze</span>medals in the <span style='color:#DF0024'>2021</span> Olympics",
       subtitle = 'Data from Olympic Committee 2021',
       caption = "<span style='color:#0085C7'>(C) J.M.Barnby, 2021</span>")+
  xlab("Total Number of Medals") +
  ylab("")
#009F3D olympic green
y2
grid.raster(logo, x = 0.9, 0.9, width = unit(2, 'cm'), height = unit(2.1, 'cm'))

grid.raster(gold, x = 0.46,  0.455, width = unit(0.6, 'cm'), height = unit(0.6, 'cm'))
grid.raster(silv, x = 0.23, 0.79, width = unit(0.5, 'cm'), height = unit(0.5, 'cm'))
grid.raster(bron, x = 0.23,  0.455, width = unit(0.6, 'cm'), height = unit(0.6, 'cm'))

# Total types of medals won -----------------------------------------------

# Set a number of 'empty bar' to add at the end of each group
long_data_filt <- long_data %>% filter(Year == '2021')

empty_bar <- 2
nObsType <- nlevels(as.factor(long_data_filt$Medal))
to_add <- data.frame(matrix(NA, empty_bar*nlevels(as.factor(long_data_filt$Name))*nObsType, ncol(long_data_filt)) )
colnames(to_add) <- colnames(long_data_filt)
to_add$Name <- rep(levels(long_data_filt$Name), each=empty_bar*nObsType )
#long_data_filt <- rbind(long_data_filt, to_add)
long_data_filt <- long_data_filt %>% arrange(Name, Abrv)
long_data_filt$id <- rep( seq(1, nrow(long_data_filt)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- long_data_filt %>% group_by(id, Abrv) %>% summarize(tot=sum(Count))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- long_data_filt %>%
  group_by(Name) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
long_data_filt$Medal <- factor(long_data_filt$Medal, levels = c('B', 'S', 'G'))

# Make the plot
z <-ggplot(long_data_filt) +

  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=Count, fill=fct_rev(Medal)), stat="identity", alpha=0.75) +
  #scale_fill_viridis(discrete=TRUE) +

  # Add text showing the value of each 100/75/50/25 lines
  #ggplot2::annotate("text", x = rep(max(long_data_filt$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +

  ylim(-150,max(label_data$tot+50, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar(clip = 'off') +
  scale_fill_manual(values = c('#C9B037','#B4B4B4','#6A3805'), guide = 'none')+

  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=Abrv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  #geom_text(data=label_data, aes(x=id, y=tot+20, label=Abrv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+
  #geom_text(data=base_data, aes(x = title, y = -18, label=Name),colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)+
  theme_void(base_family = "Poppins")
z
z +
  inset_element(p = g,# Combine plot & image
                left = 0.3,
                bottom = 0.4,
                right = 0.7,
                top = 0.6, on_top = F)

# Change in medals per week -----------------------------------------------
wide_data2 <-read.csv('/Users/josephbarnby/My Drive/Dropbox/Olympics2021/Sweepstake Logic Example - Sheet1 - 27:07:21.csv')
wide_data2 <- cleanOlympic(wide_data2)
wide_data2$day= Sys.Date()-2
wide_data3 <-read.csv('/Users/josephbarnby/My Drive/Dropbox/Olympics2021/Sweepstake Logic Example - Sheet1 - 28:07:21.csv')
wide_data3 <- cleanOlympic(wide_data3)
wide_data3$day= Sys.Date()-1

wide_data$day = Sys.Date()

wide_data_long <- rbind(wide_data, wide_data2, wide_data3)

wide_data_plot <- wide_data_long %>%
  group_by(Name, day) %>%
  mutate(Day = as.Date(day),
         meanTotMed2016 = mean(TotalMed2016),
         meanTotMed2021 = mean(TotalMed2021),
         diffTotMed     = meanTotMed2021-meanTotMed2016,
         perDiffTotMed  = (diffTotMed/meanTotMed2016)*100)

ggplot(wide_data_plot,
       aes(Day, perDiffTotMed, color= Name))+
  geom_text_repel(data = wide_data_plot %>%
                    filter(day == Sys.Date()),
    nudge_x = 5,
    direction = "y",
    hjust = "right",
    aes(label = Name),
    max.overlaps = 0
  ) +
  labs(title = "Percentage change in total medals won between <br> the <span style='color:#0085C7'>2016</span> and <span style='color:#DF0024'>2021</span> Olympics",
       subtitle = 'Data from Olympic Committee 2021',
       caption = "<span style='color:#0085C7'>(C) J.M.Barnby, 2021</span>",
       x = 'Date')+
  coord_cartesian(clip = 'off')+
  geom_line(size = 1, aes(color = ifelse(perDiffTotMed != max(perDiffTotMed), NA, Name), group = Name))+
  geom_point(data = wide_data_plot %>% filter(day == Sys.Date()),
             aes(color = ifelse(day != Sys.Date(), NA, '#DF0024')), size = 2)+
  theme(legend.position = 'none',
        plot.title = element_markdown(),
        plot.caption = element_markdown(),
        axis.title.y = element_blank(),
        plot.margin = margin(0,2,0,0, unit = 'cm'),
        panel.background = element_blank())

ggplot(wide_data_plot %>% ungroup(),
       aes(Day, PercChangeTotal))+
  geom_text_repel(data = wide_data_plot %>%
                    filter(day == Sys.Date()),
                  nudge_x = 10,
                  direction = "x",
                  aes(label = Country),
                  max.overlaps = 0
  ) +
  labs(title = "Percentage change in total medals won between <br> the <span style='color:#0085C7'>2016</span> and <span style='color:#DF0024'>2021</span> Olympics",
       subtitle = 'Data from Olympic Committee 2021',
       caption = "<span style='color:#0085C7'>(C) J.M.Barnby, 2021</span>",
       x = 'Date')+
  coord_cartesian(clip = 'off')+
  geom_line(size = 1, aes(group = Country), color = ifelse(wide_data_plot$PercChangeTotal == max(wide_data_plot['day' == Sys.Date()]$PercChangeTotal), 'gold', 'grey'))+
  geom_point(data = wide_data_plot %>% filter(day == Sys.Date()),
             aes(color = ifelse(day != Sys.Date(), NA, '#DF0024')), size = 2)+
  theme(legend.position = 'none',
        plot.title = element_markdown(size = 16),
        plot.caption = element_markdown(),
        axis.title.y = element_blank(),
        plot.margin = margin(0,2,0,0, unit = 'cm'),
        panel.background = element_blank(),
        axis.text = element_text(size = 14))

# For Homepage ------------------------------------------------------------

home <- ggplot(wide_data %>% filter(Medal == 'G')) +
  geom_segment( aes(y=reorder(Abrv, PercChangeTotal), yend=Abrv, x=0, xend=PercChangeTotal), color="dark grey", size = 1)+
  geom_vline(xintercept = 0, color = '#0085C7', size = 2)+
  geom_point( aes(x=PercChangeTotal, y=Abrv), color='#DF0024', size=5 ) +
  #geom_point( aes(x=TotalMed2016, y=Abrv), color='#0085C7', size=5 ) +
  geom_flag(aes(x=-106, y=Abrv, country = Abrv2)) +
  #facet_wrap(~Name, scales = 'free_y', nrow = 2)+
  theme_void(base_family = "Poppins")+
  coord_cartesian(xlim = c(-100,100), clip = 'off')+
  scale_x_continuous(breaks = c(-100, -50, 0, 50, 100),
                     labels = c(-100, -50, 0, 50, 100))+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),
    strip.background = element_blank(),
    plot.caption = element_markdown(face = 'italic', margin = margin(20, 1, 1, 10)),
    plot.title = element_markdown(face = 'bold'),
    plot.subtitle = element_text(margin = margin(10, 10, 20, 10)),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    #panel.grid.major.y = element_line(size = 0.1),
    panel.grid.major.y = element_blank(),
  )+
  labs(title = "Percentage change in total medals won between <br> the <span style='color:#0085C7'>2016</span> and <span style='color:#DF0024'>2020</span> Olympics",
       subtitle = 'Data from Olympic Committee 2021',
       caption = "<span style='color:#0085C7'>(C) J.M.Barnby [2021] website : joebarnby.com</span>")+
  xlab("Total Number of Medals") +
  ylab("")
#009F3D olympic green
home
home+
  inset_element(p = g,# Combine plot & image
                left = 0.6,
                bottom = 0.5,
                right = 1,
                top = 1.1)

