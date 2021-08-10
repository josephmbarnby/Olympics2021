#Loads libraries
library(tidyverse)
library(ggExtra)
library(ggrepel)
library(ggpubr)
library(ggforce)
library(png)
library(grid)
library(extrafont)
library(ggflags)
library(ggtext)


# Clean data function -----------------------------------------------------

# manipulate
cleanOlympic_long <- function(X){
  long_data <- X %>% # turn regular data into long data
    plyr::join(Abrv, by = 'Country') %>%
    group_by(Year, Medal) %>%
    mutate(MedSumPP = sum(Count)) %>%
    group_by(Country, Year) %>%
    mutate(MedSumPC = sum(Count)) %>%
    distinct()
  long_data
  return(long_data)
}

cleanOlympic_wide <- function(X){

  wide_data <- X %>% # turn long data into wide data for summaries
    pivot_wider(id_cols = c(Country, Abrv, Abrv2, Medal), names_from = 'Year', values_from = 'Count') %>%
    mutate(RawNoChange = `2021`-`2016`,
           PercChange  = (RawNoChange/`2016`)*100) %>%
    group_by(Country) %>%
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

# Scrape data from IOC website --------------------------------------------
scrapeOlympic <- function(){

content <- read_html("https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/medal-standings.htm")
tables <- content %>% html_table(fill = TRUE)
tables <- tables[[1]]
tables <- tables %>%
  rename(Country = 2, G = 3, S = 4, B = 5) %>%
  mutate(Country = ifelse(Country == 'United States of America', 'United States',
                          ifelse(Country == "People's Republic of China", 'China',
                                 ifelse(Country == 'ROC', 'Russia',
                                        ifelse(Country == 'Republic of Korea', 'South Korea', Country)))),
         Year = 2021) %>%
  pivot_longer(3:5, 'Medal', values_to = 'Count') %>%
  dplyr::select(2, 7, 8, 6)

# Load data ---------------------------------------------------------------

Odata <- read_csv('template.csv')
tables <- tables %>%
  plyr::join(Odata %>% dplyr::select(Country), by = 'Country') %>%
  na.omit() %>%
  distinct() %>%
  dplyr::select(1,2,3,4)

Odata <- Odata %>%
  filter(Year == '2016') %>%
  dplyr::select(Country, Medal, Count, Year) %>%
  rbind(tables)
Abrv   <- data.frame(Abrv = wide_data$Abrv,
                     Country = wide_data$Country,
                     Abrv2 = wide_data$Abrv2)
#Summaries
long_data <- cleanOlympic_long(Odata)
wide_data <- cleanOlympic_wide(long_data)
wide_data %>%
  summarise(totalPer = mean(TotalPercTotal))

return(wide_data)
}

wide_data <- scrapeOlympic()

# Total % Change in Medals Per Country ------------------------------------
require(magick)
font_add_google("Poppins")
showtext_auto()

logo <- image_read("hpolympics.png", 'none')
gold <- image_read("GoldOlympics.png", 'none')
silv <- image_read("SilverOlympics.png", 'none')
bron <- image_read("BronzeOlympics.png", 'none')
g    <- rasterGrob(logo, interpolate=TRUE)

x <- wide_data %>%
  summarise(totalPer = mean(TotalPercTotal)) %>%
  arrange(-totalPer) %>%
  slice(1:3)


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

# For Homepage ------------------------------------------------------------

tiff("test.tiff", units="in", width=14, height=8, res=300)
wide_data <- scrapeOlympic()
wide_data <- wide_data %>%
  mutate(`2016` = ifelse(Country == 'Japan' & Medal == 'G', 12,
                         ifelse(Country == 'Japan' & Medal == 'S', 8,
                                ifelse(Country == 'Japan' & Medal == 'B', 21, `2016`))),
         TotalMed2016 = ifelse(Country == 'Japan', 12 + 8 + 21, TotalMed2016),
         RawNoChangeTotal = ifelse(Country == 'Japan', TotalMed2021 - TotalMed2016, RawNoChangeTotal),
         PercChangeTotal = ifelse(Country == 'Japan', RawNoChangeTotal/TotalMed2016*100, PercChangeTotal))
wide_data <- wide_data %>% mutate(Country = ifelse(Country == 'Russia', 'ROC', Country),
                                  Abrv    = ifelse(Abrv == 'RUS', 'ROC', Abrv))
home <- ggplot(wide_data %>% filter(Medal == 'G')) +
  geom_segment( aes(y=reorder(Abrv, PercChangeTotal), yend=Abrv, x=0, xend=PercChangeTotal), color="dark grey", size = 1)+
  geom_vline(xintercept = 0, color = '#0085C7', size = 2)+
  geom_point( aes(x=PercChangeTotal, y=Abrv), color='#DF0024', size=5 ) +
  #geom_point( aes(x=TotalMed2016, y=Abrv), color='#0085C7', size=5 ) +
  geom_flag(aes(x=-106, y=Abrv, country = ifelse(Abrv != 'ROC', Abrv2, NA))) +
  geom_text(data = wide_data %>% filter(Medal == 'G', PercChangeTotal <= 0),
            aes(x = PercChangeTotal, y = Abrv, label = round(PercChangeTotal,1)),
            check_overlap = T, nudge_x = -5, size = 3)+
  geom_text(data = wide_data %>% filter(Medal == 'G', PercChangeTotal > 0),
            aes(x = PercChangeTotal, y = Abrv, label = round(PercChangeTotal,1)),
            check_overlap = T, nudge_x = 5, size = 3)+
  #facet_wrap(~Name, scales = 'free_y', nrow = 2)+
  theme_void(base_family = "Poppins")+
  coord_cartesian(xlim = c(-100,100), clip = 'off')+
  scale_x_continuous(breaks = c(-100, -50, 0, 50, 100),
                     labels = c(-100, -50, 0, 50, 100))+
  scale_y_discrete(expand = c(0,2))+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),
    strip.background = element_blank(),
    plot.caption = element_markdown(face = 'italic', margin = margin(20, 1, 1, 10)),
    plot.title = element_markdown(face = 'bold', size = 18),
    plot.subtitle = element_markdown(margin = margin(10,0,20,0), size = 14),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    #panel.grid.major.y = element_line(size = 0.1),
    panel.grid.major.y = element_blank()
  )+
  labs(title = "Despite the challenges of the past two years, Olympic teams have gone above and beyond",
       subtitle = "Percentage change in total medals won from the previous <br> top 25 teams between the <span style='color:#0085C7'>2016</span> and <span style='color:#DF0024'>2020</span> Olympics",
       caption = "Data from the International Olympic Committee <br> <span style='color:#0085C7'>(C) J.M.Barnby [2021] website: joebarnby.com | Twitter: @joebarnby </span>")+
  xlab("Total Number of Medals") +
  ylab("")
#009F3D olympic green
home
home+
  inset_element(p = g,# Combine plot & image
                left = 0.1,
                bottom = 0.4,
                right = 0.3,
                top = 0.9)
dev.off()


# World map  -------------------------------------------------
library(rvest)
library(wpp2019)

content <- read_html("https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/medal-standings.htm")
tables2 <- content %>% html_table(fill = TRUE)
tables2 <- tables2[[1]]
tables2 <- tables2 %>%
  rename(Country = 2, G = 3, S = 4, B = 5) %>%
  mutate(Country = ifelse(Country == 'United States of America', 'United States',
                          ifelse(Country == "People's Republic of China", 'China',
                                 ifelse(Country == 'ROC', 'Russia',
                                        ifelse(Country == 'Republic of Korea', 'South Korea', Country)))),
         Year = 2021) %>%
  pivot_longer(3:5, 'Medal', values_to = 'Count') %>%
  dplyr::select(2, 7, 8, 6)

content2016 <- read_html('https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table')
tables3 <- content2016 %>% html_table(fill = TRUE)
tables3 <- tables3[[3]]
tables3$NOC <- gsub("\\s*\\([^\\)]+\\)","",as.character(tables3$NOC))
cleanTable3 <- tables3 %>%
  rename(Country = NOC) %>%
  dplyr::select(Country, Total) %>%
  mutate(Country = case_when(Country == 'United States' ~ 'USA',
                             Country == 'Great Britain' ~ 'UK',TRUE ~ Country))

data('pop')
popEdit <- pop %>%
  dplyr::select(name, `2020`)

world <- map_data('world')
world %>%
  rename(Country = region) %>%
  plyr::join(tables2 %>%
               mutate(Country = case_when(Country == 'United States' ~ "USA",
                                          Country == 'Great Britain' ~ "UK", TRUE ~ Country)) %>%
               group_by(Country) %>%
               mutate(Tot = sum(Count)),
             type = 'left', by = 'Country') %>%
  plyr::join(popEdit %>%
               rename(Country = 1, Pop = 2) %>%
               mutate(Country = case_when(Country == 'United States of America' ~ 'USA',
                                          Country == 'United Kingdom' ~ 'UK',
                                          Country == 'Russian Federation' ~ 'Russia', TRUE ~ Country)),
             by = 'Country') %>%
  plyr::join(cleanTable3,
             by = 'Country') %>%
  distinct() %>%
  mutate(Tot = ifelse(is.na(Tot), 0, Tot)) %>%
  group_by(Country) %>%
  mutate(Perc = ((Tot-Total)/Total)*100,
         Perc = ifelse(is.na(Perc), 0, Perc),
         PerPop = Tot/(Pop),
         PerPop = ifelse(is.na(PerPop), 0, PerPop),
         logPop = log10(PerPop),
         logPop = ifelse(logPop == -Inf, -100, logPop)) -> worldPlot

filtered <- worldPlot %>%
  filter(PerPop != 0)

ggplot(worldPlot %>%
         filter(Country %in% filtered$Country,
                Year == 2021)) +
    geom_map(
           map = world,
            aes(long, lat, map_id = Country, fill = PerPop)
        )+
  scale_fill_gradient(low = 'white', high = '#009F3D', trans = 'log')+
  theme_map()+
  theme(legend.position = 'none',
        legend.title = element_blank())
