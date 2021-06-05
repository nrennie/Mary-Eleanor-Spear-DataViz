#load packages
library(tidyverse)
library(extrafont)

#load data
cotton <- tibble(read.csv("CottonViz-data.csv"))
long_data_1 <- pivot_longer(cotton, cols=2:4, values_to = "bales", names_to = "type")
long_data_1$type <- factor(long_data_1$type, levels=rev(c("US.consumption", "Exports", "Stocks")))

######################################################################################################################################
######################################################################################################################################

#plot 1 - stacked area chart and slope chart
dev.new(width=13,height=8,unit="cm", noRStudioGD = TRUE)
p1 <- ggplot() + 
  geom_area(data=long_data_1, aes(x=Year, y=bales, fill=type), alpha=0.4) +
  geom_text(data=data.frame(x=1945, y=7000, label="U.S. CONSUMPTION"), mapping=aes(x=x, y=y, label=label), colour="#994455", fontface="bold", family="Ebrima", hjust=0.5, size=5) +
  geom_text(data=data.frame(x=1945, y=11000, label="EXPORTS"), mapping=aes(x=x, y=y, label=label), colour="#997700", fontface="bold",  family="Ebrima", hjust=0.5, size=5) +
  geom_text(data=data.frame(x=1945, y=15000, label="STOCKS"), mapping=aes(x=x, y=y, label=label), colour="#004488", fontface="bold", family="Ebrima", hjust=0.5, size=5) +
  labs(subtitle="Millions of Bales") +
  coord_cartesian(expand = F) +
  scale_fill_manual("", values=c("US.consumption"="#EE99AA", "Exports"="#EECC66", "Stocks"="#6699CC")) +
  scale_y_continuous("", breaks=seq(0,25000, 5000), labels=seq(0,25,5), limits=c(0,25000), sec.axis = dup_axis()) +
  scale_x_continuous("", breaks=1942:1948, labels=c("1942", "\'43", "\'44", "\'45", "\'46", "\'47", "\'48"), limits=c(1942, 1948)) +
  theme(panel.background = element_rect(fill = "grey95", colour="grey95"),
        plot.background = element_rect(fill = "grey95", colour="grey95"),
        panel.border = element_rect(fill="transparent",colour="black"),
        legend.background = element_rect(fill = "white"),
        plot.subtitle = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        plot.title = element_text(colour = "black", size=26, face="bold", hjust = 0, family="Ebrima"),
        plot.caption = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "black", size=12, family="Ebrima"),
        axis.text.y=element_text(colour = "black", size=12, hjust = 1, family="Ebrima", margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.x=element_text(colour = "black", size=12, hjust = 1, family="Ebrima", margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y.right = element_blank(),
        axis.line.y = element_line(),
        axis.line.y.right = element_line(),
        axis.line.x = element_line(),
        axis.line.x.top = element_line(),
        axis.ticks.length.x.bottom=unit(-0.25, "cm"),
        axis.ticks.length.y.left=unit(-0.25, "cm"),
        axis.ticks.length.y.right=unit(-0.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1


lines_cons <- tibble(x=1942:1947+0.2, xend=1943:1948-0.2, y=cotton$US.consumption[1:6], yend=cotton$US.consumption[2:7])
lines_export <- tibble(x=1942:1947+0.2, xend=1943:1948-0.2, y=cotton$Exports[1:6], yend=cotton$Exports[2:7])
lines_stocks <- tibble(x=1942:1947+0.22, xend=1943:1948-0.22, y=cotton$Stocks[1:6], yend=cotton$Stocks[2:7])

p2 <- ggplot() +
  geom_text(data=cotton, mapping=aes(x=Year, y=US.consumption, label=round((US.consumption/1000),1)), colour="#994455", family="Ebrima") +
  geom_text(data=cotton, mapping=aes(x=Year, y=Exports, label=round((Exports/1000),1)), colour="#997700", family="Ebrima") +
  geom_text(data=cotton, mapping=aes(x=Year, y=Stocks, label=round((Stocks/1000),1)), colour="#004488", family="Ebrima") +
  geom_segment(data=lines_cons, aes(x=x, xend=xend, y=y, yend=yend), colour="#994455") +
  geom_segment(data=lines_export, aes(x=x, xend=xend, y=y, yend=yend), colour="#997700") +
  geom_segment(data=lines_stocks, aes(x=x, xend=xend, y=y, yend=yend), colour="#004488") +
  labs(subtitle="Millions of Bales") +
  scale_y_continuous("", breaks=c(1500, 10700, 11200), labels=c("Exports", "\nStocks", "U.S.\nConsumption"), limits=c(0,12000)) +
  scale_x_continuous("", breaks=1942:1948, labels=c("1942", "\'43", "\'44", "\'45", "\'46", "\'47", "\'48")) +
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.border = element_rect(fill="transparent",colour="black"),
        legend.background = element_rect(fill = "white"),
        plot.subtitle = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        plot.title = element_text(colour = "black", size=26, face="bold", hjust = 0, family="Ebrima"),
        plot.caption = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "black", size=12, family="Ebrima"),
        axis.text.y=element_text(colour = "black", size=12, hjust = 1, family="Ebrima", margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.x=element_text(colour = "black", size=12, hjust = 1, family="Ebrima", margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.line.x.top = element_line(),
        axis.ticks.length.x.bottom=unit(-0.25, "cm"),
        axis.ticks.length.y.left=unit(-0.25, "cm"),
        axis.ticks.length.y.right=unit(-0.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

p <- p1 + p2 + plot_layout(ncol = 2) +
  plot_annotation( 
    caption = 'N. Rennie | Source: U. S. Department of Agriculture',
    title = '\nDistribution of United States Cotton') &
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "black", size=26, face="bold", hjust = 0.5, family="Ebrima"),
        plot.caption = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"))
p


######################################################################################################################################
######################################################################################################################################

#plot 2 - waffle plot
round_250 <- function(x){
  vals <- seq(0, 25000, 250) 
  wm <- which.min(abs(vals - x))
  output <- vals[wm]
  return(output)
}
cotton$round_bales_us <- sapply(cotton$US.consumption, function(x) round_250(x))/250
cotton$round_bales_export <- sapply(cotton$Exports, function(x) round_250(x))/250
cotton$round_bales_stocks <- sapply(cotton$Stocks, function(x) round_250(x))/250
cotton$round_other <- 100 - rowSums(cotton[,6:8])
long_data_2 <- pivot_longer(cotton, cols=6:9, values_to = "bales", names_to = "type")
long_data_3 <- long_data_2[,c(1,6,7)]
long_data_4 <- tibble(box = unlist(apply(long_data_3,1, function(x) rep(x[2], x[3]))))
long_data_4$Year <- rep(1942:1948, rep(100,7))
long_data_4$x <- rep(rep(1:10,10),7)
long_data_4$y <- rep(rep(1:10,rep(10,10)),7)

dev.new(width=13,height=8,unit="cm", noRStudioGD = TRUE)
p <- ggplot(long_data_4, aes(x, y, width=.7, height=.5)) +
  geom_tile(aes(fill = box), colour = "grey85") +
  scale_y_reverse() +
  scale_fill_manual("", values=c("round_bales_us"="#33BBEE", "round_bales_export"="#EE3377", "round_bales_stocks"="#009988", "round_other"="grey85"), 
                    breaks=c("round_bales_us", "round_bales_export", "round_bales_stocks"), labels=c("U.S. Consumption", "Exports", "Stocks")) +
  facet_wrap(~Year) +
  guides(fill=guide_legend(ncol=4)) +
  labs(title = '\nDistribution of United States Cotton\n', subtitle="*Each box represents approximately 250,000 bales", caption = "N. Rennie | Source: U. S. Department of Agriculture") +
  theme(panel.background = element_rect(fill = "grey85", colour="grey85"),
        strip.background =element_rect(fill=alpha("#EE3377", 0.5)),
        strip.text = element_text(colour = 'black', family="Ebrima", size=14, face=2),
        plot.background = element_rect(fill = "grey85", colour="grey85"),
        legend.background = element_rect(fill = "grey85"),
        legend.key = element_rect(fill = "grey85", colour="grey85"), 
        legend.text =  element_text(colour = "black", size=12, family="Ebrima"),
        plot.title = element_text(colour = "black", size=22, face="bold", hjust = 0.8, vjust = -75, family="Ebrima"),
        plot.subtitle = element_text(colour = "black", size=12, hjust = 0.75, family="Ebrima", vjust = -130),
        plot.caption = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        legend.position=c(0.7, 0.13),
        plot.margin = unit(c(-2, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p





