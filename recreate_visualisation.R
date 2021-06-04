#load packages
library(tidyverse)
library(patchwork)
library(extrafont)
library(ggpattern)
library(cowplot)

dev.new(width=13,height=8,unit="cm", noRStudioGD = TRUE)

#load data
cotton <- tibble(read.csv("CottonViz-data.csv"))

#recreate original graph
long_data_1 <- pivot_longer(cotton, cols=2:4, values_to = "bales", names_to = "type")
long_data_1$type <- factor(long_data_1$type, levels=rev(c("US.consumption", "Exports", "Stocks")))

#line chart
p1 <- ggplot() +
  geom_line(data=long_data_1, mapping=aes(x=Year, y=bales, linetype=type), size=2) +
  geom_text(data=data.frame(x=1943.5, y=3000, label="Exports"), mapping=aes(x=x, y=y, label=label), family="Ebrima", hjust=0.5, size=5) +
  geom_text(data=data.frame(x=1946.5, y=11000, label="U. S. Consumption"), mapping=aes(x=x, y=y, label=label), family="Ebrima", hjust=0.5, size=5) +
  geom_text(data=data.frame(x=1946.2, y=7000, label="Carry - over\nStocks"), mapping=aes(x=x, y=y, label=label), family="Ebrima", hjust=0.5, size=5) +
  geom_segment(aes(x = 1944.0, y = 3000, xend = 1944.3, yend = 2700), arrow=arrow(length = unit(0.08, "inches"), type="closed")) +
  geom_segment(aes(x = 1945.5, y = 10900, xend = 1945.3, yend = 10000), arrow=arrow(length = unit(0.08, "inches"), type="closed")) +
  geom_segment(aes(x = 1945.5, y = 7400, xend = 1945.3, yend = 7000), arrow=arrow(length = unit(0.08, "inches"), type="closed")) +
  scale_linetype_manual("", values=c("US.consumption"="solid", "Exports"="longdash", "Stocks"="dashed")) +
  labs(subtitle="Millions of Bales") +
  coord_cartesian(expand=F) +
  scale_y_continuous("", breaks=seq(0,12000, 2000), labels=seq(0,12,2), limits=c(0,12000), sec.axis = dup_axis()) +
  scale_x_continuous("", breaks=1942:1948, labels=c("1942", "\'43", "\'44", "\'45", "\'46", "\'47", "\'48")) +
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.border = element_rect(fill="transparent",colour="black"),
        legend.background = element_rect(fill = "white"),
        plot.subtitle = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
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

p2 <- ggplot() +
  geom_col_pattern(data=long_data_1, mapping=aes(x=Year, y=bales, pattern=type, pattern_spacing=type, pattern_density=type), width = 0.7, fill="white", colour="black", pattern_fill='black') +
  scale_pattern_manual("", values=c("US.consumption"="crosshatch", "Exports"="stripe", "Stocks"="crosshatch")) +
  scale_pattern_density_manual("", values=c("US.consumption"=0.4, "Exports"=0.4, "Stocks"=0.1)) +
  scale_pattern_spacing_manual("", values=c("US.consumption"=0.01, "Exports"=0.01, "Stocks"=0.015)) +
  labs(subtitle="Millions of Bales") +
  geom_label(data=data.frame(x=1945, y=7000, label="U.S. CONSUMPTION"), mapping=aes(x=x, y=y, label=label), family="Ebrima", hjust=0.5, size=5, label.size = NA) +
  geom_label(data=data.frame(x=1945, y=11000, label="EXPORTS"), mapping=aes(x=x, y=y, label=label), family="Ebrima", hjust=0.5, size=5, label.size = NA) +
  geom_label(data=data.frame(x=1945, y=15000, label="STOCKS*"), mapping=aes(x=x, y=y, label=label), family="Ebrima", hjust=0.5, size=5, label.size = NA) +
  scale_x_continuous("*END OF SEASON, JULY 31", breaks=1942:1948, labels=c("1942", "\'43", "\'44", "\'45", "\'46", "\'47", "\'48"), limits=c(1941.5, 1948.5)) +
  scale_y_continuous("", breaks=seq(0,25000, 5000), labels=seq(0,25,5), limits=c(0,25000), sec.axis = dup_axis()) +
  coord_cartesian(expand=F) +
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.border = element_rect(fill="transparent",colour="black"),
        legend.background = element_rect(fill = "white"),
        plot.subtitle = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"),
        legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        axis.title= element_text(colour = "black", size=12, family="Ebrima", hjust=0.9),
        axis.text.y=element_text(colour = "black", size=12, hjust = 1, family="Ebrima", margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.x=element_text(colour = "black", size=12, hjust = 1, family="Ebrima"),
        axis.text.y.right = element_blank(),
        axis.line.y = element_line(),
        axis.line.y.right = element_line(),
        axis.line.x = element_line(),
        axis.line.x.top = element_line(),
        axis.ticks.length.y.left=unit(-0.25, "cm"),
        axis.ticks.length.y.right=unit(-0.25, "cm"),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2

p <- p1 + p2 + plot_layout(ncol = 2) +
  plot_annotation( 
    caption = 'Source: U. S. Department of Agriculture                                                                                                                                                                     U. S. Supply of U. S. Cotton',
    title = '\n\nDistribution of United States Cotton') &
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "black", size=26, face="bold", hjust = 0.5, family="Ebrima"),
        plot.caption = element_text(colour = "black", size=12, hjust = 0, family="Ebrima"))
p


q <- plot_grid(p) + 
  draw_label("MULTIPLE CURVE", x = 0.05, y = 0.97, hjust = 0, vjust = 0.5, color = "black", size = 17, fontface="italic", fontfamily="Ebrima") +
  draw_label("COMPONENT COLUMM", x = 0.55, y = 0.97, hjust = 0, vjust = 0.5, color = "black", size = 17, fontface="italic", fontfamily="Ebrima")
q



