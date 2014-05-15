#+ license, echo=FALSE
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("gdata")
require("data.table")
require("dplyr")
require("ggplot2")
require("cshapes")
require("gridExtra")

#+ prepare.data, echo=FALSE
IE.data <- as.data.table(read.xls("http://www.census.gov/foreign-trade/balance/country.xls")) %.%
  filter(CTY_CODE >= 1000)

#+ graph.2012.map
tmp <- IE.data %.%
  filter(year == 2012)
setnames(tmp, "CTYNAME", "id")
lev <- levels(tmp$id)
lev[lev == "Korea, South"] <- "South Korea"
lev[lev == "Korea, North"] <- "North Korea"
lev[lev == "Congo (Kinshasa)"] <- "Congo, DRC"
lev[lev == "Congo (Brazzaville)"] <- "Congo"
lev[lev == "Republic of Yemen"] <- "Yemen"
lev[lev == "Ivory Coast"] <- "Cote d'Ivoire"
lev[lev == "Burma"] <- "Myanmar"
lev[lev == "Gambia"] <- "The Gambia"
lev[lev == "St Lucia"] <- "St. Lucia"
lev[lev == "Marshall Islands"] <- "Marshall Is."
lev[lev == "St Kitts and Nevis"] <- "St. Kitts and Nevis"
lev[lev == "Solomon Islands"] <- "Solomon Is."
lev[lev == "St Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
lev[lev == "East Timor"] <- "Timor Leste"
levels(tmp$id) <- lev

gpclibPermit()
world.map <- cshp(date = as.Date("2012-1-1")) %.%
  fortify(region = 'CNTRY_NAME') %.%
  as.data.table() %.%
  merge(tmp, by = "id", all = TRUE)

graph2012 <- ggplot(data = world.map, 
                    aes(x = long, y = lat, group = group)) +
  theme_minimal(base_size = 24) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines"),
        legend.position = "bottom",
        legend.key.width = unit(4.5, "cm"),
        legend.title = element_text(size = 24)) +
  coord_fixed() +
  xlab("  <- US import more from | US export more toward ->") +
  scale_fill_gradient2(low = "red3", mid = "white", high = "dodgerblue4",
                       breaks = -5:5, name = "\nUS Export-Import balance (in millions of $)",
                       labels = expression(-10^5, -10^4, -10^3, -10^2, -10^1, 
                                           10^0, 10^1, 10^2, 10^3, 10^4, 10^5),
                       limits = c(-6, 6)) +
  guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, title.margin = 5)) +
  geom_polygon(aes(fill = log10(abs(EYR - IYR)) * sign(EYR - IYR))) + 
  geom_path(colour = 'gray', linestyle = 2) + 
  annotate("text", x = -150, y = -25, label = "2012", size = 20)


#+ graph.1992.map
tmp <- IE.data %.%
  filter(year == 1992)
setnames(tmp, "CTYNAME", "id")
lev <- levels(tmp$id)
lev[lev == "Korea, South"] <- "South Korea"
lev[lev == "Korea, North"] <- "North Korea"
lev[lev == "Congo (Kinshasa)"] <- "Congo, DRC"
lev[lev == "Congo (Brazzaville)"] <- "Congo"
lev[lev == "Republic of Yemen"] <- "Yemen"
lev[lev == "Ivory Coast"] <- "Cote d'Ivoire"
lev[lev == "Burma"] <- "Myanmar"
lev[lev == "Gambia"] <- "The Gambia"
lev[lev == "St Lucia"] <- "St. Lucia"
lev[lev == "Marshall Islands"] <- "Marshall Is."
lev[lev == "St Kitts and Nevis"] <- "St. Kitts and Nevis"
lev[lev == "Solomon Islands"] <- "Solomon Is."
lev[lev == "St Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
lev[lev == "East Timor"] <- "Timor Leste"
lev[lev == "Yugoslavia (former)"] <- "Yugoslavia"
levels(tmp$id) <- lev

gpclibPermit()
world.map <- cshp(date = as.Date("1992-1-1")) %.%
  fortify(region = 'CNTRY_NAME') %.%
  as.data.table() %.%
  merge(tmp, by = "id", all = TRUE)

graph1992 <- ggplot(data = world.map, 
                    aes(x = long, y = lat, group = group)) +
  theme_minimal(base_size = 24) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines"),
        legend.position = "bottom",
        legend.key.width = unit(4.5, "cm"),
        legend.title = element_text(size = 24)) +
  coord_fixed() +
  #xlab("  <- US import more from | US export more toward ->") +
  scale_fill_gradient2(low = "red3", mid = "white", high = "dodgerblue4",
                       breaks = -5:5, name = "\nUS Export-Import balance (in millions of $)",
                       labels = expression(-10^5, -10^4, -10^3, -10^2, -10^1, 
                                           10^0, 10^1, 10^2, 10^3, 10^4, 10^5),
                       limits = c(-6, 6)) +
  #guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, title.margin = 5)) +
  guides(fill = FALSE) +
  geom_polygon(aes(fill = log10(abs(EYR - IYR)) * sign(EYR - IYR))) + 
  geom_path(colour = 'gray', linestyle = 2) + 
  annotate("text", x = -150, y = -25, label = "1992", size = 20)


#+ plot.combined.map
png("Combined_US_IE_balance.png", width = 1200, height = 1200, bg = "#F0F0F0")
grid.arrange(graph1992, graph2012, banner, heights = c(.9, 1, .05))
dev.off()









