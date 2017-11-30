library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(reshape2)
library(scales)
library(plyr)

########## Growth Forecast ##########
# loading data and clean data   
imf.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/imf.csv', 
                  header = T)
imf.df = imf.df[11:18,] # only take data form 2015 to 2021

oecd.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/oecd.csv', 
                   header = T)
oecd.df = oecd.df[11:18,] # only take data form 2015 to 2021

wb.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/wb.csv', 
                 header = T)
wb.df = wb.df[11:15,] # only take data form 2015 to 2021

nbs.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/nbs.csv', 
                  header = T)
nbs.df = nbs.df[11:18,] # only take data form 2015 to 2021

growth.df = rbind(imf.df, oecd.df, wb.df, nbs.df)

Institutions = c(rep("IMF", times = nrow(imf.df)),
                 rep("OECD", times = nrow(oecd.df)),
                 rep("WB", times = nrow(wb.df)),
                 rep("NBS", times = nrow(nbs.df)))
Data = c(rep("0.3", times = 2),
         rep("1", times = 6),
          
         rep("0.3", times = 2),
         rep("1", times = 6),
         
         rep("0.3", times= 2),
         rep("1", times = 3),
         
         rep("0.3", times=2),
         rep("1", times = 6))

growth.df = cbind(growth.df, Institutions = as.factor(Institutions),
                  Data = as.factor(Data))


gdp_growth_forecast.plot = 
ggplot(growth.df, aes(x = year, y = growth, 
                      color = Institutions, 
                      shape = Institutions, 
                      alpha = Data, 
                      group = Institutions)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320", "#a32020")) + 
  # blue = 00BFC4, red = #F8766D, green = #7CAE00, purple = #C77CFF
  scale_alpha_identity() + 
  labs(title = "GDP Growth Forecast", 
       x = "Year", y = "Growth (%)") +
  scale_x_continuous(breaks = seq(2006, 2021, by = 1)) +
  scale_y_continuous(breaks = seq(4, 8, by = 0.2)) +
  geom_rect(aes(xmin = -Inf, xmax = 2016.5, ymin = -Inf, ymax = Inf),
            color = "white", fill = "black", alpha = 0.002) +
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_blank())

grid.newpage()
gdp_growth_forecast.plot =
arrangeGrob(gdp_growth_forecast.plot, 
            bottom = textGrob("Source: IMF, NBS of China, OECD, World Bank", 
                              x = -0.23, y =1.5,
                              hjust = -0.5,
                              gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(gdp_growth_forecast.plot)
#####################################
########## Growth Forecast 2015-2018 ########## 
imf.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/imf.csv', 
                  header = T)
oecd.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/oecd.csv', 
                   header = T)
wb.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/wb.csv', 
                 header = T)
nbs.df = read.csv('~/Desktop/pwc/gdp/growth_forecast/processed/nbs.csv', 
                  header = T)

imf.df = imf.df[11:15,]
oecd.df = oecd.df[11:15,]
wb.df = wb.df[11:15,]
nbs.df = nbs.df[11:15,]

growth.df = rbind(imf.df, oecd.df, wb.df, nbs.df)

Institutions = c(rep("IMF", times = nrow(imf.df)),
                 rep("OECD", times = nrow(oecd.df)),
                 rep("WB", times = nrow(wb.df)),
                 rep("NBS", times = nrow(nbs.df)))

growth.df = cbind(growth.df, Institutions = as.factor(Institutions))

ggplot(growth.df, aes(x = year, y = growth, color = Institutions, 
                      shape = Institutions, 
                      group = Institutions)) +
  geom_line(size = 1.5) + 
  geom_point(size = 4) +
  scale_color_manual(values = c("#602320", "#e0301e", "#dc6900", "#a32020")) + 
  labs(title = "China GDP Growth Forecast", 
       x = "Year", y = "Growth (%)") +
  scale_x_continuous(breaks = seq(2005, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(5, 8, by = 0.2)) +
  geom_rect(aes(xmin = -Inf, xmax = 2016.5, ymin = -Inf, ymax = Inf), 
            color = "white", fill = "black", alpha = 0.002) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.text = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_blank())
########## GDP Quarterly Data ##########
gdp.df = read.csv("~/Desktop/pwc/gdp/growth_forecast/raw/nbs/nbs_gdp_quarterly.csv")
gdp.df = gdp.df[,c(1:2)]
colnames(gdp.df)[2] = "gdp"
# unit in 100 million yuan  = 100,000,000
gdp.df[,2] = gdp.df[,2]* 10^8 / 10^12
# convert unit in trillion yuan
growth.df = read.csv("~/Desktop/pwc/gdp/growth_forecast/raw/nbs/nbs_growth_quarterly.csv")
gdp.df = cbind(gdp.df, growth = growth.df$growth[1:12])

# helper function
CalcFudgeAxis = function( y1, y2=y1) {
  Cast2To1 = function(x) ((ylim1[2]-ylim1[1])/(ylim2[2]-ylim2[1])*x) # x gets mapped to range of ylim2
  ylim1 <- c(min(y1),max(y1))
  ylim2 <- c(min(y2),max(y2))    
  yf <- Cast2To1(y2)
  labelsyf <- pretty(y2)  
  return(list(
    yf=yf,
    labels=labelsyf,
    breaks=Cast2To1(labelsyf)
  ))
}

# use fudgeaxis to rescale axis
FudgeAxis <- CalcFudgeAxis( gdp.df$gdp, gdp.df$growth )
cbind(gdp.df, FudgeAxis$yf) # take a look at the artificial axis

library(gtable)
library(grid)

quarterly_gdp.plot = 
ggplot(gdp.df, aes(x = quarter, y = gdp, group = 1)) + 
  geom_bar(position = "dodge", stat="identity",
           width = 0.7, color = "orange", fill = "orange", alpha = 0.5) + 
  scale_y_continuous(breaks = seq(0, 20, by = 2)) + 
  geom_line(aes(y = FudgeAxis$yf), color = "red") + 
  geom_point(aes(y = FudgeAxis$yf), color = "red") + 
  labs(title = "Quarterly GDP and \n Seasonally Adjusted Growth Rate", 
       x = "Year-Quarter", y = "GDP (Trillions of RMBs)")  +
  geom_text(data = gdp.df, aes(label = paste(as.character(round(gdp, 2))), y = gdp+0.8), 
            size = 3, color = "chocolate3") +
  geom_text(aes(label = paste(growth*100, "%", sep = ""), y = FudgeAxis$yf+0.8), 
            size = 3, color = "red")
  theme(plot.title = element_text(size=16),
        axis.text.x = element_text(size=13, angle=-45, hjust=0.001),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_blank())

grid.newpage()
quarterly_gdp.plot =
  arrangeGrob(quarterly_gdp.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(quarterly_gdp.plot)

# PlotWithFudgeAxis(gdp_quarterly_gdp.plot, FudgeAxis)
#####################################
########## GDP Annual Data ##########
agdp.df = read.csv("~/Desktop/pwc/gdp/annual_gdp/annual_gdp.csv")[,1:2]
colnames(agdp.df)[2] = "GDP100M"
agdp.df = rbind(c("Year"=2021, "GDP100M"=676708*(1+0.065)^6),
                c("Year"=2020, "GDP100M"=676708*(1+0.065)^5),
                c("Year"=2019, "GDP100M"=676708*(1+0.065)^4),
                c("Year"=2018, "GDP100M"=676708*(1+0.065)^3),
                c("Year"=2017, "GDP100M"=676708*(1+0.065)^2),
                c("Year"=2016, "GDP100M"=676708*(1+0.065)),
                c("Year"=2015, "GDP100M"=676708),
                agdp.df)
  # Now need to convert 100M to Trillion
agdp.df[,2] = agdp.df[,2]*10^8/10^12
colnames(agdp.df)[2] = "GDPTrillion"

annual_gdp.plot = 
ggplot(data=agdp.df, aes(x=Year, y=GDPTrillion)) + 
  geom_bar(stat ="identity", color = "#dc6900", fill = "#dc6900",
           width = 0.5) +
  stat_smooth(color = "#e0301e") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  geom_text(data = agdp.df[1:6,],
            aes(label = as.character(round(GDPTrillion, 0)), 
                x = Year,
                y = GDPTrillion+4), 
            size = 4,
            color = "#e0301e") +
  geom_rect(aes(xmin = -Inf, xmax = 2015+1/2, ymin = -Inf, ymax = Inf), 
            color = "white", fill = "black", alpha = 0.002) + 
  labs(title = "Annual GDP: 1995 - 2021",
       x = "Year",
       y = "GDP (Trillions of RMB)") + 
  theme(plot.title = element_text(size=16),
        axis.text.x = element_text(size=13, angle=-45, hjust=0.001),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13))

grid.newpage()
annual_gdp.plot =
  arrangeGrob(annual_gdp.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.11, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(annual_gdp.plot)
#####################################
########## Expenditure Approach ##########
ea.df = data.frame(Year = c(2014, 2009, 2004),
                   Consumption = c(45.26, 44.06, 48.42)/100,
                   Investment = c(40.45, 40.29, 36.99)/100,
                   GovSpending = c(11.89, 11.82, 12.29)/100,
                   NetExport = c(2.4, 3.83, 2.3)/100)

write.table(ea.df, file = "expenditure_approach.csv", 
            sep = ",",
            qmethod = "double")
ea.df = melt(ea.df, id = "Year")
ea.df$Year = as.factor(ea.df$Year)
levels(ea.df$variable)[3:4] = c("Government Spending", "Net Export")
ea.df$pos = ea.df$value/2

gdp_expenditure.plot = 
ggplot(data = ea.df, aes(x= Year, y = value, fill = variable)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(.~variable, scale = "free_y") +  
  scale_y_continuous(labels=percent_format(), 
                     breaks = seq(0, 0.5, by = 0.05)) + 
  scale_fill_manual(values = c("#dc6900", "#e0301e", "#602320", "#a32020")) + 
  labs(title = "GDP Breakdown: Expenditure Approach", 
       x = "Year", y = "") + 
  geom_text(aes(label = paste(value*100, "%", sep = ""), y = value+0.01), 
            size = 3) + 
  theme(legend.position = "None",
        plot.title = element_text(size=16),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_blank())

grid.newpage()
gdp_expenditure.plot =
  arrangeGrob(gdp_expenditure.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.099, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(gdp_expenditure.plot)
#####################################
########## Sector Breakdown ##########
sector.df = read.csv("~/Desktop/pwc/gdp/sector_growth/sector_growth.csv")
sector.df$sector = as.factor(sector.df$Sector)
sector.df$growth = sector.df$growth/100

sectorForecast.plot= 
ggplot(sector.df, aes(x = year, y = growth, color = Sector, 
                      shape = Sector, alpha = alpha, 
                      group = Sector)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  scale_alpha_identity() + 
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320")) +
  labs(title = "China Sectors Growth Forecasts", 
       x = "Year", y = "Growth") +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 0.15, by = 0.01)) +
  geom_rect(aes(xmin = -Inf, xmax = 2016.5, ymin = -Inf, ymax = Inf), 
            color = "white", fill = "black", alpha = 0.002) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_blank())

grid.newpage()
sectorForecast.plot =
  arrangeGrob(sectorForecast.plot, 
              bottom = textGrob("Source: The Economist Intelligence Unit (EIU)", 
                                x = -0.22, y = 1.5,
                                hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(sectorForecast.plot)
#####################################
########## Industry Breakdown ##########
industry.df = read.csv("~/Desktop/pwc/gdp/gdp_by_industry/industry.csv")
industry.df = industry.df[1:5,]
industry.df = melt(industry.df, id = "Year")
levels(industry.df$variable)[5] = "Real Estate"
colnames(industry.df)[2] = "Industry"
industry.df = industry.df[1:25,]
industry.df$value = industry.df$value/100
levels(industry.df$Industry)[2] = "Manufactory"
# ndustry.df$pos = industry.df$value/2

industryBreakdown.plot = 
ggplot(industry.df, aes(x = Year, y = value, color = Industry)) + 
  geom_line(size = 2) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320", "#a32020", "goldenrod2")) + 
  facet_grid(Industry~., scales = "free_y") + 
#  geom_bar(aes(fill = Industry), 
#           alpha = 0.8, position = "dodge", stat="identity") +
#  facet_grid(.~Industry) + 
  labs(title = "GDP Breakdown by Industry", 
       x = "Year", y = "Composition") +
  scale_y_continuous(labels = percent_format()) +
#  geom_text(aes(label = paste(value*100, "%", sep = ""), y = pos), 
#            size = 2) + 
  theme(legend.position = "none",
        plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size = 10),#, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) 

grid.newpage()
industryBreakdown.plot =
  arrangeGrob(industryBreakdown.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.10, y = 1.5,
                                hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(industryBreakdown.plot)
#####################################
########## Oxford Economics Industry Breakdown ########## 
industry.df = read.csv("~/Desktop/pwc/industry/industry.csv")
industry.df$value = industry.df$value/100

levels(industry.df$category)=c("Agricultural \nMachinery", "Ceramic \nProducts",           
                               "Coal & Lignite \nMining", "Detergents \n& Etc",           
                               "Electric\nComponents", "Iron & Steel",               
                               "Medical \nEquip.", "Motor Vehicle",              
                               "Motor Vehicle \n& Parts", "Motors Except \nVehicles",    
                               "Non-ferrous \nMetals", "Oil & Gas \nExtraction",      
                               "Other Extraction \nActivities", "Paints & Etc",               
                               "PC & Office \nEquip.", "Pharmaceuticals",            
                               "Printing & \nRecorded Media", "Pulp and Paper",           
                               "Repair \nMachinery", "Tobacco")

growing_industries.plot = 
  ggplot(industry.df, aes(x = as.factor(category), y = value,
                          group = id, fill = id)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  facet_grid(.~id , scales = "free") +
  labs(title = "China: Top 10 Fastest and Slowest Growing Industries", 
       x = "Industries", y = "Growth") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 0.1, by = 0.01)) + 
  geom_text(aes(label = paste(as.character(value*100), "%", sep = ""), 
                y = value+0.004), size = 3, color = "black") + 
  scale_fill_manual(values = c("#eb8c00", "#a32020"), guide = "none") +
  theme(legend.position = "None",
        plot.title = element_text(size=13),
        axis.text.x = element_text(size=9, angle = -90, hjust=0.001),
        axis.text.y = element_text(size=9),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_blank())

grid.newpage()
growing_industries.plot =
  arrangeGrob(growing_industries.plot, 
              bottom = textGrob("Source: Oxford Economics", 
                                x = -0.13, #y = 1.5,
                                hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(growing_industries.plot)
#####################################




########## NX Forecast ##########
trade.df = read.csv("~/Desktop/pwc/trade/trade_forecast/TRADE_BAR.csv")
trade.df$Index = as.factor(trade.df$Index)
trade.df$num =abs(trade.df$num)
trade.df$growth = round(trade.df$growth, 3)

export.df = trade.df[1:6,]
export.df$pos = export.df$num + 100
export.df$pos[c(1, 3)] = c(2200, 2300)

import.df = trade.df[7:12,]
import.df$pos = import.df$num + 100
import.df$pos[c(1, 3)] = c(1600, 1650)

nx.df = trade.df[13:18,]
nx.df$pos = nx.df$num + 100

nxForecast.plot = 
ggplot(trade.df, aes(x = year, y = num, color = Index, 
                     shape = Index, group = Index)) +
  geom_bar(aes(fill = Index), 
           stat = "identity", position = "dodge",
           width = 0.5) + geom_smooth(se = FALSE, fullrange = TRUE) + 
  scale_fill_manual(values = c("#dc6900", "#e0301e", "#602320")) +
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320")) +
  labs(title = "Balance of Trade Forecast", 
       x = "Year", y = "Billions of USD") +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(-2000, 3000, by = 250)) +
  geom_rect(aes(xmin = -Inf, xmax = 2016.5, ymin = -Inf, ymax = Inf), 
            color = "white", fill = "black", alpha = 0.002) + 
  geom_text(data = export.df,
            aes(label = paste(as.character(growth*100), "%", sep = ""), 
                x = year+0.4, y =pos), size = 3, color = "#dc6900") + 
  geom_text(data = import.df,
            aes(label = paste(as.character(growth*100), "%", sep = ""), 
                x = year+0.4, y =pos), size = 3, color = "#e0301e") + 
  geom_text(data = nx.df,
            aes(label = paste(as.character(growth*100), "%", sep = ""), 
                x = year+0.4, y =pos), size = 3, color = "#602320") + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_blank())

grid.newpage()
nxForecast.plot =
  arrangeGrob(nxForecast.plot, 
              bottom = textGrob("Source: The Economist Intelligence Unit (EIU)", 
                                x = -0.22, y = 1.5,
                                hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(nxForecast.plot)
#####################################
########## NX Quarterly ##########
tradeRaw.df = read.csv("~/Desktop/pwc/trade/trade_quarterly/trade_processed.csv")
tradeRaw.df = aggregate(tradeRaw.df[,2:4], 
                        list(quarter = tradeRaw.df$quarter), sum)

tquarter = seq(as.Date("2006/1/1"), as.Date("2016/3/1"), by = "quarter")
tnum = c(tradeRaw.df$ex, -tradeRaw.df$im, tradeRaw.df$nx)
tIndex = rep(c("Export", "Import", "Net Export"), each = nrow(tradeRaw.df))

qtrade.df = data.frame(quarter = tquarter,
                       num = tnum/1000000, # raw is 1000 USD
                       Index = tIndex)

qtrade.plot = 
ggplot(qtrade.df, aes(x = quarter, y = num, color = Index, 
                      shape = Index, group = Index)) +
  geom_line(size = 0.5) + 
  geom_point(size = 2) +
  labs(title = "Quarterly Balance of Trade", 
       x = "Year-Quarter", y = "Billions of USD") +
  scale_x_date(date_breaks = "9 month", date_labels = "%y-%b") +
  scale_y_continuous(breaks = seq(-500, 800, by = 100)) + 
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320")) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_blank())

grid.newpage()
qtrade.plot =
  arrangeGrob(qtrade.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.10, y = 1.5,
                                hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(qtrade.plot)
#####################################


########## Stock turnover and Volume ##########
# https://www.hkex.com.hk/eng/stat/statrpt/mkthl/markhighlight.htm
# Factbook: https://www.hkex.com.hk/eng/stat/statrpt/factbook/factbook.htm
# volume in shares
# Turnover in Billion of RMB

# Shanghai
sh.df = read.csv("~/Desktop/pwc/stock/quarterly/sh.csv")
sh.df$quarter = as.Date(sh.df$quarter)
sh.df$quarter = as.yearqtr(sh.df$quarter, format = "%Y-%m-%d")
sh.df$quarter = format(sh.df$quarter, format = "%YQ%q")
sh.df$volume = as.numeric(sh.df$volume)
sh.df = aggregate(sh.df[,2:3], list(quarter=sh.df$quarter), sum)
sh.df$id = rep("SH", times = nrow(sh.df))
colnames(sh.df) = c("quarter", "volume", "turnover", "id")
sh.df = sh.df[6:11,]

# Shengzhen
sz.df = read.csv("~/Desktop/pwc/stock/quarterly/sz.csv")
sz.df$quarter = as.Date(sz.df$quarter)
sz.df$quarter = as.yearqtr(sz.df$quarter, format = "%Y-%m-%d")
sz.df$quarter = format(sz.df$quarter, format = "%YQ%q")
sz.df$volume = as.numeric(sz.df$volume)
sz.df = aggregate(sz.df[,2:3], list(quarter=sz.df$quarter), sum)
sz.df$id = rep("SZ", times = nrow(sz.df))
colnames(sz.df) = c("quarter", "volume", "turnover", "id")
sz.df = sz.df[6:11,]

# Hongkong
hk.df = read.csv("~/Desktop/pwc/stock/quarterly/hk.csv")
hk.df$quarter = as.Date(hk.df$quarter)
hk.df$quarter = as.yearqtr(hk.df$quarter, format = "%Y-%m-%d")
hk.df$quarter = format(hk.df$quarter, format = "%YQ%q")
hk.df$volume = as.numeric(hk.df$volume)
hk.df = aggregate(hk.df$volume, list(quarter=hk.df$quarter), sum)
hk_turnover.df  = read.csv("~/Desktop/pwc/stock/quarterly/hk_quarterly_economic_update.csv")
hk.df$turnover = hk_turnover.df$turnover.billion.rmb
hk.df$id = rep("HK", times = nrow(hk.df))
colnames(hk.df) = c("quarter", "volume", "turnover", "id")
hk.df = hk.df[6:11, ]

stock.df = rbind(sh.df, sz.df, hk.df) 
stock.df$volume = stock.df$volume/1000000000 # convert shares to billions of shares
stock.df$turnover = stock.df$turnover/1000 # convert turnovers from millions to trillions


stockVolume.plot = 
ggplot(stock.df, aes(x = quarter, y = volume, color = id, 
                     shape = id, group = id)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(title = "China Stock Market: Quarterly Volume", 
       x = "Year", y = "Billions of Shares") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) + 
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320")) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text.x = element_text(size=10), #angle=-45, hjust=0.001),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())
grid.newpage()
stockVolume.plot =
  arrangeGrob(stockVolume.plot, 
              bottom = textGrob("Source: Yahoo Finance\n      Hong Kong Stock Exchange", 
                                x = -0.11, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(stockVolume.plot)


stockTurnover.plot = 
ggplot(stock.df, aes(x = quarter, y = turnover, color = id, 
                     shape = id, group = id)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(title = "China Stock Market: Quarterly Turnover", 
       x = "Year", y = "Trillions of RMB") +
  scale_y_continuous(breaks = seq(0, 80, by = 5)) +
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320")) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text.x = element_text(size=10),# angle=-45, hjust=0.001),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())

grid.newpage()
stockTurnover.plot =
  arrangeGrob(stockTurnover.plot, 
              bottom = textGrob("Source: Yahoo Finance\n      Hong Kong Stock Exchange", 
                                x = -0.11, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(stockTurnover.plot)
#####################################

########## Stock Index 10 years ##########
shts.df = read.csv("~/Desktop/pwc/stock/timeSeries/sh_timeseries.csv")
shts.df = shts.df[,c(1,5)]
shts.df$id = rep("SH", times = nrow(shts.df))

szts.df = read.csv("~/Desktop/pwc/stock/timeSeries/sz_timeseries.csv")
# add 2016 data manually
szts.df = szts.df[,c(1,5)]
szts16.df = data.frame(Date = as.factor(c("2016-06-01", 
                                          "2016-05-03", 
                                          "2016-04-01", 
                                          "2016-03-01", 
                                          "2016-02-01", 
                                          "2016-01-04")),
                       Close = c(11626.038, 9322.006, 
                                 9322, 10379.647, 10441.92, 
                                 10209.142))
szts.df = rbind(szts16.df, szts.df)
szts.df$id = rep("SZ", times = nrow(szts.df))
szts.df$Date = shts.df$Date

hkts.df = read.csv("~/Desktop/pwc/stock/timeSeries/hk_timeseries.csv")
hkts.df = hkts.df[,c(1,5)]
hkts.df$id = rep("HK", times = nrow(hkts.df))
hkts.df$Date = shts.df$Date

ts.df = rbind(shts.df, szts.df, hkts.df)
ts.df$id = as.factor(ts.df$id)
ts.df$Date = as.Date(ts.df$Date)

timeSeries.plot = 
ggplot(ts.df, aes(x = Date, y = Close, color = id, group = id)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 30000, by = 3000)) +
  scale_x_date(date_breaks = "12 month", date_labels = "%y-%b") +
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320")) + 
  labs(title = "China Stock Market Index (10 Years)", 
       x = "Year-Month", y = "Close") +
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())

grid.newpage()
timeSeries.plot =
  arrangeGrob(timeSeries.plot, 
              bottom = textGrob("Source: Yahoo Finance", 
                                x = -0.11, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(timeSeries.plot)
#####################################





########## RMB Exchange Rate ##########
# http://www.chinamoney.com.cn/fe/Channel/2781516?type=history
exchange.df = read.csv("~/Desktop/pwc/exchange_rate/exchangeRate.csv")
colnames(exchange.df) = c("date", "USD", "EUR", "JPY", "GBP", "AUD")

# the date column in exchange.df is in 2014-04 format
# I need to use the zoo package to change the format into quarters
library(zoo)
exchange.df$date = as.yearqtr(exchange.df$date, format = "%Y-%m")
exchange.df$date = format(exchange.df$date, format = "%yQ%q")

# aggregare function to do its job
exchange.df = aggregate(exchange.df[,2:6], list(date=exchange.df$date), mean)

# convert from wide to long dataframe
exchange.df = melt(exchange.df, id = "date")

exchange.plot = 
ggplot(exchange.df, aes(x = date, y = value, color=variable, group=variable)) + 
  geom_line(size = 1) + 
  geom_point(size = 3, aes(shape =  variable)) + 
  facet_grid(variable~., scale='free_y') + 
  labs(title = "RMB Exchange Rate", 
      x = "Quarter", y = "Exchange Rate") + 
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320", 
                                "#a32020", "#ffb600")) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text.x = element_text(size = 10, angle=-45, hjust=0.001),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())

grid.newpage()
exchange.plot =
  arrangeGrob(exchange.plot, 
              bottom = textGrob("Source: China Foreign Exchange Trade System (CFETS)", 
                                x = -0.27, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(exchange.plot)


#####################################
########## RMB Interest Rate ########## 
# http://i.stack.imgur.com/zsEHH.png
interest.df = read.csv("~/Desktop/pwc/interest/interest.csv")
interest.df$date = as.Date(interest.df$date)
colnames(interest.df) = c("dateEnd", "Save", "Lend")
interest.df$dateStart = c(interest.df$dateEnd[2:nrow(interest.df)], NA)
interest.df = interest.df[1:18,]
interest.df = melt(interest.df, id = c("dateStart", "dateEnd"))
interest.df$interestEnd = interest.df$value
date.df = interest.df[,c(1,4,5)]
c("#dc6900", "#e0301e", "#602320", "#a32020")

interest.plot = 
  ggplot(interest.df, aes(x=dateStart, xend=dateEnd, y=value, yend=interestEnd, 
                          color = variable, group = variable)) +
  geom_point() +  # Solid points to left
  geom_point(aes(x=dateEnd, y=interestEnd), shape = 1) +  # Open points to right
  geom_segment() +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5)) +
  scale_x_date(date_breaks = "6 month", date_labels = "%y-%b") +
  scale_colour_manual(values = c("#dc6900","#a32020")) + 
  labs(title = "PBC Historical Interest Rate", 
       x = "Year-Month", y = "Interest Rate") +
  geom_vline(data = date.df, aes(xintercept=as.numeric(dateStart)), 
             linetype=2, color="grey", alpha=0.7) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text.x = element_text(size = 10, angle=-45, hjust=0.001),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())

grid.newpage()
interest.plot =
  arrangeGrob(interest.plot, 
              bottom = textGrob("Source: China Foreign Exchange Trade System (CFETS)", 
                                x = -0.27, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(interest.plot)
#####################################






########## E-commerce ########## 
# Alibaba: http://www.alibabagroup.com/en/ir/earnings.php
ecommerce.df = read.csv("~/Desktop/pwc/ecommerce/ecommerceUpdate.csv")
ecommerce.df$firms = as.factor(ecommerce.df$firm)
ecommerce.df$revenue = ecommerce.df$revenue/1000
ecommerce.df$growth = round(ecommerce.df$growth, 3)


tencent.df = ecommerce.df[1:10,]
tencent.df$pos = tencent.df$revenue + 5

alibaba.df = ecommerce.df[11:20,]
alibaba.df$pos = alibaba.df$revenue + 5

jd.df = ecommerce.df[21:30,]
jd.df$pos = jd.df$revenue + 5

ecommerce.plot = 
ggplot(ecommerce.df, aes(x = year, y = revenue, color = firms, 
                         fill = firms, shape = firms, group = firms)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  geom_smooth(se = FALSE) + 
  scale_fill_manual(values = c("#dc6900", "#e0301e", "#602320")) +
  scale_color_manual(values = c("#dc6900", "#e0301e", "#602320"))  + 
  labs(title = "Top 3 Chinese E-Commerce Firms \n Revenue Trends", 
       x = "Quarter", y = "Trillions of RMB") +
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text.x = element_text(size = 10), # angle=-45, hjust=0.001),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())

grid.newpage()
ecommerce.plot =
  arrangeGrob(ecommerce.plot, 
              bottom = textGrob("Source: Alibaba Group Investor Relations          
                    Tencent Investor Relations
                    JD.com Investor Relations",
                                x = -0.23, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(ecommerce.plot)
#####################################





########## FDI ########## 
# http://data.stats.gov.cn/easyquery.htm?cn=C01
# raw data unit in 10,000 usd
fdi.df = read.csv("~/Desktop/pwc/fdi/fdi.csv")
colnames(fdi.df)[6] = "Rest of the World"
fdi.df = melt(fdi.df, id = "year")
fdi.df$value = round(fdi.df$value/100000) # now convert to 1,000,000,000 (1 trillion)
fdi.df = fdi.df[9:40,]

fdi.plot = 
ggplot(fdi.df, aes(x = year, y = value, color = variable, 
                       shape = variable, group = variable,
                       fill = variable)) +
  geom_bar(stat = "identity", width = 0.4) + 
  geom_smooth(se = FALSE, size = 0.5) +
  scale_fill_manual(values = c("#dc6900", "#602320", "#e0301e", "#a32020")) + 
  scale_color_manual(values = c("#dc6900", "#602320", "#e0301e", "#a32020")) + 
  labs(title = "Foreign Direct Investment", 
       x = "Year", y = "Trillions of USD") +
  scale_x_continuous(breaks = seq(2007, 2014, by = 1)) +
  scale_y_continuous(breaks = seq(0, 150, by = 10)) +
  theme(legend.position = "bottom",
        plot.title = element_text(size=13),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_blank())

grid.newpage()
fdi.plot =
  arrangeGrob(fdi.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(fdi.plot)
#####################################





########## ppi ########## 
ppi.df = read.csv("~/Desktop/pwc/ppi/ppi_processed.csv")

# to prepare for percentage need to divide it by 100
ppi.df$ppi = ppi.df$ppi/100

# becasue of r limitation, need to add "-1" as a dummy date for the sake
# of axis manipulation in ggplot2
ppi.df$month = as.Date(paste(ppi.df$month, "-1", sep = ""))


# generate 1 for 125 times
x = rep(1, nrow(ppi.df))
# find all even number
y = seq(1, nrow(ppi.df), by = 1) %% 2 == 0
index = which(y == FALSE)
x[index] = -1
x = x * 0.003
x[c(1,3,5, 12)] = x[c(1,3,5, 12)]*-1

ppi.df$pos = ppi.df$ppi + x
ppi_pos.df = ppi.df[1:17,] # for the sake of adding label -- look at geom_text

ppi.plot = 
ggplot(ppi.df[1:17,], aes(x = month, y = ppi, group=1)) + 
  geom_point(size = 4, color = "#a32020") + 
  geom_line(size = 1, color = "#dc6900") + 
  geom_hline(yintercept = 0, linetype = 2, color = "black") + 
  labs(title = "Producer Price Index", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(-0.06, 0, by = 0.005)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%b") +
  geom_text(data = ppi_pos.df, 
            aes(label = paste(as.character(ppi_pos.df$ppi*100), "%", sep = ""), 
            y = pos), size = 3.5, color = "#a32020") +
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
ppi.plot =
  arrangeGrob(ppi.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(ppi.plot)

#####################################
########## ppi: 10 Years ########## 
ppi.df$month = as.Date(paste(ppi.df$month, "-1", sep = ""))

ppi10y.plot = 
ggplot(ppi.df[1:136,], aes(x = month, y = ppi, group=1)) + 
  geom_point(size = 1.5, color = "#a32020") + 
  geom_line(size = 0.5, color = "#dc6900") + 
  geom_hline(yintercept = 0, linetype = 2, color = "black") + 
  labs(title = "Producer Price Index: 10-Year Trend", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(-0.10, 1, by = 0.01)) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%y-%b") + 
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
ppi10y.plot =
  arrangeGrob(ppi10y.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(ppi10y.plot)

#####################################


########## pmi ########## 
pmi.df = read.csv("~/Desktop/pwc/pmi/pmi.csv")
# convert the month columne to class: Date
pmi.df$month = as.Date(pmi.df$month)

# generate 1 for 125 times
x = rep(1, 138)
# find all even number
y = seq(1, 138, by = 1) %% 2 == 0
index = which(y == FALSE)
x[index] = -1
x[c(7, 8, 9, 13, 17, 18)] = c(1, -1, -1, -1, 1, -1)
x = x * 0.0005

pmi.df$pos = pmi.df$pmi + x
pmi_pos.df = pmi.df[1:18,]

pmi.plot = 
ggplot(pmi.df[1:18,], aes(x = month, y = pmi, group=1)) + 
  geom_point(size = 4, color = "#a32020") + 
  geom_line(size = 1, color = "#dc6900") + 
  geom_hline(yintercept = 0.50, linetype = 2, color = "black") + 
  labs(title = "Purchasing Managers' Index", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 1, by = 0.002)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%b") + 
  geom_text(data = pmi_pos.df, 
            aes(label = paste(as.character(pmi_pos.df$pmi*100), "%", sep = ""), 
                y = pos), size = 3.5, color = "#a32020") +
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
pmi.plot =
  arrangeGrob(pmi.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(pmi.plot)
########## pmi: 10 Years ########## 

pmi10y.plot = 
ggplot(pmi.df[1:137,], aes(x = month, y = pmi, group=1)) + 
  geom_point(size = 1.5, color = "#a32020") + 
  geom_line(size = 0.5, color = "#dc6900") + 
  geom_hline(yintercept = 0.50, linetype = 2, color = "black") + 
  labs(title = "Purchasing Managers' Index: 10-Year Trend", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 1, by = 0.02)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%y-%b") + 
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))


grid.newpage()
pmi10y.plot =
  arrangeGrob(pmi10y.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(pmi10y.plot)

#####################################



########## Fixed Asset Investment: Quarters ########## 
fai.df = read.csv("~/Desktop/pwc/fai/fai.csv")
fai.df$month = as.Date(fai.df$month)
fai.df = fai.df[-which(is.na(fai.df$fai)),]
fai.df$fai = fai.df$fai/100

# generate 1 for 125 times
x = rep(1, 126)
# find all even number
y = seq(1, 126, by = 1) %% 2 == 0
index = which(y == FALSE)
x[index ] = -1
x[1:16] = c(1, -1, 1, 1, 1, 1, -1, -1, -1, 1, 1, -1, -1, -1, -1, -1)
x = x * 0.003

fai.df$pos = fai.df$fai + x
fai_pos.df = fai.df[1:16,]

fai.plot = 
ggplot(fai.df[1:16,], aes(x = month, y = fai, group=1)) + 
  geom_point(size = 4, color = "#a32020") + 
  geom_line(size = 1, color = "#dc6900") + 
  labs(title = "Fixed Asset Investment: \n Accumulated Growth Rate", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 1, by = 0.005)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%b") + 
  geom_text(data = fai_pos.df,
            aes(label = paste(as.character(fai_pos.df$fai*100), "%", sep = ""), 
                y = pos), size = 3, color = "#a32020") + 
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
fai.plot =
  arrangeGrob(fai.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(fai.plot)
#####################################
########## Fixed Asset Investment: 10 Years ########## 
fai10y.plot = 
ggplot(fai.df, aes(x = month, y = fai, group=1)) + 
  geom_point(size = 1.5, color = "#a32020") + 
  geom_line(size = 0.5, color = "#dc6900") + 
  labs(title = "Fixed Asset Investment Trend: \n 10-Year Accumulated Growth", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 1, by = 0.02)) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%y-%b") + 
  #  geom_text(aes(label = paste(as.character(fai.df[1:34,2]*100), "%", sep = ""), 
  #                y = pos), size = 2) +
  # geom_vline(data = fai_dates.df,
  # aes(xintercept=as.numeric(month)),
  # linetype=2, colour="black") + 
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
fai10y.plot =
  arrangeGrob(fai10y.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(fai10y.plot)
#####################################




########## Consumption ########## 
# Retail Sales of Consumer Goods Accumulated Growth Rate
con.df = read.csv("~/Desktop/pwc/consumption/consumption_yoy_growth_rate.csv")
con.df$month = as.Date(con.df$month)
con.df$growth = con.df$growth/100

# generate 1 for 125 times
x  = c(1, -1, -1, 1, -1, -1, 1, 1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 1)
x = x * 0.0006

con.df$pos = con.df$growth + x
con_pos.df = con.df

comsumption.plot = 
ggplot(con.df, aes(x = month, y = growth, group=1)) + 
  geom_point(size = 4, color = "#a32020") + 
  geom_line(size = 1, color = "#dc6900") + 
  labs(title = "Retail Sales of Consumer Goods: \n Year Growth Rate", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(), 
                     breaks = seq(0.07, 0.158, by = 0.001)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%b") + 
  geom_text(data = con_pos.df,
            aes(label = paste(as.character(con_pos.df$growth*100), "%", sep = ""), 
                y = pos), size = 3, color = "#a32020") + 
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
comsumption.plot =
  arrangeGrob(comsumption.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(comsumption.plot)
#####################################
########## Consumption: 10 Years ########## 
consumption10y.plot= 
ggplot(con.df[1:131,], aes(x = month, y = growth, group=1)) + 
  geom_point(size = 1.5, color = "#a32020", alpha = 0.7) + 
  geom_line(size = 1, color = "#dc6900", alpha = 0.7) + 
  labs(title = "Retail Sales of Consumer Goods: \n 10-Year Accumulated Growth", 
       x = "Year-Month", y = "") + 
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0.09, 0.225, by = 0.01)) + 
  scale_x_date(date_breaks = "9 month", date_labels = "%y-%b") + 
  theme(plot.title = element_text(size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10, angle=-45, hjust=0.001),
        axis.title.x = element_text(size=13))

grid.newpage()
consumption10y.plot =
  arrangeGrob(consumption10y.plot, 
              bottom = textGrob("Source: NBS of China", 
                                x = -0.1, hjust = -0.5,
                                gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(consumption10y.plot)
#####################################


