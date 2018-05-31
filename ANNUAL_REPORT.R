# Laszlo Kiss 17/03/2018
# 

list.of.packages <- c("astsa","xts","eurostat", "ggplot2", "tidyr","dplyr","tmap","RColorBrewer","leaflet","itsmr","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(eurostat)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tmap)
library(RColorBrewer)
library(leaflet)
library(xts)
library(forecast)
library(tseries)
library(plyr)


decomp.plot <- function(x, main = NULL, ...)
{
  if(is.null(main))
    main <- paste("Decomposition of", x$type, "time series")
  plot(cbind(observed = x$random + if (x$type == "additive")
    x$trend + x$seasonal
    else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal,
    random = x$random), main = main, ...)
}


id_year <- "nrg_chddr2_a"
id <- "nrg_chddr2_m"
data <- get_eurostat(id,time_format = "date")
data_year <- get_eurostat(id_year,time_format = "raw")
data_year_all <- data_year[,2:5]
#View(data_year_all)
#View(data)
#View(nrg_chddr2_m)
data_all <- data[,2:5]
data_all_HDD <- data_all[data_all$indic_nrg == "HDD",]
data_all_CDD <- data_all[data_all$indic_nrg == "CDD",]
data_all_CDD_wide_spread <- spread(data_all_CDD,time,values)
data_all_HDD_wide_spread <- spread(data_all_HDD,time,values)
data_all_year_HDD <- data_year_all[data_year_all$indic_nrg == "HDD",]
data_all_year_CDD <- data_year_all[data_year_all$indic_nrg == "CDD",]
data_all_year_HDD_wide_spread <- spread(data_all_year_HDD,time,values)
data_all_year_CDD_wide_spread <- spread(data_all_year_CDD,time,values)
sp_data_all_HDD_wide_spread <- merge_eurostat_geodata(data = data_all_HDD_wide_spread,geocolumn = "geo")
sp_data_all_CDD_wide_spread <- merge_eurostat_geodata(data = data_all_CDD_wide_spread,geocolumn = "geo")
# data_HU <- (subset(data_all_HDD, grepl(pattern =  "HU",data_all_HDD$geo)))
# View(data_HU)
# p_HU <- ggplot(data_HU, aes(x = time,y = values, colour = geo))
# p_HU <-  p_HU + geom_line()
# print(p_HU)


#View(data_all_year_CDD_wide_spread)
data <- subset(data,indic_nrg == "HDD") #to plot
#data_labels <- label_eurostat(id,fix_duplicated = TRUE)

#View(data[data$indic_nrg != "HDD",])
#View(data_labels)
# validcountries <- c("BE","NL","LU")
validcountries <- c("HU")



data_year_HDD_mean <- dplyr::filter(data_all_year_HDD_wide_spread,nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo))
data_year_CDD_mean <- dplyr::filter(data_all_year_CDD_wide_spread,nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo))
# data_year_HDD_mean <- data_all_year_HDD_wide_spread
# data_year_CDD_mean <- data_all_year_CDD_wide_spread
HDD_means_by_year <- c(data_year_HDD_mean[,2],rowMeans(data_year_HDD_mean[,-1:-2]))
CDD_means_by_year <- c(data_year_CDD_mean[,2],rowMeans(data_year_CDD_mean[,-1:-2]))
# View(data_year_HDD_mean[,3:length(data_year_HDD_mean)])

#View(data_year_HDD_mean)
HDD_meandf_by_year <- data.frame((matrix(ncol = 2, nrow = nrow(data_year_HDD_mean))))
colnames(HDD_meandf_by_year) <- c("geo","meanHDDvaluebyyear")

HDD_meandf_by_year[,1] <- data_year_HDD_mean$geo

for(i in 1:nrow(data_year_HDD_mean)){
  HDD_meandf_by_year[i,2] <- (mean((as.numeric(data_year_HDD_mean[i,-1:-2]))))
}
#View(HDD_meandf_by_year)

# validcountries <- c("HU")
data2plot <- dplyr::filter(data,nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo))
#ggplot(data = data2plot, aes(x=time,y=values),+ geom_line(colour = geo))
#xyplot(values~time,type=c('l','p'),groups= geo,data=data2plot,auto.key=T)
#View(data2plot)
#rm(data2plot)
data2plotl <- label_eurostat(data2plot, fix_duplicated = TRUE)
#View(data2plotl)
#duplicates <- c("Közép-Magyarország (NUTS 2013)","Slovenija")
#data2plotl_subs <- subset(data2plotl, time %in% 2000:2010 & !(geo %in% duplicates))
data2plotl_subs <- subset(data2plotl,time >= "2000-01-01" & time <= "2005-01-01")
# View(data2plotl_subs)
p <- ggplot(data2plotl_subs, aes(x = time, y = values, colour = geo))
p <- p + geom_line()
p <- p + geom_smooth()
# print(p)

ggplot(data2plotl_subs) + geom_line(aes(x=time, y=values, color=geo)) +
  labs(title = "Heating Degree Days in HUNGARY", x = "Years", y = "number of HDD", color = "EU regions") +
  theme(legend.position="bottom")


nrg_chddr2_m <- get_eurostat("nrg_chddr2_m",time_format = "num") 
#View(nrg_chddr2_m)
#duplicates <- c("Közép-Magyarország (NUTS 2013)")
nrg_chddr2_m <- subset(nrg_chddr2_m,indic_nrg == "HDD")
nrg_chddr2_m <- subset(nrg_chddr2_m, time == 2000 & !(geo %in% duplicates))
#validcountries <- c("HU", "AT", "SK","CZ")
nrg_chddr2_m <- dplyr::filter(nrg_chddr2_m,nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo))
#nrg_chddr2_m <- label_eurostat(nrg_chddr2_m, fix_duplicated = TRUE)
#View(nrg_chddr2_m)

sp_data <- nrg_chddr2_m %>%
  # subsetting to year 2014 and NUTS-3 level
  dplyr::filter(time == 2000-01-01, nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo)) %>%
  # label the single geo column
  mutate(label = paste0(label_eurostat(.,fix_duplicated = TRUE)[["geo"]], "\n", values),
         hdd = cut_to_classes(values, n = 10)) %>%
  # merge with geodata
  merge_eurostat_geodata(data=.,geocolumn="geo",resolution = "01", all_regions = FALSE, output_class="spdf")




df_centers <- data.frame(matrix(ncol = 4, nrow = length(sp_data@data$NUTS_ID)))
x <- c("geo","lng", "lat","name")
colnames(df_centers) <- x



df_centers[,1] <- sp_data@data$NUTS_ID 


for(i in 1:24){ 
  df_centers[i,2] <- sp_data@polygons[[i]]@labpt[1] 
  df_centers[i,3] <- sp_data@polygons[[i]]@labpt[2]
  df_centers[i,4] <- sp_data@data$label[i]
  
}

#rm(df_centers)
#print(sp_data@polygons[[1]]@labpt[1])

df_circles <- merge.data.frame(df_centers,HDD_meandf_by_year, by="geo") 


# plot map
data(Europe)
map2 <- tm_shape(Europe) +
  tm_fill("lightgrey") +
  tm_shape(sp_data, is.master = TRUE) +
  tm_polygons("hdd", title = "HDD in 2000",
              palette = "Oranges", border.col = "white") +
  tm_text("label", just = "right") +
  tm_scale_bar() +
  tm_format_Europe(legend.outside = FALSE, attr.outside = TRUE) +
  tm_legend(text.size = 0.5) +
  tm_dots()
tmap_mode("view")
map2


lf <-  tmap_leaflet(map2)

leaflet() %>% 
  addTiles(group = "OpenStreeMap.Default")%>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
  addPolygons(data = sp_data, group = "NUTS Region border") %>% 
  addMarkers(data = df_centers,lng = ~lng,lat = ~lat, group = "Heating days indicator by month",
             popup = ~paste0("Longitude: ", lng,"<br>","Latitude: ", lat,"<br>","Name: ", name,"<br>","Code: ", geo)) %>% 
  addLayersControl(baseGroups = c("OpenStreeMap.Default","CartoDB.Positron"),
                   overlayGroups = c("NUTS Region border","Heating days indicator by month", "Mean circles" ))


#leaflet(df_centers) %>% addMarkers(df_centers,lng = ~df_centers$lng,lat = ~df_centers$lat)


data_all_CDD_wide_spread_ts <- spread(dplyr::filter(data_all_CDD,nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo)),geo,values)
data_all_HDD_wide_spread_ts <- spread(dplyr::filter(data_all_HDD,nchar(as.character(geo)) == 4, grepl(paste(validcountries,collapse = '|'),geo)),geo,values)


# View(data_all_HDD_wide_spread_ts)

ts_list <- list()
frame_list <- list()

for(i in 3:length(colnames(data_all_HDD_wide_spread_ts))){
  x <- paste(colnames(data_all_HDD_wide_spread_ts)[i],"_ts")
  x <- gsub(" ", "", x, fixed = TRUE)
  y <- paste(colnames(data_all_HDD_wide_spread_ts)[i],"_frame")
  y <- gsub(" ", "", y, fixed = TRUE)
  assign(x,ts((data_all_HDD_wide_spread_ts)[i], c('values'),frequency = 12, start = c(1974,1), end = c(2017,12)))
  assign(y,data_all_HDD_wide_spread_ts[i])
  ts_temp <- ts((data_all_HDD_wide_spread_ts)[i], c('values'),frequency = 12, start = c(1974,1), end = c(2017,12))
  frame_temp <- data_all_HDD_wide_spread_ts[i]
  frame_temp$clean = tsclean(ts_temp)
  frame_temp$hdd_ma1year = ma(frame_temp$clean,order=12)
  x <- paste(colnames(data_all_HDD_wide_spread_ts)[i],"_ma")
  x <- gsub(" ", "", x, fixed = TRUE)
  ts_temp <- ts(na.omit(frame_temp$clean), frequency=12, start = c(1974,1), end = c(2017,12))
  assign(x,ts_temp)
  y <- paste(colnames(data_all_HDD_wide_spread_ts)[i],"_decomp")
  y <- gsub(" ", "", y, fixed = TRUE)
  temp_decomp <- decompose(ts_temp)
  assign(y,temp_decomp)
  decomp.plot(temp_decomp,main = paste("Decomposition of",x))
  x <- paste("deseasonal_",colnames(data_all_HDD_wide_spread_ts)[i])
  x <- gsub(" ", "", x, fixed = TRUE)
  deseasonal_temp <- seasadj(temp_decomp)
  assign(x,deseasonal_temp)
  fit<-auto.arima(deseasonal_temp, seasonal=FALSE)
  # tsdisplay(residuals(fit), lag.max=36, main=paste('Model Residuals for ',x))
  ts_list[[i]] = ts_temp
  frame_list[[i]] = frame_temp

}

###########################

y <- ts(data_all_HDD_wide_spread_ts[,3:26],
        start =c(1974,1), end = c(2017,12), frequency = 12)

parameters <- list()
parameters_diff <- list()

for(i in 1:ncol(y)){
  y1 <- y[,i]
  ydiff1 <- diff(y[,i],12)
  
  AuModel <- auto.arima(y1,  max.p=5, max.q=5,
                        max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1, 
                        start.p=1, start.q=1, start.P=1, start.Q=1)
  
  AuModel_diff <- auto.arima(ydiff1,  max.p=5, max.q=5,
                             max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1, 
                             start.p=1, start.q=1, start.P=1, start.Q=1)
  
  
  parameters[[i]] <- AuModel$arma
  parameters_diff[[i]] <- AuModel_diff$arma
  
}

df_parameters <- as.data.frame(matrix(unlist(parameters),nrow=24,byrow = T))
df_parameters_diff <- as.data.frame(matrix(unlist(parameters_diff),nrow=24,byrow = T))


df_parameters <- setNames(df_parameters, c("p", "q", "P", "Q", "m", "d", "D"))
df_parameters_diff <- setNames(df_parameters_diff, c("p", "q", "P", "Q", "m", "d", "D"))
# write.csv2(df_parameters,"df_parameters.csv")
# write.csv2(df_parameters_diff,"df_parameters_diff.csv")

df_parameters_with_count <- ddply(df_parameters,.(p,q,P,Q,m,d,D),nrow)
df_parameters_with_count <- setNames(df_parameters_with_count, c("p", "q", "P", "Q", "m", "d", "D","count"))

df_parameters_diff_with_count <- ddply(df_parameters_diff,.(p,q,P,Q,m,d,D),nrow)
df_parameters_diff_with_count <- setNames(df_parameters_diff_with_count, c("p", "q", "P", "Q", "m", "d", "D","count"))

df_parameters_with_count <- df_parameters_with_count[order(-df_parameters_with_count$count),]
df_parameters_diff_with_count <- df_parameters_diff_with_count[order(-df_parameters_diff_with_count$count),]

write.csv2(df_parameters_with_count,"df_parameters_with_count.csv")
write.csv2(df_parameters_diff_with_count,"df_parameters_diff_with_count.csv")