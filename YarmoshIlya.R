#дл€ региона 45 рассчитайте урожайность пшеницы в 2015 году
#вз€в дл€ расчЄта средние суммы активных температур за предыдущие 25 лет, 
#с 2 ближайших метеостанций но рассчитав колонку di самосто€тельно, как долю мес€ца,
#когда среднедневные температуры были выше 7 градусов,
#но учитыва€, что посев не может начатьс€ раньше середины апрел€, а вегетаци€ составл€ет 4 мес€ца

library(rnoaa)
library(lubridate)
library(tidyverse)

station_data = ghcnd_stations()
write_csv(station_data,file="station_data.csv")
station_data = read_csv("station_data.csv")
kurgan = data.frame(id = "KURGAN", 
                            latitude = 55.422574,  
                            longitude = 65.348391)
kurgan_stations = meteo_nearby_stations(lat_lon_df = kurgan,
                                     station_data = station_data,
                                     limit = 15,var = c("PRCP","TAVG"),
                                      year_min = 2015, 
                                      year_max = 2015)
                            
 
kurgan_id = kurgan_stations[["KURGAN"]][["id"]]
kurgan_id[1]
all_kurgan_data=meteo_tidy_ghcnd(stationid = kurgan_id)

write.csv(all_kurgan_data,file="all_kurgan_meteo.csv")
all_kurgan_data=read.csv(file="all_kurgan_meteo.csv")

all_kurgan_data=all_kurgan_data %>%
                         filter(id %in% kurgan_id[1:2])


all_kurgan_data  = all_kurgan_data %>% mutate( year = year(date), 
                                               month=month(date))   %>%
                                        filter(year>1989,year<2016)  %>%
                                        select(id,year,month,tavg)%>%
                                        mutate(tavg=tavg/10)%>%
                                        filter(tavg > 7)%>% 
                                        group_by(id,year,month)%>%
                                        summarise(tsum = sum(tavg),
                                            di = n()/31)%>%
                                        mutate(dicum = cumsum(di)-3)%>%
                                        filter(dicum < 1) %>% mutate(
                                          di = case_when(
                                          dicum > 0 ~ di-dicum,
                                          dicum <= 0 ~ di))%>%
                                        ungroup()%>%group_by(month)%>%
                                        summarise(tsum = mean(tsum),
                                                  di = mean(di)) %>%
                                        filter(di >0)
  
  
#m	afi	bfi	di
m = matrix(c(1,	 0,0,0,
             2,	 0,0,0,
             3,	 0,0,0,
             4,	 32.11,	11.30, 0.33,
             5,	 26.31,	9.26,	 1.00,
             6,	 25.64,	9.03,  1.00,
             7,	 23.20,	8.16,	 1.00,
             8,	 18.73,	6.59,	 0.32,
             9,	 16.30, 5.73,	 0,
             10, 13.83,	4.87,	 0,
             11, 0,0,0,
             12, 0,0,0), ncol = 4, byrow = T) 

m = as.data.frame(m)
names(m) = c("month",	"afi",	"bfi",	"di")
m = m %>% select(-di)


all_data= left_join(m,all_kurgan_data, by="month")

all_data[is.na(all_data)]=0

all_data = all_data %>% mutate(

Yj=((afi+bfi*tsum)*di)*300/(160*2.2*(100-25))/100 #t/ha
)

yield = sum(all_data$Yj)







