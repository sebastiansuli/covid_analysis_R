# COVID-19 in Poland analysis; the script is part of a larger project, 
#mainly dealing with tableau and tableau integration with r

library(dplyr)  
library(tidyr) 
library(maptools)
library(rgdal) #read_org (better than read_sf)
library(rgeos) #spatial data support
library(sf) #just in case
library(ggplot2) #viz


#files with cases, deaths, population1 and population2

zakazenia <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane/zakazenia.csv", 
                      header = TRUE, sep=',') 
zgony <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane/ewp_dsh_zgony_po_szczep_202203080932.csv", 
                  header = TRUE, sep=',') 
lud_pow <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane/ludnosc_pow — kopia.csv", 
                    header = TRUE, sep=';') 
lud_woj <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane/ludnosc_woj — kopia.csv", 
                    header = TRUE, sep=';') 

head(zakazenia)
head(zgony)
summary(zakazenia)
summary(zgony)


#(that's why ogr is better)

# start_time <- Sys.time()
# shp <- read_sf("C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane","Województwa") #shp file
# readOGR_time <- Sys.time() - start_time
# readOGR_time
# start_time <- Sys.time()
# csv <- read.csv("C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane/zakazenia.csv")
# read_sf_time <- Sys.time() - start_time
# read_sf_time


#provinces; spatial data for map vizzes

pow <- readOGR("C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane","Powiaty")
woj <- readOGR("C:/Users/sebas/OneDrive/Pulpit/praca_lic/Dane","Województwa")

pow <- spTransform(pow, CRS("+init=epsg:4326"))
woj <- spTransform(woj, CRS("+init=epsg:4326"))


#maps of Poland; counties/provinces

plot(pow)
plot(woj)


#char to data

str(zakazenia) #"2022-03-07" - string (should be date format)
zakazenia$data_rap_zakazenia <- as.factor(zakazenia$data_rap_zakazenia)
zakazenia$data_rap_zakazenia <- as.Date(zakazenia$data_rap_zakazenia,format="%Y-%m-%d")

zgony$data_rap_zgonu <- as.factor(zgony$data_rap_zgonu)
zgony$data_rap_zgonu <- as.Date(zgony$data_rap_zgonu,format="%Y-%m-%d")

str(zakazenia)


#transform territory codes; 2, 4 etc. to 02, 04...needed for join with spatial data

zakazenia$teryt_woj <- formatC(zakazenia$teryt_woj, width = 2, format = "d", flag = "0")
zakazenia$teryt_pow <- formatC(zakazenia$teryt_pow, width = 4, format = "d", flag = "0")
zgony$teryt_woj <- formatC(zgony$teryt_woj, width = 2, format = "d", flag = "0")
zgony$teryt_pow <- formatC(zgony$teryt_pow, width = 4, format = "d", flag = "0")

as.character(zakazenia$teryt_woj)
str(zakazenia)


#group by territory

ag_zgony <- zgony %>%
  group_by(teryt_woj) %>%
  summarise(liczba_zaraportowanych_zgonow = sum(liczba_zaraportowanych_zgonow)) %>%
  ungroup()

ag_zakazenia <- zakazenia %>%
  group_by(teryt_pow) %>%
  summarise(liczba_zaraportowanych_zakazonych = sum(liczba_zaraportowanych_zakazonych)) %>%
  ungroup()

#viz

plot(zakazenia$data_rap_zakazenia, zakazenia$liczba_zaraportowanych_zakazonych)
plot(ag_zakazenia$Group.1, log(ag_zakazenia$x), xlab = "Data", 
     ylab = "Potwierdzone zakażenia", main = "Potwierdzone zakażenia, log",
     type = "b", col = "red", lwd = 3, pch = 10)

#line cases
ggplot(ag_zakazenia, aes(x=data_rap_zakazenia, y=liczba_zaraportowanych_zakazonych)) + 
  geom_line() + theme_classic() +
  labs(title = „Confirmed cases”, x = „Date”, y = „Daily confirmed cases”)

#line deaths
ggplot(ag_zgony, aes(x=data_rap_zgonu, y=liczba_zaraportowanych_zgonow)) + 
  geom_line() + theme_classic() +
  labs(title = „Confirmed deaths”, x = „Date”, y = „Daily confirmed deaths”)

ggplot(ag_zgony2, aes(x=kat_wiek, y=liczba_zaraportowanych_zgonow, fill = kat_wiek)) + geom_col() + theme_minimal() +
  labs(title = "Potwierdzone zgony wywołane chorobą COVID-19 w Polsce w podziale na kategorie wiekowe",
       x = "Kategoria wiekowa", y = "Suma potwierdzonych zgonów")

ggplot(ag_zakazenia, aes(x=data_rap_zakazenia, y=liczba_zaraportowanych_zakazonych, color=plec)) + 
  geom_point() + theme_minimal() +
  labs(title = "Potwierdzone zakażenia wywołane chorobą COVID-19 w Polsce w podziale na płci",
       x = "Data", y = "Suma potwierdzonych zakażeń")

woj_mapa2 %>%
  ggplot() +
  geom_polygon(aes(long, lat, group=group, fill=liczba_zaraportowanych_zakazonych), color="gray40") +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0) +
  coord_map() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

pow_mapa %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = liczba_zaraportowanych_zakazonych), color = "gray") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_map() +
  theme_void()

woj_mapa2 %>%
  ggplot() +
  geom_sf(aes(long, lat, liczba_zaraportowanych_zakazonych, pch = 21, size = n, fill = alpha("red", 0.7)))

ggplot(ag_zakazenia$liczba_zaraportowanych_zakazonych) +
  geom_polygon(aes(_df$long, pow_df$lat, group=TERYT, fill=TERYT), color="gray", show.legend = FALSE) +
  coord_map() +
  theme_void()


#arima model (to improve)

tsdata <- ts(ag_zgony$liczba_zaraportowanych_zgonow, frequency = 3)
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)

mymodel <- auto.arima(ag_zgony$liczba_zaraportowanych_zgonow)
mymodel
plot.ts(mymodel$residuals)
myforecast <- forecast(mymodel, level=c(95), h=10*3)
plot(myforecast)

