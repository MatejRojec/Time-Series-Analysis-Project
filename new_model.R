Alfragide_Amadora_2 <- readxl::read_excel("Data1/2021 Alfragide-Amadora.xlsx") %>%
  dplyr::rename(time = 'Alfragide/Amadora', ozon = 'Ozono (µg/m3)') %>%
  as.data.frame()   %>%
  mutate(date = as.Date(time)) 
Alfragide_Amadora$ozon <- na_interpolation(Alfragide_Amadora$ozon)
Alfragide_Amadora$time <- as.POSIXct(Alfragide_Amadora$time, format = "%Y-%m-%d %H:%M:%S")


Alfragide_Amadora_3 <- Alfragide_Amadora_2 %>% 
  group_by(date) %>%
  summarize(mean_ozon = mean(ozon))

Alfragide_Amadora_3 <- merge(Alfragide_Amadora_2, Alfragide_Amadora_3, by = "date")

Alfragide_Amadora_3$deviation <- Alfragide_Amadora_3$ozon - Alfragide_Amadora_3$mean_ozon

trend_model <- lm(deviation ~ as.numeric(as.POSIXlt(date)$hour), data = Alfragide_Amadora_3)
Alfragide_Amadora_3$detrended <- residuals(trend_model)


adf_test <- adf.test(Alfragide_Amadora$daily_seasonal)
print(adf_test)
