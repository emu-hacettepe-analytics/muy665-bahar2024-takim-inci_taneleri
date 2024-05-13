# Gerekli kütüphaneleri yükleyin
library(tidyverse)
library(lubridate)

# Verileri okuyun
data <- read.csv("ElkUrtm23.csv")

# 'Tarih' sütununu tarih formatına çevirin ve mevsimleri ekleyin
data$Tarih <- dmy(data$Tarih)
data$Mevsim <- ifelse(month(data$Tarih) %in% c(12, 1, 2), "Kış",
                      ifelse(month(data$Tarih) %in% c(3, 4, 5), "İlkbahar",
                             ifelse(month(data$Tarih) %in% c(6, 7, 8), "Yaz", "Sonbahar")))

# Saat ve mevsime göre gruplayarak ortalama toplam üretimi hesaplayın
seasonal_averages <- data %>%
  group_by(Saat, Mevsim) %>%
  summarise(OrtalamaToplam = mean(Toplam, na.rm = TRUE))

# Bar grafiği çizin
ggplot(seasonal_averages, aes(x = Saat, y = OrtalamaToplam, fill = Mevsim)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mevsimlere Göre Aylık Saatlik Elektrik Üretim Ortalamaları",
       x = "Saat",
       y = "Ortalama Üretim (MW)")
seasonal_averages[1,]<- NULL
