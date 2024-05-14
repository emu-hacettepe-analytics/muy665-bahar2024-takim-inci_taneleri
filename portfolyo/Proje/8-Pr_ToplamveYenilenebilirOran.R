library(tidyverse)
library(lubridate)

# Veriyi yükle
data <- read.csv("ElkUrtm23.csv")
data$Tarih <- data$X...Tarih
data$X...Tarih <- NULL
# Tarih formatını düzelt
data$Tarih <- dmy(data$Tarih)

# Aylık toplamları hesapla
monthly_data <- data %>%
  mutate(Ay = floor_date(Tarih, "month")) %>%
  group_by(Ay) %>%
  summarise(
    Toplam = sum(Toplam),
    Yenilenebilir = sum(Barajli + Akarsu + Ruzgar + Gunes + Jeotermal + Biyokutle),
    .groups = 'drop'
  )

# Yüzdesel dağılımı hesapla
monthly_data$YenilenebilirYuzde = (monthly_data$Yenilenebilir / monthly_data$Toplam) * 100

# Grafik çiz
ggplot(monthly_data, aes(x = Ay)) +
  geom_col(aes(y = Toplam), fill = "blue", alpha = 0.7) +
  geom_col(aes(y = Yenilenebilir), fill = "green", alpha = 0.7) +
  labs(title = "Aylık Toplam ve Yenilenebilir Enerji Üretimi", x = "Ay", y = "Enerji Üretimi (MWh)") +
  theme_minimal()

# Yüzdelik dağılım grafiği
ggplot(monthly_data, aes(x = Ay, y = YenilenebilirYuzde, group = 1)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Yenilenebilir Enerji Üretiminin Yüzdesel Dağılımı", x = "Ay", y = "Yüzde (%)") +
  theme_minimal()



