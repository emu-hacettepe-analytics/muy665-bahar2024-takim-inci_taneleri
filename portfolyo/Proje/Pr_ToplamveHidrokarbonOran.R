library(tidyverse)
library(lubridate)

# Veriyi yükle
data <- read.csv("ElkUrtm23.csv")

# Tarih formatını düzelt
data$Tarih <- dmy(data$Tarih)

# Aylık toplamları hesapla
monthly_data <- data %>%
  mutate(Ay = floor_date(Tarih, "month")) %>%
  group_by(Ay) %>%
  summarise(
    Toplam = sum(Toplam),
    Hidrokarbon = sum(DogalGaz + IthalKomur + FuelOil + TasKomur),
    .groups = 'drop'
  )

# Yüzdesel dağılımı hesapla
monthly_data$HidrokarbonYuzde = (monthly_data$Hidrokarbon / monthly_data$Toplam) * 100

# Grafik çiz
ggplot(monthly_data, aes(x = Ay)) +
  geom_col(aes(y = Toplam), fill = "blue", alpha = 0.7) +
  geom_col(aes(y = Hidrokarbon), fill = "red", alpha = 0.7) +
  labs(title = "Aylık Toplam ve Hidrokarbon Enerji Üretimi", x = "Ay", y = "Enerji Üretimi (MWh)") +
  theme_minimal()

# Yüzdelik dağılım grafiği
ggplot(monthly_data, aes(x = Ay, y = HidrokarbonYuzde, group = 1)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Hidrokarbon Enerji Üretiminin Yüzdesel Dağılımı", x = "Ay", y = "Yüzde (%)") +
  theme_minimal()
