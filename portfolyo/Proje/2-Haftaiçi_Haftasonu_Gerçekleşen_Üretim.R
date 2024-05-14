# Gerekli kütüphaneleri yükleyin
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales) # Binlik ayırıcılar için scales kütüphanesi

# Veriyi doğru kodlama ile okuyun
data <- read.csv("../Downloads/ElkUrtm23.csv", fileEncoding="UTF-8-BOM")

# Sütun adlarını düzeltin
names(data)[names(data) == "ï..Tarih"] <- "Tarih"

# Tarih sütununu date tipine çevirin
data$Tarih <- as.Date(data$Tarih, format="%d.%m.%Y")

# NA değerlerini veri setinden çıkarın
data <- na.omit(data)

# Hafta sonu/hafta içi ve ay bilgilerini çıkarın
data$GunTipi <- ifelse(wday(data$Tarih) %in% c(1, 7), "Hafta Sonu", "Hafta İçi")
data$Ay <- month(data$Tarih, label = TRUE, locale = "Turkish")

# Ay ve gün tipine göre gruplayın ve toplamı alın, gruplamayı kaldırın
monthly_data <- data %>%
  group_by(Ay, GunTipi) %>%
  summarise(Toplam = sum(Toplam, na.rm = TRUE), .groups = "drop")

# Grafik çizin
ggplot(monthly_data, aes(x=Ay, y=Toplam, fill=GunTipi)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Aylık Toplam Elektrik Üretimi: Hafta İçi vs Hafta Sonu",
       x="Ay", y="Toplam Elektrik (MWh)") +
  scale_y_continuous(labels = scales::comma) + # Binlik ayırıcılar ekleniyor
  scale_fill_manual(values=c("darkblue", "orange")) +
  theme_minimal()

