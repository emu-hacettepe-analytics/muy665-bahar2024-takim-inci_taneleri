# Gerekli kütüphaneleri yükleyin
library(ggplot2)
library(dplyr)
library(tidyr)

# Veriyi doğru kodlama ile okuyun
data <- read.csv("../Downloads/ElkUrtm23.csv", fileEncoding="UTF-8-BOM")

# Sütun adlarını düzeltin
names(data)[names(data) == "ï..Tarih"] <- "Tarih"

# Tarih dönüşümünü kontrol edin
data$Tarih <- as.Date(data$Tarih, format = "%d.%m.%Y")

# Ay ve mevsim bilgilerini ekleyin
data$Ay <- as.numeric(format(data$Tarih, "%m"))
data$Mevsim <- ifelse(data$Ay %in% c(12, 1, 2), "Kış",
                      ifelse(data$Ay %in% c(3, 4, 5), "İlkbahar",
                             ifelse(data$Ay %in% c(6, 7, 8), "Yaz", "Sonbahar")))

# Veriyi uzun formatına çevirin
long_data <- data %>%
  select(Mevsim, DogalGaz, Barajli, Linyit, Akarsu, IthalKomur, Ruzgar, Gunes, FuelOil, Jeotermal, TasKomur, Biyokutle, Uluslararasi, AtikIsi) %>%
  pivot_longer(cols = -Mevsim, names_to = "UretimTuru", values_to = "Miktar")

# Mevsimlere ve üretim türlerine göre gruplayıp toplam üretimi hesaplayın
seasonal_data <- long_data %>%
  group_by(Mevsim, UretimTuru) %>%
  summarise(ToplamMiktar = sum(Miktar, na.rm = TRUE))

# Grafik çizdirin
ggplot(seasonal_data, aes(x = Mevsim, y = ToplamMiktar, fill = UretimTuru)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = label_comma()) +  # y eksenindeki sayıları virgülle ayrılmış biçimde göster
  labs(title = "Mevsimlere Göre Enerji Üretim Türleri",
       x = "Mevsim",
       y = "Toplam Üretim (MWh)",
       fill = "Üretim Türü") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

