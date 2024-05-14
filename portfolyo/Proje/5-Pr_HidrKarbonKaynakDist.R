library(tidyverse)
library(lubridate)


# Aylık toplamları hesapla ve hidrokarbon kaynaklarına göre ayır
monthly_hydrocarbons <- data %>%
  mutate(Ay = floor_date(Tarih, "month")) %>%
  group_by(Ay) %>%
  summarise(
    DogalGaz = sum(DogalGaz),
    IthalKomur = sum(IthalKomur),
    FuelOil = sum(FuelOil),
    TasKomur = sum(TasKomur),
    .groups = 'drop'
  )

# Erişilebilir format için veriyi uzun formata çevir
monthly_long <- pivot_longer(monthly_hydrocarbons, cols = c(DogalGaz, IthalKomur, FuelOil, TasKomur),
                             names_to = "Enerji_Kaynagi", values_to = "Uretim")

# Grafik çiz
ggplot(monthly_long, aes(x = Ay, y = Uretim, fill = Enerji_Kaynagi)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Aylık Hidrokarbon Enerji Kaynaklarının Dağılımı",
       x = "Ay",
       y = "Toplam Üretim (MWh)",
       fill = "Enerji Kaynağı") +
  theme_minimal()

