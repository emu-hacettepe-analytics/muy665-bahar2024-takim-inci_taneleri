library(tidyverse)
library(lubridate)

# Aylık toplamları hesapla ve yenilenebilir enerji kaynaklarına göre ayır
monthly_renewables <- data %>%
  mutate(Ay = floor_date(Tarih, "month")) %>%
  group_by(Ay) %>%
  summarise(
    Barajli = sum(Barajli),
    Akarsu = sum(Akarsu),
    Ruzgar = sum(Ruzgar),
    Gunes = sum(Gunes),
    Jeotermal = sum(Jeotermal),
    Biyokutle = sum(Biyokutle),
    .groups = 'drop'
  )

# Erişilebilir format için veriyi uzun formata çevir
monthly_long <- pivot_longer(monthly_renewables, cols = c(Barajli, Akarsu, Ruzgar, Gunes, Jeotermal, Biyokutle),
                             names_to = "Enerji_Kaynagi", values_to = "Uretim")

# Grafik çiz
ggplot(monthly_long, aes(x = Ay, y = Uretim, fill = Enerji_Kaynagi)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Aylık Yenilenebilir Enerji Kaynaklarının Dağılımı",
       x = "Ay",
       y = "Toplam Üretim (MWh)",
       fill = "Enerji Kaynağı") +
  theme_minimal()
