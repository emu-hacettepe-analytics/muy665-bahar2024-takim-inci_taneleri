library(tidyverse)


# Veriyi İthal ve Yerli olarak grupla
data_long <- pivot_longer(data, 
                          cols = c("DogalGaz", "Linyit", "IthalKomur", "FuelOil", "TasKomur", "Uluslararasi",
                                   "Barajli", "Akarsu", "Ruzgar", "Gunes", "Jeotermal", "Biyokutle", "AtikIsi"),
                          names_to = "Enerji_Kaynagi", values_to = "Uretim")

# Enerji kaynaklarını İthal ve Yerli olarak etiketle
data_long <- data_long %>%
  mutate(Grup = case_when(
    Enerji_Kaynagi %in% c("DogalGaz", "Linyit", "IthalKomur", "FuelOil", "TasKomur", "Uluslararasi") ~ "İthal",
    TRUE ~ "Yerli"
  ))

# Yıllık toplamları hesapla
yearly_totals <- data_long %>%
  group_by(Grup) %>%
  summarise(ToplamUretim = sum(Uretim, na.rm = TRUE))

# Bar grafiği çiz
ggplot(yearly_totals, aes(x = Grup, y = ToplamUretim, fill = Grup)) +
  geom_col() +
  labs(title = "Yıllık Toplam Üretim: İthal vs. Yerli",
       x = "Grup",
       y = "Toplam Üretim (MWh)") +
  scale_fill_manual(values = c("İthal" = "red", "Yerli" = "green")) +
  theme_minimal()
