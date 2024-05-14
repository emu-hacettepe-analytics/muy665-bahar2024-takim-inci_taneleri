library(tidyverse)


# Enerji kaynaklarına göre toplamları hesapla
energy_totals <- colSums(data[, c("DogalGaz", "Barajli", "Linyit", "Akarsu", "IthalKomur", "Ruzgar", 
                                  "Gunes", "FuelOil", "Jeotermal", "TasKomur", "Biyokutle", 
                                  "Uluslararasi", "AtikIsi")])

# Toplam üretim
total_production <- sum(energy_totals)

# Yüzdelik hesaplamaları yap
energy_percentages <- energy_totals / total_production * 100

# Enerji kaynaklarını ve yüzdelerini bir dataframe'e çevir
energy_df <- data.frame(Enerji_Kaynagi = names(energy_percentages), Yuzde = energy_percentages)

# Pie chart çiz
pie(energy_df$Yuzde, labels = paste(energy_df$Enerji_Kaynagi, 
                                    sprintf("(%.1f%%)", energy_df$Yuzde)), 
    main = "Yıllık Enerji Üretiminin Kaynaklara Göre Dağılımı",
    col = rainbow(length(energy_df$Yuzde)))

# Efsane ekleme
legend("topright", legend = energy_df$Enerji_Kaynagi, fill = rainbow(length(energy_df$Yuzde)))
