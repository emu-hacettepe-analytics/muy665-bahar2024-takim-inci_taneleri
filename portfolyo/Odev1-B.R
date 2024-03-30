# mtcars veri setini yükle
installed.packages("library")
installed.packages("mtcars")
# mtcars veri setini yükle


# Özel Özet Fonksiyonu Yazın
custom_summary <- function(vec) {
  summary_list <- list(
    Ortalama = mean(vec),
    Medyan = median(vec),
    Standart_Sapma = sd(vec),
    Minimum = min(vec),
    Maksimum = max(vec)
  )
  return(summary_list)
}

# Fonksiyonu Döngü Kullanarak Uygulama
cat("Fonksiyonu Döngü Kullanarak Uygulama:\n")
for (col_name in colnames(mtcars)) {
  cat(col_name, ":\n")
  cat("------------------------------\n")
  result <- custom_summary(mtcars[[col_name]])
  print(result)
  cat("\n")
}

# apply ile Alternatif Bir Yaklaşım
cat("\napply ile Alternatif Bir Yaklaşım:\n")
apply(mtcars, 2, custom_summary)

