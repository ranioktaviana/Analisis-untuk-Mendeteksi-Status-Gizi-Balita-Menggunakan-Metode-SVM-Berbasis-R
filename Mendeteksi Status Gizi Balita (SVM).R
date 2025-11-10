# Kelompok1
# Praktikum R - Klasifikasi SVM Data Balita

# =======================
# 1. Load Library
# =======================
library(readxl)
library(caret)
library(e1071)
library(ggplot2)
library(reshape2)
library(dplyr)
library(GGally)   

# =======================
# 2. Membaca Dataset
# =======================
data_balita <- read_excel("D:/Semester 5/Matkom/R/data_balita.xlsx")
View(data_balita)

# Pastikan nama kolom sesuai
colnames(data_balita)

# Ubah variabel target menjadi faktor (karena klasifikasi)
data_balita$`Status Gizi` <- as.factor(data_balita$`Status Gizi`)

# =======================
# 3. Eksplorasi Data
# =======================
# Pilih variabel numerik untuk analisis korelasi
num_data <- data_balita[, sapply(data_balita, is.numeric)]

# =======================
# 🔹 Tambahan: Grafik Korelasi Antar Variabel (Seperti Gambar 4)
# =======================
# Pastikan data numerik tidak kosong
if (ncol(num_data) > 1) {
  ggpairs(
    data = data_balita,
    columns = which(sapply(data_balita, is.numeric)), # ambil kolom numerik
    aes(color = `Status Gizi`, alpha = 0.7),
    title = "Correlation Plot of Variables (Status Gizi)"
  )
} else {
  print("Tidak ada cukup variabel numerik untuk membuat grafik korelasi.")
}

# =======================
# 10. Statistik Deskriptif per Kategori Status Gizi
# =======================
stat_gizi <- data_balita %>%
  group_by(`Status Gizi`) %>%
  summarise(
    Jumlah = n(),
    Rata_rata = mean(`Tinggi Badan (cm)`, na.rm = TRUE),
    Median = median(`Tinggi Badan (cm)`, na.rm = TRUE),
    Modus_Tinggi_Badan = modus(`Tinggi Badan (cm)`),
    Std_Deviasi = sd(`Tinggi Badan (cm)`, na.rm = TRUE),
    Minimum = min(`Tinggi Badan (cm)`, na.rm = TRUE),
    Maksimum = max(`Tinggi Badan (cm)`, na.rm = TRUE)
  )

View(stat_gizi)

# =======================
# 11. Statistik Deskriptif Tambahan (Range, Varians, SD)
# =======================
range_umur <- max(data_balita$`Umur (bulan)`, na.rm = TRUE) - min(data_balita$`Umur (bulan)`, na.rm = TRUE)
range_tinggi <- max(data_balita$`Tinggi Badan (cm)`, na.rm = TRUE) - min(data_balita$`Tinggi Badan (cm)`, na.rm = TRUE)

var_umur <- var(data_balita$`Umur (bulan)`, na.rm = TRUE)
var_tinggi <- var(data_balita$`Tinggi Badan (cm)`, na.rm = TRUE)

sd_umur <- sd(data_balita$`Umur (bulan)`, na.rm = TRUE)
sd_tinggi <- sd(data_balita$`Tinggi Badan (cm)`, na.rm = TRUE)

cat("=== Statistik Deskriptif Tambahan Data Balita ===\n")
cat("Umur (bulan):\n")
cat("  Range          :", range_umur, "\n")
cat("  Varians        :", var_umur, "\n")
cat("  Standar Deviasi:", sd_umur, "\n\n")

cat("Tinggi Badan (cm):\n")
cat("  Range          :", range_tinggi, "\n")
cat("  Varians        :", var_tinggi, "\n")
cat("  Standar Deviasi:", sd_tinggi, "\n")
