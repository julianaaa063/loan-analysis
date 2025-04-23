# Install dan panggil package yang diperlukan
install.packages("readxl")   # Jika belum terinstall
install.packages("MVN")      # Untuk uji normalitas multivariat
library(readxl)
library(MVN)

# 1. Membaca data dari file Excel
data <- read_xlsx("C:/Users/lenov/Downloads/outlier.xlsx")

# 2. Menentukan nama kolom untuk setiap tahun (ganti sesuai dataset Anda)
tahun_list <- c("2021", "2022", "2023")

# 3. Looping untuk setiap tahun
for (tahun in tahun_list) {
  cat("\n========== Analisis Tahun", tahun, "==========\n")
  
  # Pilih subset data untuk tahun tertentu (ganti dengan nama kolom yang sesuai)
  data_tahun <- data[, c(paste0("Variabel1_", tahun), paste0("Variabel2_", tahun), paste0("Variabel3_", tahun))]
  
  # 4. Menghitung vektor rata-rata (μ) dan matriks kovarians (Σ)
  mean_vector <- colMeans(data_tahun, na.rm = TRUE)  # Menghitung mean tiap variabel
  cov_matrix <- cov(data_tahun, use = "complete.obs")  # Menghitung matriks kovarians
  
  # 5. Menghitung Jarak Mahalanobis
  mahalanobis_distance <- mahalanobis(data_tahun, mean_vector, cov_matrix)
  
  # 6. Menentukan batas outlier (Chi-square threshold)
  threshold <- qchisq(0.975, df=ncol(data_tahun))  # 97.5% untuk α = 0.05
  
  # 7. Menandai outlier
  outliers <- which(mahalanobis_distance > threshold)
  
  # 8. Menampilkan hasil
  if (length(outliers) > 0) {
    print(paste("Observasi yang termasuk outlier di tahun", tahun, ":", outliers))
  } else {
    print(paste("Tidak ada outlier di tahun", tahun))
  }
}
