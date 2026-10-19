# ==============================================================
# EVALUASI TENGAH SEMESTER PEMROGRAMAN SPASIAL
# Tahap 2: Perhitungan Indeks Spektral
# Wilayah Studi: Kabupaten Pidie Jaya, Provinsi Aceh
# --------------------------------------------------------------
# Anggota 1 : Aisyah Meylinda Sari (5016241115)
# Anggota 2 : Nabilla Putri Badrina (5016241130)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi :
# 1. Membaca citra hasil crop dari tahap 1
# 2. Menghitung NDVI, NDBI, NDWI, dan NDCI
# 3. Menyimpan raster indeks ke folder data/processed
# 4. Membuat visualisasi indeks (gabungan dan per indeks)
# --------------------------------------------------------------
# Data:
# - Citra hasil crop Kab. Pidie Jaya (pidiejaya_crop.tif)
# ==============================================================


# ==============================================================
# 0. PERSIAPAN
# ==============================================================

# --- 0.1 Pengaturan PROJ ---
# Lokasi PROJ diarahkan ke folder bawaan package terra
# agar tidak terjadi galat pada proses transformasi koordinat
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")
Sys.setenv(PROJ_LIB  = system.file("proj", package = "terra"))
Sys.setenv(PROJ_DATA = system.file("proj", package = "terra"))

# --- 0.2 Memanggil package ---
library(terra)   # data raster


# ==============================================================
# 1. MEMBACA CITRA HASIL CROP
# ==============================================================

# --- 1.1 Membaca raster ---
citra <- rast("data/processed/pidiejaya_crop.tif")

# --- 1.2 Memisahkan band ---
b2  <- citra[["B02"]]
b3  <- citra[["B03"]]
b4  <- citra[["B04"]]
b5  <- citra[["B05"]]
b8  <- citra[["B08"]]
b11 <- citra[["B11"]]


# ==============================================================
# 2. PERHITUNGAN INDEKS SPEKTRAL
# ==============================================================

# --- 2.1 Fungsi normalized difference ---
# Piksel yang jumlahnya nol dijadikan NA agar tidak terjadi pembagian nol
nd <- function(a, b) {
  hasil <- (a - b) / (a + b)
  hasil[(a + b) == 0] <- NA
  hasil
}

# --- 2.2 Menghitung indeks ---
ndvi <- nd(b8,  b4)    # vegetasi : (NIR - Red) / (NIR + Red)
ndbi <- nd(b11, b8)    # bangunan : (SWIR - NIR) / (SWIR + NIR)
ndwi <- nd(b3,  b8)    # air      : (Green - NIR) / (Green + NIR) [McFeeters]
ndci <- nd(b5,  b4)    # klorofil : (RedEdge - Red) / (RedEdge + Red)

# --- 2.3 Menggabungkan indeks ---
indeks <- c(ndvi, ndbi, ndwi, ndci)
names(indeks) <- c("NDVI", "NDBI", "NDWI", "NDCI")


# ==============================================================
# 3. PENYIMPANAN HASIL
# ==============================================================

# --- 3.1 Menyimpan raster indeks ---
# Folder output dibuat dulu sebelum menulis file apa pun
dir.create("output", showWarnings = FALSE, recursive = TRUE)
writeRaster(indeks, "data/processed/pidiejaya_indeks.tif", overwrite = TRUE)

# --- 3.2 Ringkasan nilai ---
print(global(indeks, c("min", "mean", "max"), na.rm = TRUE))


# ==============================================================
# 4. VISUALISASI INDEKS
# ==============================================================

# --- 4.1 Gambar gabungan (4 panel) ---
png("output/indeks_spektral.png", width = 2400, height = 2000, res = 200)
plot(indeks, nr = 2)
dev.off()
plot(indeks, nr = 2)   # tampil di panel Plots

# --- 4.2 Gambar per indeks ---
for (i in names(indeks)) {
  # simpan ke file
  png(paste0("output/", i, ".png"), width = 2000, height = 1800, res = 200)
  plot(indeks[[i]], main = i)
  dev.off()
  
  # tampil di panel Plots
  plot(indeks[[i]], main = i)
}