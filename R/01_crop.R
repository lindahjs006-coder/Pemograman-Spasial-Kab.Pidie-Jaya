# ==============================================================
# EVALUASI TENGAH SEMESTER PEMROGRAMAN SPASIAL
# Tahap 1: Pemotongan Citra Sentinel-2 L2A
# Wilayah Studi: Kabupaten Pidie Jaya, Provinsi Aceh
# --------------------------------------------------------------
# Anggota 1 : Aisyah Meylinda Sari (5016241115)
# Anggota 2 : Nabilla Putri Badrina (5016241130)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi :
# 1. Membaca band B02, B03, B04, B05, B08, B11 dan SCL
# 2. Menyamakan resolusi seluruh band menjadi 10 m
# 3. Mengonversi nilai DN menjadi reflektansi
# 4. Memotong citra menggunakan batas Kabupaten Pidie Jaya
# 5. Menyimpan hasil pemotongan ke folder data/processed
# --------------------------------------------------------------
# Data:
# - Sentinel-2 L2A tile T46NHL, akuisisi 29 November 2025
# - Batas administrasi kabupaten skala 1:50.000 (shapefile)
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
library(sf)      # data vektor


# ==============================================================
# 1. PEMBACAAN BAND SENTINEL-2
# ==============================================================

# --- 1.1 Lokasi data citra ---
safe <- "data/raw/sentinel2/S2B_MSIL2A_20251129T035019_N0511_R104_T46NHL_20251129T060330.SAFE"

# --- 1.2 Fungsi pembaca band ---
# Mencari file .jp2 pada folder .SAFE berdasarkan kode band dan
# resolusi. Proses berhenti jika file tidak ditemukan atau ganda.
baca_band <- function(kode, res) {
  f <- list.files(safe, pattern = paste0("_", kode, "_", res, "m\\.jp2$"),
                  recursive = TRUE, full.names = TRUE)
  if (length(f) != 1) stop("Band ", kode, "_", res, "m tidak ditemukan/ganda di ", safe)
  rast(f)
}

# --- 1.3 Band resolusi 10 m ---
# B02 (biru), B03 (hijau), B04 (merah), B08 (NIR)
b2  <- baca_band("B02", 10)
b3  <- baca_band("B03", 10)
b4  <- baca_band("B04", 10)
b8  <- baca_band("B08", 10)

# --- 1.4 Band resolusi 20 m ---
# B05 (red edge), B11 (SWIR), SCL (acuan penentuan threshold)
b5  <- baca_band("B05", 20)
b11 <- baca_band("B11", 20)
scl <- baca_band("SCL", 20)      # berisi kode kelas, bukan reflektansi


# ==============================================================
# 2. PRA-PEMROSESAN
# ==============================================================

# --- 2.1 Penyamaan resolusi ---
# Band 20 m di-resample ke 10 m mengikuti grid B04
b5  <- resample(b5,  b4, method = "bilinear")
b11 <- resample(b11, b4, method = "bilinear")
scl <- resample(scl, b4, method = "near")      # data kelas wajib memakai "near"

# --- 2.2 Penggabungan band ---
citra <- c(b2, b3, b4, b5, b8, b11)
names(citra) <- c("B02", "B03", "B04", "B05", "B08", "B11")

# --- 2.3 Konversi ke reflektansi ---
# Baseline N0511: offset 1000, faktor skala 10000
citra <- (citra - 1000) / 10000
citra[citra < 0] <- 0     # nilai negatif diubah menjadi 0


# ==============================================================
# 3. BATAS ADMINISTRASI
# ==============================================================

# --- 3.1 Membaca shapefile kabupaten ---
kab <- st_read("data/raw/batas/ADMINISTRASIKABUPATEN_AR_50K.shp")
names(kab)

# --- 3.2 Memilih Kabupaten Pidie Jaya ---
pidiejaya <- kab[kab$WADMKK == "Pidie Jaya", ]      
nrow(pidiejaya)                                     

# --- 3.3 Menyamakan CRS dengan citra ---
pidiejaya_v <- project(vect(pidiejaya), crs(citra))

# Visualisasi citra dan batas kabupaten
plotRGB(citra, r = 3, g = 2, b = 1, stretch = "lin")
lines(pidiejaya_v, col = "red", lwd = 2)


# ==============================================================
# 4. PEMOTONGAN CITRA
# ==============================================================

# --- 4.1 Crop dan mask ---
# crop : memotong sesuai kotak batas kabupaten
# mask : piksel di luar batas kabupaten menjadi NA
citra_pj <- crop(citra, pidiejaya_v) |> mask(pidiejaya_v)
scl_pj   <- crop(scl,   pidiejaya_v) |> mask(pidiejaya_v)
names(scl_pj) <- "SCL"

# --- 4.2 Menyimpan hasil ---
dir.create("data/processed", showWarnings = FALSE)
writeRaster(citra_pj, "data/processed/pidiejaya_crop.tif", overwrite = TRUE)
writeRaster(scl_pj,   "data/processed/pidiejaya_scl.tif",  overwrite = TRUE,
            datatype = "INT1U")      # disimpan sebagai integer (data kelas)

# --- 4.3 Visualisasi hasil ---
plotRGB(citra_pj, r = 3, g = 2, b = 1, stretch = "lin")