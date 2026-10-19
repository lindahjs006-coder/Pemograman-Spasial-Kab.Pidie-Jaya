# ==============================================================
# EVALUASI TENGAH SEMESTER PEMROGRAMAN SPASIAL
# Tahap 5: Ekspor Hasil Klasifikasi untuk Aplikasi Shiny
# Wilayah Studi: Kabupaten Pidie Jaya, Provinsi Aceh
# --------------------------------------------------------------
# Anggota 1 : Aisyah Meylinda Sari (5016241115)
# Anggota 2 : Nabilla Putri Badrina (5016241130)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi :
# 1. Membaca hasil klasifikasi dan batas administrasi
# 2. Menghitung luas tiap kelas per kecamatan dan per kabupaten (km2)
# 3. Menyimpan batas kecamatan dan kabupaten dalam format GeoPackage
# 4. Mengubah raster klasifikasi menjadi poligon ringan (WGS84)
# --------------------------------------------------------------
# Catatan:
# Jalankan setelah 03_classify.R dan 04_zonal.R.
# Seluruh keluaran dibaca oleh 06_shiny.R.
# --------------------------------------------------------------
# Data:
# - pidiejaya_kelas.tif
# - Batas administrasi kecamatan dan kabupaten skala 1:50.000 (shapefile)
# ==============================================================


# ==============================================================
# 0. PERSIAPAN
# ==============================================================

# --- 0.1 Pengaturan PROJ ---
# Perbaikan galat PROJ yang bentrok dengan PostgreSQL.
# Harus dijalankan sebelum library(terra)
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")
Sys.setenv(PROJ_LIB  = system.file("proj", package = "terra"))
Sys.setenv(PROJ_DATA = system.file("proj", package = "terra"))

# --- 0.2 Memanggil package ---
library(terra)   # data raster dan vektor
library(sf)      # data vektor (penyimpanan GeoPackage)

# --- 0.3 Pengaturan memori dan folder output ---
terraOptions(memfrac = 0.8, todisk = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)


# ==============================================================
# 1. MENYIAPKAN DATA
# ==============================================================

# --- 1.1 Membaca hasil klasifikasi ---
nama_kelas <- c("Vegetasi", "Terbangun/Lahan terbuka", "Perairan/Tambak", "Awan")
kelas <- rast("data/processed/pidiejaya_kelas.tif")

# --- 1.2 Batas kecamatan ---
kec <- vect("data/raw/batas/ADMINISTRASIKECAMATAN_AR_50K.shp")   # SESUAIKAN
kec <- kec[toupper(gsub(" ", "", kec$WADMKK)) == "PIDIEJAYA", ]
kec <- project(kec, crs(kelas))
kec$kecamatan <- tools::toTitleCase(tolower(kec$NAMOBJ))
stopifnot(nrow(kec) == 8)      # Pidie Jaya memiliki 8 kecamatan

# --- 1.3 Batas kabupaten ---
kab <- vect("data/raw/batas/ADMINISTRASIKABUPATEN_AR_50K.shp")   # SESUAIKAN
kab <- kab[toupper(gsub(" ", "", kab$WADMKK)) == "PIDIEJAYA", ]
kab <- project(kab, crs(kelas))


# ==============================================================
# 2. PERHITUNGAN LUAS KELAS
# ==============================================================

# --- 2.1 Luas per kecamatan ---
# Satuan km2 (1 piksel = 100 m2)
hasil <- lapply(seq_len(nrow(kec)), function(i) {
  f   <- freq(mask(crop(kelas, kec[i]), kec[i]))
  cnt <- f$count[match(1:4, f$value)]
  cnt[is.na(cnt)] <- 0
  cnt
})
km2 <- do.call(rbind, hasil) * 100 / 1e6

luas_kec <- data.frame(Kecamatan = kec$kecamatan, round(km2, 2), check.names = FALSE)
names(luas_kec)[2:5] <- nama_kelas
luas_kec$Total <- rowSums(luas_kec[, 2:5])
write.csv(luas_kec, "output/luas_kelas_per_kecamatan.csv", row.names = FALSE)

# --- 2.2 Luas seluruh kabupaten ---
tot  <- colSums(km2)
luas <- data.frame(kelas = nama_kelas, luas_km2 = round(tot, 2),
                   persen = round(100 * tot / sum(tot), 2))
write.csv(luas, "output/luas_kelas_pidiejaya.csv", row.names = FALSE)


# ==============================================================
# 3. PENYIMPANAN BATAS ADMINISTRASI
# ==============================================================
# Format GeoPackage, CRS UTM zona 46N

st_write(st_as_sf(kec)[, "kecamatan"], "output/kecamatan_zonal_pidiejaya.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
st_write(st_as_sf(kab)[, 1], "output/batas_kab_pidiejaya.gpkg",
         delete_dsn = TRUE, quiet = TRUE)


# ==============================================================
# 4. KLASIFIKASI MENJADI POLIGON
# ==============================================================
# Raster 10 m terlalu berat untuk peta interaktif, sehingga diubah
# menjadi poligon: resolusi 30 m, disederhanakan, dan diproyeksikan
# ke WGS84.

# --- 4.1 Agregasi raster ---
# fact = 3 -> piksel 10 m menjadi 30 m (nilai modus)
kelas_k <- aggregate(kelas, fact = 3, fun = "modal", na.rm = TRUE)

# --- 4.2 Konversi ke poligon ---
pk <- as.polygons(kelas_k, dissolve = TRUE)
names(pk) <- "kode"
pk <- st_as_sf(pk)

# --- 4.3 Penyederhanaan dan transformasi CRS ---
pk <- st_make_valid(st_simplify(pk, dTolerance = 30))
pk <- st_transform(pk, 4326)

# --- 4.4 Menyimpan hasil ---
st_write(pk, "output/klasifikasi_poligon_wgs84.gpkg",
         delete_dsn = TRUE, quiet = TRUE)


# ==============================================================
# 5. RINGKASAN
# ==============================================================
print(luas); print(luas_kec)