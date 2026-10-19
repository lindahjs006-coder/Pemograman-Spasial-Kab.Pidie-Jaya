# ==============================================================
# EVALUASI TENGAH SEMESTER PEMROGRAMAN SPASIAL
# Tahap 4: Statistik Zonal per Kecamatan
# Wilayah Studi: Kabupaten Pidie Jaya, Provinsi Aceh
# --------------------------------------------------------------
# Anggota 1 : Aisyah Meylinda Sari (5016241115)
# Anggota 2 : Nabilla Putri Badrina (5016241130)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi :
# 1. Membaca hasil klasifikasi dan batas kecamatan
# 2. Menghitung luas tiap kelas tutupan lahan per kecamatan
# 3. Menyusun tabel ringkas luas dan persentase per kecamatan
# 4. Membuat grafik komposisi dan peta persentase terbangun
# 5. Menghitung statistik zonal indeks (NDVI, NDBI, NDWI, NDCI)
# 6. Mengambil sampel piksel untuk histogram pada 06_shiny.R
# --------------------------------------------------------------
# Data:
# - pidiejaya_kelas.tif, pidiejaya_indeks.tif
# - Batas administrasi kecamatan skala 1:50.000 (shapefile)
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

# --- 0.2 Membersihkan grafik dan memanggil package ---
graphics.off()
library(terra)   # data raster dan vektor

# --- 0.3 Pengaturan memori dan folder output ---
# Opsional: jika kapasitas drive C: terbatas, arahkan berkas sementara
# ke drive lain dengan menghapus tanda komentar pada baris berikut
# terraOptions(tempdir = "D:/terra_tmp")
terraOptions(memfrac = 0.8, todisk = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)


# ==============================================================
# 1. MENYIAPKAN DATA
# ==============================================================

# --- 1.1 Membaca hasil klasifikasi ---
kelas <- rast("data/processed/pidiejaya_kelas.tif")
nama_kelas <- c("Vegetasi", "Terbangun/Lahan terbuka", "Perairan/Tambak", "Awan")
warna      <- c("#2E8B3E", "#D62728", "#1F77B4", "#BDBDBD")

# --- 1.2 Membaca batas kecamatan ---
kec <- vect("data/raw/batas/ADMINISTRASIKECAMATAN_AR_50K.shp")   # SESUAIKAN
kec <- kec[toupper(gsub(" ", "", kec$WADMKK)) == "PIDIEJAYA", ]

# --- 1.3 Menyamakan CRS dengan citra ---
# Dari WGS84 ke UTM zona 46N
kec <- project(kec, crs(kelas))
stopifnot(nrow(kec) == 8)      # Pidie Jaya memiliki 8 kecamatan
kec$kecamatan <- tools::toTitleCase(tolower(kec$NAMOBJ))


# ==============================================================
# 2. PERHITUNGAN PIKSEL PER KELAS PER KECAMATAN
# ==============================================================

# --- 2.1 Menghitung piksel ---
# Proses dilakukan satu kecamatan per putaran agar hemat memori
hasil <- lapply(seq_len(nrow(kec)), function(i) {
  k <- kec[i]
  r <- mask(crop(kelas, k), k)
  f <- freq(r)
  data.frame(kecamatan = k$kecamatan,
             kelas     = nama_kelas,
             count     = f$count[match(1:4, f$value)])
})
long <- do.call(rbind, hasil)
long$count[is.na(long$count)] <- 0

# --- 2.2 Konversi ke luas ---
long$luas_ha <- long$count * 100 / 10000          # 10 m x 10 m = 100 m2
long$kelas   <- factor(long$kelas, levels = nama_kelas)


# ==============================================================
# 3. TABEL RINGKAS
# ==============================================================

# --- 3.1 Tabel luas dan persentase (format lebar) ---
luas <- as.data.frame.matrix(xtabs(luas_ha ~ kecamatan + kelas, data = long))
luas <- luas[, nama_kelas]
total <- rowSums(luas)
persen <- round(100 * luas / total, 2)

ringkas <- data.frame(
  kecamatan = rownames(luas),
  total_ha  = round(total, 1),
  setNames(round(luas, 1),   paste0(nama_kelas, "_ha")),
  setNames(persen,           paste0(nama_kelas, "_persen")),
  check.names = FALSE, row.names = NULL
)

# --- 3.2 Persentase terhadap area teramati (tanpa awan) ---
# Lebih adil untuk kecamatan yang tertutup awan cukup luas
teramati <- total - luas[, "Awan"]
ringkas$Vegetasi_persen_tanpa_awan <- round(100 * luas[, "Vegetasi"] / teramati, 2)
ringkas$Terbangun_persen_tanpa_awan <-
  round(100 * luas[, "Terbangun/Lahan terbuka"] / teramati, 2)

# --- 3.3 Luas poligon sebagai pembanding ---
# Dihitung dari poligon pada proyeksi UTM (bukan dari kolom LUASWH)
ringkas$luas_poligon_ha <- round(
  expanse(kec, unit = "ha")[match(ringkas$kecamatan, kec$kecamatan)], 1)

# --- 3.4 Menyimpan hasil ---
write.csv(long,    "output/zonal_kecamatan_long.csv",  row.names = FALSE)
write.csv(ringkas, "output/zonal_kecamatan_ringkas.csv", row.names = FALSE)
print(ringkas[, c("kecamatan", "total_ha", paste0(nama_kelas, "_persen"))])


# ==============================================================
# 4. GRAFIK BATANG BERTUMPUK
# ==============================================================
# Persentase tiap kelas tutupan lahan per kecamatan

# --- 4.1 Fungsi grafik ---
# Kecamatan diurutkan berdasarkan persentase terbangun/lahan terbuka
urut <- order(persen[, "Terbangun/Lahan terbuka"])
gambar_grafik <- function() {
  layout(1)
  par(mar = c(5, 10, 7, 2))
  mp <- barplot(t(as.matrix(persen[urut, ])), horiz = TRUE, las = 1,
                col = warna, border = "white", xlim = c(0, 100),
                names.arg = rownames(persen)[urut], cex.names = 0.9,
                xlab = "Persentase luas kecamatan (%)", main = "")
  title(main = "Komposisi Tutupan Lahan per Kecamatan\nKabupaten Pidie Jaya",
        line = 4)
  usr <- par("usr")
  legend(x = 50, y = usr[4] + 0.04 * (usr[4] - usr[3]),
         legend = nama_kelas, fill = warna, ncol = 2,
         xjust = 0.5, yjust = 0, bty = "n", cex = 0.85, xpd = NA)
}

# --- 4.2 Menyimpan dan menampilkan grafik ---
png("output/grafik_kecamatan.png", width = 2800, height = 1800, res = 300)
gambar_grafik()
dev.off()
gambar_grafik()


# ==============================================================
# 5. PETA PERSENTASE TERBANGUN/LAHAN TERBUKA
# ==============================================================

# --- 5.1 Menambahkan atribut persentase ke batas kecamatan ---
kec$terbangun_pct <- persen[kec$kecamatan, "Terbangun/Lahan terbuka"]

# --- 5.2 Fungsi peta ---
gambar_peta_kec <- function() {
  layout(1)
  par(mar = c(3, 4, 4, 1))
  plot(kec, "terbangun_pct", type = "continuous",
       col = hcl.colors(20, "Reds", rev = TRUE),
       main = "Persentase Terbangun/Lahan Terbuka per Kecamatan",
       axes = TRUE)
  text(kec, labels = kec$kecamatan, cex = 0.6)
}

# --- 5.3 Menyimpan dan menampilkan peta ---
png("output/peta_terbangun_kecamatan.png", width = 2800, height = 2400, res = 300)
gambar_peta_kec()
dev.off()
gambar_peta_kec()


# ==============================================================
# 6. ZONAL STATISTICS INDEKS (NDVI, NDBI, NDWI, NDCI)
# ==============================================================
# Hasil: output/zonal_indeks_pidiejaya.csv dan output/sampel_indeks.csv
# (keduanya dibaca oleh 06_shiny.R).
# Agar hemat disk, proses dilakukan di memori, satu kecamatan per
# putaran, dan berkas sementara dibersihkan pada setiap putaran.

# --- 6.1 Pengaturan memori ---
graphics.off()
terraOptions(memfrac = 0.6, todisk = FALSE)       # jangan paksa menulis ke disk
tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)

# --- 6.2 Membaca raster indeks ---
indeks <- c("NDVI", "NDBI", "NDWI", "NDCI")
stat   <- c("mean", "min", "max", "median", "sd")

r_idx <- rast("data/processed/pidiejaya_indeks.tif")
names(r_idx) <- indeks
kec_idx <- project(kec, crs(r_idx))

# --- 6.3 Statistik per kecamatan ---
zonal_idx <- do.call(rbind, lapply(seq_len(nrow(kec_idx)), function(i) {
  k <- kec_idx[i]
  m <- values(mask(crop(r_idx, k), k))            # matriks piksel x 4 indeks
  baris <- unlist(lapply(indeks, function(ix) {
    x <- m[, ix]; x <- x[is.finite(x)]
    v <- c(mean(x), min(x), max(x), median(x), sd(x))
    names(v) <- paste0(ix, "_", stat)
    v
  }))
  rm(m); gc()
  tmpFiles(current = TRUE, remove = TRUE)
  message("zonal indeks selesai: ", k$kecamatan)
  data.frame(Kecamatan = k$kecamatan, as.list(baris), check.names = FALSE)
}))
write.csv(zonal_idx, "output/zonal_indeks_pidiejaya.csv", row.names = FALSE)

# --- 6.4 Sampel piksel untuk histogram ---
# Diambil 100.000 piksel secara acak
set.seed(1)
sampel <- spatSample(r_idx, size = 100000, method = "random", na.rm = TRUE)
write.csv(sampel, "output/sampel_indeks.csv", row.names = FALSE)

# --- 6.5 Pembersihan dan ringkasan ---
tmpFiles(current = TRUE, remove = TRUE)
print(zonal_idx[, c("Kecamatan", paste0(indeks, "_mean"))])