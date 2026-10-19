# ==============================================================
# EVALUASI TENGAH SEMESTER PEMROGRAMAN SPASIAL
# Tahap 3: Penentuan Threshold, Klasifikasi Tutupan Lahan, dan Validasi
# Wilayah Studi: Kabupaten Pidie Jaya, Provinsi Aceh
# --------------------------------------------------------------
# Anggota 1 : Aisyah Meylinda Sari (5016241115)
# Anggota 2 : Nabilla Putri Badrina (5016241130)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi :
# 1. Membaca citra, indeks spektral, dan SCL dari tahap sebelumnya
# 2. Menentukan threshold dengan acuan SCL
# 3. Mengklasifikasikan tutupan lahan menjadi 4 kelas
#    (vegetasi, terbangun/lahan terbuka, perairan/tambak, awan)
# 4. Membuat peta klasifikasi
# 5. Memvalidasi hasil klasifikasi terhadap SCL
# --------------------------------------------------------------
# Data:
# - pidiejaya_crop.tif, pidiejaya_indeks.tif, pidiejaya_scl.tif
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

# --- 0.2 Memanggil package ---
library(terra)   # data raster dan vektor

# --- 0.3 Folder output ---
dir.create("output", showWarnings = FALSE, recursive = TRUE)


# ==============================================================
# 1. MENYIAPKAN DATA
# ==============================================================

# --- 1.1 Membaca citra dan indeks ---
citra  <- rast("data/processed/pidiejaya_crop.tif")
indeks <- rast("data/processed/pidiejaya_indeks.tif")
ndvi <- indeks[["NDVI"]]; ndbi <- indeks[["NDBI"]]
ndwi <- indeks[["NDWI"]]; ndci <- indeks[["NDCI"]]
b2   <- citra[["B02"]]

# --- 1.2 Skala reflektansi ---
# Kalau nilainya masih 0-10000, diubah menjadi 0-1
if (global(b2, "max", na.rm = TRUE)[[1]] > 2) {
  b2 <- b2 / 10000
}

# --- 1.3 Membaca SCL ---
# SCL = Scene Classification Layer Sentinel-2 L2A (hasil Sen2Cor).
# Dipakai sebagai acuan pembanding, bukan kebenaran lapangan.
f_scl <- "data/processed/pidiejaya_scl.tif"
ada_scl <- file.exists(f_scl)
if (ada_scl) {
  scl <- rast(f_scl)
  if (!compareGeom(indeks, scl, stopOnError = FALSE)) scl <- resample(scl, indeks, method = "near")
} else {
  message("File SCL belum ada -> bagian B dan E dilewati. ",
          "Jalankan ulang 01_crop.R (versi baru) untuk membuatnya.")
}
nama_scl <- c("1" = "Saturasi", "2" = "Gelap", "3" = "Bayangan awan",
              "4" = "Vegetasi", "5" = "Non-vegetasi", "6" = "Air",
              "7" = "Tak terklasifikasi", "8" = "Awan sedang",
              "9" = "Awan tinggi", "10" = "Cirrus")


# ==============================================================
# 2. DASAR PENENTUAN THRESHOLD (ACUAN SCL)
# ==============================================================
# Hasil: output/indeks_per_kelas_SCL.csv dan output/uji_threshold_SCL.csv
if (ada_scl) {
  data_all <- c(indeks, b2, scl)
  names(data_all) <- c("NDVI", "NDBI", "NDWI", "NDCI", "B02", "SCL")
  
  set.seed(1)
  d <- spatSample(data_all, size = 300000, method = "regular",
                  na.rm = TRUE, as.df = TRUE)
  d <- d[d$SCL %in% 3:10, ]
  
  # --- 2.1 Ringkasan per kelas SCL (mean, p05, median, p95) ---
  ringkas <- function(x) c(mean = mean(x), p05 = unname(quantile(x, .05)),
                           med = median(x), p95 = unname(quantile(x, .95)))
  vars <- c("NDVI", "NDBI", "NDWI", "NDCI", "B02")
  
  per_scl <- do.call(rbind, lapply(split(d, d$SCL), function(g) {
    baris <- unlist(lapply(vars, function(v) {
      r <- ringkas(g[[v]]); names(r) <- paste0(v, "_", names(r)); r
    }))
    data.frame(SCL = g$SCL[1], kelas = nama_scl[as.character(g$SCL[1])],
               n = nrow(g), t(round(baris, 3)), row.names = NULL)
  }))
  write.csv(per_scl, "output/indeks_per_kelas_SCL.csv", row.names = FALSE)
  print(per_scl[, c("SCL", "kelas", "n", "NDVI_mean", "NDBI_mean",
                    "NDWI_mean", "NDCI_mean", "B02_mean")])
  
  # --- 2.2 Uji kandidat threshold ---
  # pct_target  = % piksel kelas SCL acuan yang lolos (makin tinggi makin baik)
  # pct_lainnya = % piksel kelas lain yang ikut lolos (makin rendah makin baik)
  cek <- function(nama, lolos, kelas_target) {
    data.frame(uji = nama,
               pct_target  = round(100 * mean(lolos[d$SCL %in% kelas_target]), 1),
               pct_lainnya = round(100 * mean(lolos[!d$SCL %in% kelas_target]), 1))
  }
  uji <- rbind(
    do.call(rbind, lapply(c(0.3, 0.4, 0.5, 0.6), function(t)       # vegetasi (SCL 4)
      cek(paste0("NDVI > ", t), d$NDVI > t, 4))),
    do.call(rbind, lapply(c(-0.1, 0, 0.1), function(t)             # air (SCL 6)
      cek(paste0("NDWI > ", t), d$NDWI > t, 6))),
    do.call(rbind, lapply(c(0.15, 0.20, 0.25, 0.30), function(t)   # awan (SCL 8,9,10)
      cek(paste0("B02 > ", t), d$B02 > t, c(8, 9, 10))))
  )
  write.csv(uji, "output/uji_threshold_SCL.csv", row.names = FALSE)
  print(uji)
}


# ==============================================================
# 3. KLASIFIKASI 4 KELAS
# ==============================================================

# --- 3.1 Threshold ---
# Dipilih dari uji_threshold_SCL.csv dan pengecekan visual
T_NDVI     <- 0.30   # vegetasi : NDVI > T_NDVI (96,8% vegetasi lolos, 19,9% lainnya)
T_NDWI     <- 0.00   # air      : NDWI > T_NDWI (55,5% air lolos, hanya 0,5% salah)
T_NDCI_ABS <- 0.05   # awan     : |NDCI| < T_NDCI_ABS (spektrum datar, B5 ~ B4)
T_BLUE     <- 0.15   # ... DAN B02 > T_BLUE (awan itu terang)
MIN_AWAN   <- 100    # gumpalan awan < 100 piksel (~1 ha) dianggap palsu

# --- 3.2 Penentuan kelas ---
# 1 = Vegetasi, 2 = Terbangun/Lahan terbuka, 3 = Perairan/Tambak, 4 = Awan
# Default piksel valid = Terbangun/Lahan terbuka (NDVI rendah),
# lalu ditimpa berurutan, yang terakhir menang:
#   Vegetasi (NDVI) -> Perairan (NDWI) -> Awan (NDCI + B02)
kelas <- ifel(is.na(ndvi), NA, ifel(ndvi > T_NDVI, 1, 2))
kelas <- ifel(ndwi > T_NDWI, 3, kelas)
kelas <- ifel(abs(ndci) < T_NDCI_ABS & b2 > T_BLUE, 4, kelas)

# --- 3.3 Pembersihan awan ---
# Gumpalan kecil dibuang, lalu tepi awan diperluas
awan   <- ifel(kelas == 4, 1, 0)
pt     <- patches(awan, directions = 8, zeroAsNA = TRUE)
ukuran <- freq(pt)
kecil  <- ukuran$value[ukuran$count < MIN_AWAN]
awan   <- ifel(is.na(pt) | pt %in% kecil, 0, 1)

awan2 <- focal(awan, w = 11, fun = "max", na.policy = "omit", na.rm = TRUE)
kelas <- ifel(!is.na(kelas) & awan2 == 1, 4, kelas)

# --- 3.4 Penghalusan noise ---
# Noise garam-merica dihaluskan dengan modus 3x3
kelas <- focal(kelas, w = 3, fun = "modal", na.policy = "omit", na.rm = TRUE)
names(kelas) <- "kelas"

nama_kelas <- c("Vegetasi", "Terbangun/Lahan terbuka", "Perairan/Tambak", "Awan")
warna      <- c("#2E8B3E", "#D62728", "#1F77B4", "#BDBDBD")

# --- 3.5 Menyimpan hasil dan menghitung luas ---
writeRaster(kelas, "data/processed/pidiejaya_kelas.tif", overwrite = TRUE,
            datatype = "INT1U")

# Luas dan persentase per kelas (4 kelas selalu muncul)
fr <- freq(kelas)
tabel <- data.frame(kode = 1:4, kelas = nama_kelas)
tabel$count   <- fr$count[match(tabel$kode, fr$value)]
tabel$count[is.na(tabel$count)] <- 0
tabel$luas_ha <- tabel$count * 100 / 10000      # 10 m x 10 m = 100 m2
tabel$persen  <- round(tabel$count / sum(tabel$count) * 100, 2)
write.csv(tabel, "output/luas_kelas.csv", row.names = FALSE)
print(tabel)


# ==============================================================
# 4. PETA KLASIFIKASI
# ==============================================================

# --- 4.1 Batas kecamatan ---
kec <- vect("data/raw/batas/ADMINISTRASIKECAMATAN_AR_50K.shp")   # SESUAIKAN
kec <- kec[toupper(gsub(" ", "", kec$WADMKK)) == "PIDIEJAYA", ]
kec <- project(kec, crs(kelas))
kec <- crop(kec, ext(kelas))

# --- 4.2 Warna dan teks legenda ---
coltab(kelas) <- data.frame(value = 1:4, col = warna)
teks_legenda  <- sprintf("%s\n(%.1f%% | %s ha)", nama_kelas, tabel$persen,
                         format(round(tabel$luas_ha), big.mark = ".", decimal.mark = ","))

# --- 4.3 Fungsi peta ---
# Kiri = peta, kanan = panel legenda
gambar_peta <- function() {
  layout(matrix(1:2, nrow = 1), widths = c(3.2, 1.2))
  par(oma = c(3, 0, 4, 0))
  
  # Panel 1: peta
  par(mar = c(2, 4, 1, 0.5))
  plot(kelas, legend = FALSE, main = "", axes = TRUE)
  lines(kec, col = "black", lwd = 0.8)
  sbar(d = 10000, xy = "bottomright", type = "bar", divs = 2,
       labels = c("0", "5", "10 km"), cex = 0.8)
  north(type = 1)
  
  # Panel 2: legenda di luar peta
  par(mar = c(2, 0, 1, 1))
  plot.new()
  legend("topleft", legend = teks_legenda, fill = warna, border = "grey30",
         title = "Legenda Tutupan Lahan", bty = "n",
         cex = 0.8, y.intersp = 1.6, x.intersp = 0.6,
         xpd = NA, inset = c(0, 0.05))
  
  # Judul dan sumber di margin luar
  mtext("Peta Klasifikasi Tutupan Lahan", outer = TRUE, side = 3,
        line = 2, font = 2, cex = 1.2)
  mtext("Kabupaten Pidie Jaya, Aceh", outer = TRUE, side = 3,
        line = 0.5, cex = 1)
  mtext("Sumber: Sentinel-2 L2A (Copernicus, 29 Nov 2025); Batas Administrasi: Ina-Geoportal",
        outer = TRUE, side = 1, line = 1.5, cex = 0.7)
}

# --- 4.4 Menyimpan dan menampilkan peta ---
png("output/peta_klasifikasi.png", width = 3300, height = 2400, res = 300)
gambar_peta()
dev.off()

gambar_peta()   # tampil di panel Plots (kalau legenda terlihat aneh, buka PNG-nya)


# ==============================================================
# 5. VALIDASI TERHADAP SCL
# ==============================================================
# Hasil klasifikasi dibandingkan dengan SCL sebagai acuan pembanding
if (ada_scl) {
  ct <- crosstab(c(kelas, scl), long = TRUE)      # kolom: kelas, SCL, n
  names(ct) <- c("kode", "SCL", "n")
  ct$kelas    <- nama_kelas[as.integer(as.character(ct$kode))]
  ct$SCL_nama <- nama_scl[as.character(ct$SCL)]
  
  # Tabel silang (baris = hasil klasifikasi, kolom = SCL), persen per baris
  tb  <- xtabs(n ~ kelas + SCL_nama, data = ct)
  tbp <- round(100 * prop.table(tb, margin = 1), 1)
  print(tbp)
  write.csv(as.data.frame.matrix(tbp), "output/validasi_vs_SCL_persen.csv")
  write.csv(as.data.frame.matrix(tb),  "output/validasi_vs_SCL_piksel.csv")
}