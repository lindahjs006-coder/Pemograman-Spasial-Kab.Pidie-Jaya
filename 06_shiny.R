# ==============================================================
# EVALUASI TENGAH SEMESTER PEMROGRAMAN SPASIAL
# Tahap 6: Informasi Spasial Interaktif (Dashboard Shiny)
# Wilayah Studi: Kabupaten Pidie Jaya, Provinsi Aceh
# --------------------------------------------------------------
# Anggota 1 : Aisyah Meylinda Sari (5016241115)
# Anggota 2 : Nabilla Putri Badrina (5016241130)
# Kelas     : Pemrograman Spasial A
# --------------------------------------------------------------
# Deskripsi :
# Dashboard Shiny + Leaflet untuk menampilkan hasil klasifikasi
# tutupan lahan Sentinel-2 L2A (29 November 2025).
# 1. Peta   : OSM, Dark Map, Heatmap, Choropleth, Zonal Indeks
# 2. Tabel  : Statistik indeks, luas kelas per kecamatan
# 3. Metode : Dasar threshold, validasi vs SCL, galeri peta statis
# 4. Tampilan : Tema hijau-teal, kartu judul peta, skala,
#               panah utara, dan sumber data pada tiap peta
# --------------------------------------------------------------
# Catatan:
# Jalankan skrip 01 sampai 05 terlebih dahulu.
# Cara menjalankan: klik Run App > Run External.
# Font Poppins dimuat dari Google Fonts (butuh internet);
# jika offline, tampilan otomatis memakai font cadangan.
# --------------------------------------------------------------
# Data:
# - Seluruh berkas pada folder output 
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
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(terra)

# --- 0.3 Pengaturan folder kerja ---
# Pengaman: jika skrip diletakkan di folder R/, pindah ke folder proyek
if (!dir.exists("output") && dir.exists("../output")) setwd("..")

# Agar gambar PNG di folder output dapat ditampilkan di aplikasi
addResourcePath("hasil", "output")


# ==============================================================
# 1. DATA
# ==============================================================

# --- 1.1 Parameter umum ---
nama_kelas   <- c("Vegetasi", "Terbangun/Lahan terbuka", "Perairan/Tambak", "Awan")
warna        <- c("#2E8B3E", "#D62728", "#1F77B4", "#BDBDBD")
indeks       <- c("NDVI", "NDBI", "NDWI", "NDCI")
palet        <- c(NDVI = "Greens", NDBI = "Reds", NDWI = "Blues", NDCI = "Purples")
warna_indeks <- c(NDVI = "#2E8B3E", NDBI = "#D62728", NDWI = "#1F77B4", NDCI = "#6A51A3")
stat         <- c("mean", "min", "max", "median", "sd")

# Keterangan sumber data (ditampilkan pada kartu judul tiap peta)
sumber_data <- "Sumber: Sentinel-2 L2A (Copernicus); batas administrasi: Ina-Geoportal"

# Fungsi pembaca CSV opsional (mengembalikan NULL jika berkas tidak ada)
baca_csv <- function(f, ...) if (file.exists(f)) read.csv(f, check.names = FALSE, ...) else NULL

# --- 1.2 Batas administrasi ---
# Hasil 05_export.R (UTM 46N), ditransformasi ke WGS84
kec_utm <- st_read("output/kecamatan_zonal_pidiejaya.gpkg", quiet = TRUE)
kec_ll  <- st_transform(kec_utm, 4326)

f_kab   <- "output/batas_kab_pidiejaya.gpkg"
ada_kab <- file.exists(f_kab)
if (ada_kab) kab_ll <- st_transform(st_read(f_kab, quiet = TRUE), 4326)

# Titik label nama kecamatan (selalu berada di dalam poligon)
label_kec  <- suppressWarnings(st_point_on_surface(kec_ll))
gaya_label <- labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                           style = list("font-weight" = "bold", "font-size" = "11px",
                                        "color" = "black",
                                        "text-shadow" = "1px 1px 2px white"))

# --- 1.3 Klasifikasi tutupan lahan dan tabel luas ---
kls_poly <- st_read("output/klasifikasi_poligon_wgs84.gpkg", quiet = TRUE)
luas     <- read.csv("output/luas_kelas_pidiejaya.csv")
luas_kec <- read.csv("output/luas_kelas_per_kecamatan.csv", check.names = FALSE)

# Ringkasan lengkap per kecamatan (hasil 04_zonal.R, opsional)
ringkas <- baca_csv("output/zonal_kecamatan_ringkas.csv")

# --- 1.4 Persentase terbangun per kecamatan ---
i <- match(kec_ll$kecamatan, luas_kec$Kecamatan)
kec_ll$terbangun_km2 <- luas_kec[["Terbangun/Lahan terbuka"]][i]
kec_ll$terbangun_pct <- round(100 * kec_ll$terbangun_km2 / luas_kec$Total[i], 2)

# Titik pusat kecamatan (untuk lingkaran pada Dark Map)
xy <- st_coordinates(suppressWarnings(st_point_on_surface(kec_ll)))
kec_ll$lng <- xy[, 1]; kec_ll$lat <- xy[, 2]

# --- 1.5 Titik acak area terbangun (heatmap) ---
set.seed(1)
terb <- kls_poly[kls_poly$kode == 2, ]
pts  <- st_coordinates(suppressWarnings(
  st_sample(st_union(terb), size = 8000, type = "random")))
heat <- data.frame(long = pts[, 1], lat = pts[, 2])

# --- 1.6 Zonal statistics indeks (NDVI, NDBI, NDWI, NDCI) ---
# Dihitung satu kali oleh 04_zonal.R; di sini hanya dibaca (ringan)
f_zonal <- "output/zonal_indeks_pidiejaya.csv"
kolom_zonal <- as.vector(outer(indeks, stat, paste, sep = "_"))
zonal <- if (file.exists(f_zonal)) read.csv(f_zonal) else NULL
ada_indeks <- !is.null(zonal) && all(kolom_zonal %in% names(zonal))
if (ada_indeks) {
  zi <- match(kec_ll$kecamatan, zonal$Kecamatan)
  for (k in kolom_zonal) kec_ll[[k]] <- zonal[[k]][zi]
}

# --- 1.7 Sampel piksel indeks untuk histogram ---
# Dibuat oleh 04_zonal.R
f_sampel <- "output/sampel_indeks.csv"
sampel <- if (file.exists(f_sampel)) read.csv(f_sampel) else NULL

# --- 1.8 Tabel dasar threshold dan validasi ---
# Hasil 03_classify.R (opsional)
per_scl <- baca_csv("output/indeks_per_kelas_SCL.csv")
uji_thr <- baca_csv("output/uji_threshold_SCL.csv")
val_pct <- baca_csv("output/validasi_vs_SCL_persen.csv", row.names = 1)
val_pks <- baca_csv("output/validasi_vs_SCL_piksel.csv", row.names = 1)

# --- 1.9 Aturan klasifikasi ---
# Sama dengan 03_classify.R
aturan <- data.frame(
  Urutan = 1:4,
  Kelas  = c("Terbangun/Lahan terbuka (dasar)", "Vegetasi", "Perairan/Tambak", "Awan"),
  Aturan = c("Semua piksel valid (NDVI rendah)",
             "NDVI > 0,30",
             "NDWI > 0,00",
             "|NDCI| < 0,05 DAN B02 > 0,15 (gumpalan < 100 piksel dibuang)"),
  Keterangan = c("Default", "Menimpa kelas 1", "Menimpa kelas 1-2", "Menimpa semua; tepi diperluas 11x11")
)

# --- 1.10 Pesan jika data indeks belum tersedia ---
pesan_indeks <- div(style = "padding:20px;",
                    h4("Data indeks belum tersedia."),
                    p("Jalankan 04_zonal.R sekali untuk membuat zonal_indeks_pidiejaya.csv dan sampel_indeks.csv."))

# --- 1.11 Tema tampilan (CSS) ---
# Warna utama dapat diubah lewat variabel pada :root
css_tema <- "
:root {
  --hijau-tua: #0B3D2E;
  --hijau:     #1B7F5C;
  --teal:      #20A39E;
  --emas:      #F2B134;
  --latar:     #F3F7F5;
}
body, h1, h2, h3, h4, h5, .main-header .logo {
  font-family: 'Poppins', sans-serif !important;
}
.content-wrapper, .right-side {background: var(--latar) !important;}

/* Header */
.main-header .logo {
  background: linear-gradient(135deg, var(--hijau-tua), var(--hijau)) !important;
  line-height: normal !important; text-align: left; padding-top: 7px;
}
.main-header .navbar {
  background: linear-gradient(90deg, var(--hijau), var(--teal)) !important;
}
.judul-utama {display: block; font-size: 17px; font-weight: 700; letter-spacing: .3px;}
.judul-sub   {display: block; font-size: 12px; margin-top: 2px; color: var(--emas); font-weight: 500;}

/* Sidebar */
.main-sidebar, .left-side {
  background: linear-gradient(180deg, #0B3D2E 0%, #0F2F3A 100%) !important;
}
.sidebar-menu > li > a {
  font-weight: 600; font-size: 14px; padding: 12px 15px;
  border-left: 4px solid transparent;
}
.sidebar-menu > li:hover > a,
.sidebar-menu > li.active > a {
  background: rgba(255,255,255,.08) !important;
  border-left-color: var(--emas) !important;
}
.sidebar-menu .treeview-menu {background: rgba(0,0,0,.18) !important;}
.sidebar-menu .treeview-menu > li > a {color: #cfe3da !important; font-size: 13px;}
.sidebar-menu .treeview-menu > li.active > a,
.sidebar-menu .treeview-menu > li > a:hover {color: var(--emas) !important; font-weight: 600;}

/* Box */
.box {
  border-radius: 14px; border-top: none !important;
  box-shadow: 0 4px 14px rgba(11,61,46,.10); overflow: hidden;
}
.box.box-solid.box-primary {border: none !important;}
.box.box-solid.box-primary > .box-header {
  background: linear-gradient(90deg, var(--hijau), var(--teal)) !important;
  color: #fff; padding: 12px 15px;
}
.box-title {font-weight: 600 !important; font-size: 15px !important;}
.table > thead > tr > th {
  background: #E6F2ED; color: var(--hijau-tua);
  border-bottom: 2px solid var(--teal) !important;
}
.table-striped > tbody > tr:nth-of-type(odd) {background: #F7FBF9;}

/* Value box */
.small-box {border-radius: 14px; box-shadow: 0 4px 14px rgba(0,0,0,.12);}
.small-box h3 {font-weight: 700; font-size: 26px;}
.small-box .icon {opacity: .25;}

/* Peta */
.peta-wrap {position: relative;}
#lahan_osm, #lahan_dark, #lahan_heat, #kec_chor, #peta_indeks {
  height: calc(100vh - 80px) !important;
  border-radius: 14px; box-shadow: 0 4px 14px rgba(0,0,0,.18);
}
.panel-judul {
  position: absolute; z-index: 1000; top: 14px; left: 60px;
  background: rgba(11,61,46,.88); color: #fff;
  padding: 10px 18px; border-radius: 12px; max-width: 400px;
  border-left: 5px solid var(--emas);
  box-shadow: 0 4px 12px rgba(0,0,0,.3);
}
.panel-judul b      {font-size: 15px; display: block;}
.panel-judul small  {font-size: 11.5px; color: #cfe3da; display: block;}
.panel-judul .sumber {font-size: 10px; color: #9fc3b3; margin-top: 4px;}
.panel-judul i      {margin-right: 6px; color: var(--emas);}
.panah-utara {
  background: rgba(255,255,255,.92); padding: 4px 10px; border-radius: 8px;
  text-align: center; font-weight: 700; line-height: 1.1;
  box-shadow: 0 2px 8px rgba(0,0,0,.25);
}
.leaflet-control-layers, .leaflet-control.info.legend {
  border-radius: 10px !important; box-shadow: 0 2px 10px rgba(0,0,0,.25) !important;
}

/* Lain-lain */
.scroll-x {overflow-x: auto;}
.galeri img {max-width: 100%; border: 1px solid #ddd; border-radius: 10px; margin-bottom: 15px;}
"

# --- 1.12 Fungsi bantu tampilan peta ---
# Kartu judul melayang di atas peta (judul, keterangan, dan sumber data)
peta_wrap <- function(id, judul, ket, ikon) {
  div(class = "peta-wrap",
      div(class = "panel-judul",
          tags$b(icon(ikon), judul),
          tags$small(ket),
          tags$small(class = "sumber", sumber_data)),
      leafletOutput(id))
}

# Panah utara (HTML sederhana yang ditempel sebagai kontrol peta)
panah_utara <- "<div class='panah-utara'>&#9650;<br>U</div>"

# Skala dan panah utara, ditambahkan di akhir rantai tiap peta
elemen_peta <- function(m) {
  m %>%
    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    addControl(html = panah_utara, position = "bottomleft")
}


# ==============================================================
# 2. TAMPILAN (UI)
# ==============================================================
ui <- dashboardPage(
  skin  = "green",
  title = "Informasi Spasial Interaktif Kabupaten Pidie Jaya",
  
  # --- 2.1 Header ---
  dashboardHeader(
    title = tags$div(
      tags$span("Informasi Spasial Interaktif", class = "judul-utama"),
      tags$span("Kabupaten Pidie Jaya", class = "judul-sub")),
    titleWidth = 320),
  
  # --- 2.2 Sidebar (menu) ---
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("Maps", tabName = "maps", icon = icon("globe"), startExpanded = TRUE,
               menuSubItem("Tutupan Lahan OSM",          tabName = "m_osm",    icon = icon("map"), selected = TRUE),
               menuSubItem("Tutupan Lahan Dark",         tabName = "m_dark",   icon = icon("map")),
               menuSubItem("Heatmap Terbangun",          tabName = "m_heat",   icon = icon("fire")),
               menuSubItem("Choropleth Kecamatan",       tabName = "m_chor",   icon = icon("map")),
               menuSubItem("Zonal Indeks per Kecamatan", tabName = "m_indeks", icon = icon("layer-group"))),
      menuItem("Statistik", tabName = "statistik", icon = icon("chart-bar"), startExpanded = TRUE,
               menuSubItem("Statistik Indeks",           tabName = "m_stat", icon = icon("table")),
               menuSubItem("Luas Kelas per Kecamatan",   tabName = "m_luas", icon = icon("chart-bar"))),
      menuItem("Metode", tabName = "metode", icon = icon("flask"), startExpanded = TRUE,
               menuSubItem("Dasar Threshold",            tabName = "m_thr",    icon = icon("sliders-h")),
               menuSubItem("Validasi vs SCL",            tabName = "m_val",    icon = icon("check-circle")),
               menuSubItem("Galeri Peta Statis",         tabName = "m_galeri", icon = icon("image")))
    )
  ),
  
  # --- 2.3 Isi halaman ---
  dashboardBody(
    # Font dan tema (CSS pada bagian 1.11)
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&display=swap"),
      tags$style(HTML(css_tema))),
    
    tabItems(
      # --- Halaman peta ---
      tabItem("m_osm",
              peta_wrap("lahan_osm", "Tutupan Lahan",
                        "Hasil klasifikasi Sentinel-2 L2A, 29 November 2025", "map")),
      tabItem("m_dark",
              peta_wrap("lahan_dark", "Dark Map",
                        "Lingkaran = persentase terbangun per kecamatan", "moon")),
      tabItem("m_heat",
              peta_wrap("lahan_heat", "Heatmap Terbangun",
                        "Kepadatan titik area terbangun/lahan terbuka", "fire")),
      tabItem("m_chor",
              peta_wrap("kec_chor", "Choropleth Kecamatan",
                        "Persentase terbangun/lahan terbuka per kecamatan", "layer-group")),
      tabItem("m_indeks",
              if (ada_indeks)
                peta_wrap("peta_indeks", "Zonal Indeks",
                          "Rata-rata NDVI, NDBI, NDWI, NDCI per kecamatan", "leaf")
              else pesan_indeks),
      
      # --- Halaman statistik indeks ---
      tabItem("m_stat",
              fluidRow(
                valueBoxOutput("vb_luas", width = 3),
                valueBoxOutput("vb_terb", width = 3),
                valueBoxOutput("vb_kec",  width = 3),
                valueBoxOutput("vb_top",  width = 3)),
              fluidRow(
                box(title = "Luas Tiap Kelas Tutupan Lahan", width = 4,
                    status = "primary", solidHeader = TRUE, tableOutput("tabel_luas")),
                box(title = "Zonal Statistics per Kecamatan", width = 8,
                    status = "primary", solidHeader = TRUE,
                    if (ada_indeks) tagList(
                      selectInput("pilih_indeks", "Pilih indeks:", choices = indeks),
                      tableOutput("tabel_zonal")) else pesan_indeks)),
              if (ada_indeks) fluidRow(
                box(title = "Histogram Nilai Indeks", width = 6,
                    status = "primary", solidHeader = TRUE, plotOutput("histogram")),
                box(title = "Rata-rata Indeks per Kecamatan", width = 6,
                    status = "primary", solidHeader = TRUE, plotOutput("grafik_kec")))),
      
      # --- Halaman luas kelas per kecamatan ---
      tabItem("m_luas",
              fluidRow(box(title = "Luas Kelas Tutupan Lahan per Kecamatan", width = 12,
                           status = "primary", solidHeader = TRUE,
                           div(class = "scroll-x", tableOutput("tabel_luas_kec")))),
              fluidRow(box(title = "Komposisi Tutupan Lahan per Kecamatan (%)", width = 12,
                           status = "primary", solidHeader = TRUE,
                           plotOutput("grafik_komposisi", height = 550))),
              fluidRow(box(title = "Ringkasan Lengkap (ha, %, dan % tanpa awan)", width = 12,
                           status = "primary", solidHeader = TRUE,
                           div(class = "scroll-x", tableOutput("tabel_ringkas"))))),
      
      # --- Halaman dasar threshold ---
      tabItem("m_thr",
              fluidRow(box(title = "Aturan Klasifikasi 4 Kelas", width = 12,
                           status = "primary", solidHeader = TRUE,
                           tableOutput("tabel_aturan"))),
              fluidRow(box(title = "Nilai Indeks per Kelas SCL (mean, p05, median, p95)", width = 12,
                           status = "primary", solidHeader = TRUE,
                           div(class = "scroll-x", tableOutput("tabel_per_scl")))),
              fluidRow(box(title = "Uji Kandidat Threshold", width = 6,
                           status = "primary", solidHeader = TRUE,
                           p("pct_target = % piksel kelas acuan yang lolos (makin tinggi makin baik);",
                             "pct_lainnya = % piksel kelas lain yang ikut lolos (makin rendah makin baik)."),
                           tableOutput("tabel_uji")))),
      
      # --- Halaman validasi vs SCL ---
      tabItem("m_val",
              fluidRow(box(title = "Hasil Klasifikasi vs SCL (% per baris)", width = 12,
                           status = "primary", solidHeader = TRUE,
                           p("Baris = kelas hasil klasifikasi; kolom = kelas SCL Sentinel-2.",
                             "SCL dipakai sebagai acuan pembanding, bukan kebenaran lapangan."),
                           div(class = "scroll-x", tableOutput("tabel_val_pct")))),
              fluidRow(box(title = "Komposisi SCL di dalam Tiap Kelas", width = 6,
                           status = "primary", solidHeader = TRUE,
                           plotOutput("grafik_val", height = 420)),
                       box(title = "Jumlah Piksel", width = 6,
                           status = "primary", solidHeader = TRUE,
                           div(class = "scroll-x", tableOutput("tabel_val_pks"))))),
      
      # --- Halaman galeri ---
      tabItem("m_galeri",
              fluidRow(box(title = "Peta & Grafik Statis ", width = 12,
                           status = "primary", solidHeader = TRUE,
                           uiOutput("galeri"))))
    )
  )
)


# ==============================================================
# 3. SERVER
# ==============================================================
server <- function(input, output, session) {
  
  # --- 3.1 Peta OSM ---
  output$lahan_osm <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri.WorldStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,   group = "Esri.WorldImagery")
    
    for (k in 1:4) {
      d <- kls_poly[kls_poly$kode == k, ]
      if (nrow(d) > 0)
        m <- m %>% addPolygons(data = d, fillColor = warna[k], fillOpacity = 0.8,
                               stroke = FALSE, group = nama_kelas[k])
    }
    
    m <- m %>%
      addPolygons(data = kec_ll, fill = FALSE, color = "black", weight = 1,
                  label = ~kecamatan, group = "Batas Kecamatan")
    if (ada_kab)
      m <- m %>% addPolygons(data = kab_ll, fill = FALSE, color = "black", weight = 3,
                             group = "Batas Kabupaten")
    
    m %>%
      addLabelOnlyMarkers(data = label_kec, label = ~kecamatan,
                          labelOptions = gaya_label, group = "Nama Kecamatan") %>%
      addLayersControl(
        baseGroups    = c("OpenStreetMap", "Esri.WorldStreetMap", "Esri.WorldImagery"),
        overlayGroups = c(nama_kelas, "Batas Kecamatan",
                          if (ada_kab) "Batas Kabupaten", "Nama Kecamatan"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(position = "bottomright", colors = warna,
                labels = paste0(nama_kelas, " (", luas$persen, "%)"),
                title = "Kelas Tutupan Lahan", opacity = 1) %>%
      elemen_peta() %>%
      addResetMapButton()
  })
  
  # --- 3.2 Dark Map ---
  output$lahan_dark <- renderLeaflet({
    pal <- colorNumeric("OrRd", kec_ll$terbangun_pct)
    leaflet(data = kec_ll) %>%
      addProviderTiles(providers$CartoDB.DarkMatter,
                       options = tileOptions(minZoom = 0, maxZoom = 11)) %>%
      addCircles(lng = ~lng, lat = ~lat, radius = ~terbangun_pct * 80, weight = 1,
                 color = ~pal(terbangun_pct), fillColor = ~pal(terbangun_pct),
                 fillOpacity = 0.7,
                 popup = ~paste0("<b>", kecamatan, "</b><br>Terbangun/Lahan terbuka: ",
                                 terbangun_pct, "%"),
                 label = ~paste0(kecamatan, ": ", terbangun_pct, "%"),
                 group = "Points") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = tileOptions(minZoom = 11, maxZoom = 16)) %>%
      addLegend(position = "bottomright", pal = pal, values = ~terbangun_pct,
                title = "Terbangun/Lahan terbuka (%)") %>%
      elemen_peta() %>%
      addResetMapButton()
  })
  
  # --- 3.3 Heatmap ---
  output$lahan_heat <- renderLeaflet({
    leaflet(data = heat) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = tileOptions(minZoom = 0, maxZoom = 16)) %>%
      addHeatmap(lng = ~long, lat = ~lat, intensity = 1,
                 blur = 20, max = 0.3, radius = 15) %>%
      elemen_peta() %>%
      addResetMapButton()
  })
  
  # --- 3.4 Choropleth ---
  output$kec_chor <- renderLeaflet({
    bins <- c(0, 5, 10, 20, 40, 100)
    pal  <- colorBin("YlOrBr", domain = kec_ll$terbangun_pct,
                     na.color = "transparent", bins = bins)
    customLabel <- paste0("Kecamatan: ", kec_ll$kecamatan, "<br/>",
                          "Terbangun/Lahan terbuka: ", kec_ll$terbangun_pct, "%<br/>",
                          "Luas: ", round(kec_ll$terbangun_km2, 2), " km2") %>%
      lapply(htmltools::HTML)
    
    leaflet(kec_ll) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = tileOptions(minZoom = 2, maxZoom = 16)) %>%
      addPolygons(fillColor = ~pal(terbangun_pct), fillOpacity = 0.9,
                  stroke = TRUE, color = "white", weight = 0.8,
                  highlight = highlightOptions(weight = 4, fillOpacity = 0.4),
                  label = customLabel, smoothFactor = 0.2) %>%
      addLabelOnlyMarkers(data = label_kec, label = ~kecamatan,
                          labelOptions = gaya_label) %>%
      addLegend(pal = pal, values = ~terbangun_pct, position = "bottomright",
                title = "Terbangun/Lahan terbuka (%)") %>%
      elemen_peta() %>%
      addResetMapButton()
  })
  
  # --- 3.5 Peta zonal indeks ---
  if (ada_indeks) output$peta_indeks <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Citra Satelit")
    
    for (ix in indeks) {
      nilai <- kec_ll[[paste0(ix, "_mean")]]
      pal   <- colorNumeric(palet[ix], nilai)
      isi_popup <- paste0(
        "<b>Kecamatan ", kec_ll$kecamatan, "</b><br><b>", ix, "</b><br>",
        "Mean: ",   round(nilai, 3), "<br>",
        "Median: ", round(kec_ll[[paste0(ix, "_median")]], 3), "<br>",
        "Min: ",    round(kec_ll[[paste0(ix, "_min")]], 3), "<br>",
        "Max: ",    round(kec_ll[[paste0(ix, "_max")]], 3), "<br>",
        "SD: ",     round(kec_ll[[paste0(ix, "_sd")]], 3))
      m <- m %>%
        addPolygons(data = kec_ll, fillColor = pal(nilai), fillOpacity = 0.75,
                    color = "white", weight = 1, popup = isi_popup,
                    label = paste0(kec_ll$kecamatan, " | ", ix, ": ", round(nilai, 3)),
                    group = ix) %>%
        addLegend(position = "bottomright", pal = pal, values = nilai,
                  title = paste(ix, "(rata-rata)"), opacity = 1, group = ix)
    }
    
    m <- m %>%
      addPolygons(data = kec_ll, color = "black", weight = 1, fill = FALSE,
                  group = "Batas Kecamatan")
    if (ada_kab)
      m <- m %>% addPolygons(data = kab_ll, color = "black", weight = 3, fill = FALSE,
                             group = "Batas Kabupaten")
    m %>%
      addLabelOnlyMarkers(data = label_kec, label = ~kecamatan,
                          labelOptions = gaya_label, group = "Nama Kecamatan") %>%
      addLayersControl(
        baseGroups    = c("OpenStreetMap", "Citra Satelit"),
        overlayGroups = c(if (ada_kab) "Batas Kabupaten", "Batas Kecamatan",
                          "Nama Kecamatan", indeks),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("NDBI", "NDWI", "NDCI")) %>%
      elemen_peta() %>%
      addResetMapButton()
  })
  
  # --- 3.6 Statistik indeks ---
  # Ringkasan angka utama (value box)
  output$vb_luas <- renderValueBox(
    valueBox(paste0(format(round(sum(luas$luas_km2), 1), big.mark = "."), " km2"),
             "Total luas wilayah", icon = icon("ruler-combined"), color = "green"))
  
  output$vb_terb <- renderValueBox(
    valueBox(paste0(luas$persen[grepl("Terbangun", luas$kelas)][1], "%"),
             "Terbangun/Lahan terbuka", icon = icon("city"), color = "red"))
  
  output$vb_kec <- renderValueBox(
    valueBox(nrow(kec_ll), "Jumlah kecamatan",
             icon = icon("map-marked-alt"), color = "teal"))
  
  output$vb_top <- renderValueBox(
    valueBox(kec_ll$kecamatan[which.max(kec_ll$terbangun_pct)],
             "Kecamatan terbangun tertinggi", icon = icon("arrow-up"), color = "yellow"))
  
  output$tabel_luas <- renderTable({
    data.frame("Kelas" = luas$kelas, "Luas (km2)" = luas$luas_km2,
               "Persen (%)" = luas$persen, check.names = FALSE)
  }, digits = 2)
  
  if (ada_indeks) {
    output$tabel_zonal <- renderTable({
      ix <- input$pilih_indeks
      tb <- zonal[, c("Kecamatan", paste0(ix, "_", stat))]
      names(tb) <- c("Kecamatan", "Mean", "Min", "Max", "Median", "Std. Deviasi")
      tb
    }, digits = 4, striped = TRUE)
    
    output$histogram <- renderPlot({
      ix <- input$pilih_indeks
      validate(need(!is.null(sampel), "Sampel piksel indeks belum tersedia."))
      hist(sampel[[ix]], breaks = 100, col = warna_indeks[ix], border = NA,
           main = paste("Histogram", ix, "Kabupaten Pidie Jaya"),
           xlab = paste("Nilai", ix), ylab = "Frekuensi")
    })
    
    output$grafik_kec <- renderPlot({
      ix <- input$pilih_indeks
      v  <- zonal[[paste0(ix, "_mean")]]
      o  <- order(v)
      par(mar = c(4, 9, 2, 1))
      barplot(v[o], names.arg = zonal$Kecamatan[o], horiz = TRUE, las = 1,
              col = warna_indeks[ix], cex.names = 0.8,
              main = paste("Rata-rata", ix, "per Kecamatan"),
              xlab = paste("Rata-rata", ix))
    })
  }
  
  # --- 3.7 Luas kelas per kecamatan ---
  output$tabel_luas_kec <- renderTable({
    km2 <- luas_kec[, nama_kelas]
    pct <- round(100 * km2 / luas_kec$Total, 2)
    names(km2) <- paste(nama_kelas, "(km2)")
    names(pct) <- paste(nama_kelas, "(%)")
    cbind(Kecamatan = luas_kec$Kecamatan, km2, "Total (km2)" = luas_kec$Total, pct)
  }, digits = 2, striped = TRUE)
  
  output$grafik_komposisi <- renderPlot({
    pct <- 100 * as.matrix(luas_kec[, nama_kelas]) / luas_kec$Total
    rownames(pct) <- luas_kec$Kecamatan
    pct <- pct[order(pct[, "Terbangun/Lahan terbuka"]), ]
    par(mar = c(4, 9, 2, 12), xpd = TRUE)
    barplot(t(pct), horiz = TRUE, las = 1, col = warna, border = NA,
            cex.names = 0.8, xlab = "Persentase luas (%)",
            main = "Komposisi Tutupan Lahan per Kecamatan")
    legend("right", inset = c(-0.3, 0), legend = nama_kelas, fill = warna, bty = "n")
  })
  
  output$tabel_ringkas <- renderTable({
    validate(need(!is.null(ringkas), "Jalankan 04_zonal.R untuk membuat zonal_kecamatan_ringkas.csv"))
    ringkas
  }, digits = 2, striped = TRUE)
  
  # --- 3.8 Dasar threshold ---
  output$tabel_aturan <- renderTable(aturan, striped = TRUE)
  
  output$tabel_per_scl <- renderTable({
    validate(need(!is.null(per_scl), "Jalankan 03_classify.R (butuh pidiejaya_scl.tif)."))
    per_scl
  }, digits = 3, striped = TRUE)
  
  output$tabel_uji <- renderTable({
    validate(need(!is.null(uji_thr), "Jalankan 03_classify.R (butuh pidiejaya_scl.tif)."))
    uji_thr
  }, digits = 1, striped = TRUE)
  
  # --- 3.9 Validasi vs SCL ---
  output$tabel_val_pct <- renderTable({
    validate(need(!is.null(val_pct), "validasi_vs_SCL_persen.csv belum ada."))
    cbind(Kelas = rownames(val_pct), val_pct)
  }, digits = 1, striped = TRUE)
  
  output$tabel_val_pks <- renderTable({
    validate(need(!is.null(val_pks), "validasi_vs_SCL_piksel.csv belum ada."))
    cbind(Kelas = rownames(val_pks), val_pks)
  }, digits = 0, striped = TRUE)
  
  output$grafik_val <- renderPlot({
    validate(need(!is.null(val_pct), "validasi_vs_SCL_persen.csv belum ada."))
    m <- t(as.matrix(val_pct))
    par(mar = c(4, 4, 2, 1))
    barplot(m, col = hcl.colors(nrow(m), "Set 3"), las = 1, border = "white",
            ylab = "% piksel", main = "Isi tiap kelas menurut SCL")
    legend("topright", legend = rownames(m), fill = hcl.colors(nrow(m), "Set 3"),
           bty = "n", cex = 0.7)
  })
  
  # --- 3.10 Galeri peta statis ---
  output$galeri <- renderUI({
    f <- c(peta_klasifikasi.png         = "Peta Klasifikasi Tutupan Lahan",
           peta_terbangun_kecamatan.png = "Persentase Terbangun per Kecamatan",
           grafik_kecamatan.png         = "Komposisi Tutupan Lahan per Kecamatan",
           indeks_spektral.png          = "Indeks Spektral (NDVI, NDBI, NDWI, NDCI)")
    ada <- file.exists(file.path("output", names(f)))
    if (!any(ada)) return(p("Belum ada gambar di folder output."))
    div(class = "galeri",
        lapply(which(ada), function(j)
          tagList(h4(f[[j]]), tags$img(src = paste0("hasil/", names(f)[j])))))
  })
}


# ==============================================================
# 4. MENJALANKAN APLIKASI
# ==============================================================
shinyApp(ui, server)