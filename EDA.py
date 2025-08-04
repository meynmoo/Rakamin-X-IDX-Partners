# Muat paket yang dibutuhkan
library(ggplot2)  # Untuk visualisasi
library(GGally)   # Untuk visualisasi korelasi
library(dplyr)    # Untuk manipulasi data
library(tidyr)    # Untuk pivot data

# === 1. Membaca Data ===
loan_data <- read_csv("C:/Users/ASUS/Downloads/loan_data_2007_2014.csv")

# === 2. Definisikan kategori status dan Filter Data ===
good_status <- c("Fully Paid")
bad_status <- c("Charged Off", "Default", "Late (31-120 days)", "Late (16-30 days)")

# Filter data dan ubah label
loan_data <- loan_data |>
  filter(loan_status %in% c(good_status, bad_status)) |>
  mutate(
    loan_status = case_when(
      loan_status %in% good_status ~ "GOOD",
      loan_status %in% bad_status ~ "BAD"
    ),
    loan_status = factor(loan_status)
  )

# Cek distribusi loan_status
table(loan_data$loan_status)

# === 3. Cek Missing Values dan Penanganannya ===
missing_data <- loan_data |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") |>
  filter(missing_count > 0)

# Tampilkan hasil missing data
print(missing_data)

# Hapus kolom yang seluruhnya NA
loan_data <- loan_data |>
  select(where(~ !all(is.na(.))))  # Menghapus kolom yang seluruhnya NA

# Hapus baris yang memiliki NA di kolom numerik yang penting (contoh: annual_inc)
loan_data$annual_inc[is.na(loan_data$annual_inc)] <- median(loan_data$annual_inc, na.rm = TRUE)
loan_data$int_rate[is.na(loan_data$int_rate)] <- median(loan_data$int_rate, na.rm = TRUE)
loan_data$loan_amnt[is.na(loan_data$loan_amnt)] <- median(loan_data$loan_amnt, na.rm = TRUE)

# Fungsi untuk menghitung modus (nilai terbanyak) pada kolom kategorikal
Mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Mengembalikan nilai yang paling sering muncul
}

# Imputasi dengan modus (nilai terbanyak) untuk kolom kategorikal
loan_data$emp_title[is.na(loan_data$emp_title)] <- Mode(loan_data$emp_title)
loan_data$emp_length[is.na(loan_data$emp_length)] <- Mode(loan_data$emp_length)
loan_data$home_ownership[is.na(loan_data$home_ownership)] <- Mode(loan_data$home_ownership)

# === 4. Hapus Kolom yang Memiliki Missing Values Tinggi ===
loan_data <- loan_data |>
  select(-mths_since_last_record, -mths_since_last_delinq, -mths_since_last_major_derog)

# === 5. Imputasi Kolom Numerik yang Masih Memiliki Missing Values ===
loan_data$tot_coll_amt[is.na(loan_data$tot_coll_amt)] <- median(loan_data$tot_coll_amt, na.rm = TRUE)
loan_data$tot_cur_bal[is.na(loan_data$tot_cur_bal)] <- median(loan_data$tot_cur_bal, na.rm = TRUE)
loan_data$total_rev_hi_lim[is.na(loan_data$total_rev_hi_lim)] <- median(loan_data$total_rev_hi_lim, na.rm = TRUE)

# === 6. Hapus Kolom yang Tidak Relevan ===
loan_data <- loan_data |>
  select(-desc)

# === 7. Cek Missing Values Setelah Pembersihan ===
missing_data_after <- loan_data |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") |>
  filter(missing_count > 0)

# Tampilkan kolom yang masih ada missing values
print(missing_data_after)

# === 8. Cek Dimensi Dataset Setelah Pembersihan ===
dim(loan_data)  # Berapa baris dan kolom yang tersisa?

# Cek struktur dataset untuk memastikan tidak ada missing values
glimpse(loan_data)  # Lihat struktur data setelah imputasi

# === 9. Visualisasi Distribusi Variabel Numerik ===
plot1 <- loan_data |>
  select(loan_amnt, int_rate, annual_inc) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribusi Variabel Numerik")

ggsave("distribusi_variabel_numerik.png", plot = plot1)

# === 10. Korelasi Antara Fitur Numerik ===
numeric_data <- loan_data |>
  select(loan_amnt, int_rate, annual_inc, dti)  # Pilih kolom yang relevan
plot2 <- ggpairs(numeric_data, title = "Korelasi antara Fitur Numerik")

ggsave("korelasi_antara_fitur_numerik.png", plot = plot2)

# === 11. Boxplot: Perbandingan int_rate dengan loan_status ===
plot3 <- loan_data |>
  ggplot(aes(x = loan_status, y = int_rate, fill = loan_status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribusi Suku Bunga berdasarkan Status Pinjaman")

ggsave("boxplot_suku_bunga_loan_status.png", plot = plot3)

# === 12. Visualisasi Kategorikal: Purpose dan Loan Status ===
plot4 <- loan_data |>
  ggplot(aes(x = purpose, fill = loan_status)) +
  geom_bar(position = "fill") +
  labs(y = "Proporsi", title = "Distribusi Status Pinjaman berdasarkan Tujuan Pinjaman") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("distribusi_status_pinjaman_purpose.png", plot = plot4)

# === 13. Deteksi Outliers ===
plot5 <- loan_data |>
  ggplot(aes(y = loan_amnt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Deteksi Outlier pada Loan Amount")

ggsave("deteksi_outlier_loan_amount.png", plot = plot5)

# === 14. Visualisasi Lanjutan: Term Pinjaman vs Loan Status ===
plot6 <- loan_data |>
  ggplot(aes(x = term, fill = loan_status)) +
  geom_bar(position = "fill") +
  labs(y = "Proporsi", title = "Distribusi Status Pinjaman berdasarkan Durasi Pinjaman") +
  theme_minimal()

ggsave("distribusi_status_pinjaman_term.png", plot = plot6)

# === 15. Analisis Univariat dan Bivariat ===
plot7 <- loan_data |>
  ggplot(aes(x = grade, fill = loan_status)) +
  geom_bar(position = "fill") +
  labs(y = "Proporsi", title = "Distribusi Status Pinjaman berdasarkan Grade Pinjaman") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("distribusi_status_pinjaman_grade.png", plot = plot7)

plot8 <- loan_data |>
  ggplot(aes(x = loan_amnt, y = annual_inc, color = loan_status)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scatter Plot Loan Amount vs Annual Income")

ggsave("scatter_plot_loan_amount_vs_annual_income.png", plot = plot8)

# === 16. Kesimpulan dan Penyimpanan Hasil EDA ===
write.csv(loan_data, "loan_data_eda.csv")

# Ringkasan akhir
cat("Selesai dengan EDA! Semua grafik dan data telah disimpan.")
