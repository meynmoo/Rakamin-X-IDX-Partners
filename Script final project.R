# Muat paket yang dibutuhkan
library(readr)   # Paket untuk membaca file CSV
library(dplyr)   # Paket untuk manipulasi data
library(tidyr)   # Paket untuk pivot data

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
# Cek nilai yang hilang (missing values) di dataset
missing_data <- loan_data |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") |>
  filter(missing_count > 0)

# Tampilkan hasil missing data
print(missing_data)

# === 4. Hapus Kolom dengan Lebih dari 90% Missing Values ===
# Hitung proporsi missing values per kolom
na_ratio <- colMeans(is.na(loan_data))

# Hapus kolom yang lebih dari 90% missing values
loan_data <- loan_data[, na_ratio <= 0.9]  # Hapus kolom yang memiliki lebih dari 90% NA

# === 5. Imputasi Missing Values untuk Kolom Numerik ===
# Imputasi dengan median untuk kolom numerik
loan_data$annual_inc[is.na(loan_data$annual_inc)] <- median(loan_data$annual_inc, na.rm = TRUE)
loan_data$int_rate[is.na(loan_data$int_rate)] <- median(loan_data$int_rate, na.rm = TRUE)
loan_data$loan_amnt[is.na(loan_data$loan_amnt)] <- median(loan_data$loan_amnt, na.rm = TRUE)

# === 6. Fungsi Mode untuk Imputasi Kolom Kategorikal ===
# Fungsi untuk menghitung modus (nilai terbanyak) pada kolom kategorikal
Mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Mengembalikan nilai yang paling sering muncul
}

# Imputasi dengan modus (nilai terbanyak) untuk kolom kategorikal
loan_data$emp_title[is.na(loan_data$emp_title)] <- Mode(loan_data$emp_title)
loan_data$emp_length[is.na(loan_data$emp_length)] <- Mode(loan_data$emp_length)
loan_data$home_ownership[is.na(loan_data$home_ownership)] <- Mode(loan_data$home_ownership)

# === 7. Hapus Kolom yang Masih Memiliki Missing Values Tinggi ===
# Hapus kolom yang memiliki lebih dari 50% missing values (misalnya, mths_since_last_record, mths_since_last_delinq, dsb)
loan_data <- loan_data |>
  select(-mths_since_last_record, -mths_since_last_delinq, -mths_since_last_major_derog)

# === 8. Imputasi Kolom Numerik yang Masih Memiliki Missing Values ===
# Misalnya, imputasi kolom dengan median untuk 'tot_coll_amt', 'tot_cur_bal', dan 'total_rev_hi_lim'
loan_data$tot_coll_amt[is.na(loan_data$tot_coll_amt)] <- median(loan_data$tot_coll_amt, na.rm = TRUE)
loan_data$tot_cur_bal[is.na(loan_data$tot_cur_bal)] <- median(loan_data$tot_cur_bal, na.rm = TRUE)
loan_data$total_rev_hi_lim[is.na(loan_data$total_rev_hi_lim)] <- median(loan_data$total_rev_hi_lim, na.rm = TRUE)

# === 9. Hapus Kolom yang Tidak Relevan ===
# Misalnya, jika 'desc' kolom deskripsi teks tidak relevan untuk analisis, hapus saja
loan_data <- loan_data |>
  select(-desc)

# === 10. Cek Missing Values Setelah Pembersihan ===
missing_data_after <- loan_data |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") |>
  filter(missing_count > 0)

# Tampilkan kolom yang masih ada missing values
print(missing_data_after)

# === 11. Cek Dimensi Dataset Setelah Pembersihan ===
dim(loan_data)  # Berapa baris dan kolom yang tersisa?

# Cek struktur dataset untuk memastikan tidak ada missing values
glimpse(loan_data)  # Lihat struktur data setelah imputasi
