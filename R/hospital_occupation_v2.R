# Author: Francisco Bischoff
#
# devtools::install_github("r-lib/gargle") # install this to fix non-english characters
# devtools::install_github("tidyverse/googlesheets4") # install this to new features
library(googlesheets4)
library(readxl)
library(stringr)
library(tidyverse)
library(lubridate)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("lag", "dplyr", quiet = TRUE)

# Read the dataset----

path <- "data/"
date <- paste0(strsplit(as.character(today()), "-")[[1]], collapse = "")
cache_file <- paste0(path, "vagas_hospitais.rda")

message("Starting process for date ", date, ".")

if (file.exists(cache_file)) {
  message("Loading cache file.")
  load(cache_file)
} else {
  timeline_lotacao <- NULL
  timeline_ocupados <- NULL
  timeline_vagas <- NULL
  timeline_taxa_ocupacao <- NULL
}

message("Loading input data.")
data_uci <- read_excel(paste0(path, "V_HOSP_UCI_LOTACAO_OCUP_VAGAS_", date, ".xlsx"))
data_interm <- read_excel(paste0(path, "V_HOSP_UC_INTERM_LOT_OCUP_VAG_", date, ".xlsx"))
data_isolam <- read_excel(paste0(path, "V_HOSP_CAMAS_ISOL_LOT_OCU_VAG_", date, ".xlsx"))

# select columns----
data_uci <- data_uci %>%
  select(
    data_reg, ars, id_hosp, cod_hosp, sigla_hosp, hosp, cod_tipo_uci, tipo_uci,
    lotacao_uci, ocup_uci, vagas_uci, reg_atualiz
  ) %>%
  rename(cod_tipo = cod_tipo_uci, tipo = tipo_uci, lotacao = lotacao_uci, ocup = ocup_uci, vagas = vagas_uci)

data_interm <- data_interm %>%
  select(
    data_reg, ars, id_hosp, cod_hosp, sigla_hosp, hosp, cod_tipo_uc_interm, tipo_uc_interm,
    lotacao_uc_interm, ocup_uc_interm, vagas_uc_interm, reg_atualiz
  ) %>%
  rename(cod_tipo = cod_tipo_uc_interm, tipo = tipo_uc_interm, lotacao = lotacao_uc_interm, ocup = ocup_uc_interm, vagas = vagas_uc_interm)

data_isolam <- data_isolam %>%
  select(
    data_reg, ars, id_hosp, cod_hosp, sigla_hosp, hosp, cod_tipo_isol, tipo_isol,
    lotacao_isol, ocup_isol, vagas_isol, reg_atualiz
  ) %>%
  rename(cod_tipo = cod_tipo_isol, tipo = tipo_isol, lotacao = lotacao_isol, ocup = ocup_isol, vagas = vagas_isol)

data_all <- rbind(data_uci, data_interm, data_isolam)

# remove rows that have all NA in a selection of columns and zero in lotacao----
# data_all <- data_all %>%
#  filter_at(vars(data_reg, lotacao, ocup, vagas), all_vars(!is.na(.))) %>%
#  filter(lotacao != 0)

# summarize----
data_summary <- data_all %>%
  group_by(ars, tipo) %>%
  summarise(
    lotacao = sum(lotacao, na.rm = TRUE), ocup = sum(ocup, na.rm = TRUE),
    vagas = sum(vagas, na.rm = TRUE), taxa_ocup = ifelse(lotacao > 0, ocup / lotacao, NA)
  ) %>%
  ungroup()

# sort cells, but keep totals on top----
data_total <- data_summary %>%
  filter(tipo == "Total") %>%
  arrange(ars)
data_not_total <- data_summary %>%
  filter(tipo != "Total") %>%
  arrange(ars, desc(tipo))

data_final <- rbind(data_total, data_not_total)

# build timelines ----
if (is.null(timeline_lotacao)) {
  timeline_lotacao <- select(data_final, ars, tipo)
}
if (is.null(timeline_ocupados)) {
  timeline_ocupados <- select(data_final, ars, tipo)
}
if (is.null(timeline_vagas)) {
  timeline_vagas <- select(data_final, ars, tipo)
}
if (is.null(timeline_taxa_ocupacao)) {
  timeline_taxa_ocupacao <- select(data_final, ars, tipo)
}

# if it is a new date, add column and save the cache file
if (!(date %in% names(timeline_lotacao))) {
  timeline_lotacao <- timeline_lotacao %>% mutate(!!date := data_final$lotacao)
  timeline_ocupados <- timeline_ocupados %>% mutate(!!date := data_final$ocup)
  timeline_vagas <- timeline_vagas %>% mutate(!!date := data_final$vagas)
  timeline_taxa_ocupacao <- timeline_taxa_ocupacao %>% mutate(!!date := data_final$taxa_ocup)

  save(timeline_lotacao, timeline_ocupados, timeline_vagas, timeline_taxa_ocupacao,
    file = cache_file
  )
  message("Cache saved.")
} else {
  warning("Cache was already up-to-date.")
}

# if the output doesn't have the new date, create sheet and upload results----
if (!(date %in% sheets_sheet_names("10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA"))) {
  sheets_sheet_add("10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = date)
  sheets_write(data_final, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = date)
  sheets_write(timeline_lotacao, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = "timeline_lotacao")
  sheets_write(timeline_ocupados, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = "timeline_ocupados")
  sheets_write(timeline_vagas, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = "timeline_vagas")
  sheets_write(timeline_taxa_ocupacao, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = "timeline_taxa_ocupacao")

  message("Done!")
} else {

  warning("Upstream was already up-to-date.")
}
