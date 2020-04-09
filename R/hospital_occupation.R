# Author: Francisco Bischoff
#
# devtools::install_github("r-lib/gargle") # install this to fix non-english characters
# devtools::install_github("tidyverse/googlesheets4") # install this to new features
library(googlesheets4)
library(stringr)
library(tidyverse)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("lag", "dplyr", quiet = TRUE)
# Read the dataset

#### From google sheets, old format
data_all <- read_sheet("10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = 1)
data_all <- distinct_all(data_all)
dataset_urg <- filter(data_all, cod_tipo_unidade_observacao == "URG", !str_detect(des_variavel, "qualquer tipo"))
dataset <- filter(data_all, cod_tipo_unidade_observacao == "HOSP", !str_detect(des_variavel, "qualquer tipologia"))

#### HOSP ----

types <- paste("UCI", "intermédios", sep = "|")
dataset_types <- dataset %>%
  mutate(tipo = str_extract(des_variavel, types)) %>%
  mutate(tipo = case_when(
    str_detect(des_variavel, "internam|sob resp") ~ "internamento",
    TRUE ~ as.character(tipo)
  ))

subtypes <- paste("cirúrgicos", "coronários", "polivalente",
  "pressão negativa", "pressão positiva", "sem.*pressão",
  sep = "|"
)
dataset_subtypes <- dataset_types %>%
  mutate(subtipo = str_extract(des_variavel, subtypes)) %>%
  mutate(subtipo = case_when(
    str_detect(des_variavel, "qualquer tipo") ~ "qualquer",
    str_detect(des_variavel, "outros tipos") ~ "outro",
    str_detect(des_variavel, "queimados") ~ "queimados",
    str_detect(des_variavel, "pedi|crian") ~ "pediátrico",
    str_detect(des_variavel, "neon") ~ "neonatal",
    TRUE ~ as.character(subtipo)
  ))


dataset_idade <- dataset_subtypes %>%
  mutate(idade = case_when(
    str_detect(des_variavel, "adulto|obst|gine") ~ "adulto",
    str_detect(des_variavel, "pedi|crian") ~ "criança",
    str_detect(des_variavel, "neon") ~ "neonatal"
  ))


dataset_especialidade <- dataset_idade %>%
  mutate(especialidade = case_when(
    str_detect(des_variavel, "obst|gine") ~ "obst/gine",
    str_detect(des_variavel, "medi|médi") ~ "médico",
    str_detect(des_variavel, "cir") ~ "cirúrgico",
    str_detect(des_variavel, "pedi|crian|neon") ~ "pediatria",
    str_detect(des_variavel, "psiq") ~ "psiquatria"
  ))

dataset_contagem <- dataset_especialidade %>%
  group_by(cod_unidade_observacao, cod_tipo_unidade_observacao, tipo, subtipo, idade, especialidade) %>%
  add_tally() %>%
  ungroup() %>%
  arrange(cod_unidade_observacao, cod_tipo_unidade_observacao, tipo, subtipo, idade, especialidade, desc(unidade_medida))

stopifnot(!any(dataset_contagem$n != 2))

data_final <- dataset_contagem %>%
  mutate(camas = lead(resultado), taxa_ocupacao = round(resultado * 100 / lead(resultado), 1)) %>%
  filter(unidade_medida == "utente", camas != 0) %>%
  rename(utentes = resultado) %>%
  select(
    data_altera, cod_tipo_unidade_observacao, abr_unidade_observacao, des_variavel,
    utentes, camas, taxa_ocupacao, tipo:especialidade
  ) %>%
  arrange(cod_tipo_unidade_observacao, abr_unidade_observacao, desc(tipo), idade)

#### URG ----

dataset_types_urg <- dataset_urg %>%
  mutate(tipo = case_when(
    str_detect(des_variavel, "isolamento") ~ "isolamento",
    str_detect(des_variavel, "SO") ~ "SO",
    str_detect(des_variavel, "maca") ~ "maca",
    str_detect(des_variavel, "outro serv") ~ "hospedeiro",
    str_detect(des_variavel, "noutros serv") ~ "hospedeiro externo",
    str_detect(des_variavel, "internam") ~ "internamento",
    str_detect(des_variavel, "próprio") ~ "internamento"
  ))

subtypes_urg <- paste("coronários", "polivalente",
  "pressão negativa", "pressão positiva", "sem.*pressão",
  sep = "|"
)
dataset_subtypes_urg <- dataset_types_urg %>%
  mutate(subtipo = str_extract(des_variavel, subtypes_urg)) %>%
  mutate(subtipo = case_when(
    str_detect(des_variavel, "qualquer tipo") ~ "qualquer",
    str_detect(des_variavel, "outros tipos") ~ "outro",
    str_detect(des_variavel, "queimados") ~ "queimados",
    TRUE ~ as.character(subtipo)
  ))

dataset_idade_urg <- dataset_subtypes_urg %>%
  mutate(idade = case_when(
    str_detect(des_variavel, "adulto|obst|gine") ~ "adulto",
    str_detect(des_variavel, "pedi|crian") ~ "criança",
    str_detect(des_variavel, "neon") ~ "neonatal"
  ))

dataset_especialidade_urg <- dataset_idade_urg %>%
  mutate(especialidade = case_when(
    str_detect(des_variavel, "obst|gine") ~ "obst/gine",
    str_detect(des_variavel, "outr.*méd") ~ "outras médico",
    str_detect(des_variavel, "medi") ~ "médico",
    str_detect(des_variavel, "outr.*cir") ~ "outras cirúrgico",
    str_detect(des_variavel, "cir") ~ "cirúrgico",
    str_detect(des_variavel, "pedi|crian|neon") ~ "pediatria",
    str_detect(des_variavel, "psiq") ~ "psiquatria"
  ))

urg_utentes_internados <- dataset_especialidade_urg %>% filter(unidade_medida == "utente", tipo == "internamento" | tipo == "hospedeiro")
urg_utentes_sem_local <- dataset_especialidade_urg %>% filter(unidade_medida == "utente", tipo == "maca" | tipo == "hospedeiro externo")
urg_utentes_outros <- dataset_especialidade_urg %>% filter(unidade_medida == "utente", tipo == "isolamento" | tipo == "SO")
urg_camas <- dataset_especialidade_urg %>% filter(unidade_medida != "utente")

# join "internamento" and "hospedeiros" from the same physical place.
urg_utentes_internados <- urg_utentes_internados %>%
  arrange(cod_unidade_observacao, des_variavel) %>%
  mutate(resultado = if_else(row_number() %% 2 == 0, lag(resultado) + resultado, resultado)) %>%
  filter(tipo == "internamento")


urg_utentes <- bind_rows(urg_utentes_internados, urg_utentes_outros)

dataset_contagem_urg <- bind_rows(urg_utentes, urg_camas) %>%
  group_by(cod_unidade_observacao, cod_tipo_unidade_observacao, tipo, subtipo, especialidade) %>%
  add_tally() %>%
  ungroup() %>%
  arrange(cod_unidade_observacao, cod_tipo_unidade_observacao, tipo, subtipo, especialidade, desc(unidade_medida))

stopifnot(!any(dataset_contagem_urg$n != 2))

data_final_urg <- dataset_contagem_urg %>%
  mutate(camas = lead(resultado), taxa_ocupacao = round(resultado * 100 / lead(resultado), 1)) %>%
  filter(unidade_medida == "utente", camas != 0) %>%
  rename(utentes = resultado) %>%
  select(
    data_altera, cod_tipo_unidade_observacao, abr_unidade_observacao, des_variavel,
    utentes, camas, taxa_ocupacao, tipo:especialidade
  )
message("urg_utentes_sem_local still not handled")
sem_local_final_urg <- filter(urg_utentes_sem_local, resultado != 0) %>%
  mutate(camas = NA, taxa_ocupacao = NA) %>%
  rename(utentes = resultado) %>%
  select(
    data_altera, cod_tipo_unidade_observacao, abr_unidade_observacao, des_variavel,
    utentes, camas, taxa_ocupacao, tipo:especialidade
  )
data_final_urg <- bind_rows(data_final_urg, sem_local_final_urg) %>%
  arrange(cod_tipo_unidade_observacao, abr_unidade_observacao, desc(tipo), idade)

# save(data_final, file = "data/dataset.rda")


sheets_write(data_final, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = 2)
sheets_write(data_final_urg, "10_1ENc3Flc_2TDYrhRa8GTwicC5AURo9iEuNFJzOduA", sheet = 3)


message("Done!")
