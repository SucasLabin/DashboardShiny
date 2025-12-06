# # ---------------------------------------------------------
# # CARREGAMENTO E PROCESSAMENTO DOS DADOS
# # ---------------------------------------------------------
# 
# dadoscams2023 <- read.csv2("daily_pm25_all_regions_2023.csv")
# dadoscams2022 <- read.csv2("daily_pm25_all_regions_2022.csv")
# dadosdon2023 <- readxl::read_xlsx("Donkelar_dados_completos_consolidado_2023.xlsx")
# dadosdon2022 <- readxl::read_xlsx("Donkelar_dados_completos_consolidado_2022.xlsx")
# 
# # DON consolidado
# base_total <- rbind(dadosdon2022, dadosdon2023)
# base_don <- base_total |> filter(SIGLA_UF %in% c('15', 'PA'))
# 
# # CAMS 2023
# long23 <- reshape2::melt(dadoscams2023, id.vars="Date")
# colnames(long23) <- c("Date","CodRes","pm2.5")
# long23$Cod <- substring(long23$CodRes, 4,10)
# long23$Cod2 <- substring(long23$Cod, 1,6)
# long23$UF <- substring(long23$Cod, 1,2)
# long23$Date2 <- dmy(long23$Date)
# long23$ano <- year(long23$Date2)
# long23$month <- month(long23$Date2)
# 
# # CAMS 2022
# long22 <- reshape2::melt(dadoscams2022, id.vars="Date")
# colnames(long22) <- c("Date","CodRes","pm2.5")
# long22$Cod <- substring(long22$CodRes, 4,10)
# long22$UF <- substring(long22$Cod, 1,2)
# long22$Date2 <- ymd(long22$Date)
# long22$ano <- year(long22$Date2)
# long22$month <- month(long22$Date2)
# 
# # CAMS completo
# long22$Cod2 <- NA   # adiciona a coluna que falta
# base_cams <- rbind(long23, long22) |> filter(UF == '15')
# base_cams$CodRes <- stringr::str_remove(base_cams$CodRes, 'ID_')
# 
# # Agrupamentos (municipio-ano)
# cams_municipio_ano <- base_cams %>%
#   group_by(CodRes, ano) %>%
#   summarise(pm2.5_mean = mean(pm2.5, na.rm=TRUE))
# 
# don_municipio_ano <- base_don %>%
#   group_by(CD_MUN, Ano) %>%
#   summarise(Media_PM25 = mean(Media_PM25, na.rm=TRUE))
# 
# # Mapa
# mun_shp <- read_municipality(year = 2020) |>
#   filter(str_detect(code_muni, '15')) |>
#   mutate(code_muni = as.character(code_muni))
# 
# mapa_cams <- mun_shp |> left_join(cams_municipio_ano, by = c("code_muni"="CodRes"))
# mapa_don  <- mun_shp |> left_join(don_municipio_ano, by = c("code_muni"="CD_MUN"))
# 
# # Merge para análises
# base_merge <- mapa_cams |> st_drop_geometry() %>%
#   select(code_muni, ano, PM25_CAMS = pm2.5_mean) %>%
#   inner_join(
#     mapa_don |> st_drop_geometry() %>%
#       select(code_muni, ano = Ano, PM25_DON = Media_PM25),
#     by="code_muni"
#   ) |> drop_na()
# 
# # ---------------------------
# # Salvamento
# # ---------------------------
# 
# saveRDS(base_cams, "base_cams.rds")
# saveRDS(base_don, "base_don.rds")
# saveRDS(base_merge, "base_merge.rds")
# saveRDS(base_total, "base_total.rds")
# saveRDS(cams_municipio_ano, "cams_municipio_ano.rds")
# saveRDS(dadoscams2022, "dadoscams2022.rds")
# saveRDS(dadoscams2023, "dadoscams2023.rds")
# saveRDS(dadosdon2022, "dadosdon2022.rds")
# saveRDS(dadosdon2023, "dadosdon2023.rds")
# saveRDS(don_municipio_ano, "don_municipio_ano.rds")
# saveRDS(long22, "long22.rds")
# saveRDS(long23, "long23.rds")
# saveRDS(mapa_cams, "mapa_cams.rds")
# saveRDS(mapa_don, "mapa_don.rds")
# saveRDS(mun_shp, "mun_shp.rds")




# ------------------------------
# Carregamento
# ------------------------------

base_cams <- readRDS("base_cams.rds")
base_don <- readRDS("base_don.rds")
base_merge <- readRDS("base_merge.rds")
base_total <- readRDS("base_total.rds")

cams_municipio_ano <- readRDS("cams_municipio_ano.rds")
dadoscams2022 <- readRDS("dadoscams2022.rds")
dadoscams2023 <- readRDS("dadoscams2023.rds")

dadosdon2022 <- readRDS("dadosdon2022.rds")
dadosdon2023 <- readRDS("dadosdon2023.rds")
don_municipio_ano <- readRDS("don_municipio_ano.rds")

long22 <- readRDS("long22.rds")
long23 <- readRDS("long23.rds")

mapa_cams <- readRDS("mapa_cams.rds")
mapa_don <- readRDS("mapa_don.rds")
mun_shp <- readRDS("mun_shp.rds")

