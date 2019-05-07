source("funcoes.R")

# Carregando Dados da simulação
# Salvar Resultados com apenas 10 anos simulados.

results_path = "C:/Temporario/rdm-results-backup/"

#save(results, file = paste0(results_path,"results_final.rda"))

#results = readRDS()

load(paste0(results_path,"results_final.rda"))


# Tornando os resultados mais leves:

# Observando apenas duas medi??es por ano para economizar espa?o:
ano_inicial = min(results$DadosSimulados$time)
ano_final = max(results$DadosSimulados$time)
vetor_tempo = seq.default(from = ano_inicial, to = ano_final, length.out = 21)

results$DadosSimulados = subset(results$DadosSimulados, time %in% vetor_tempo)


# Formas Simples de Calcular Índices de Robustez:
robustness = function(x) {
  mean(x)/sd(x)
}

percentile_75 = function(x) {
  quantile(x, probs = c(0.75))
}

## Calculando Resultados Adicionais no Último Período

# Calculando algumas variáveis com base nos dados Simulados:

results$DadosUltimoPeriodo$sPriceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("sPrice1", "sPrice2", "sPrice3", "sPrice4")])

results$DadosUltimoPeriodo$aPerformanceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("aPerformance1", "aPerformance2", "aPerformance3", "aPerformance4")])

results$DadosUltimoPeriodo$sTotalInstalledBase = rowSums(results$DadosUltimoPeriodo[,c("sInstalledBase1", "sInstalledBase2", "sInstalledBase3", "sInstalledBase4")])

## Analisando o Regret de Diversas Variáveis:


analise_regret_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = results$Opcoes$VarResposta, var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_profit = analise_regret_completa$ResumoEstrategias

analise_regret_installed_base_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sInstalledBase1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_installed_base = analise_regret_installed_base_completa$ResumoEstrategias

analise_regret_share_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aOrderShare1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_share = analise_regret_share_completa$ResumoEstrategias

analise_regret_performance_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPerformance1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_performance = analise_regret_performance_completa$ResumoEstrategias

analise_regret_patentes_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPatentesEmpresaTemAcesso1", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_patentes = analise_regret_patentes_completa$ResumoEstrategias

analise_regret_adopters_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sCumulativeAdopters", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_adopters  = analise_regret_adopters_completa$ResumoEstrategias

analise_regret_average_price_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sPriceAvg", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias, sentido = "min")

analise_regret_average_price  = analise_regret_average_price_completa$ResumoEstrategias

analise_regret_average_performance_completa  = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "aPerformanceAvg", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_average_performance  = analise_regret_average_performance_completa$ResumoEstrategias

analise_regret_total_installed_base_completa = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, var_resposta = "sTotalInstalledBase", var_cenarios = results$Opcoes$VarCenarios, var_estrategias = results$Opcoes$VarEstrategias)

analise_regret_total_installed_base  = analise_regret_total_installed_base_completa$ResumoEstrategias


# Unindo Datasets:

dataset_todas_as_metricas = left_join(analise_regret_profit, analise_regret_installed_base, by = "Lever") %>%
  left_join(., analise_regret_share, by = "Lever") %>% 
  left_join(., analise_regret_performance, by = "Lever") %>%
  left_join(., analise_regret_patentes, by = "Lever") %>%
  left_join(., analise_regret_adopters, by = "Lever") %>% 
  left_join(., analise_regret_average_price, by = "Lever") %>%
  left_join(., analise_regret_average_performance, by = "Lever") %>%
  left_join(., analise_regret_total_installed_base, by = "Lever")

# Filtrando Só o que eu quero usar e Unindo com a Descrição dos Levers:
dataset_todas_as_metricas  = dataset_todas_as_metricas %>%
  dplyr::select(., matches("RegretPercentil75|Lever")) %>%
  left_join(., results$Inputs$Levers, by = "Lever") 

# Escrever para um arquivo:

write.csv(dataset_todas_as_metricas, file = "output_regret_metricas.csv", row.names = F)

View(dataset_todas_as_metricas)

dados_simulados = results$DadosSimulados %>% 
  group_by(time, Lever) %>%
  summarise_all(funs(mean, sd, robustness, percentile_75))


dados_simulados = dplyr::inner_join(dados_simulados, results$Inputs$Levers)

dados_ultimo_ano = dados_simulados[dados_simulados$time == 2028,]

View(dados_ultimo_ano)

# Funcionalidade
write.csv(x = dados_simulados, file = "output_analise_multi_objetivo.csv", row.names = F)

write.csv(x = dados_ultimo_ano, file = "output_analise_multi_objetivo.csv", row.names = F)



#### Analisando Estratégia 31 ####
## Single Objective Analysis
estrategia_profit = 31

threshold_profit = as.numeric(analise_regret_profit[which(analise_regret_profit$Lever==estrategia_profit),"sNPVProfit1RegretPercentil75"]) 

planilha_inputs = "./../calibracao/params_calibracao_opcao1.xlsx"

uncertainty_original_names = names(df_vulnerabilidade_profit[,5:ncol(df_vulnerabilidade_profit)])

uncertainty_new_names = c("Discard.Rate", "Demand.Elastic.", 
                          "Mkt.Size", "Inov.Adop.Fract.", "Dif.Str.", 
                          "Rep.Delay", "Cap.Acqui.Delay", "Sens.Attract.Avail.", 
                          "Sens.Attract.Price", "Learn.Curv.Str.", "Fixed.Var.Ratio.", 
                          "Cap.Utiliz.", "Sens.Price.Cost", "Sens.Price.Demmand.Suppl.", 
                          "Sens.Price.Share", "R&D.Real.Time", "Patent.Cost", 
                          "Patent.Eval.Time", "Rej.Ratio", "Patent.Useful.Time", 
                          "Patent.Perf.Slope", "Sens.Attract.Perf.", "Init.Reord.Share", 
                          "Open.R&D.2", "Open.R&D.3", "Open.R&D.4", "R&D.Budgt.2", 
                          "R&D.Budgt.3", "R&D.Budgt.4", "Tgt.Mkt.Share.2", "Tgt.Mkt.Share.3", 
                          "Tgt.Mkt.Share.4", "Mkt.Strat.2", "Mkt.Strat.3", 
                          "Mkt.Strat.4")

df_vulnerabilidade_profit = obter_df_vulnerabilidade(results = results, 
                                              estrategia_candidata = estrategia_profit, 
                                              variavel_resposta = "sNPVProfit1Regret" , 
                                              threshold = threshold_profit, 
                                              planilha_inputs = planilha_inputs, 
                                              sentido_vulnerabilidade = ">=",
                                              AnaliseRegret = analise_regret_completa)

df_vulnerabilidade_profit$aSwitchForCapacityStrategy2 = round(df_vulnerabilidade_profit$aSwitchForCapacityStrategy2, digits = 0)
df_vulnerabilidade_profit$aSwitchForCapacityStrategy3 = round(df_vulnerabilidade_profit$aSwitchForCapacityStrategy3, digits = 0)
df_vulnerabilidade_profit$aSwitchForCapacityStrategy4 = round(df_vulnerabilidade_profit$aSwitchForCapacityStrategy4, digits = 0)

df_vulnerabilidade_profit13 = obter_df_vulnerabilidade(results = results, 
                                                     estrategia_candidata = 13, 
                                                     variavel_resposta = "sNPVProfit1Regret" , 
                                                     threshold = threshold_profit, 
                                                     planilha_inputs = planilha_inputs, 
                                                     sentido_vulnerabilidade = ">=",
                                                     AnaliseRegret = analise_regret_completa)

df_vulnerabilidade_profit13$aSwitchForCapacityStrategy2 = round(df_vulnerabilidade_profit13$aSwitchForCapacityStrategy2, digits = 0)
df_vulnerabilidade_profit13$aSwitchForCapacityStrategy3 = round(df_vulnerabilidade_profit13$aSwitchForCapacityStrategy3, digits = 0)
df_vulnerabilidade_profit13$aSwitchForCapacityStrategy4 = round(df_vulnerabilidade_profit13$aSwitchForCapacityStrategy4, digits = 0)

df_vulnerabilidade_profit15 = obter_df_vulnerabilidade(results = results, 
                                                       estrategia_candidata = 15, 
                                                       variavel_resposta = "sNPVProfit1Regret" , 
                                                       threshold = threshold_profit, 
                                                       planilha_inputs = planilha_inputs, 
                                                       sentido_vulnerabilidade = ">=",
                                                       AnaliseRegret = analise_regret_completa)

df_vulnerabilidade_profit15$aSwitchForCapacityStrategy2 = round(df_vulnerabilidade_profit15$aSwitchForCapacityStrategy2, digits = 0)
df_vulnerabilidade_profit15$aSwitchForCapacityStrategy3 = round(df_vulnerabilidade_profit15$aSwitchForCapacityStrategy3, digits = 0)
df_vulnerabilidade_profit15$aSwitchForCapacityStrategy4 = round(df_vulnerabilidade_profit15$aSwitchForCapacityStrategy4, digits = 0)


names(df_vulnerabilidade_profit) = c(names(df_vulnerabilidade_profit[,1:4]),uncertainty_new_names)
names(df_vulnerabilidade_profit13) = c(names(df_vulnerabilidade_profit[,1:4]),uncertainty_new_names)
names(df_vulnerabilidade_profit15) = c(names(df_vulnerabilidade_profit[,1:4]),uncertainty_new_names)

# Gerando Arquivos para Análise do Profit:
write.csv(df_vulnerabilidade_profit$CasoInteresse, file = "profit_resposta.csv")

# Strategies as Categorical Variables:
write.csv(df_vulnerabilidade_profit[,5:ncol(df_vulnerabilidade_profit)], file = "profit_incertezas.csv")

# Analising Compromise Strategy:

# Gerando Arquivos para Análise do Profit:
write.csv(df_vulnerabilidade_profit13$CasoInteresse, file = "profit_resposta13.csv")

# Strategies as Categorical Variables:
write.csv(df_vulnerabilidade_profit13[,5:ncol(df_vulnerabilidade_profit13)], file = "profit_incertezas13.csv")


# Gerando Arquivos para Análise do Profit:
write.csv(df_vulnerabilidade_profit15$CasoInteresse, file = "profit_resposta15.csv")

# Strategies as Categorical Variables:
write.csv(df_vulnerabilidade_profit15[,5:ncol(df_vulnerabilidade_profit15)], file = "profit_incertezas15.csv")

