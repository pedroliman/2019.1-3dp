#### Initial Setup ####
# Este arquivo apenas roda a análise após a execução da Simulação.
## Rodar Antes de Gerar o Source:
opcoes = list(
  VarResposta = "sNPVProfit1",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 10,
  VarTempo = "time",
  VarCriterio = "RegretPercentil75",
  SentidoCriterio = "min",
  Paralelo = TRUE,
  ModoParalelo = "PSOCK", # PSOCK - Windows e Linux. FORK - Apenas UNIX
  SimularApenasCasoBase = FALSE,
  FullFactorialDesign = TRUE,
  FiltrarCasosPlausiveis = FALSE
)
INICIALIZAR_ESTOQUES_COM_CASO_BASE = FALSE; SIMULAR_HISTORICO_DIFERENTE = FALSE; ANO_INICIO_AVALIACAO = 2018
planilha_inputs = "params_calibracao.xlsx"
START<-2018; FINISH <-2028; STEP<-0.0625; SIM_TIME <- seq(START, FINISH, by=STEP)
VERIFICAR_STOCKS = FALSE; VERIFICAR_CHECKS = FALSE; CHECK_PRECISION = 0.001; 
BROWSE_ON_DIFF = TRUE; VERIFICAR_GLOBAL = FALSE;
source('funcoes.R', encoding = 'UTF-8')


#### Load Results ####
# Carregando Dados da simulação
# Salvar Resultados com apenas 10 anos simulados.
# Win
results_path = "C:/Temporario/rdm-results-backup/"
# Linux
results_path = "/media/pedro/OS/Temporario/rdm-results-backup/"
#save(results, file = paste0(results_path,"results_final.rda"))

#results = readRDS()

load(paste0(results_path,"results_final.rda"))

#### Regret Analysis for Tradeoff Plots ####

# Observando apenas duas medi??es por ano para economizar espa?o:
ano_inicial = min(results$DadosSimulados$time)
ano_final = max(results$DadosSimulados$time)
vetor_tempo = seq.default(from = ano_inicial, to = ano_final, length.out = 21)

results$DadosSimulados = subset(results$DadosSimulados, time %in% vetor_tempo)


## Calculando Resultados Adicionais no Último Período

# Calculando algumas variáveis com base nos dados Simulados:

results$DadosUltimoPeriodo$sPriceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("sPrice1", "sPrice2", "sPrice3", "sPrice4")])

results$DadosUltimoPeriodo$aPerformanceAvg = rowSums(results$DadosUltimoPeriodo[,c("aOrderShare1", "aOrderShare2", "aOrderShare3", "aOrderShare4")] * results$DadosUltimoPeriodo[,c("aPerformance1", "aPerformance2", "aPerformance3", "aPerformance4")])

results$DadosUltimoPeriodo$sTotalInstalledBase = rowSums(results$DadosUltimoPeriodo[,c("sInstalledBase1", "sInstalledBase2", "sInstalledBase3", "sInstalledBase4")])

# Arredondando os Valores das Estratégias:

results$Ensemble[,"aSwitchForCapacityStrategy2"] = round(results$Ensemble[,"aSwitchForCapacityStrategy2"],0)
results$Ensemble[,"aSwitchForCapacityStrategy3"] = round(results$Ensemble[,"aSwitchForCapacityStrategy3"],0)
results$Ensemble[,"aSwitchForCapacityStrategy4"] = round(results$Ensemble[,"aSwitchForCapacityStrategy4"],0)




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

dados_simulados = results$DadosSimulados %>% 
  group_by(time, Lever) %>%
  summarise_all(funs(mean, sd, robustness, percentile_75))

dados_simulados = dplyr::inner_join(dados_simulados, results$Inputs$Levers)

dados_ultimo_ano = dados_simulados[dados_simulados$time == 2028,]

# Funcionalidade
write.csv(x = dados_simulados, file = "output_analise_multi_objetivo.csv", row.names = F)

write.csv(x = dados_ultimo_ano, file = "output_analise_multi_objetivo.csv", row.names = F)

uncertainty_original_names = names(df_vulnerabilidade_profit[,5:ncol(df_vulnerabilidade_profit)])

uncertainty_new_names = c("Discard.Rate", "Demand.Elastic.", 
                          "Mkt.Size", "Inov.Adop.Fract.", "Dif.Str.", 
                          "Rep.Delay", "Cap.Acqui.Delay", "Sens.Attract.Avail.", 
                          "Sens.Attract.Price", "Learn.Curv.Str.", "Fixed.Var.Ratio.", 
                          "Cap.Utiliz.", "Sens.Price.Cost", "Sens.Price.Demmand.Suppl.", 
                          "Sens.Price.Share", "RD.Real.Time", "Patent.Cost", 
                          "Patent.Eval.Time", "Rej.Ratio", "Patent.Useful.Time", 
                          "Patent.Perf.Slope", "Sens.Attract.Perf.", "Init.Reord.Share", 
                          "Open.RD.2", "Open.RD.3", "Open.RD.4", "RD.Budgt.2", 
                          "RD.Budgt.3", "RD.Budgt.4", "Tgt.Mkt.Share.2", "Tgt.Mkt.Share.3", 
                          "Tgt.Mkt.Share.4", "Mkt.Strat.2", "Mkt.Strat.3", 
                          "Mkt.Strat.4")



#### Vulnerability Dataframes ####

estrategias_analisar = c(31, 13, 15)

variaveis_interesse = c("sNPVProfit1", "aPerformance1", "aOrderShare1", "sTotalInstalledBase", "aPerformanceAvg", "sPriceAvg")
sentido = c(rep("max",5), "min")
sufixo = c("Regret")
sentidos_vulnerabilidade = c(">=", ">=")
variavel_percentil = c("Percentil75")

vulnerability_analysis_list = list()

for (estrategia in estrategias_analisar) {
  print(paste("Analisando Estrategia ",estrategia))
  
  for (variavel_interesse in variaveis_interesse) {
    print(paste("Variavel ",variavel_interesse))
    
    posicao_variavel = which(variaveis_interesse==variavel_interesse)
    
    # Criando DF de Vulnerabilidade:
    analise_regret = calcular_e_resumir_regret(dados = results$DadosUltimoPeriodo, 
                                               var_resposta = variavel_interesse, 
                                               var_cenarios = results$Opcoes$VarCenarios, 
                                               var_estrategias = results$Opcoes$VarEstrategias,
                                               sentido = sentido[posicao_variavel])
    
    # Definindo Threshold:
    threshold = as.numeric(analise_regret$ResumoEstrategias[which(analise_regret$ResumoEstrategias$Lever==estrategia),paste0(variavel_interesse, sufixo, variavel_percentil)]) 
    
    # Criando DF De vulnerabilidade
    df_vulnerabilidade = obter_df_vulnerabilidade(results = results, 
                                                         estrategia_candidata = estrategia, 
                                                         variavel_resposta = paste0(variavel_interesse,sufixo), 
                                                         threshold = threshold, 
                                                         planilha_inputs = planilha_inputs, 
                                                         sentido_vulnerabilidade = ">=",
                                                         AnaliseRegret = analise_regret)
    
    # Alterando o Nome do DF De Vulnerabilidade:
    names(df_vulnerabilidade) = c(names(df_vulnerabilidade[,1:4]),uncertainty_new_names)
    
    analysis_name = paste0(estrategia,variavel_interesse,sufixo)
    # Gerando Arquivos para Análise no PRIM:
    write.csv(df_vulnerabilidade$CasoInteresse, file = paste0(estrategia,variavel_interesse,sufixo,"_resposta.csv"))
    
    # Strategies as Categorical Variables:
    write.csv(df_vulnerabilidade[,5:ncol(df_vulnerabilidade)], file = paste0(estrategia,variavel_interesse,sufixo,"_incertezas.csv"))
    
    vulnerability_analysis_list[[analysis_name]] = df_vulnerabilidade[,c(1,5:ncol(df_vulnerabilidade))]
    
  }
  
}



#### Random Forest Importance Analysis ####

library(randomForest)

# Using for since I don't know how to use apply for this
rf_models_list = list()
rf_models_oobs = list()
rf_models_oobs_df = data.frame()
rf_models_importance_df = data.frame()

for (item in names(vulnerability_analysis_list)){
  print(paste0("Creating RF Model for ", item))
  rf_model = randomForest(formula = factor(CasoInteresse) ~ ., data = vulnerability_analysis_list[[item]])
  OOB_error_rate_estimate = rf_model$err.rate[rf_model$ntree,1]
  importance_order = order(rf_model$importance, decreasing = T)
  variable_names = rownames(rf_model$importance)
  rf_models_importance_df = rbind(rf_models_importance_df, data.frame(
    Variable = variable_names,
    ImportanceOrder = importance_order,
    MeanDecreaseGini = unname(rf_model$importance),
    Model = item
  ))
  rf_models_oobs_df = rbind(rf_models_oobs_df,
                            data.frame(
                              Model = item,
                              OOB = OOB_error_rate_estimate
                            ))
  rf_models_list[[item]] = rf_model
}

write.csv(rf_models_importance_df,"rf_models_importance_df.csv")
write.csv(rf_models_oobs_df, "rf_models_oobs_df.csv")


#### Boruta Feature Analysis ####
library(Boruta)

boruta_importance_results = data.frame()
boruta_output_list = list()

for (item in names(vulnerability_analysis_list)){
  print(paste0("Executing Boruta Feature Analysis for ", item))
  
  boruta_output <- Boruta(factor(CasoInteresse) ~ ., data = vulnerability_analysis_list[[item]], doTrace=2)
  
  boruta_output_list[[item]] = boruta_output
  
  parametros_selecionados_boruta = getSelectedAttributes(boruta_output)
  tabela_resultados_boruta = arrange(cbind(attr=rownames(attStats(boruta_output)), attStats(boruta_output)),desc(medianImp))
  tabela_resultados_boruta$Rank = rownames(tabela_resultados_boruta)
  tabela_resultados_boruta$Model = item
  
  boruta_importance_results = rbind(boruta_importance_results, tabela_resultados_boruta)
  
}

boruta_importance_results


boruta_importance_results$Lever = ifelse(grepl("31",boruta_importance_results$Model),31,
                                         ifelse(grepl("13",boruta_importance_results$Model),13,
                                                ifelse(grepl("15",boruta_importance_results$Model),15,0)))



boruta_importance_results$Outcome = ifelse(grepl("NPVProfit1Regret",boruta_importance_results$Model),"NPV",ifelse(grepl("Performance1Regret",
                                    boruta_importance_results$Model),"Product Perf.",
                                    ifelse(grepl("OrderShare1Regret",boruta_importance_results$Model),"Mkt. Share",
                                           ifelse(grepl("PriceAvgRegret",boruta_importance_results$Model),"Avg. Price",
                                                  ifelse(grepl("PerformanceAvgRegret",boruta_importance_results$Model),"Avg. Perf.",
                                                         ifelse(grepl("TotalInstalledBaseRegret",boruta_importance_results$Model),"Installed Base",
                                                                " "))))))


## Gerando Código das Estratégias:
levers_df = results$Inputs$Levers

levers_df$Strategy = paste0(ifelse(levers_df$aSwitchForCapacityStrategy1 == 1, "A", "C"),
       ".",
       levers_df$aDesiredMarketShare1*100,
       ".R",
       substr(format(round(levers_df$aOrcamentoPeD1, 2), nsmall = 2),3,4),
       ".",
       substr(format(round(levers_df$aPercPeDAberto1, 2), nsmall = 2),3,4)
)

levers_df = levers_df[,c("Lever", "Strategy")]

final_boruta_results = dplyr::left_join(boruta_importance_results, levers_df)

colnames(final_boruta_results)[1] = "Uncertainty"
colnames(final_boruta_results)[2] = "Importance"

final_boruta_results$Rank = as.numeric(final_boruta_results$Rank)

writexl::write_xlsx(final_boruta_results, "boruta_results.xlsx")
