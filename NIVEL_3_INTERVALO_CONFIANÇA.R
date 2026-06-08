# ============================================
# INTERVALO DE CONFIANÇA (95%)
# ============================================

# ETAPA 1: INTERVALO DE CONFIANÇA - SUDESTE 2024

# Selecionar amostra: estados do Sudeste em 2024
dados_ic_sudeste <- dados_completos %>%
  filter(
    ANO == 2024,
    UF %in% c("SP", "RJ", "MG", "ES")
  )

# Estatísticas da amostra
n_sudeste <- nrow(dados_ic_sudeste)
media_sudeste <- mean(dados_ic_sudeste$MBAS_TOTAL, na.rm = TRUE)
desvio_sudeste <- sd(dados_ic_sudeste$MBAS_TOTAL, na.rm = TRUE)

# CORREÇÃO: Calcular o erro padrão
erro_padrao_sudeste <- desvio_sudeste / sqrt(n_sudeste)

# Nível de confiança: 95% -> Z = 1.96
Z <- 1.96
erro_maximo_sudeste <- Z * erro_padrao_sudeste

# Intervalo de Confiança
IC_inferior_sudeste <- media_sudeste - erro_maximo_sudeste
IC_superior_sudeste <- media_sudeste + erro_maximo_sudeste

# Exibir resultados
cat("=== INTERVALO DE CONFIANÇA (95%) - SUDESTE 2024 ===\n\n")
cat("Amostra: Sudeste (SP, RJ, MG, ES) - Ano 2024\n")
cat("Tamanho da amostra (n):", n_sudeste, "\n")
cat("Média amostral (x̄):", round(media_sudeste, 3), "\n")
cat("Desvio padrão amostral (s):", round(desvio_sudeste, 3), "\n")
cat("Erro padrão (EP = s/√n):", round(erro_padrao_sudeste, 3), "\n")
cat("Erro máximo de estimativa:", round(erro_maximo_sudeste, 3), "\n\n")
cat("Intervalo de Confiança (95%):\n")
cat("  [", round(IC_inferior_sudeste, 3), ";", round(IC_superior_sudeste, 3), "]\n\n")
cat("Interpretação: Com 95% de confiança, a verdadeira média nacional\n")
cat("de MBAS por operação está entre", round(IC_inferior_sudeste, 3), "e", round(IC_superior_sudeste, 3), ".\n")

# ============================================
# ETAPA 2: FUNÇÃO PARA CALCULAR INTERVALO DE CONFIANÇA
# ============================================

calcular_ic <- function(dados, coluna_mbas = "MBAS_TOTAL", confianca = 0.95) {
  # dados: dataframe com os dados
  # coluna_mbas: nome da coluna com os valores de MBAS
  # confianca: nível de confiança (padrão 0.95 = 95%)
  
  # Calcular Z para o nível de confiança
  Z <- qnorm(1 - (1 - confianca) / 2)
  
  # Estatísticas
  n <- nrow(dados)
  media <- mean(dados[[coluna_mbas]], na.rm = TRUE)
  desvio <- sd(dados[[coluna_mbas]], na.rm = TRUE)
  erro_padrao <- desvio / sqrt(n)
  erro_maximo <- Z * erro_padrao
  
  # Intervalo de Confiança
  IC_inferior <- media - erro_maximo
  IC_superior <- media + erro_maximo
  
  # Retornar resultados como lista
  return(list(
    n = n,
    media = media,
    desvio = desvio,
    erro_padrao = erro_padrao,
    erro_maximo = erro_maximo,
    IC_inferior = IC_inferior,
    IC_superior = IC_superior,
    nivel_confianca = confianca,
    Z = Z
  ))
}

# Testar a função com os dados do Sudeste
resultado_sudeste <- calcular_ic(dados_ic_sudeste)
print(resultado_sudeste)

# ============================================
# ETAPA 3: INTERVALO DE CONFIANÇA POR REGIÃO (2024)
# ============================================

# Definir as regiões do Brasil
dados_regioes <- dados_completos %>%
  filter(ANO == 2024) %>%
  mutate(
    Regiao = case_when(
      UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      UF %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      UF %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      UF %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ "Outros"
    )
  ) %>%
  filter(Regiao != "Outros")

# Calcular IC para cada região
regioes <- unique(dados_regioes$Regiao)
resultados_regioes <- list()

for(reg in regioes) {
  dados_reg <- dados_regioes %>% filter(Regiao == reg)
  if(nrow(dados_reg) > 0) {
    resultados_regioes[[reg]] <- calcular_ic(dados_reg)
    resultados_regioes[[reg]]$nome <- reg
    resultados_regioes[[reg]]$n_operacoes <- nrow(dados_reg)
  }
}

# Exibir resultados em tabela
cat("\n=== INTERVALO DE CONFIANÇA (95%) POR REGIÃO - 2024 ===\n\n")
tabela_regioes <- data.frame(
  Regiao = character(),
  N_Operacoes = integer(),
  Media = numeric(),
  IC_Inferior = numeric(),
  IC_Superior = numeric(),
  stringsAsFactors = FALSE
)

for(reg in names(resultados_regioes)) {
  r <- resultados_regioes[[reg]]
  tabela_regioes <- rbind(tabela_regioes, data.frame(
    Regiao = r$nome,
    N_Operacoes = r$n_operacoes,
    Media = round(r$media, 3),
    IC_Inferior = round(r$IC_inferior, 3),
    IC_Superior = round(r$IC_superior, 3)
  ))
}

print(tabela_regioes)

# Gráfico comparativo dos ICs por região
grafico_ic_regioes <- ggplot(tabela_regioes, aes(x = Regiao, y = Media, color = Regiao)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior), width = 0.2, size = 1) +
  labs(
    title = "Intervalo de Confiança (95%) por Região - 2024",
    subtitle = "Comparação da média de MBAS por operação entre regiões brasileiras",
    x = "Região",
    y = "MBAS por Operação"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # Inverter eixo para melhor leitura

print(grafico_ic_regioes)
ggplotly(grafico_ic_regioes)

# ============================================
# ETAPA 4: INTERVALO DE CONFIANÇA PARA UM ESTADO ESPECÍFICO
# ============================================

# Selecionar um estado (exemplo: São Paulo)
estado_escolhido <- "SP"

dados_estado <- dados_completos %>%
  filter(ANO == 2024, UF == estado_escolhido)

resultado_estado <- calcular_ic(dados_estado)

cat("\n=== INTERVALO DE CONFIANÇA (95%) - ESTADO:", estado_escolhido, "=== \n\n")
cat("Ano: 2024\n")
cat("Tamanho da amostra (n):", resultado_estado$n, "\n")
cat("Média amostral (x̄):", round(resultado_estado$media, 3), "\n")
cat("Intervalo de Confiança (95%):\n")
cat("  [", round(resultado_estado$IC_inferior, 3), ";", round(resultado_estado$IC_superior, 3), "]\n")

# Comparar com a média da região Sudeste
cat("\n=== COMPARAÇÃO: ESTADO vs REGIÃO ===\n")
cat("Média do estado", estado_escolhido, ":", round(resultado_estado$media, 3), "\n")
cat("Média da região Sudeste:", round(resultado_sudeste$media, 3), "\n")
cat("Diferença:", round(resultado_estado$media - resultado_sudeste$media, 3), "\n")

# Gráfico comparativo
dados_comparacao_estado <- data.frame(
  Amostra = c("Sudeste (Região)", estado_escolhido),
  Media = c(resultado_sudeste$media, resultado_estado$media),
  IC_Inf = c(resultado_sudeste$IC_inferior, resultado_estado$IC_inferior),
  IC_Sup = c(resultado_sudeste$IC_superior, resultado_estado$IC_superior)
)

grafico_comp_estado <- ggplot(dados_comparacao_estado, aes(x = Amostra, y = Media, color = Amostra)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup), width = 0.2, size = 1) +
  labs(
    title = paste0("Comparação: Intervalo de Confiança (95%) - Sudeste vs ", estado_escolhido),
    subtitle = "Ano 2024",
    x = "",
    y = "MBAS por Operação"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(grafico_comp_estado)
ggplotly(grafico_comp_estado)

# ============================================
# ETAPA 5: INTERVALO DE CONFIANÇA POR ANO (Comparação Temporal)
# ============================================

# Calcular IC para cada ano (usando Sudeste como amostra)
anos <- c(2023, 2024, 2025)
resultados_anos <- list()

for(ano in anos) {
  dados_ano <- dados_completos %>%
    filter(ANO == ano, UF %in% c("SP", "RJ", "MG", "ES"))
  
  if(nrow(dados_ano) > 0) {
    resultados_anos[[as.character(ano)]] <- calcular_ic(dados_ano)
    resultados_anos[[as.character(ano)]]$ano <- ano
    resultados_anos[[as.character(ano)]]$n_operacoes <- nrow(dados_ano)
  }
}

# Criar tabela comparativa
tabela_anos <- data.frame(
  Ano = integer(),
  N_Operacoes = integer(),
  Media = numeric(),
  IC_Inferior = numeric(),
  IC_Superior = numeric(),
  stringsAsFactors = FALSE
)

for(ano in names(resultados_anos)) {
  r <- resultados_anos[[ano]]
  tabela_anos <- rbind(tabela_anos, data.frame(
    Ano = r$ano,
    N_Operacoes = r$n_operacoes,
    Media = round(r$media, 3),
    IC_Inferior = round(r$IC_inferior, 3),
    IC_Superior = round(r$IC_superior, 3)
  ))
}

cat("\n=== INTERVALO DE CONFIANÇA (95%) POR ANO ===\n")
print(tabela_anos)

# Gráfico de evolução temporal (com IC)
grafico_evolucao <- ggplot(tabela_anos, aes(x = Ano, y = Media)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 4, color = "darkred") +
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior), width = 0.3, size = 1, color = "darkgreen") +
  geom_ribbon(aes(ymin = IC_Inferior, ymax = IC_Superior), alpha = 0.2, fill = "steelblue") +
  labs(
    title = "Evolução da Média de MBAS com Intervalo de Confiança (95%)",
    subtitle = "Amostra: Sudeste (SP, RJ, MG, ES) | 2023-2025",
    x = "Ano",
    y = "MBAS por Operação"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = anos)

print(grafico_evolucao)
ggplotly(grafico_evolucao, tooltip = c("x", "y", "ymin", "ymax"))

# ============================================
# ETAPA 6: Distribuição Normal com IC 
# ============================================

# Criar a curva normal baseada na distribuição amostral da média
x_vals <- seq(media_sudeste - 4 * erro_padrao_sudeste, 
              media_sudeste + 4 * erro_padrao_sudeste, 
              length.out = 300)

curva_normal <- data.frame(
  MBAS = x_vals,
  Densidade = dnorm(x_vals, mean = media_sudeste, sd = erro_padrao_sudeste)
)

# Pontos para anotações da regra 68-95-99.7
um_sigma_inf <- media_sudeste - erro_padrao_sudeste
um_sigma_sup <- media_sudeste + erro_padrao_sudeste
dois_sigma_inf <- media_sudeste - 2 * erro_padrao_sudeste
dois_sigma_sup <- media_sudeste + 2 * erro_padrao_sudeste
tres_sigma_inf <- media_sudeste - 3 * erro_padrao_sudeste
tres_sigma_sup <- media_sudeste + 3 * erro_padrao_sudeste

# Gráfico da distribuição normal
grafico_normal <- ggplot(curva_normal, aes(x = MBAS, y = Densidade)) +
  # Curva normal
  geom_line(color = "darkblue", size = 1.2) +
  
  # Área do IC de 95% (Z = ±1.96)
  geom_area(data = subset(curva_normal, MBAS >= IC_inferior_sudeste & MBAS <= IC_superior_sudeste),
            aes(y = Densidade), fill = "steelblue", alpha = 0.4) +
  
  # Linha vertical da média
  geom_vline(xintercept = media_sudeste, color = "darkred", linetype = "solid", size = 1) +
  
  # Linhas dos limites do IC
  geom_vline(xintercept = IC_inferior_sudeste, color = "darkgreen", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = IC_superior_sudeste, color = "darkgreen", linetype = "dashed", size = 0.8) +
  
  # Linhas dos limites de 1, 2 e 3 sigmas (opcional - regra 68-95-99.7)
  geom_vline(xintercept = um_sigma_inf, color = "purple", linetype = "dotted", size = 0.5, alpha = 0.5) +
  geom_vline(xintercept = um_sigma_sup, color = "purple", linetype = "dotted", size = 0.5, alpha = 0.5) +
  geom_vline(xintercept = dois_sigma_inf, color = "purple", linetype = "dotted", size = 0.5, alpha = 0.3) +
  geom_vline(xintercept = dois_sigma_sup, color = "purple", linetype = "dotted", size = 0.5, alpha = 0.3) +
  
  # Anotações da média e IC
  annotate("text", x = media_sudeste, y = max(curva_normal$Densidade) * 0.95,
           label = paste0("μ = ", round(media_sudeste, 2)), 
           color = "darkred", size = 3.5, fontface = "bold") +
  
  annotate("text", x = IC_inferior_sudeste - (erro_padrao_sudeste * 0.8), 
           y = max(curva_normal$Densidade) * 0.15,
           label = paste0("IC 95%\n", round(IC_inferior_sudeste, 2)), 
           color = "darkgreen", size = 3) +
  
  annotate("text", x = IC_superior_sudeste + (erro_padrao_sudeste * 0.8), 
           y = max(curva_normal$Densidade) * 0.15,
           label = paste0("IC 95%\n", round(IC_superior_sudeste, 2)), 
           color = "darkgreen", size = 3) +
  
  # Anotação da área sombreada
  annotate("text", x = media_sudeste, y = max(curva_normal$Densidade) * 0.3,
           label = "Área central = 95%\n(Intervalo de Confiança)", 
           color = "steelblue", size = 3.5, fontface = "italic") +
  
  # Título e labels
  labs(
    title = "D) Distribuição Amostral da Média (Teorema Central do Limite)",
    subtitle = paste0("EP = ", round(erro_padrao_sudeste, 3), " | IC 95% = [", 
                      round(IC_inferior_sudeste, 2), "; ", round(IC_superior_sudeste, 2), "]"),
    x = "MBAS por Operação",
    y = "Densidade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

# ============================================
# ETAPA 7: DASHBOARD COMPARATIVO COMPLETO
# ============================================

if (!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
}

# Gráfico 1: IC por região
grafico_regioes <- ggplot(tabela_regioes, aes(x = reorder(Regiao, Media), y = Media, color = Regiao)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior), width = 0.2, size = 1) +
  coord_flip() +
  labs(title = "IC (95%) por Região", x = "", y = "MBAS") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico 2: Evolução temporal
grafico_temporal <- ggplot(tabela_anos, aes(x = Ano, y = Media)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior), width = 0.3, size = 0.8) +
  labs(title = "Evolução Temporal (IC 95%)", x = "Ano", y = "MBAS") +
  theme_minimal()

# Gráfico 3: Boxplot da amostra (Sudeste 2024)
grafico_box <- ggplot(dados_ic_sudeste, aes(x = "Sudeste 2024", y = MBAS_TOTAL)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  labs(title = "Distribuição da Amostra", x = "", y = "MBAS") +
  theme_minimal()

# Dashboard
dashboard_2x2 <- (grafico_regioes + grafico_temporal) / (grafico_box + grafico_normal) +
  plot_annotation(
    title = "Análise do Intervalo de Confiança (95%) - MBAS por Operação",
    subtitle = paste0("Amostra: Sudeste (SP, RJ, MG, ES) - 2024 | n = ", n_sudeste, 
                      " | Média = ", round(media_sudeste, 2),
                      " | IC = [", round(IC_inferior_sudeste, 2), "; ", round(IC_superior_sudeste, 2), "]"),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "darkgray")
    )
  )
print(dashboard_2x2)

# ============================================
# TESTE DE HIPÓTESES: Média do Sudeste vs Média Nacional
# H₀: μ_sudeste = μ_nacional
# H₁: μ_sudeste ≠ μ_nacional
# ============================================

# Calcular média nacional (todas as regiões, exceto Sudeste)
dados_nacionais <- dados_completos %>%
  filter(ANO == 2024, !UF %in% c("SP", "RJ", "MG", "ES"))

media_nacional <- mean(dados_nacionais$MBAS_TOTAL, na.rm = TRUE)

# Teste t para uma amostra
teste_t_mbas <- t.test(dados_ic_sudeste$MBAS_TOTAL, mu = media_nacional)

# Exibir resultados
cat("=== TESTE DE HIPÓTESES: Média de MBAS ===\n\n")
cat("Média do Sudeste:", round(media_sudeste, 3), "\n")
cat("Média Nacional (sem Sudeste):", round(media_nacional, 3), "\n")
cat("Diferença:", round(media_sudeste - media_nacional, 3), "\n\n")

cat("Resultado do Teste t:\n")
cat("  Estatística t:", round(teste_t_mbas$statistic, 4), "\n")
cat("  Graus de liberdade:", round(teste_t_mbas$parameter, 0), "\n")
cat("  p-valor:", round(teste_t_mbas$p.value, 6), "\n\n")

if(teste_t_mbas$p.value < 0.05) {
  cat("✅ Conclusão: Rejeitamos H₀. A média do Sudeste é SIGNIFICATIVAMENTE diferente da média nacional.\n")
  cat("   Isso indica que o Sudeste não é uma amostra representativa do país para MBAS.\n")
} else {
  cat("❌ Conclusão: Não rejeitamos H₀. A média do Sudeste NÃO é estatisticamente diferente da média nacional.\n")
  cat("   Isso indica que o Sudeste pode ser usado como amostra representativa do país.\n")
}

# ============================================
# GRÁFICO DE DISTRIBUIÇÃO NORMAL PARA TESTE T
# Média do Sudeste vs Média Nacional
# ============================================

# 1. Calcular média nacional (excluindo Sudeste)
dados_nacionais <- dados_completos %>%
  filter(ANO == 2024, !UF %in% c("SP", "RJ", "MG", "ES"))

media_nacional <- mean(dados_nacionais$MBAS_TOTAL, na.rm = TRUE)

# 2. Teste t para uma amostra
teste_t_mbas <- t.test(dados_ic_sudeste$MBAS_TOTAL, mu = media_nacional)

# 3. Extrair estatística t
t_calculado <- teste_t_mbas$statistic

# 4. Criar curva normal padrão
z_vals <- seq(-4, 4, length.out = 300)
curva_normal_padrao <- data.frame(Z = z_vals, Densidade = dnorm(z_vals))

# 5. Valor crítico (bilateral, α = 0.05)
z_critico <- qnorm(0.975)  # ~1.96

# 6. Criar gráfico
grafico_z_teste <- ggplot(curva_normal_padrao, aes(x = Z, y = Densidade)) +
  geom_line(color = "darkblue", size = 1) +
  geom_area(data = subset(curva_normal_padrao, Z >= abs(t_calculado)), 
            aes(y = Densidade), fill = "red", alpha = 0.5) +
  geom_area(data = subset(curva_normal_padrao, Z <= -abs(t_calculado)), 
            aes(y = Densidade), fill = "red", alpha = 0.5) +
  geom_vline(xintercept = t_calculado, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -t_calculado, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = z_critico, color = "orange", linetype = "dotted", size = 1) +
  geom_vline(xintercept = -z_critico, color = "orange", linetype = "dotted", size = 1) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "solid", size = 0.5) +
  annotate("text", x = t_calculado + 0.3, y = 0.05, 
           label = paste0("t = ", round(t_calculado, 4)), color = "red", size = 3.5) +
  annotate("text", x = z_critico + 0.3, y = 0.25, 
           label = paste0("Z crítico = ±", round(z_critico, 3)), color = "orange", size = 3.5) +
  annotate("text", x = 0, y = 0.35, 
           label = paste0("p-valor = ", round(teste_t_mbas$p.value, 6)), 
           color = ifelse(teste_t_mbas$p.value < 0.05, "red", "darkgreen"), size = 4, fontface = "bold") +
  annotate("text", x = 0, y = 0.3, 
           label = ifelse(teste_t_mbas$p.value < 0.05, "Decisão: REJEITAR H₀", "Decisão: NÃO REJEITAR H₀"), 
           color = ifelse(teste_t_mbas$p.value < 0.05, "red", "darkgreen"), size = 4, fontface = "bold") +
  labs(
    title = "Teste de Hipóteses: Média do Sudeste vs Média Nacional",
    subtitle = "H₁: μ_sudeste ≠ μ_nacional | Área vermelha = p-valor (bilateral)",
    x = "Estatística t (escala Z)",
    y = "Densidade"
  ) +
  theme_minimal()

# Exibir
print(grafico_z_prop)
