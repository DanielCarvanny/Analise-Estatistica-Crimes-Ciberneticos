# ============================================
# TESTE DE HIPÓTESES
# ============================================

# ETAPA 1: TESTE DE HIPÓTESES BÁSICO
# Comparação Brasil: 2024 vs 2025

# Filtrar dados dos anos de interesse
dados_2024_test <- dados_completos %>% filter(ANO == 2024)
dados_2025_test <- dados_completos %>% filter(ANO == 2025)

# Calcular proporções de sucesso (prisão em flagrante)
n_2024 <- nrow(dados_2024_test)
n_2025 <- nrow(dados_2025_test)

sucessos_2024 <- dados_2024_test %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
sucessos_2025 <- dados_2025_test %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()

p_2024 <- sucessos_2024 / n_2024
p_2025 <- sucessos_2025 / n_2025

# Exibir estatísticas descritivas
cat("=== ESTATÍSTICAS DESCRITIVAS ===\n\n")
cat("2024:\n")
cat("  Total de operações:", n_2024, "\n")
cat("  Operações com prisão:", sucessos_2024, "\n")
cat("  Proporção de sucesso (p):", round(p_2024 * 100, 4), "%\n\n")

cat("2025:\n")
cat("  Total de operações:", n_2025, "\n")
cat("  Operações com prisão:", sucessos_2025, "\n")
cat("  Proporção de sucesso (p):", round(p_2025 * 100, 4), "%\n\n")

cat("Diferença observada (2025 - 2024):", round((p_2025 - p_2024) * 100, 4), "%\n")

# Função para Teste Z de duas proporções
teste_z_proporcoes <- function(p1, n1, p2, n2, hipotese = "maior", alpha = 0.05) {
  # p1, n1: proporção e tamanho do grupo 1 (2024)
  # p2, n2: proporção e tamanho do grupo 2 (2025)
  # hipotese: "maior" (p2 > p1), "menor" (p2 < p1), "diferente" (p2 != p1)
  # alpha: nível de significância (padrão 0.05)
  
  # Proporção combinada sob H₀
  p_combinado <- (p1 * n1 + p2 * n2) / (n1 + n2)
  
  # Erro padrão
  erro_padrao <- sqrt(p_combinado * (1 - p_combinado) * (1/n1 + 1/n2))
  
  # Estatística Z
  Z <- (p2 - p1) / erro_padrao
  
  # Valor crítico para o nível de significância
  if(hipotese == "maior") {
    Z_critico <- qnorm(1 - alpha)
    p_valor <- 1 - pnorm(Z)
    conclusao <- ifelse(p_valor < alpha, "Rejeita H₀", "Não rejeita H₀")
    interpretacao <- ifelse(p_valor < alpha, 
                            "Há evidências de que p2025 > p2024",
                            "Não há evidências suficientes de que p2025 > p2024")
  } else if(hipotese == "menor") {
    Z_critico <- qnorm(alpha)
    p_valor <- pnorm(Z)
    conclusao <- ifelse(p_valor < alpha, "Rejeita H₀", "Não rejeita H₀")
    interpretacao <- ifelse(p_valor < alpha, 
                            "Há evidências de que p2025 < p2024",
                            "Não há evidências suficientes de que p2025 < p2024")
  } else {
    Z_critico <- qnorm(1 - alpha/2)
    p_valor <- 2 * (1 - pnorm(abs(Z)))
    conclusao <- ifelse(p_valor < alpha, "Rejeita H₀", "Não rejeita H₀")
    interpretacao <- ifelse(p_valor < alpha, 
                            "Há evidências de que p2025 ≠ p2024",
                            "Não há evidências suficientes de que p2025 ≠ p2024")
  }
  
  return(list(
    p1 = p1, n1 = n1,
    p2 = p2, n2 = n2,
    diferenca = p2 - p1,
    p_combinado = p_combinado,
    erro_padrao = erro_padrao,
    Z = Z,
    Z_critico = Z_critico,
    p_valor = p_valor,
    alpha = alpha,
    hipotese = hipotese,
    conclusao = conclusao,
    interpretacao = interpretacao
  ))
}

# Executar teste (H₁: p2025 > p2024)
resultado_brasil <- teste_z_proporcoes(p_2024, n_2024, p_2025, n_2025, hipotese = "maior")

# Exibir resultados do teste
cat("\n=== TESTE DE HIPÓTESES ===\n\n")
cat("Hipótese Nula (H₀): p2025 = p2024 (não houve aumento)\n")
cat("Hipótese Alternativa (H₁): p2025 > p2024 (houve aumento)\n")
cat("Nível de significância (α):", resultado_brasil$alpha, "\n\n")

cat("=== RESULTADO DO TESTE ===\n")
cat("Estatística Z calculada:", round(resultado_brasil$Z, 4), "\n")
cat("Valor crítico Z (unilateral superior):", round(resultado_brasil$Z_critico, 4), "\n")
cat("p-valor:", round(resultado_brasil$p_valor, 6), "\n\n")

cat("=== DECISÃO ===\n")
cat("Conclusão estatística:", resultado_brasil$conclusao, "\n")
cat("Interpretação:", resultado_brasil$interpretacao, "\n")

if(resultado_brasil$p_valor < 0.05) {
  cat("\n✅ Como p-valor < 0.05, rejeitamos H₀.\n")
  cat("O aumento observado de", round((p_2025 - p_2024) * 100, 4), 
      "% é ESTATISTICAMENTE SIGNIFICATIVO.\n")
} else {
  cat("\n❌ Como p-valor ≥ 0.05, não rejeitamos H₀.\n")
  cat("O aumento observado pode ser devido ao ACASO.\n")
}

# ============================================
# ETAPA 2: VISUALIZAÇÃO DO TESTE DE HIPÓTESES
# ============================================

# Gráfico 1: Comparação das proporções
dados_comp <- data.frame(
  Ano = c("2024", "2025"),
  Proporcao = c(p_2024, p_2025),
  Sucessos = c(sucessos_2024, sucessos_2025),
  Total = c(n_2024, n_2025)
)

grafico_comp <- ggplot(dados_comp, aes(x = Ano, y = Proporcao, fill = Ano)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0(round(Proporcao * 100, 3), "%\n(", Sucessos, "/", Total, ")")), 
            vjust = -0.5, size = 5) +
  labs(
    title = "Comparação da Proporção de Sucesso: 2024 vs 2025",
    subtitle = paste0("Diferença observada: +", round((p_2025 - p_2024) * 100, 3), 
                      "% | p-valor = ", round(resultado_brasil$p_valor, 6)),
    x = "Ano",
    y = "Proporção de Operações com Prisão em Flagrante"
  ) +
  ylim(0, max(dados_comp$Proporcao) * 1.3) +
  theme_minimal() +
  scale_fill_manual(values = c("2024" = "#F39C12", "2025" = "#E74C3C"))

print(grafico_comp)
ggplotly(grafico_comp, tooltip = c("x", "y"))

# Gráfico 2: Distribuição Normal com estatística Z
z_values <- seq(-4, 4, length.out = 300)
normal_curve <- data.frame(Z = z_values, Densidade = dnorm(z_values))

# Região crítica (Z > Z_critico ≈ 1.645 para α = 0.05 unilateral)
Z_critico <- qnorm(0.95)  # ~1.645

grafico_z <- ggplot(normal_curve, aes(x = Z, y = Densidade)) +
  geom_line(color = "darkblue", size = 1.2) +
  # Área do p-valor (Z > Z_calculado)
  geom_area(data = subset(normal_curve, Z >= resultado_brasil$Z), 
            aes(y = Densidade), fill = "red", alpha = 0.5) +
  # Área crítica (Z > Z_critico)
  geom_area(data = subset(normal_curve, Z >= Z_critico), 
            aes(y = Densidade), fill = "orange", alpha = 0.3) +
  # Linha da estatística Z calculada
  geom_vline(xintercept = resultado_brasil$Z, color = "red", linetype = "dashed", size = 1) +
  # Linha do valor crítico
  geom_vline(xintercept = Z_critico, color = "orange", linetype = "dotted", size = 1) +
  # Linha zero (H₀)
  geom_vline(xintercept = 0, color = "darkgray", linetype = "solid", size = 0.5) +
  # Anotações
  annotate("text", x = resultado_brasil$Z + 0.4, y = 0.1, 
           label = paste0("Z = ", round(resultado_brasil$Z, 2)), color = "red", size = 4) +
  annotate("text", x = Z_critico + 0.4, y = 0.05, 
           label = paste0("Z crítico = ", round(Z_critico, 2)), color = "orange", size = 3.5) +
  annotate("text", x = 2.5, y = 0.3, 
           label = paste0("p-valor = ", round(resultado_brasil$p_valor, 4)), 
           color = "darkred", size = 4, fontface = "bold") +
  labs(
    title = "Distribuição Normal Padrão sob H₀",
    subtitle = "Área vermelha: p-valor | Área laranja: região crítica (α = 0.05)",
    x = "Estatística Z",
    y = "Densidade"
  ) +
  theme_minimal()

print(grafico_z)
ggplotly(grafico_z, tooltip = c("x", "y"))

# ============================================
# ETAPA 3: TESTE DE HIPÓTESES POR REGIÃO
# ============================================

# Função para preparar dados por região
preparar_dados_regiao <- function(regiao_ufs, dados_completos) {
  dados_regiao <- dados_completos %>%
    filter(UF %in% regiao_ufs)
  
  dados_2024 <- dados_regiao %>% filter(ANO == 2024)
  dados_2025 <- dados_regiao %>% filter(ANO == 2025)
  
  n_2024 <- nrow(dados_2024)
  n_2025 <- nrow(dados_2025)
  
  sucessos_2024 <- dados_2024 %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
  sucessos_2025 <- dados_2025 %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
  
  p_2024 <- ifelse(n_2024 > 0, sucessos_2024 / n_2024, 0)
  p_2025 <- ifelse(n_2025 > 0, sucessos_2025 / n_2025, 0)
  
  return(list(
    n_2024 = n_2024, n_2025 = n_2025,
    sucessos_2024 = sucessos_2024, sucessos_2025 = sucessos_2025,
    p_2024 = p_2024, p_2025 = p_2025,
    diferenca = p_2025 - p_2024
  ))
}

# Definir regiões
regioes <- list(
  Norte = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
  Nordeste = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  CentroOeste = c("DF", "GO", "MT", "MS"),
  Sudeste = c("ES", "MG", "RJ", "SP"),
  Sul = c("PR", "RS", "SC")
)

# Calcular teste para cada região
resultados_regioes <- list()

for(nome_regiao in names(regioes)) {
  dados_reg <- preparar_dados_regiao(regioes[[nome_regiao]], dados_completos)
  
  if(dados_reg$n_2024 > 0 & dados_reg$n_2025 > 0) {
    teste <- teste_z_proporcoes(
      dados_reg$p_2024, dados_reg$n_2024,
      dados_reg$p_2025, dados_reg$n_2025,
      hipotese = "maior"
    )
    
    resultados_regioes[[nome_regiao]] <- list(
      dados = dados_reg,
      teste = teste
    )
  }
}

# Criar tabela comparativa
cat("\n=== TESTE DE HIPÓTESES POR REGIÃO ===\n\n")

tabela_regioes_teste <- data.frame(
  Regiao = character(),
  p_2024 = numeric(),
  p_2025 = numeric(),
  Diferenca = numeric(),
  Z = numeric(),
  p_valor = numeric(),
  Significativo = character(),
  stringsAsFactors = FALSE
)

for(reg in names(resultados_regioes)) {
  r <- resultados_regioes[[reg]]
  tabela_regioes_teste <- rbind(tabela_regioes_teste, data.frame(
    Regiao = reg,
    p_2024 = round(r$dados$p_2024 * 100, 3),
    p_2025 = round(r$dados$p_2025 * 100, 3),
    Diferenca = round(r$dados$diferenca * 100, 3),
    Z = round(r$teste$Z, 3),
    p_valor = round(r$teste$p_valor, 6),
    Significativo = ifelse(r$teste$p_valor < 0.05, "✅ Sim", "❌ Não")
  ))
}

print(tabela_regioes_teste)

# Gráfico comparativo por região
grafico_regioes_teste <- ggplot(tabela_regioes_teste, aes(x = reorder(Regiao, Diferenca), y = Diferenca)) +
  geom_bar(stat = "identity", aes(fill = Significativo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  geom_text(aes(label = paste0(round(Diferenca, 2), "%\n(p=", round(p_valor, 4), ")")), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Teste de Hipóteses por Região: Aumento da Proporção de Sucesso (2024→2025)",
    subtitle = "Barras verdes indicam aumento estatisticamente significativo (p-valor < 0.05)",
    x = "Região",
    y = "Diferença percentual (p2025 - p2024)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("✅ Sim" = "#2ECC71", "❌ Não" = "#E74C3C")) +
  coord_flip()

print(grafico_regioes_teste)
ggplotly(grafico_regioes_teste, tooltip = c("x", "y", "fill"))

# ============================================
# ETAPA 4: TESTE DE HIPÓTESES POR TIPO DE CRIME
# ============================================

# Identificar os principais tipos de crime
tipos_crime <- dados_completos %>%
  filter(ANO %in% c(2024, 2025)) %>%
  group_by(`ÁREA DE ATRIBUIÇÃO`) %>%
  summarise(total = n()) %>%
  filter(total > 100) %>%  # Pega apenas crimes com muitas ocorrências
  pull(`ÁREA DE ATRIBUIÇÃO`)

cat("=== TESTE DE HIPÓTESES POR TIPO DE CRIME ===\n")
cat("Tipos de crime analisados:\n")
print(tipos_crime)

# Função para calcular teste por tipo de crime
testar_por_crime <- function(tipo_crime) {
  dados_crime <- dados_completos %>%
    filter(`ÁREA DE ATRIBUIÇÃO` == tipo_crime)
  
  dados_2024 <- dados_crime %>% filter(ANO == 2024)
  dados_2025 <- dados_crime %>% filter(ANO == 2025)
  
  n_2024 <- nrow(dados_2024)
  n_2025 <- nrow(dados_2025)
  
  sucessos_2024 <- dados_2024 %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
  sucessos_2025 <- dados_2025 %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
  
  p_2024 <- ifelse(n_2024 > 0, sucessos_2024 / n_2024, 0)
  p_2025 <- ifelse(n_2025 > 0, sucessos_2025 / n_2025, 0)
  
  if(n_2024 > 0 & n_2025 > 0) {
    teste <- teste_z_proporcoes(p_2024, n_2024, p_2025, n_2025, hipotese = "maior")
    return(list(
      tipo = tipo_crime,
      p_2024 = p_2024, p_2025 = p_2025,
      n_2024 = n_2024, n_2025 = n_2025,
      diferenca = p_2025 - p_2024,
      p_valor = teste$p_valor,
      significativo = teste$p_valor < 0.05
    ))
  }
  return(NULL)
}

# Aplicar para cada tipo de crime
resultados_crime <- list()
for(tipo in tipos_crime) {
  res <- testar_por_crime(tipo)
  if(!is.null(res)) {
    resultados_crime[[tipo]] <- res
  }
}

# Criar tabela
tabela_crime_teste <- data.frame(
  Tipo_Crime = character(),
  p_2024 = numeric(),
  p_2025 = numeric(),
  Diferenca = numeric(),
  p_valor = numeric(),
  Significativo = character(),
  stringsAsFactors = FALSE
)

for(tipo in names(resultados_crime)) {
  r <- resultados_crime[[tipo]]
  tabela_crime_teste <- rbind(tabela_crime_teste, data.frame(
    Tipo_Crime = substr(r$tipo, 1, 40),
    p_2024 = round(r$p_2024 * 100, 2),
    p_2025 = round(r$p_2025 * 100, 2),
    Diferenca = round(r$diferenca * 100, 2),
    p_valor = round(r$p_valor, 6),
    Significativo = ifelse(r$significativo, "✅ Sim", "❌ Não")
  ))
}

print(tabela_crime_teste)

# Gráfico
grafico_crime_teste <- ggplot(tabela_crime_teste, aes(x = reorder(Tipo_Crime, Diferenca), y = Diferenca)) +
  geom_bar(stat = "identity", aes(fill = Significativo)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Teste de Hipóteses por Tipo de Crime (2024 → 2025)",
    x = "Tipo de Crime",
    y = "Diferença percentual (p2025 - p2024)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("✅ Sim" = "#2ECC71", "❌ Não" = "#E74C3C"))

print(grafico_crime_teste)

# ============================================
# ETAPA 5: TESTE DE HIPÓTESES POR ESTADO (RANKING)
# ============================================

# Identificar estados com dados suficientes
estados <- unique(dados_completos$UF)
estados <- estados[!is.na(estados) & estados != ""]

resultados_estados <- list()

for(estado in estados) {
  dados_estado <- dados_completos %>% filter(UF == estado)
  
  dados_2024 <- dados_estado %>% filter(ANO == 2024)
  dados_2025 <- dados_estado %>% filter(ANO == 2025)
  
  n_2024 <- nrow(dados_2024)
  n_2025 <- nrow(dados_2025)
  
  # Só incluir estados com pelo menos 10 operações em cada ano
  if(n_2024 >= 10 & n_2025 >= 10) {
    sucessos_2024 <- dados_2024 %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
    sucessos_2025 <- dados_2025 %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
    
    p_2024 <- sucessos_2024 / n_2024
    p_2025 <- sucessos_2025 / n_2025
    
    teste <- teste_z_proporcoes(p_2024, n_2024, p_2025, n_2025, hipotese = "maior")
    
    resultados_estados[[estado]] <- list(
      estado = estado,
      p_2024 = p_2024, p_2025 = p_2025,
      n_2024 = n_2024, n_2025 = n_2025,
      diferenca = p_2025 - p_2024,
      p_valor = teste$p_valor,
      significativo = teste$p_valor < 0.05
    )
  }
}

# Criar tabela e ordenar por diferença
tabela_estados_teste <- data.frame(
  Estado = character(),
  p_2024 = numeric(),
  p_2025 = numeric(),
  Diferenca = numeric(),
  p_valor = numeric(),
  Significativo = character(),
  stringsAsFactors = FALSE
)

for(estado in names(resultados_estados)) {
  r <- resultados_estados[[estado]]
  tabela_estados_teste <- rbind(tabela_estados_teste, data.frame(
    Estado = r$estado,
    p_2024 = round(r$p_2024 * 100, 2),
    p_2025 = round(r$p_2025 * 100, 2),
    Diferenca = round(r$diferenca * 100, 2),
    p_valor = round(r$p_valor, 6),
    Significativo = ifelse(r$significativo, "✅ Sim", "❌ Não")
  ))
}

# Ordenar por diferença (maior aumento primeiro)
tabela_estados_teste <- tabela_estados_teste %>%
  arrange(desc(Diferenca))

cat("\n=== TOP 10 ESTADOS COM MAIOR AUMENTO (2024 → 2025) ===\n")
print(head(tabela_estados_teste, 10))

cat("\n=== TOP 10 ESTADOS COM MAIOR QUEDA (2024 → 2025) ===\n")
print(tail(tabela_estados_teste, 10))

# Gráfico dos top 10
top_estados <- head(tabela_estados_teste, 10)

grafico_estados_teste <- ggplot(top_estados, aes(x = reorder(Estado, Diferenca), y = Diferenca)) +
  geom_bar(stat = "identity", aes(fill = Significativo)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0(round(Diferenca, 1), "%\n(p=", round(p_valor, 4), ")")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Estados com Maior Aumento de Eficácia (2024 → 2025)",
    subtitle = "Barras verdes indicam aumento estatisticamente significativo (p-valor < 0.05)",
    x = "Estado",
    y = "Diferença percentual (p2025 - p2024)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("✅ Sim" = "#2ECC71", "❌ Não" = "#F39C12"))

print(grafico_estados_teste)

# ============================================
# ETAPA 6: DASHBOARD COMPLETO - TESTE DE HIPÓTESES
# ============================================

if (!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
}

# Gráfico 1: Comparação Brasil (2024 vs 2025)
grafico_brasil <- ggplot(dados_comp, aes(x = Ano, y = Proporcao, fill = Ano)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0(round(Proporcao * 100, 2), "%")), vjust = -0.5, size = 5) +
  labs(title = "Brasil", x = "", y = "Sucesso (%)") +
  ylim(0, max(dados_comp$Proporcao) * 1.2) +
  theme_minimal() +
  scale_fill_manual(values = c("2024" = "#F39C12", "2025" = "#E74C3C"))

# Gráfico 2: Comparação por Região (resumido)
top_regioes <- tabela_regioes_teste %>%
  arrange(desc(Diferenca)) %>%
  head(5)

grafico_regioes_res <- ggplot(top_regioes, aes(x = reorder(Regiao, Diferenca), y = Diferenca)) +
  geom_bar(stat = "identity", aes(fill = Significativo)) +
  coord_flip() +
  labs(title = "Por Região", x = "", y = "Diferença (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("✅ Sim" = "#2ECC71", "❌ Não" = "#E74C3C"), guide = "none")

# Gráfico 3: Distribuição Normal (Z)
grafico_z_res <- ggplot(normal_curve, aes(x = Z, y = Densidade)) +
  geom_line(color = "darkblue", size = 1) +
  geom_area(data = subset(normal_curve, Z >= resultado_brasil$Z), 
            aes(y = Densidade), fill = "red", alpha = 0.5) +
  geom_vline(xintercept = resultado_brasil$Z, color = "red", linetype = "dashed") +
  labs(title = "Distribuição Normal", x = "Z", y = "") +
  theme_minimal()

# Dashboard final
dashboard_hipotese <- (grafico_brasil + grafico_regioes_res) / grafico_z_res +
  plot_annotation(
    title = "Teste de Hipóteses: A Eficácia Aumentou de 2024 para 2025?",
    subtitle = paste0("Brasil: Diferença = ", round((p_2025 - p_2024) * 100, 2), 
                      "% | p-valor = ", round(resultado_brasil$p_valor, 6),
                      ifelse(resultado_brasil$p_valor < 0.05, " | ✅ Aumento SIGNIFICATIVO", " | ❌ Aumento NÃO significativo")),
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(dashboard_hipotese)