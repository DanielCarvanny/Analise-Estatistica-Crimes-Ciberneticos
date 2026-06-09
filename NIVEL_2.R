# ============================================
# PROBABILIDADE CLÁSSICA
# Chance de uma operação resultar em prisão por ano
# ============================================

# Função para calcular probabilidade de sucesso por ano
calcular_prob_ano <- function(df, ano) {
  dados_ano <- df %>% filter(ANO == ano)
  total <- nrow(dados_ano)
  sucessos <- dados_ano %>% filter(`PRISÕES EM FLAGRANTE` > 0) %>% nrow()
  prob <- sucessos / total
  return(data.frame(Ano = ano, Probabilidade = prob, Total = total, Sucessos = sucessos))
}

# Calcular para cada ano
anos <- unique(dados_completos$ANO)
resultados_prob <- do.call(rbind, lapply(anos, function(a) calcular_prob_ano(dados_completos, a)))

# Exibir resultados numéricos
cat("=== PROBABILIDADE DE SUCESSO POR ANO ===\n")
print(resultados_prob)

# Gráfico de barras
grafico_prob <- ggplot(resultados_prob, aes(x = factor(Ano), y = Probabilidade, fill = factor(Ano))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Probabilidade * 100, 2), "%\n(", Sucessos, "/", Total, ")")), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Probabilidade de uma Operação Resultar em Prisão em Flagrante",
    subtitle = "Comparação anual | P(A) = Casos favoráveis / Casos possíveis",
    x = "Ano",
    y = "Probabilidade de Sucesso"
  ) +
  ylim(0, max(resultados_prob$Probabilidade) * 1.3) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

# Exibir gráfico estático
print(grafico_prob)

# Exibir gráfico interativo
ggplotly(grafico_prob, tooltip = c("x", "y"))

# ============================================
# TESTE DE HIPÓTESES: Proporção de Sucesso vs Meta
# H₀: p <= 0.10 (meta de 10%)
# H₁: p > 0.10
# ============================================

# Meta institucional (alterada para 10%)
meta <- 0.10  # 10% (antes era 0.02)

# Teste de proporção para 2025
teste_prop_2025 <- prop.test(x = sucessos_2025, n = n_2025, p = meta, alternative = "greater")

cat("=== TESTE DE HIPÓTESES: Proporção de Sucesso vs Meta (10%) ===\n\n")
cat("Meta institucional:", meta * 100, "%\n")
cat("Proporção observada em 2025:", round(p_2025 * 100, 4), "%\n\n")

cat("Resultado do teste:\n")
cat("  Estatística X²:", round(teste_prop_2025$statistic, 4), "\n")
cat("  p-valor:", round(teste_prop_2025$p.value, 6), "\n\n")

if(teste_prop_2025$p.value < 0.05) {
  cat("✅ A proporção de sucesso em 2025 é SIGNIFICATIVAMENTE MAIOR que a meta de 10%.\n")
  cat("   A meta foi atingida e superada com significância estatística!\n")
} else {
  cat("❌ Não há evidências de que a proporção de sucesso seja maior que a meta de 10%.\n")
  cat("   A diferença observada pode ser devida ao acaso.\n")
}

# ============================================
# GRÁFICO DE DISTRIBUIÇÃO NORMAL PARA TESTE DE PROPORÇÃO
# Proporção de Sucesso (2025) vs Meta (10%)
# ============================================

# 1. Calcular a estatística Z a partir do qui-quadrado
z_calculado_prop <- sqrt(teste_prop_2025$statistic)

# 2. Criar curva normal padrão
z_vals <- seq(-4, 4, length.out = 300)
curva_normal <- data.frame(Z = z_vals, Densidade = dnorm(z_vals))

# 3. Valor crítico (unilateral superior, α = 0.05)
z_critico <- qnorm(0.95)  # ~1.645

# 4. Criar gráfico
grafico_z_prop <- ggplot(curva_normal, aes(x = Z, y = Densidade)) +
  geom_line(color = "darkblue", size = 1) +
  geom_area(data = subset(curva_normal, Z >= z_critico), 
            aes(y = Densidade), fill = "orange", alpha = 0.3) +
  geom_area(data = subset(curva_normal, Z >= z_calculado_prop), 
            aes(y = Densidade), fill = "red", alpha = 0.5) +
  geom_vline(xintercept = z_calculado_prop, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = z_critico, color = "orange", linetype = "dotted", size = 1) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "solid", size = 0.5) +
  annotate("text", x = z_calculado_prop + 0.3, y = 0.05, 
           label = paste0("Z = ", round(z_calculado_prop, 4)), color = "red", size = 3.5) +
  annotate("text", x = z_critico + 0.3, y = 0.25, 
           label = paste0("Z crítico = ", round(z_critico, 3)), color = "orange", size = 3.5) +
  annotate("text", x = 2.5, y = 0.35, 
           label = paste0("p-valor = ", round(teste_prop_2025$p.value, 6)), 
           color = ifelse(teste_prop_2025$p.value < 0.05, "red", "darkgreen"), size = 4, fontface = "bold") +
  annotate("text", x = 2.5, y = 0.3, 
           label = ifelse(teste_prop_2025$p.value < 0.05, 
                          "Decisão: REJEITAR H₀", 
                          "Decisão: NÃO REJEITAR H₀"), 
           color = ifelse(teste_prop_2025$p.value < 0.05, "red", "darkgreen"), size = 4, fontface = "bold") +
  labs(
    title = "Teste de Hipóteses: Proporção de Sucesso em 2025 vs Meta de 10%",
    subtitle = "H₁: p > 10% | Área laranja = região crítica | Área vermelha = p-valor",
    x = "Estatística Z",
    y = "Densidade"
  ) +
  theme_minimal()

# Exibir gráfico
print(grafico_z_prop)

# Salvar (opcional)
ggsave("teste_hipotese_normal_proporcao_10pct.png", grafico_z_prop, width = 10, height = 6, dpi = 300)

ggsave("teste_hipotese_normal_proporcao.png", grafico_z_prop, width = 10, height = 6, dpi = 300)

ggsave("histograma_probabilidade_prisao.png", grafico_prob, width = 10, height = 6, dpi = 300)
