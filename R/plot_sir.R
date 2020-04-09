plot_sir <- function(dataset, report_rate, events, time) {
  total_time <- min(nrow(dataset), time)
  max_value <- max((dataset$casos_confirmados / report_rate)[1:total_time],
    dataset$sim_casos[1:total_time],
    na.rm = TRUE
  )
  e_pos <- events$pos * max_value * 0.05

  plot(0, 0,
    xlim = c(1, total_time), ylim = c(0, max_value), type = "n",
    main = "Comparação de dados simulados e reais", ylab = "Número de Casos",
    xlab = "Dias"
  )
  lines(dataset$casos_confirmados / report_rate, col = "blue", lwd = 2)
  lines(dataset$n_obitos, col = "brown", lwd = 2)

  lines(dataset$sim_obitos, col = "brown", lwd = 1, lty = 2)
  lines(dataset$sim_casos, col = "blue", lwd = 1, lty = 2)

  abline(v = events$idx, lty = 3)
  points(events$idx, e_pos, pch = 20)
  text(events$idx, e_pos, label = events$labels, pos = 4, cex = 0.6, offset = 0.3)
  legend("topleft",
    c("Dados reais", "Simulação", "Infetados", "Óbitos"),
    lty = c(1, 2, 0, 0),
    pch = c(-1, -1, 15, 15),
    col = c(1, 1, "blue", "brown"),
    title = "Legenda", inset = .05
  )
}
