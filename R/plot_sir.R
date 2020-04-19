plot_sir <- function(dataset, report_rate, events, time, update, ...) {
  pars <- list(...)

  total_time <- min(nrow(dataset), time)
  max_value <- max((dataset$casos_confirmados / report_rate)[1:total_time],
    dataset$sim_casos[1:total_time],
    na.rm = TRUE
  )
  e_pos <- events$pos * max_value * 0.05

  if (is.null(pars$ylim)) {
    ylimit <- c(0, max_value)
  } else {
    ylimit <- pars$ylim
  }

  plot(0, 0,
    xlim = c(1, total_time), ylim = ylimit, type = "n",
    main = "Comparação de dados simulados e reais", ylab = "Número de Casos",
    sub = paste("Atualizado em ", update),
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

plot_ly_sir <- function(data, containers, stacked = FALSE, events = NULL, real_data = NULL, grouped = FALSE, log = FALSE) {
  p <- c(0.10, 0.5, 0.90)

  p_names <- purrr::map_chr(p, ~ paste0(.x * 100))

  p_funs <- map(p, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

  data <- as.data.frame(data$sim) %>%
    select(sim, time, f.num, s.num, e.num, i.num, q.num, h.num, r.num) %>%
    mutate(time = as.Date("2020-02-28") + time) %>%
    group_by(time) %>%
    summarize_at(vars(ends_with("num")), funs(!!!p_funs))

  compcols <- c(
    "s.num" = "yellow", "e.num" = "orange", "i.num" = "red",
    "q.num" = "cyan", "h.num" = "magenta", "r.num" = "lightgreen",
    "f.num" = "black", "n_obitos" = "brown",
    "casos_confirmados" = "blue",
    "report_rate" = "green"
  )

  # complabels <- c(
  #   "s.num" = "Suscetíveis", "e.num" = "Assintomáticos",
  #   "i.num" = "Sintomáticos", "q.num" = "Em Quarentena",
  #   "h.num" = "Casos Graves", "r.num" = "Recuperados",
  #   "f.num" = "Óbitos", "n_obitos" = "Óbitos Reais",
  #   "casos_confirmados" = "Casos Confirmados Reais",
  #   "report_rate" = "Casos Reais estimados"
  # )

  complabels <- c(
    "s.num" = "Susceptibles", "e.num" = "Asymptomatics",
    "i.num" = "Symptomatics", "q.num" = "On Quarantine",
    "h.num" = "Severe Cases", "r.num" = "Recovered",
    "f.num" = "Deaths", "n_obitos" = "Deaths (real data)",
    "casos_confirmados" = "Infected (real data)",
    "report_rate" = "Infected (estimation)"
  )

  fig <- plot_ly(data, x = ~time, hoverinfo = "x+y+text")

  if (!grouped) {
    for (i in containers) {
      if (!stacked) {
        fig <- fig %>% add_trace(
          y = data[[paste0(i, "_90")]], type = "scatter", mode = "lines",
          line = list(color = "transparent"), legendgroup = i,
          showlegend = FALSE, name = "q90", text = paste(complabels[i], "q90")
        )
        fig <- fig %>% add_trace(
          y = data[[paste0(i, "_10")]], type = "scatter", mode = "lines",
          fill = "tonexty", fillcolor = toRGB(compcols[i], 0.2),
          line = list(color = "transparent"), legendgroup = i,
          showlegend = FALSE, name = "q10", text = paste(complabels[i], "q10")
        )
        fig <- fig %>% add_trace(
          y = data[[paste0(i, "_50")]], type = "scatter", mode = "lines",
          line = list(color = compcols[i], width = 2), legendgroup = i,
          showlegend = TRUE, name = complabels[i], text = paste(complabels[i], "q50")
        )
      } else {
        fig <- fig %>% add_trace(
          y = data[[paste0(i, "_50")]], type = "scatter", mode = "lines",
          line = list(color = compcols[i], width = 2), legendgroup = i, stackgroup = 1,
          showlegend = TRUE, name = complabels[i], text = paste(complabels[i], "q50")
        )
      }
    }
  }
  else {
    fig <- fig %>% add_trace(
      y = data[["f.num_50"]], type = "scatter", mode = "lines",
      line = list(color = compcols["f.num"], width = 2), stackgroup = 1,
      showlegend = TRUE, name = complabels["f.num"]
    )

    fig <- fig %>% add_trace(
      y = data[["r.num_50"]] + data[["h.num_50"]] + data[["i.num_50"]] + data[["e.num_50"]] + data[["q.num_50"]],
      type = "scatter", mode = "lines",
      line = list(color = compcols["r.num"], width = 2), stackgroup = 1,
      showlegend = TRUE, name = "Infected"
    )
  }

  if (!is.null(real_data)) {
    fig <- fig %>%
      add_trace(
        x = real_data$data_relatorio,
        y = real_data$n_obitos, type = "scatter", mode = "lines",
        line = list(color = compcols["n_obitos"], width = 4, dash = "dash"),
        legendgroup = "real",
        showlegend = TRUE, name = complabels["n_obitos"], text = complabels["n_obitos"]
      ) %>%
      add_trace(
        x = real_data$data_relatorio,
        y = real_data$casos_confirmados,
        type = "scatter", mode = "lines",
        line = list(color = compcols["casos_confirmados"], width = 4, dash = "dash"),
        legendgroup = "real",
        showlegend = TRUE, name = complabels["casos_confirmados"], text = complabels["casos_confirmados"]
      ) %>%
      add_trace(
        x = real_data$data_relatorio,
        y = real_data$casos_confirmados / real_data$report_rate,
        type = "scatter", mode = "lines",
        line = list(color = compcols["report_rate"], width = 4, dash = "dash"),
        legendgroup = "real",
        showlegend = TRUE, name = complabels["report_rate"], text = complabels["report_rate"]
      )
  }

  if (!is.null(events)) {
    fig <- fig %>% add_markers(
      x = as.Date(events$dates), y = 1,
      name = "Events",
      hoverinfo = "text",
      text = events$labels,
      marker = list(
        symbol = "line-ns",
        color = "rgb(56, 34, 15)",
        size = 5,
        line = list(color = "rgb(56, 34, 15)", width = 2)
      )
    )
  }

  fig <- fig %>% layout(
    title = "COVID-19 Simulation - Portugal North",
    legend = list(
      traceorder = "grouped",
      tracegroupgap = 10,
      bgcolor = "rgba(229,229,229, 0.5)",
      bordercolor = "rgb(127,127,127)",
      borderwidth = 1,
      x = 0.80,
      y = 0.95
    ),
    paper_bgcolor = "rgb(255,255,255)", plot_bgcolor = "rgb(229,229,229)",
    xaxis = list(
      title = "Date",
      gridcolor = "rgb(255,255,255)",
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE,
      tickcolor = "rgb(127,127,127)",
      ticks = "outside",
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Cases",
      gridcolor = "rgb(255,255,255)",
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE,
      tickcolor = "rgb(127,127,127)",
      ticks = "outside",
      zeroline = FALSE,
      type = ifelse(log, "log", "linear")
    )
  )

  fig
}

# plot_ly_sir(baseline_sim, c("f.num", "h.num", "i.num", "q.num", "e.num", "r.num"), events = events, stacked = TRUE, real_data = geo_norte)
