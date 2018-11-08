# versao 1 com uma linha grossa para o valor inicial, na cor escura correspondente à da barra.

plot_estatico <- ggplot(dados_plot, aes(y = Valor, x = reorder(Categoria, Valor))) +
  geom_bar(aes(fill = CoresNormais), stat = 'identity') +
  geom_tile(aes(y = inicial, fill = CoresEscuras, color = CoresEscuras), width = 0.9) +   
  geom_text(aes(y = Valor + 0.003, 
                label = percent(round(Valor, 4)), 
                color = CoresEscuras), 
            position = position_dodge(1),
            size = 4, family = "Source Sans Pro", vjust = 0) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = c(0, 0), limits = c(0,0.09)) +
  scale_fill_identity() +
  scale_color_identity() +
  #scale_color_manual(values = paleta_darker) +
  #scale_fill_manual(values = paleta) +
  labs(
    x = NULL,
    y = "Percentual do PIB",
    title = "Despesas do Governo Federal -- 2008 a 2017 (valores iniciais destacados)"
  ) +
  #geom_text(aes(label = Ano), x = 1, y = 0.05, size = 5) +
  tema() +
  theme(legend.position = 'none')

# versao 2 com uma linhas finas para o valor inicial e final, ambas na cor escura correspondente à da barra.

plot_estatico <- ggplot(dados_plot, aes(y = Valor, x = reorder(Categoria, Valor))) +
  geom_bar(aes(fill = CoresNormais), stat = 'identity') +
  geom_tile(aes(y = inicial, fill = CoresEscuras, color = CoresEscuras), width = 0.9) +   
  geom_tile(aes(y = inicial, fill = CoresEscuras, color = CoresEscuras), width = 0.9, height = 0.00001) +
  geom_tile(aes(y = final, fill = CoresEscuras, color = CoresEscuras), width = 0.9, height = 0.00001) +
  geom_text(aes(y = Valor + 0.003, 
                label = percent(round(Valor, 4)), 
                color = CoresEscuras), 
            position = position_dodge(1),
            size = 4, family = "Source Sans Pro", vjust = 0) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = c(0, 0), limits = c(0,0.09)) +
  scale_fill_identity() +
  scale_color_identity() +
  #scale_color_manual(values = paleta_darker) +
  #scale_fill_manual(values = paleta) +
  labs(
    x = NULL,
    y = "Percentual do PIB",
    title = "Despesas do Governo Federal -- 2008 a 2017 (valores iniciais destacados)"
  ) +
  #geom_text(aes(label = Ano), x = 1, y = 0.05, size = 5) +
  tema() +
  theme(legend.position = 'none')
