# calendario do mes
library(calendR)
library(lubridate)

library(tidyverse)

# identifica o primeiro sábado de cada mes (x)
prim_sab <- function(x) 7 * ceiling(as.numeric(x-1+6)/7) + as.Date(1-6, origin="1970-01-01")

cal_mes <- function(ano = 2024,
                    mes, # mes a mostrar
                    subtitulo = '',
                    #ultimo_dia, # ultimo dia do mes c(28,29,30,31)
                    feriados, # dias que são feriados c(1,8,25)
                    #sabado, # primeiro sabado do mes
                    meias, # dias em que se realizam 1/2 hoRas c(2,8,15,22,29)
                    apresenta # nome / dep dos apresentadores de cada sessao c('J. Poças \n DRGD/NDA', 'A. Cunha', 'J. Garra', 'B. Lima', 'C. Ribeiro')
                    , ...){
  #img <- "https://i.pinimg.com/originals/10/1e/f6/101ef6a9e146b23de28fa2cd568ad17b.jpg"
  #img <- jpeg::readJPEG("images/bg1.jpg") 
  
  data <- as.Date(paste(ano,mes,1,sep = '-'))
  
  ultimo_dia <- as.character(ceiling_date(data, "month") - days(1)) |>   substr(start = 9,stop = 10) |> as.numeric()
  
  sabado <- prim_sab(data) |> as.character()  |>  substr(start = 9,stop = 10) |> as.numeric()
  
  data <- as.Date(paste(ano,mes,1,sep = '-'))
  
  ultimo_dia <- as.character(ceiling_date(data, "month") - days(1)) |>   substr(start = 9,stop = 10) |> as.numeric()
  
  sabado <- prim_sab(data) |> as.character()  |>  substr(start = 9,stop = 10) |> as.numeric()
  
  eventos <- rep(NA,ultimo_dia)
  if(!is.na(feriados)) eventos[feriados] <- 'feriado'
  if(!is.na(feriados)) cores <- c("pink","lightblue", "lightpink") else cores <- c("lightblue", "lightpink")
  eventos[meias] <- 'meia_hora'
  eventos[c(seq(from = sabado, to = ultimo_dia, by=7),
            seq(from = sabado + 1, to = ultimo_dia, by=7))] <- 'weekend'
  
  meses <- c('January','February','March','April','May','June',
             'July','August', 'September','October','November','December')
  
  calendR::calendR(year = ano, month = mes,
                   subtitle = paste('Theme:',subtitulo),
          special.days = eventos,
          special.col = cores, # Color of the special.days
          months.pos = 0.5,
          weeknames = c('Mo','Tu', 'We', 'Th', 'Fr', 'Sa', 'Su'),
          start = 'M',
          title = meses[mes],
          text = apresenta,
          text.pos = meias,       # Days of the month where to put the texts
          text.size = 3,               # Font size of the text
          text.col = 4,
          lty = 1,
          margin = 0.1,
          ...)
}

## função para graficos com resultados de questionario SLIDO
gg_slido <- function(sess = 20240103){ # o unico input é a data da sessão
  
  dados <- slido %>%  filter(sessao == sess)
  questao <- dados$questao[1]
  n <- dados$n[1]
  
  dados_long <- dados %>% 
    pivot_longer(cols = r1:r4,
                 names_to = 'answer',
                 values_to = 'value') %>% 
    mutate(answer = factor(answer, 
                             levels = paste0('r',seq(1:4)),
                             labels = c("I'm expeRt",'common useR','raRely used','neveR used')
    ),
    answer = fct_rev(answer)
    ) 
  
  limiteyy <- max(dados_long$value) + 0.1
  
  dados_long %>% 
    ggplot(aes(x= answer, y= value)) +
    #geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +
    geom_segment( aes(xend=answer, yend=0)) +
    geom_point( size=4, color="blue") +
    geom_text(aes(label = answer),
              nudge_x=0.3, y = 0, #nudge_y=-0.03,
              hjust = 0,
              check_overlap=T) +
    geom_text(aes(label = paste0(value*100,'%')),
              nudge_x=-0.08, nudge_y=0.05,
              #hjust = 0,
              check_overlap=T) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_percent()
                       , limits = c(0,limiteyy)
    ) + 
    labs(x = '', y = 'percentage of responses',
         title = paste("What's your useR level with:", questao, '?'),
         caption = paste('(responded by a total of', n,'useRs)')) +
    theme_classic() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.caption = element_text(color = "blue", face = "italic"),
          plot.title = element_text(size=9))
}


## funções para resumo ######

# conta users diferentes para o ano
dif_users <- function(dados = atendents,
                      ano = 2024){
  du <- dados %>% 
    filter(ano == ano) %>% 
    distinct(name)  %>% nrow()
  
  return(du)
}

# conta assistentes por curador / sessao
n_sessoes <- function(dados = atendents,
                      ano = 2024){
  ns <- dados %>%
    filter(ano == ano) %>%
    count(curator, session)
  
  return(ns)
}

# percentagem de assistentes do sexo feminino por ano
users_perc_f <- function(dados = atendents,
                         ano = 2024){
  
  perc_f <- atendents %>%
    filter(ano == ano) %>% 
    left_join(users) %>% # tabela com os utilizadores
    janitor::tabyl(sexo) %>% 
    filter(sexo == 'f') %>%
    .$valid_percent %>%
    .[[1]] *100
  
  return(perc_f)
  
}


## grafico com o número de assistentes por sessão
library(wesanderson)
assit_sessao <- function(dados = n_sessoes(ano = 2024),
                         media_sessoes = med_sessao2024){
  dados %>% 
    ggplot(aes(x = session, y = n, fill=curator)) +
    geom_bar(stat="identity", color="blue") +
    scale_x_date(date_breaks = 'month', date_labels = '%b') + 
    xlab(NULL) +
    ylab('number of attendees') +
    geom_hline(yintercept=media_sessoes, linetype="dashed", 
               color = "red") +
    theme_minimal() + 
    theme(legend.position = 'none') + 
    scale_fill_brewer(palette="PuBuGn")
}

