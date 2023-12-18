# calendario do mes
library(calendR)
library(lubridate)

library(tidyverse)

# identifica o primeiro sábado de cada mes
prim_sab <- function(x) 7 * ceiling(as.numeric(x-1+6)/7) + as.Date(1-6, origin="1970-01-01")

cal_mes <- function(ano = 2024,
                    mes, # mes a mostrar
                    #ultimo_dia, # ultimo dia do mes c(28,29,30,31)
                    feriados, # dias que são feriados c(1,8,25)
                    #sabado, # primeiro sabado do mes
                    meias, # dias em que se realizam 1/2 hoRas c(2,8,15,22,29)
                    apresenta # nome / dep dos apresentadores de cada sessao c('J. Poças \n DRGD/NDA', 'A. Cunha', 'J. Garra', 'B. Lima', 'C. Ribeiro')
                    ){
  
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
          lty = 9)
  
  
}

