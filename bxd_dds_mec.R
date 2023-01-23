library(tidyverse)
library(rvest)
library(lubridate)
library(RSelenium)
library(netstat)

rs_driver_object <- rsDriver(
  browser = "firefox", 
  firefoxver = "109",
  verbose = F,
  port = free_port()
)

availabe_lks <- read_html("https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/sinopses-estatisticas/educacao-superior-graduacao") %>% html_element("body") %>% 
  html_nodes(css = "a" ) %>% 
  html_attr("href") %>% 
  str_subset(., "https://download.inep.gov.br/") %>% 
  str_subset("\\.zip")
pasta <- "censo/"



baixa_zip <- function(link, pasta) {
  # baixa arquivos
  # link, pasta: local para baixar e extrair
  
  if(!dir.exists(paths = pasta))
    dir.create(pasta)
  
  temp <- tempfile(tmpdir = pasta)
  download.file(link, temp, method = "wget", extra = "--no-check-certificate")
  unzip(temp, exdir = pasta)
  
  
  unlink(temp)
}



map(.f =baixa_zip, availabe_lks, pasta)
