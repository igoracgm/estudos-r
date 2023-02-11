# https://www.gem.wiki/Category:Coal_mines_in_China

# Objetivo: a partir do link, 
# extrair de todas as páginas subseq a parte "MINE DETAILS" e armazená-la em df

library(tidyverse)
library(rvest)
library(lubridate)
library(RSelenium)
library(netstat)

url_base_ini <- "https://www.gem.wiki/Category:Coal_mines_in_China"
url_base_fim <- "https://www.gem.wiki/w/index.php?title=Category:Coal_mines_in_China&pagefrom=%E9%84%82%E5%B0%94%E5%A4%9A%E6%96%AF%E5%B8%82%E5%87%86%E6%A0%BC%E5%B0%94%E6%97%97%E8%81%9A%E8%83%BD%E7%85%A4%E7%82%AD%E9%9B%86%E5%9B%A2%E6%9C%89%E9%99%90%E8%B4%A3%E4%BB%BB%E5%85%AC%E5%8F%B8%E5%A3%95%E8%B5%96%E6%A2%81%E7%85%A4%E7%9F%BF#mw-pages"




pg <- read_html(url_base_ini)

prx_pag <- pg %>% 
  html_element("body") %>% 
  html_nodes(xpath = '/html/body/div[1]/div[6]/div/div/div[3]/div[1]/div[2]/div/a[2]') %>% 
  html_attr("href") %>% 
  paste0("https://www.gem.wiki", .)

lista_link <- pg  %>% 
  html_element("body") %>% 
  html_nodes(".mw-content-ltr") %>% 
  html_elements("ul") %>%
  html_nodes(css = "a") %>%
  html_attr("href")
  




prx_pag <- url_base_ini
lista_link <- character()

while (prx_pag != url_base_fim) {
  pg <- read_html(prx_pag)
  
  prx_pag <- pg %>% 
    html_element("body") %>% 
    html_nodes(xpath = '/html/body/div[1]/div[6]/div/div/div[3]/div[1]/div[2]/div/a[2]') %>% 
    html_attr("href") %>% 
    paste0("https://www.gem.wiki", .)
  
  
  lista_link <- pg  %>% 
    html_element("body") %>% 
    html_nodes(".mw-content-ltr") %>% 
    html_elements("ul") %>%
    html_nodes(css = "a") %>%
    html_attr("href") %>% c(lista_link, .)
  
  
  
  # verifica se tem prox pag
  l <- pg %>% 
    html_element("body") %>% 
    html_nodes(xpath = '/html/body/div[1]/div[6]/div/div/div[3]/div[1]/div[2]/div/a[2]') 
  
  
  if(!str_detect(as.character(l), "next page"))
    break
  
  print(prx_pag)
}

glimpse(lista_link)

# ainda não sei trabalhar com os char chinês, alors, pego os primeiros 1000 só
lista_link_1000 <-lista_link[1:1000] 

# função pra pegar os dados de uma página



link <- "/Changzhi_Xishan_Coal_Mine"

ext_mina <- function(link) {
  
  
  
  mina <- str_remove(link, "/") %>% 
    str_replace_all("_", " ")
  
  link <- paste0("https://www.gem.wiki", link)
  
  txt <- link %>% 
    read_html() %>% 
    html_element("body") %>% 
    html_elements(xpath = "/html/body/div[1]/div[6]/div/div/div[3]/div[1]/div/ul") %>% 
    html_nodes("li") %>% 
    html_text2() %>% 
    str_remove_all("\\[.*?\\]") 
  
  
  df <- txt %>% 
    str_remove_all("\\[.*?\\]") %>% 
    str_split_fixed(":",n = 2) %>%
    as_tibble() %>% 
    setNames(c("classe", "valor")) %>% 
    mutate(valor = str_trim(.$valor)) %>% 
    mutate(mina = mina )
 print(mina)
  return(df)
}

amostra <- lista_link_1000

lista <- map(amostra, .f = ext_mina)

df_minas <- do.call("rbind", lista)



## salvando em um csv
dirtrio <- "dds_minas/"

if(!dir.exists(dirtrio))
  dir.create(dirtrio)



readr::write_csv(df_minas, file = paste0(dirtrio, "df_minas.csv"), col_names = T)
