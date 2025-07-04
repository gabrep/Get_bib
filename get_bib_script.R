#Pacotes necessários
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
packages <- c("readtext", "pdftools", "stringr", "httr", "jsonlite", "progress")
lapply(packages, install_if_missing)

library(readtext)
library(pdftools)
library(stringr)
library(httr)
library(jsonlite)
library(progress)  # <--- carregando o pacote progress

#1 Extrair códigos DOI da referencia
extrair_dois <- function(texto) {
  library(stringr)
  
  dois <- str_extract_all(texto,
                          "10\\.\\d{4,9}/[^\\s\\)\\]\\[\\{\\}<>\",;]+")[[1]]
  
  dois <- unique(dois)
  
  limpar_doi <- function(doi) {
    doi <- trimws(doi)
    doi <- gsub("[[:punct:][:space:]]+$", "", doi)
    return(doi)
  }
  
  dois <- sapply(dois, limpar_doi, USE.NAMES = FALSE)
  return(dois)
}

#2 Buscar entrada BibTeX via CrossRef
buscar_bibtex <- function(doi) {
  url <- paste0("https://doi.org/", doi)
  r <- GET(url, add_headers(Accept = "application/x-bibtex"))
  if (status_code(r) == 200) {
    return(content(r, "text", encoding = "UTF-8"))
  } else {
    warning(paste("Erro ao buscar DOI:", doi))
    return(NULL)
  }
}

#3 Fluxo principal
cat("Digite o caminho completo do manuscrito (.pdf ou .docx):\n")

caminho <- readline("> ")

if (!file.exists(caminho)) {
  stop("Arquivo não encontrado. Verifique o caminho digitado.")
}

ext <- tools::file_ext(caminho)

if (ext == "pdf") {
  texto <- pdf_text(caminho) %>% paste(collapse = " ")
} else if (ext == "docx") {
  texto <- readtext(caminho)$text
} else {
  stop("Formato não suportado Use arquivos .pdf ou .docx.")
}

dois <- extrair_dois(texto)

if (length(dois) == 0) {
  cat("Nenhum DOI encontrado no documento.\n")
  quit()
}

cat(paste("Foram encontrados", length(dois), "DOIs no manuscrito.\n"))

cat("Buscando metadados via DOI...\n")

pb <- progress_bar$new(
  format = "  [:bar] :current/:total (:percent) em :elapsed",
  total = length(dois), clear = FALSE, width = 60
)

bibtex_list <- vector("list", length(dois))

for (i in seq_along(dois)) {
  pb$tick()
  bibtex_list[[i]] <- buscar_bibtex(dois[i])
}

bibtex_list <- Filter(Negate(is.null), bibtex_list)

cat("Escolha o que deseja fazer:\n")
cat("1: Salvar BibTeX em um arquivo para importação manual\n")
escolha <- readline("Digite 1 ou ESC para cancelar: ")

if (escolha == "1") {
  pasta_destino <- dirname(caminho)
  arquivo_saida <- file.path(pasta_destino, "referencias_extraidas.bib")
  writeLines(unlist(bibtex_list), arquivo_saida)
  cat(paste("Arquivo BibTeX salvo como", arquivo_saida, "\n"))
} else {
  cat("Opção inválida. Encerrando script.\n")
}
