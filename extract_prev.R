# extract_var.R

source("temp_prev_vf2.r") # caminho para o seu arquivo .r

args <- commandArgs(trailingOnly = TRUE)
var_name <- args[1]

cat(get(var_name)) # imprimir o valor da variável
