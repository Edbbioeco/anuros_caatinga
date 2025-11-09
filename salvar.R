# Instalar pacotes necessários, se ainda não estiverem instalados
if(!requireNamespace("rentrez")) install.packages("rentrez")
if(!requireNamespace("ape")) install.packages("ape")
if(!requireNamespace("phangorn")) install.packages("phangorn")
if(!requireNamespace("msa")) install.packages("msa")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("msa")

# Carregar pacotes
library(rentrez)
library(ape)
library(phangorn)
library(msa)
library(DECIPHER)

species_list <- c("Homo sapiens", "Pan troglodytes", "Mus musculus")  # Exemplo de espécies

# Função para baixar sequências de DNA de uma espécie do GenBank
baixar_dna_nuclear <- function(especie) {
  # Criar o termo de busca para DNA nuclear da espécie
  termo_busca <- paste0(especie, " AND nuclear genomic DNA[filter]")

  # Executar a busca na base de dados "nuccore"
  resultado_busca <- rentrez::entrez_search(db = "nuccore", term = termo_busca, retmax = 5)  # retmax define o máximo de resultados por espécie

  # Verificar se há resultados
  if (resultado_busca$count > 0) {
    # Baixar as sequências no formato GenBank usando os IDs retornados pela busca
    sequencias <- rentrez::entrez_fetch(db = "nuccore", id = resultado_busca$ids, rettype = "gb", retmode = "text")
    return(sequencias)
  } else {
    return(paste("Nenhuma sequência de DNA nuclear encontrada para a espécie:", especie))
  }
}

h_s <- entrez_search(db = "nucleotide", term = "Boana creptans", retmax = 10)
h_s$ids[1]
rentrez::entrez_fetch(db = "nucleotide", id = h_s$ids, rettype = "fasta")
# Passo 2: Baixar as sequências de DNA de todas as espécies
sequences <- lapply(especies, get_genetic_data)
names(sequences) <- especies
sequences
# Remover espécies sem sequências disponíveis
sequences <- sequences[!sapply(sequences, is.na)]
cat(substr(sequences, 1, 1000))
identificar_tipo_sequencia <- function(sequence_data) {
  if (grepl("mitochondrion", sequence_data, ignore.case = TRUE)) {
    return("DNA Mitocondrial")
  } else if (grepl("COI", sequence_data, ignore.case = TRUE) ||
             grepl("cytochrome c oxidase subunit I", sequence_data, ignore.case = TRUE)) {
    return("Gene COI")
  } else if (grepl("RNA", sequence_data, ignore.case = TRUE)) {
    return("RNA")
  } else if (grepl("nuclear", sequence_data, ignore.case = TRUE)) {
    return("DNA Nuclear")
  } else {
    return("Tipo de sequência não identificado")
  }
}

# Aplicar a função à sequência baixada
tipo_sequencia <- identificar_tipo_sequencia(sequences[1])
cat("Tipo de sequência:", tipo_sequencia)

# Passo 3: Alinhar as sequências
# Transformar as sequências em um formato que o pacote 'msa' possa trabalhar
sequence_names <- names(sequences)
fasta_data <- paste("> ", sequence_names, "\n", unlist(sequences), collapse = "\n")
writeLines(fasta_data, "sequences.fasta")

# Ler o arquivo FASTA
sequencias <- Biostrings::readDNAStringSet("sequences.fasta")

# Alinhar as sequências usando DECIPHER
alinhamento <- DECIPHER::AlignSeqs(sequencias)

alinhamento_matrix <- as.matrix(alinhamento)

# Calcular a matriz de distâncias entre as sequências
dist_matrix <- ape::dist.dna(alinhamento_matrix)

# Construir a árvore filogenética usando o método Neighbor-Joining (NJ)
arvore_nj <- ape::nj(dist_matrix)

# Ajustar um modelo de evolução de substituição (opcional)
fit <- phangorn::pml(arvore_nj, data = alinhamento_matrix)

# Ajustar a árvore para não ser ultramétrica
arvore_nao_ultrametrica <- phangorn::optim.pml(fit, model = "GTR")

# Plotar a árvore não ultramétrica
ape::plot.phylo(arvore_nao_ultrametrica$tree, main = "Árvore Filogenética Não Ultramétrica")

# Salvar a árvore em formato Newick
ape::write.tree(arvore_nao_ultrametrica$tree, file = "arvore_nao_ultrametrica.newick")

# Ajustar a árvore para não ser ultramétrica
arvore_nao_ultrametrica <- phangorn::optim.pml(fit, model = "GTR")

# Plotar a árvore não ultramétrica
ape::plot.phylo(arvore_nao_ultrametrica$tree, main = "Árvore Filogenética Não Ultramétrica")

# Salvar a árvore em formato Newick
ape::write.tree(arvore_nao_ultrametrica$tree, file = "arvore_nao_ultrametrica.newick")
































































# Carregar as sequências no formato DNA
dna_sequences <- Biostrings::readDNAStringSet("sequences.fasta", format = "fasta")

sequencias <- readDNAStringSet("sequences.fasta")

# Converter de volta para DNAStringSet
dna_sequences_clean_set <- Biostrings::DNAStringSet(sequencias)

# Escrever o novo arquivo FASTA corrigido
Biostrings::writeXStringSet(sequencias, "cleaned_sequences.fasta", format = "fasta")

# Agora tente alinhar as sequências corrigidas
alignment <- msa::msa(sequencias, method = "ClustalW", type = "dna")

# Passo 4: Converter o alinhamento para o formato adequado para filogenia
aligned_sequences <- ape::as.DNAbin(alignment)

# Passo 5: Estimar uma árvore filogenética não ultramétrica
# Método de Máxima Verossimilhança (ML)
phyDat_alignment <- phangorn::phyDat(aligned_sequences, type = "DNA")
dm <- phangorn::dist.ml(phyDat_alignment)  # Matriz de distância pela verossimilhança
treeML <- ape::nj(dm)  # Árvore inicial pelo algoritmo Neighbor Joining
fit <- phangorn::pml(treeML, phyDat_alignment)  # Ajustar o modelo ML à árvore

# Ajustar a árvore por Máxima Verossimilhança (otimizar comprimento de ramos)
fit_opt <- phangorn::optim.pml(fit, model = "GTR", rearrangement = "stochastic")  # Modelo GTR como exemplo

# Passo 6: Visualizar a árvore não ultramétrica (ML)
ape::plot.phylo(fit_opt$tree, main = "Árvore Não Ulramétrica (ML)")

# Alternativa: Método de Máxima Parcimônia (MP)
treeMP <- phangorn::pratchet(phyDat_alignment)  # Heurística de parcimônia
ape::plot.phylo(treeMP, main = "Árvore Não Ulramétrica (MP)")
