# ============ Compresor Huffman + Bigramas ============
# Genera dos archivos:
#   - output.bin : los bits comprimidos
#   - tabla.rds  : la tabla de códigos y el relleno (padding)
# Idea: antes de aplicar Huffman, las parejas de caracteres (bigramas) que
# más se repiten se tratan como UN solo símbolo. Así Huffman les asigna un
# único código corto en lugar de dos códigos.
# ======================================================


# ============ Libraries ============
library(stringr)
# ===================================


# ============ Parámetros ============
# Un bigrama solo se agrupa si aparece al menos minBigramFreq veces
# (cada bigrama agrega una entrada a la tabla, así que debe compensar)
minBigramFreq <- 10
# Máximo de bigramas distintos que se agrupan (los más frecuentes)
maxBigrams <- 200
# ====================================


# ============ Functions ============

# ----- For Create Nodes -----
createNode <- function(sym = NULL, frec, l = NULL, r = NULL) {
  list(symbol = sym, frequency = frec, left = l, right = r)
}

# -------- Leaf Node? --------
isLeafNode <- function(singleNode){
  if((is.null(singleNode$left)) && (is.null(singleNode$right))){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}


# ------- DFS Function -------
dfs <- function (singleNode, code = ""){
  
  if (isLeafNode(singleNode)){
    result <- code
    names(result) <- singleNode$symbol
    return(result)
  }
  
  c(dfs(singleNode$left,  paste0(code, "0")),
    dfs(singleNode$right, paste0(code, "1")))
}

# ===================================


# ================================ Huffman + Bigramas ================================
# (1). get text
    # -----------------------------------------------------------------------
    # For text in a file
    selectedFile <- file.choose()
    input <- readChar(selectedFile, file.info(selectedFile)$size, useBytes = FALSE)
    input <- unlist(strsplit(input, ""))
    # -----------------------------------------------------------------------
    # For a mannual string
    
    #input <- "nanobanana"
    #input <- unlist(strsplit(input, ""))
    # -----------------------------------------------------------------------

# (2). Agrupar bigramas: convertir los caracteres en "tokens"
    # Un token es un carácter suelto o un bigrama (dos caracteres juntos).
    # El resto del algoritmo (Huffman) trabaja sobre los tokens.
    n <- length(input)
    
    if (n > 1) {
      
      # Todas las parejas consecutivas: la posición i es la pareja (input[i], input[i+1])
      pairs <- paste0(input[-n], input[-1])
      
      # Contar cuántas veces aparece cada pareja y quedarse con las más frecuentes
      pairCounts <- sort(table(pairs), decreasing = TRUE)
      pairCounts <- pairCounts[pairCounts >= minBigramFreq]
      keep <- names(pairCounts)[seq_len(min(length(pairCounts), maxBigrams))]
      
      # Posiciones donde empieza un bigrama elegido
      cand <- pairs %in% keep
      
      # Los bigramas pueden solaparse ("aaa" contiene "aa" dos veces).
      # En cada racha de candidatos consecutivos se toma uno sí y uno no,
      # empezando por el primero, para que ninguno se solape con otro.
      runs <- rle(cand)
      take <- cand & (sequence(runs$lengths) %% 2 == 1)
      starts <- which(take)
      
      # Reemplazar: el primer carácter pasa a ser el bigrama y se elimina el segundo
      if (length(starts) > 0) {
        input[starts] <- pairs[starts]
        input <- input[-(starts + 1)]
      }
    }

# (3). get token´s frequencies and store those frequencies in a dataFrame
    occurrences <- sort(table(input), decreasing = FALSE)
    
    dataFrame <- data.frame(
      symbols = names(occurrences),
      frecuencies = as.vector(occurrences)
    )
    
    totalFrequency <- sum(dataFrame$frecuencies, na.rm = TRUE)

# (4). build binary tree

    # Crete the leaf nodes
    nodes <- lapply(seq_len(nrow(dataFrame)), function(k) {
      createNode(dataFrame$symbols[k], dataFrame$frecuencies[k])
    })
    
    while (length(nodes) > 1){
      
      # Sort the frequencies
      freqs <- sapply(nodes, function(x) x$frequency)
      nodes <- nodes[order(freqs)]
      
      #Choose left and right
      left <- nodes[[1]]
      right <- nodes[[2]]
      
      #Create the new node
      newNode <- createNode(paste0(left$symbol, right$symbol),
                 left$frequency + right$frequency,
                 left,
                 right)
      
      #Erease both of the recently merged nodes
      nodes <- c(list(newNode), nodes[-c(1, 2)])
    }
    
    binaryTree <- nodes[[1]]

# (5). Build the binary for each token in the tree

    #Function with dfs
    codes <- dfs(binaryTree)
    
    # Caso especial: un solo token distinto -> el árbol es una hoja y su código quedaría vacío
    if (isLeafNode(binaryTree)) {
      codes <- c("0")
      names(codes) <- binaryTree$symbol
    }
    
    #New column in the dataFrame
    dataFrame$binaryKey <- codes[dataFrame$symbols]

# (6). Build the file with the codes

    # 1. Cadena completa de bits del texto (un código por token)
    bits <- paste0(codes[input], collapse = "")
    
    # 2. Rellenar solo el final hasta completar un múltiplo de 8
    padding <- (8 - nchar(bits) %% 8) %% 8
    bits <- paste0(bits, strrep("0", padding))
    
    # 3. Cortar en bloques de 8 y convertir cada uno a un número
    inicio <- seq(1, nchar(bits), by = 8)
    bytes <- strtoi(substring(bits, inicio, inicio + 7), base = 2)
    
    # 4. Elegir dónde guardar y escribir el archivo binario
    outputPath <- file.choose(new = TRUE)
    writeBin(as.raw(bytes), outputPath)
    
    # 5. Guardar la tabla y el relleno junto al archivo
    #    (la tabla ya incluye los bigramas como símbolos de dos caracteres)
    saveRDS(list(codes = codes, padding = padding),
            file.path(dirname(outputPath), "tabla.rds"))
# ====================================================================================
