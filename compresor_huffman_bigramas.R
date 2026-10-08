# ============ Compresor Huffman con bigramas ============
# Al terminar, se guardan dos archivos:
#   - output.bin: contiene los bits comprimidos
#   - tabla.rds: guarda los códigos y el relleno necesario
# Antes de aplicar Huffman, el programa junta las parejas de caracteres
# que aparecen con más frecuencia. Cada pareja cuenta como un solo símbolo,
# así puede recibir un código más corto que si se codificaran sus caracteres
# por separado.
# =======================================================


# ============ Paquetes necesarios ============
library(stringr)
# =============================================


# ============ Parámetros ============
# Solo se agrupan los bigramas que aparecen al menos minBigramFreq veces.
# Como cada uno ocupa un lugar en la tabla, conviene elegir los que se repiten.
minBigramFreq <- 10
# Límite de bigramas distintos que se agrupan, se priorizan los más frecuentes.
maxBigrams <- 200
# ====================================


# ============ Funciones ============

# ----- Crear nodos del árbol -----
createNode <- function(sym = NULL, frec, l = NULL, r = NULL) {
  list(symbol = sym, frequency = frec, left = l, right = r)
}

# -------- Es una hoja o que ? --------
isLeafNode <- function(singleNode){
  if((is.null(singleNode$left)) && (is.null(singleNode$right))){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}


# ------- Recorrido en profundidad -------
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
# (1). Leer el texto de entrada
    # -----------------------------------------------------------------------
    # Opción habitual: elegir un archivo de texto.
    selectedFile <- file.choose()
    input <- readChar(selectedFile, file.info(selectedFile)$size, useBytes = FALSE)
    input <- unlist(strsplit(input, ""))
    # -----------------------------------------------------------------------
    # Para probar con una cadena escrita a mano, se pueden usar estas líneas:
    
    #input <- "nanobanana"
    #input <- unlist(strsplit(input, ""))
    # -----------------------------------------------------------------------

# (2). Agrupar bigramas: convertir los caracteres en "tokens"
    # Un token es un carácter suelto o un bigrama (dos caracteres juntos).
    # A partir de aquí, Huffman trabaja con estos tokens.
    n <- length(input)
    
    if (n > 1) {
      
      # Formar cada pareja consecutiva: en i se juntan input[i] e input[i + 1].
      pairs <- paste0(input[-n], input[-1])
      
      # Contar las parejas y conservar las que más se repiten.
      pairCounts <- sort(table(pairs), decreasing = TRUE)
      pairCounts <- pairCounts[pairCounts >= minBigramFreq]
      keep <- names(pairCounts)[seq_len(min(length(pairCounts), maxBigrams))]
      
      # Marcar las posiciones donde comienza un bigrama seleccionado.
      cand <- pairs %in% keep
      
      # Algunos bigramas se solapan; por ejemplo, "aaa" contiene dos "aa".
      # Cuando hay candidatos seguidos, se agrupa uno de cada dos, empezando
      # por el primero, para que ningún carácter se use en más de un bigrama.
      runs <- rle(cand)
      take <- cand & (sequence(runs$lengths) %% 2 == 1)
      starts <- which(take)
      
      # Sustituir cada pareja: el primer carácter se convierte en el bigrama
      # y el segundo se quita de la secuencia.
      if (length(starts) > 0) {
        input[starts] <- pairs[starts]
        input <- input[-(starts + 1)]
      }
    }

# (3). Contar las apariciones de cada token y guardarlas en una tabla
    occurrences <- sort(table(input), decreasing = FALSE)
    
    dataFrame <- data.frame(
      symbols = names(occurrences),
      frecuencies = as.vector(occurrences)
    )
    
    totalFrequency <- sum(dataFrame$frecuencies, na.rm = TRUE)

# (4). Construir el árbol binario

    # Crear una hoja por cada símbolo.
    nodes <- lapply(seq_len(nrow(dataFrame)), function(k) {
      createNode(dataFrame$symbols[k], dataFrame$frecuencies[k])
    })
    
    while (length(nodes) > 1){
      
      # Ordenar los nodos por frecuencia, de menor a mayor.
      freqs <- sapply(nodes, function(x) x$frequency)
      nodes <- nodes[order(freqs)]
      
      # Tomar los dos nodos con menor frecuencia.
      left <- nodes[[1]]
      right <- nodes[[2]]
      
      # Unirlos en un nodo nuevo.
      newNode <- createNode(paste0(left$symbol, right$symbol),
                 left$frequency + right$frequency,
                 left,
                 right)
      
      # Retirar los nodos unidos y añadir el nuevo al conjunto.
      nodes <- c(list(newNode), nodes[-c(1, 2)])
    }
    
    binaryTree <- nodes[[1]]

# (5). Obtener el código binario de cada token

    # Recorrer el árbol en profundidad para generar los códigos.
    codes <- dfs(binaryTree)
    
    # Si solo hay un token, el árbol es una hoja y el recorrido daría un
    # código vacío; se le asigna "0" para poder codificarlo.
    if (isLeafNode(binaryTree)) {
      codes <- c("0")
      names(codes) <- binaryTree$symbol
    }
    
    # Añadir a la tabla el código asignado a cada símbolo.
    dataFrame$binaryKey <- codes[dataFrame$symbols]

# (6). Crear el archivo comprimido

    # 1. Reemplazar cada token por su código y unir los bits.
    bits <- paste0(codes[input], collapse = "")
    
    # 2. Añadir ceros al final hasta completar bytes de 8 bits.
    padding <- (8 - nchar(bits) %% 8) %% 8
    bits <- paste0(bits, strrep("0", padding))
    
    # 3. Separar los bits en grupos de 8 y convertirlos en números.
    inicio <- seq(1, nchar(bits), by = 8)
    bytes <- strtoi(substring(bits, inicio, inicio + 7), base = 2)
    
    # 4. Elegir la ubicación y escribir el archivo binario.
    outputPath <- file.choose(new = TRUE)
    writeBin(as.raw(bytes), outputPath)
    
    # 5. Guardar junto al archivo la tabla de códigos y la cantidad de ceros
    #    añadidos. Los bigramas aparecen como símbolos de dos caracteres.
    saveRDS(list(codes = codes, padding = padding),
            file.path(dirname(outputPath), "tabla.rds"))
# ====================================================================================
