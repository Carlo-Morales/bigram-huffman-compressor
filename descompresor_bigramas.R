# ============ Descompresor Huffman con bigramas ============
# Para recuperar el texto, se necesitan los dos archivos del compresor:
#   - output.bin: contiene los bits comprimidos
#   - tabla.rds: guarda los códigos y el relleno añadido
# En la tabla, cada símbolo puede ser un carácter o una pareja de caracteres.
# Al unirlos después de decodificar, se reconstruye el texto original.
# ===========================================================


# (1). Elegir el archivo comprimido y cargar la tabla de códigos.
binPath <- file.choose()
tablePath <- file.path(dirname(binPath), "tabla.rds")

if (!file.exists(tablePath)) {
  stop("No encontré tabla.rds en la misma carpeta que el archivo .bin")
}

info <- readRDS(tablePath)
codes <- info$codes        # Cada nombre es un símbolo y su valor es el código correspondiente.
padding <- info$padding    # Cantidad de bits de relleno que se añadieron al final.


# (2). Leer los bytes y convertirlos de nuevo en una cadena de bits.
bytes <- as.integer(readBin(binPath, "raw", n = file.info(binPath)$size))

# Crear una tabla de 256 entradas: cada valor de byte (0-255) corresponde a 8 bits.
byteToBits <- sapply(0:255, function(b) {
  paste0(rev(as.integer(intToBits(b))[1:8]), collapse = "")
})

bits <- paste0(byteToBits[bytes + 1], collapse = "")

# Quitar los bits de relleno añadidos al final.
bits <- substr(bits, 1, nchar(bits) - padding)

# (3). Preparar la búsqueda inversa: de cada código a su símbolo.
# Un entorno funciona como diccionario y permite buscar los códigos rápidamente.
reverseTable <- new.env(hash = TRUE)
for (k in seq_along(codes)) {
  reverseTable[[ codes[[k]] ]] <- names(codes)[k]
}

# (4). Decodificar los bits.
# Se acumulan de uno en uno hasta encontrar un código. Entonces se guarda
# el símbolo correspondiente (carácter o bigrama) y se empieza de nuevo.
# Esto funciona porque ningún código es el prefijo de otro.
bitVector <- strsplit(bits, "")[[1]]
output <- character(length(bitVector))   # Se reserva espacio y se recorta al terminar.
count <- 0
current <- ""

for (bit in bitVector) {
  current <- paste0(current, bit)
  symbol <- reverseTable[[current]]
  if (!is.null(symbol)) {
    count <- count + 1
    output[count] <- symbol
    current <- ""
  }
}

# Al unir los símbolos, cada bigrama recupera sus dos caracteres originales.
text <- paste0(output[seq_len(count)], collapse = "")


# (5). Guardar el texto recuperado.
outputPath <- file.path(dirname(binPath), "recuperado.txt")
writeLines(text, outputPath, sep = "", useBytes = TRUE)

cat("Listo. Texto guardado en:", outputPath, "\n")
# ===========================================================
