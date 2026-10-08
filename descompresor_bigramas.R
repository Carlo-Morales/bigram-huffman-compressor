# ============ Decompresor de Huffman + Bigramas ============
# Necesita los dos archivos que genera el compresor:
#   - output.bin : los bits comprimidos
#   - tabla.rds  : la tabla de códigos y el relleno (padding)
# Nota: en la tabla, un símbolo puede ser un carácter o un bigrama
# (dos caracteres). Al decodificar se concatenan tal cual, por lo que
# el texto original se reconstruye exactamente.
# ===========================================================


# (1). Elegir el archivo comprimido y cargar la tabla
binPath <- file.choose()
tablePath <- file.path(dirname(binPath), "tabla.rds")

if (!file.exists(tablePath)) {
  stop("No encontré tabla.rds en la misma carpeta que el archivo .bin")
}

info <- readRDS(tablePath)
codes <- info$codes        # vector con nombres: nombre = símbolo (carácter o bigrama), valor = código
padding <- info$padding    # bits de relleno al final


# (2). Leer los bytes del archivo y convertirlos a una cadena de bits
bytes <- as.integer(readBin(binPath, "raw", n = file.info(binPath)$size))

# Tabla de 256 entradas: cada byte (0-255) -> su texto de 8 bits
byteToBits <- sapply(0:255, function(b) {
  paste0(rev(as.integer(intToBits(b))[1:8]), collapse = "")
})

bits <- paste0(byteToBits[bytes + 1], collapse = "")

# Quitar el relleno del final
bits <- substr(bits, 1, nchar(bits) - padding)

# (3). Preparar la búsqueda inversa: código -> símbolo
# Un "environment" funciona como diccionario y es rápido
reverseTable <- new.env(hash = TRUE)
for (k in seq_along(codes)) {
  reverseTable[[ codes[[k]] ]] <- names(codes)[k]
}

# (4). Decodificar
# Se va acumulando bit por bit; cuando lo acumulado coincide con un código,
# se anota el símbolo (carácter o bigrama) y se reinicia
# (gracias a que ningún código es prefijo de otro)
bitVector <- strsplit(bits, "")[[1]]
output <- character(length(bitVector))   # espacio de sobra, se recorta al final
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

# Al unir los símbolos, cada bigrama vuelve a ser sus dos caracteres originales
text <- paste0(output[seq_len(count)], collapse = "")


# (5). Guardar el texto recuperado
outputPath <- file.path(dirname(binPath), "recuperado.txt")
writeLines(text, outputPath, sep = "", useBytes = TRUE)

cat("Listo. Texto guardado en:", outputPath, "\n")
# ===========================================================
