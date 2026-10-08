

Readme · MD
# bigram-huffman-compressor
 
Lossless text compressor in R that combines bigram grouping with Huffman coding, plus a matching decompressor.
 
## How it works
 
1. The input file is read and split into individual characters.
2. The most frequent pairs of consecutive characters (bigrams) are detected and grouped into single symbols. Overlapping bigrams are resolved so that no character is used twice.
3. Symbol frequencies are counted and a Huffman binary tree is built over the resulting symbols (single characters and bigrams).
4. Each symbol is replaced by its Huffman code, and the bit string is packed into bytes.
5. Two files are written: the compressed binary and a table with the codes and the padding.
Because a bigram is just a symbol made of two characters, the decompressor only needs to decode the bits and concatenate the symbols to recover the original text exactly.
 
## Files
 
| File | Description |
|------|-------------|
| `compresor_huffman_bigramas.R` | Compressor (bigrams + Huffman) |
| `decompresor_bigramas.R` | Decompressor, compatible with the compressor above |
 
## Requirements
 
- R (4.x recommended)
- The `stringr` package:
```r
install.packages("stringr")
```
 
## Usage
 
### Compress
 
1. Run `compresor_huffman_bigramas.R`.
2. Select the input `.txt` file when prompted.
3. Choose where to save the compressed file (for example `output.bin`).
This produces:
 
- `output.bin`: the compressed bits.
- `tabla.rds`: the code table and padding, saved in the same folder as the binary.
### Decompress
 
1. Run `decompresor_bigramas.R`.
2. Select the compressed `.bin` file when prompted. `tabla.rds` must be in the same folder.
The recovered text is saved as `recuperado.txt` in that folder.
 
## Parameters
 
At the top of the compressor you can tune the bigram grouping:
 
| Parameter | Default | Meaning |
|-----------|---------|---------|
| `minBigramFreq` | `10` | Minimum number of occurrences for a bigram to be grouped |
| `maxBigrams` | `200` | Maximum number of distinct bigrams to group |
 
Each grouped bigram adds an entry to the code table, so it only pays off if it appears often enough.
 
## Notes
 
- Compression is lossless: the recovered file matches the original.
- The code table is stored in `tabla.rds` and is needed to decompress.
- Designed for plain text (`.txt`) files.
 
Claude terminó la respuesta
