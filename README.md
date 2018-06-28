# bitfolder
A bitwise huffman tree-like compressor for large data globs

**Usage:**

* Compresses data at the bit level. The effectiveness of this method is highly dependent upon the type of data being compressed. Set SIZE to at least the size your input in bytes, and run COMPRESS in the following way:

    [size of input] COMPRESS [input]

    for example:
    1024 COMPRESS [1024 BYTE STRING]
    
* Compressed text is returned in the CIPHERTEXT memory slot and the size of the compressed string (which is needed to decompress) is returned on the parameter stack. DECOMP takes the length of the compressed string as an argument and looks for the compressed data in the CIPHERTEXT memory register.
* STR_DECOMP decompresses an ascci string given a bit length, formatted similarly to the matching COMPRESS function.
* STR_COMP behaves the same as COMPRESS, but returns the compressed data as a hexadecimal string, preceeded by the string's total length and compression length.
