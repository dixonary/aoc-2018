readUppers :: ReadP [Char]
readUppers = do
    let upper  = satisfy isUpper 
        ignore = munch $ not.isUpper

    upper `endBy` ignore

readLowers :: ReadP [Char]
readLowers = do
    let upper  = satisfy isLower
        ignore = munch $ not.isLower

    upper `endBy` ignore
