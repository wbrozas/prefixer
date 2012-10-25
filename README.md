prefixer
========

Converts infix to prefix and reduces algebraic expressions (similar to textRamAlpha but less bugs and more readable)

Also textRamAlpha handles factorial and this does not.

    ghc --make prefixer.hs
    ./prefixer [-r] <file name with expression>

Input Format: All chars should be separated by spaces, operators are +-*/()

`( 3 * x + 2 ) * ( 3 * x + 2 )`  
`( x * y + 5 ) / y` 
