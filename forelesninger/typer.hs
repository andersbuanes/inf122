-- En type representerer en mengde verdier
-- <uttrykk> :: <type>

-- heltal ...,-2,-1,0,1,2,...   type Int og Integer
--                              +, -, *, div, mod

-- boolske verdier True, False  type Bool
--                              not, ||, &&, or, and

-- tegn ...,'A',...,'z',...     type Char
--                              toUpper, toLower i Data.Char

-- strenger som "abc"           type String
--                              ++, head, tail

-- brøktall som 1.2, 1.234      type Float
--                              +, -, *, /

-- Int, Float er typer          Num, Fractional er typeklasser

-- Funksjonstyper
-- Gitt typeK og typeM har vi en type typeK -> typeM
-- Konstruksjon: for x :: typeK, <uttrykk> :: typeM
-- <navn> x = <uttrykk>
-- f.eks: double x = x + x
