use "String";
use "Char";

fun gcd (a: int, b: int) =
    if b=0 then a else
    gcd(b, a mod b);

fun allCaps string =
    if size(string)=1 then String.str(toUpper(sub(string,0))) else
    String.str(toUpper(sub(string, 0))) ^ allCaps(extract(string,1));
