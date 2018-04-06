fun gcd (a: int, b: int) =
    if b=0 then a else
    gcd(b, a mod b);

fun allCaps string:string =
    if size(string)=1 then String.str(Char.toUpper(String.sub(string,0))) else
    String.str(Char.toUpper(String.sub(string, 0))) ^ allCaps(String.extract(string,1,NONE));

fun firstCaps (list) =
    if list = [] then [] else
    String.str(Char.toUpper(String.sub(List.hd(list),0)))::
    firstCaps(List.tl(list));

fun swap []=[]
| swap (x::xs)=List.hd(xs)::x::List.tl(xs);

fun rotate(0,list)=list
| rotate(n,list)=List.drop(list,n)@List.take(list,n);

fun deleteHelper(n,list) = implode(List.take(list,n)@List.drop(list,n+1));
fun delete(n,string) =  deleteHelper(n,String.explode(string));

fun intpow(base, 0)=1
| intpow(base, index)=
if index mod 2=1 then
base*intpow(base*base, (index-1) div 2)
else
intpow(base*base, index div 2);

(*
fun rotate2Helper(0,xs,old)=xs@old
| rotate2Helper(n,x::xs,old)=rotate2Helper(n-1,xs,old::x);
fun rotate2(n,x::xs)=rotate2Helper(n-1,xs,[x]);
*)

fun rotate2(0,x)=x
|rotate2(n,x::xs)=rotate2(n-1,xs@[x])
|rotate2(n,nil)=nil;

fun rotate3Helper(0,x::xs)=x::xs
|rotate3Helper(n,x::xs)=rotate3Helper(n-1,xs)@[x]
|rotate3Helper(n,nil)=nil;

fun rotate3(n,lst)=
let val actual=n mod  List.length(lst)
in rotate3Helper(actual,lst)
end;

fun delete2()
