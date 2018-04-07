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

fun delete2(n:int) string=deleteHelper(n,String.explode(string));

fun delete5(string) =delete(5,string);

fun repeat (0,obj)=[]
|repeat (n:int, obj)= obj::repeat(n-1,obj)

fun constructor nil n = nil
|constructor (h::t) n = (h n)::(constructor t n);

(*
This is the higher order function
*)
fun apply(function,nil)=[]
|apply(function, x::xs)=
function(x)@apply(function,xs)

fun iseven(x)= if x mod 2 =0 then [x] else [];
fun evens(list)=apply(iseven,list);

fun isLowerFirst(string)=if Char.isLower(List.hd(String.explode(string))) then [string] else [];
fun lowerFirsts(list)=apply(isLowerFirst,list)

fun myToUpper(char)=[Char.toUpper(char)];
fun allCaps2(string)=String.implode(apply(myToUpper,String.explode(string)));
(*
fun applyWithParam(function,param,nil)=[]
|applyWithParam(function, param, x::xs)=
function(x,param)@apply(function,param,xs);
*)

fun fileToListString(filename)=
    let val file = TextIO.openIn filename
    val poem = TextIO.inputAll file
    val _ = TextIO.closeIn file
in String.tokens (fn c => c = #"\n") poem
end;

fun prune(nil,target)=[]
|prune(string::otherstrs,target)=if String.isSubstring string target
then string::prune(otherstrs,target)
else prune(otherstrs,target);

(*
fun prune(target) str::strs = if String.isSubstring(target,str)
then string::prune(target) strs
else prune(target) strs;
*)

fun printStrings(nil,target:string)=[]
|printStrings(x::xs,target:string)=if String.isSubstring target x then
(TextIO.print(x^"\n"); printStrings(xs,target))
else
(printStrings(xs,target));
fun find(target:string,filename)=(printStrings(fileToListString(filename),target);());

(*

fun hello(target,filename)(printStrings(prune(fileToListString(filename),target)); ());
TextIO.print(TextIO.stdout,["hello","you"])

fun transform(fxn) nil=[]
|transform (fxn) list = [fxn(List.hd(list))] :: (transform fxn List.tl(list));
*)

fun whatever(fxn) a = [fxn a handle except=> a];
fun transform(fxn) nil=[]
|transform (fxn) list = apply(whatever(fxn),list);
transform(fn x => 15 div x)[1,2,3,4];

fun succ(O)=O+1;
datatype Natural = O
|succ of Natural

fun convert(n: Natural): int =
    case n of
    O => 0
    | succ(m) => 1 + convert(m)

fun add(n1:Natural, n2: Natural): Natural =
    case n1 of
    O => n2
    | succ(n_1)=> add(n_1, succ(n2))

fun mul(n1:Natural, n2:Natural): Natural=
    case n1 of
    O=> O
    | succ(n_1)=> add(n2, mul(n_1,n2))

fun hadd(list)= List.foldr add O list;
