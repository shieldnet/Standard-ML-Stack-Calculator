exception SyntaxError;
exception NotDefined;
exception NotValid;

fun cisInt c = ord c >= ord #"0" andalso ord c <= ord #"9";
fun cisAlp c = ord c >= ord #"a" andalso ord c <= ord #"z"
              orelse ord c >= ord #"A" andalso ord c <= ord #"Z";
fun cisOpr c = c = #"+" orelse c = #"-" orelse c = #"*" orelse c = #"/"
              orelse c = #"(" orelse c = #")" orelse c = #"{" orelse c = #"}";

fun toInt s =
  let
    fun sub nil = 0
      | sub (h::t) = (ord h - ord #"0") + 10 * sub t
  in
    sub (rev (explode s))
  end;

fun isInt s = cisInt (hd (explode s));
fun isAlp s = cisAlp (hd (explode s));
fun isOpr s = cisOpr (hd (explode s));

fun separate s =
  let
    fun skipSpace nil = nil
      | skipSpace (h::t) =
          if h = #" " then skipSpace t else (h::t)
    fun getString (L1,nil) = (L1,nil)
      | getString (L1,h2::t2) =
          if cisAlp h2 then getString (L1@[h2], t2)
          else if cisOpr h2 then (L1, h2::t2)
          else if h2 = #" " then (L1, skipSpace t2)
          else raise SyntaxError
    fun getInteger (L1,nil) = (L1,nil)
      | getInteger (L1,h2::t2) =
          if cisInt h2 then getInteger (L1@[h2], t2)
          else if cisOpr h2 then (L1, h2::t2)
          else if h2 = #" " then (L1, skipSpace t2)
          else raise SyntaxError
    fun subSep nil = nil
      | subSep (h::t) =
          if h = #" " then subSep (skipSpace t)
          else if cisAlp h then
             let
               val (value,rest) = getString ([h],t)
             in
               value::(subSep rest)
             end
          else if cisInt h then
             let
               val (value,rest) = getInteger ([h],t)
             in
               value::(subSep rest)
             end
          else if cisOpr h then [h]::(subSep t)
          else raise SyntaxError
  in
    map implode (subSep (explode s))
  end;

fun fact n =
  if n > 0 then n * fact (n - 1)
  else if n = 0 then 1
  else raise NotValid;

fun fibo n =
  let
    fun subFibo n f1 f2 =
      if n = 0 then f1
      else if n = 1 then f2
      else subFibo (n - 1) f2 (f1 + f2)
  in
    if n < 0 then raise NotValid
    else subFibo n 0 1
  end;
