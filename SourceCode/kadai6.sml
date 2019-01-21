fun findValue alp nil = raise NotDefined
    | findValue alp (h::t) = 
        let
            fun first (x, _) = x
            fun second (_ ,y) = y
        in
            if first h = alp
                then second h
            else findValue alp t
        end;
    
fun compute s mapL =
    let
        fun EXP nil = raise SyntaxError
            | EXP (h::t) =
                if isInt h then (toInt h, t)
                else if h = "fact" orelse h = "fibo"
                    then FUNC (h::t)
                else if isAlp h
                    then 
                else if List.exists(fn x => x = h) ["+", "-", "*", "/","(",")"] 
                    then COMP (h::t)
                else raise SyntaxError
        
        and FUNC nil = raise SyntaxError
            | FUNC (h::t) =
                 if h = "fact" then
                    let 
                        val (v1,t1) = EXP t
                    in
                        (fact v1, t1)
                    end
                else if h = "fibo" then
                    let 
                        val (v1,t1) = EXP t
                    in
                        (fibo v1, t1)
                    end
                else if findValue h mapL
                    raise SyntaxError

        and COMP nil = raise SyntaxError
            | COMP (h::t) =
                if h = "(" then COMP t
                else if h = ")" then COMP t

                else if isAlp h then FUNC (h::t)

                (* The Case of "+" *)
                else if h = "+" then
                    let
                        val (v1,t1) = EXP t
                        val (v2,t2) = EXP t1
                    in
                        (v1 + v2, t2)
                    end

                (* The Case of "*" *)    
                else if h = "*" then
                    let
                        val (v1,t1) = EXP t
                        val (v2,t2) = EXP t1
                    in
                        (v1 * v2, t2)
                    end
                (* The Case of "/" *)
                (* Can Only Divide int / int *)
                else if h = "/" then
                    let
                        val (v1,t1) = EXP t
                        val (v2,t2) = EXP t1
                    in
                        (v1 div v2, t2)
                    end

                (* The Case of "-" *)
                else if h = "-" then
                    let
                        val (v1,t1) = EXP t
                        val (v2,t2) = EXP t1
                    in
                        (v1 - v2, t2)
                    end                  

                else
                    raise SyntaxError

    in
        let
                val (result,rest) = EXP (separate s)
        in
                if rest <> nil then result else raise SyntaxError
        end
    end;
