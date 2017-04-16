module Eval
    open System

    type private Operator =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Power

    type private Token =
        | Number of Double
        | OpenBracket
        | CloseBracket
        | Operator of Operator

    let private tokenize i =
        let rec tokenize s tokens =
            match s with
            | [] -> tokens
            | c :: tail when Char.IsWhiteSpace(c) -> tokenize tail tokens
            | '(' :: tail -> tokenize tail (OpenBracket :: tokens)
            | ')' :: tail -> tokenize tail (CloseBracket :: tokens)
            | '+' :: tail -> tokenize tail (Operator Add :: tokens)
            | '-' :: tail -> tokenize tail (Operator Subtract :: tokens)
            | '*' :: tail -> tokenize tail (Operator Multiply :: tokens)
            | '^' :: tail -> tokenize tail (Operator Power :: tokens)
            | '/' :: tail -> tokenize tail (Operator Divide :: tokens)
            | c :: tail when Char.IsDigit(c) || c = ',' || c = '.' -> 
                let rec doubleToValue s acc =
                    match s with
                    | ',' :: tail 
                    | '.' :: tail 
                        -> doubleToValue tail (',' :: acc)
                    | c :: tail when Char.IsDigit(c) -> doubleToValue tail (c :: acc)
                    | _ -> 
                        match acc with
                        | ',' :: _ -> (Number (Double.Parse (String.Concat (Seq.rev acc) + "0")), s)
                        | _ -> (Number (Double.Parse (String.Concat (Seq.rev acc))), s)
                let (Token, t) = doubleToValue (c :: tail) []
                tokenize t (Token :: tokens)
            | n :: _ -> failwith ("Parse error - invalid character detected: " + n.ToString())
        List.rev (tokenize (Seq.toList i) [])

    let private evalTokens input =
        
        let eval a b op =
            match op with
            | Operator Add -> b + a
            | Operator Subtract -> b - a
            | Operator Multiply -> b * a
            | Operator Divide -> b / a
            | Operator Power -> b ** a
            | _ -> failwith "unexpected operator"
    
        let pop (stack:'a list) = (stack.Head, stack.Tail.Head, stack.Tail.Tail)
    
        let rec eval_rec input numstack (opstack:Token list) =
            match input with
            | Number n :: tail -> eval_rec tail (n::numstack) opstack
            | Operator o :: tail ->
                if opstack.Length <> 0 && opstack.Head > (Operator o) then
                    let firstNum, secondNum, numstackRem = pop numstack
                    let e = eval firstNum secondNum opstack.Head
                    eval_rec tail (e::numstackRem) (Operator o::opstack.Tail)
                else
                    eval_rec tail numstack (Operator o::opstack)
            | OpenBracket :: tail -> eval_rec tail numstack (OpenBracket::opstack)
            | CloseBracket :: tail ->
                match opstack with
                | Operator op :: opTail ->
                    let firstNum, secondNum, numstackRem = pop numstack
                    let e = eval firstNum secondNum (Operator op)
                    eval_rec input (e::numstackRem) opTail
                | OpenBracket :: _ ->
                    eval_rec tail numstack opstack.Tail
                | _ -> failwith "error parsing input"
            | [] -> 
                match opstack with
                | Operator op :: tail ->
                    let firstNum, secondNum, numstackRem = pop numstack
                    let e = eval firstNum secondNum (Operator op)
                    eval_rec [] (e::numstackRem) tail
                | [] -> numstack.Head
                | _ -> failwith "error parsing input"

        eval_rec input [] []

    let eval input = evalTokens (tokenize input)