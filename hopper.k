module HOPPER-SYNTAX 
    import DOMAINS
    syntax Pgm ::= "program" "{" Block "}"      

    syntax Skip ::= "Skip"

    syntax Block ::= "{" Stmt "}" 
                | Stmt

    syntax Stmt ::= "if" "(" BExp ")" Block [strict(1)]
                | "if" "(" BExp ")" Block ElseIfStmt [strict(1)]
                | BExp "?" Block ":" Block [strict(1)]
                | "while" "(" BExp ")" Block
                | "do" Block "while" "(" BExp ")"
                | "for" "(" Type UniqueIdAssigner ";" BExp ";" AExpMod ")" Block
                | AExp
                | DecVar ";"
                | Assigner
                | AExpMod
                | Stmt Stmt [left]

    syntax Type ::= "integer" | "boolean"

    syntax Ids ::= List{Id, ","}
    
    syntax DecVar ::= Type Ids

    syntax UniqueIdAssigner ::= Id "=" AExp  [strict(2)]

    syntax Assigner ::= DecVar "=" AExp ";" [strict(2)]
                    |  UniqueIdAssigner ";"

    syntax ElseIfStmt ::= "elif" "(" BExp ")" Block
                        | "else" Block
                        | "elif" "(" BExp ")" Block ElseIfStmt

    syntax BExp ::= Bool
                    | AExp "<" AExp [strict]
                    | AExp "<=" AExp [strict]
                    | AExp ">" AExp [strict]
                    | AExp ">=" AExp [strict]
                    | "(" BExp ")" [bracket]


    syntax AExpMod ::= Id "++" 

    syntax AExp ::= Int
                | Id
                | AExp "+" AExp [seqstrict, left]

    rule program { B:Block } => B [structural]

    rule S1:Stmt S2:Stmt => S1 ~> S2
    
    rule Skip => .
    
    rule if (true) { S:Stmt } => S
    rule if (false) { _ } => Skip
    rule if (true) { B } S:ElseIfStmt => B
    rule if (false) { _ } S:ElseIfStmt => S
    rule elif ( B:BExp ) { S1:Stmt } S2:ElseIfStmt => if ( B ) { S1 } S2
    rule elif ( B:BExp ) { S:Stmt } => if ( B ) { S }
    rule else { S } => S

    rule true ? B:Block : _ => B 
    rule false ? _ : B:Block => B

    rule while (B:BExp) { S:Stmt } => if(B){ S while (B) { S } } [structural]
    
    rule do { S:Stmt } while (B:BExp) => S ~> if(B){ do { S } while (B) } [structural]
    
    rule for (T:Type I:Id = V:AExp ; B:BExp ; Amod:AExpMod ) {S} 
        => T I = V ; ~> while (B) { S Amod } [structural]
        
    rule I1:Int + I2:Int => I1 +Int I2
    rule I1:Int < I2:Int => I1 <Int I2
    rule I1:Int > I2:Int => I1 >Int I2

    rule <k> T:Type I:Id ; => Skip ... </k>
        <env> MEnv:Map => MEnv I |-> L </env>
        <store> MStore:Map => MStore L |-> 0 </store>
        <loc> L:Int => L +Int 1 </loc>
                    
    rule <k> T:Type (I:Id, Xs:Ids) ; => T Xs ; ... </k>
        <env> MEnv:Map => MEnv I |-> L </env>
        <store> MStore:Map => MStore L |-> 0 </store>
        <loc> L:Int => L +Int 1 </loc>

    rule <k> T:Type (I:Id, Xs:Ids) = V:Int ; => T Xs = V ; ... </k>
        <env> MEnv:Map => MEnv I |-> L </env>
        <store> MStore:Map => MStore L |-> V </store>
        <loc> L:Int => L +Int 1 </loc>

    rule T:Type .Ids ; => Skip  
    rule T:Type .Ids = _:Int ; => Skip  

    rule <k> integer I:Id = V:Int ; => Skip ... </k>
        <env> MEnv:Map => MEnv I |-> L </env>
        <store> MStore:Map => MStore L |-> V </store>
        <loc> L:Int => L +Int 1 </loc>

    rule <k> I:Id = V:Int ; => Skip ... </k>
        <env> ... I |-> L ... </env>
        <store> ... L |-> (_ => V) ... </store>

    rule <k> I:Id => V ... </k>
        <env> ... I |-> L ... </env>
        <store> ... L |-> V ... </store>

    rule <k> I:Id ++ => Skip ... </k>
        <env> ... I |-> L ... </env>
        <store> ... L |-> (V => V +Int 1) ... </store>


    configuration <T>
        <k> $PGM:Pgm </k>
        <env> .Map </env>
        <store> .Map </store>
        <loc> 1 </loc>
    </T>
 
endmodule

module HOPPER
    import DOMAINS
    import HOPPER-SYNTAX

    syntax KResult ::= Int | Bool
endmodule