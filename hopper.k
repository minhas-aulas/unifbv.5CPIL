module HOPPER-SYNTAX 
    import DOMAINS
    syntax Pgm ::= "program" "{" Block "}"      

    syntax Skip ::= "Skip"

    syntax Block ::= "{" Stmt "}" 
                | Stmt
                | Stmt Block

    syntax Stmt ::= "if" "(" BExp ")" Block [strict(1)]
                | "if" "(" BExp ")" Block ElseIfStmt [strict(1)]
                | BExp "?" Block ":" Block [strict(1)]
                | "while" "(" BExp ")" Block [strict(1)]
                | AExp
                | DecVar ";"
                | Assigner
                | Exp

    syntax Type ::= "integer" | "boolean"

    syntax DecVar ::= Type Id

    syntax Assigner ::= DecVar "=" AExp ";" [strict(2)]
                    | Id "=" AExp ";" [strict(2)]

    syntax ElseIfStmt ::= "elif" "(" BExp ")" Block
                        | "else" Block
                        | "elif" "(" BExp ")" Block ElseIfStmt

    syntax BExp ::= Bool
                    | Int "<" Int
                    | Int "<" Id
                    | Int ">" Int 
                    | "(" BExp ")" [bracket]


    syntax Exp ::= Id "++" 

    syntax AExp ::= Int
                | Id
                | AExp "+" AExp [seqstrict, left]

    rule program { B:Block } => B [structural]

    rule S:Stmt B:Block => S ~> B
    
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

    rule I1:Int + I2:Int => I1 +Int I2
    rule I1:Int < I2:Int => I1 <Int I2
    rule I1:Int > I2:Int => I1 >Int I2

    rule <k> T:Type I:Id ; => Skip ... </k>
        <env> MEnv:Map => MEnv I |-> L </env>
        <store> MStore:Map => MStore L |-> 0 </store>
        <loc> L:Int => L +Int 1 </loc>

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