module HOPPER-SYNTAX 
    import DOMAINS
    syntax Pgm ::= "program" "{" Block "}"      

    syntax Skip ::= "Skip"

    syntax Block ::= "{" Stmt "}"

    syntax Stmt ::= "if" "(" BExp ")" Block [strict(1)]
                | "if" "(" BExp ")" Block ElseIfStmt [strict(1)]
                | AExp

    syntax ElseIfStmt ::= "elif" "(" BExp ")" Block
                        | "else" Block
                        | "elif" "(" BExp ")" Block ElseIfStmt

    syntax BExp ::= Bool
                    | Int "<" Int
                    | Int ">" Int 

    syntax AExp ::= Int
                | Int "+" Int

    rule Skip => .
    
    rule if (true) { S:Stmt } => S
    rule if (false) { _ } => Skip
    rule if (true) { B } S:ElseIfStmt => B
    rule if (false) { _ } S:ElseIfStmt => S
    rule elif ( B:BExp ) { S1:Stmt } S2:ElseIfStmt => if ( B ) { S1 } S2
    rule elif ( B:BExp ) { S:Stmt } => if ( B ) { S }
    rule else { S } => S

    rule I1:Int + I2:Int => I1 +Int I2
    rule I1:Int < I2:Int => I1 <Int I2
    rule I1:Int > I2:Int => I1 >Int I2
 
endmodule

module HOPPER
    import DOMAINS
    import HOPPER-SYNTAX

    syntax KResult ::= Int | Bool
endmodule