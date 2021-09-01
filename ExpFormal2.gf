concrete ExpFormal of Exp = {

oper ExpType = {s : Str ; c : Connectivity ; p : Prec} ;

-- param Connectivity = CInfix | CPrefix | CPostfix | CNone ;

param

  Connectivity = CInfixL | CPrefix | CNone ;
  -- Precedence = PSimple | PComplex ;
  Bool = True | False ;


oper

  par : Prec -> ExpType -> Str =
    \p,exp -> case lessPrec exp.p p of {
      True => parenth exp.s ;
      -- False => parenthOpt exp.s -- "(" ++ exp.s ++ ")"
      False => exp.s -- "(" ++ exp.s ++ ")"
    } ;

  parenth : Str -> Str = \s -> "(" ++ s ++ ")" ;
  parenthOpt : Str -> Str = \s -> variants {s ; "(" ++ s ++ ")"} ;

  Prec : PType = Predef.Ints 2 ;

  mkPrec : Connectivity -> Prec -> Str -> ExpType = \c,p,s ->
    {s = s ; p = p ; c = c} ;

  constant : Str -> ExpType = \s -> {s = s ; p = maxPrec ; c = CNone } ;
  maxPrec = 2 ;

  nextPrec : Prec -> Prec = \p -> case <p : Prec> of {
    2 => 2 ;
    n => Predef.plus n 1
    } ;

  lessPrec : Prec -> Prec -> Bool = \p,q ->
    case <<p,q> : Prec * Prec> of {
      <1,1> | <1,0> | <0,0> => False ;
      <1,_> | <0,_>         => True ;
      _ => False
    } ;

lincat
  Exp = ExpType ;
  Var = ExpType ;

lin
  EVar v = v ;


  -- this is wrong
  -- p "3 + 4 * 5"
  --   Exp> EApp (EApp (EVar VTimes) (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EVar (VInt 4)))) (EVar (VInt 5))

  -- how to distinguish infixl vs infixr
  EApp f a = apply f a ;

oper
  -- apply : ExpType -> ExpType -> ExpType = \f,a -> case f.c of {
  --   CInfixL => {s = par f.p a ++ f.s ; c = CPrefix ; p = (f.p) } ;
  --   CPrefix => {s = f.s ++ par (nextPrec f.p) a ; c = a.c ; p = (f.p) } ;
  --   CNone => {s = "APPLY" ++ f.s ++ a.s ; c = CNone ; p = 2}
  --   } ;

  -- Add e1 e2 = { s = ( usePrec 0 e1 ++ "+" ++ usePrec 1 e2) ; p = 0 } ;

  -- apply : ExpType -> ExpType -> ExpType = \plus,a -> case plus.c of {
  --   CInfixL => {s = par plus.p a ++ plus.s ; c = CPrefix ; p = (plus.p) } ;
  --   -- CPrefix => {s = plus.s ++ par (nextPrec plus.p) a ; c = a.c ; p = (plus.p) } ;
  --   CPrefix => {s = plus.s ++ par (nextPrec plus.p) a ; c = a.c ; p = (plus.p) } ;
  --   CNone => {s = "APPLY" ++ plus.s ++ a.s ; c = CNone ; p = 2}
  --   } ;

  apply : ExpType -> ExpType -> ExpType = \plus,a -> case <plus.c, a.c> of {
    <_,CPrefix|CInfixL>  -- either the operand is not CNone,
      | <CNone,_>          -- or the  operator is CNone
      -- => a ** {s = nonExist} ;
         => {s = "APPLY" ++ plus.s ++ a.s ; c = CNone ; p = 2} ;
    <CInfixL,_> => {s = par plus.p a ++ plus.s ; c = CPrefix ; p = (plus.p) } ;
    -- CPrefix => {s = plus.s ++ par (nextPrec plus.p) a ; c = a.c ; p = (plus.p) } ;
    <CPrefix,_> => {s = plus.s ++ par (nextPrec plus.p) a ; c = a.c ; p = (plus.p) }
    } ;

  ss : Str -> { s : Str } = \s -> { s = s } ;


  -- last one is wrong
  -- EApp (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EApp (EVar VTimes) (EVar (VInt 4)))) (EVar (VInt 5))
  --   l EApp (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EApp (EVar VAdd) (EVar (VInt 4)))) (EVar (VInt 5))
    -- APPLY 3 + 4 + 5

  -- EApp (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EApp (EVar VTimes) (EVar (VInt 4)))) (EVar (VInt 5))

  -- cc -table apply (apply (infixl "+" 0) (constant "3")) (apply (infixl "*" 1) (constant "4"))

  -- apply (apply (apply (infixl "+" 0) (constant "3"))) (apply (apply (infixl "*" 1) (constant "4")))


  -- p "( 3 + 4 ) + 5"
  --   The parser failed at token 7: "5"
  --   3 msec
  --   > p " 3 + ( 4  + 5 )"
  --   The sentence is not complete

  infixl : Str -> Prec -> ExpType = \str,pr -> {s = str ; c = CInfixL ; p = pr} ;

  -- cc apply (infixl "+" 0) (constant "1")
  -- cc apply (apply (infixl "+" 0) (constant "1")) (constant "2")
  -- wrong ast
  -- EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EApp (EApp (EVar VAdd) (EVar (VInt 4))) (EVar (VInt 5)))
  -- apply (apply {s = "+" ; c = CInfixL ; p = 0} {s = "3" ; c = CNone ; p = 2}) (apply (apply {s = "+" ; c = CInfixL ; p = 0} {s = "4" ; c = CNone ; p = 2}) {s = "5" ; c = CNone ; p = 2})

  -- (apply {s = "+" ; c = CInfixL ; p = 0} {s = "3" ; c = CNone ; p = 2})

  -- apply (apply {s = "+" ; c = CInfixL ; p = 0} {s = "3" ; c = CNone ; p = 2}) {s = "4" ; c = CNone ; p = 2}

  -- apply {s = "+" ; c = CInfixL ; p = 0} (apply (apply {s = "+" ; c = CInfixL ; p = 0} {s = "3" ; c = CNone ; p = 2}) {s = "4" ; c = CNone ; p = 2})

  -- apply (apply {s = "+" ; c = CInfixL ; p = 0} (apply (apply {s = "+" ; c = CInfixL ; p = 0} {s = "3" ; c = CNone ; p = 2}) {s = "4" ; c = CNone ; p = 2})) {s = "5" ; c = CNone ; p = 2}

  -- desired ast
  -- EApp (EApp (EVar VAdd) (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EVar (VInt 4)))) (EVar (VInt 5))


  -- how to get from an AST to the form below

lin


  -- p "( 3 + 4 ) + 5"
  --   EApp (EApp (EVar VAdd) (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EVar (VInt 4)))) (EVar (VInt 5))

    -- l (EApp (EVar VAdd) (EApp (EApp (EVar VAdd) (EVar (VInt 3))) (EVar (VInt 4))))
    -- ( 3 + 4 ) +

  -- getting rid of prefix doesn't work

  VAdd = {s = "+" ; c = CInfixL ; p = 0} ;
  VTimes = {s = "*" ; c = CInfixL ; p = 1} ;
  -- VFac = {s = "!" ; c = CPostfix ; p = PSimple} ;
  -- VNeg = {s = "-" ; c = CPrefix ; p = PSimple} ;

  VInt i = {s = i.s ; c = CNone ; p = 2} ;


}
