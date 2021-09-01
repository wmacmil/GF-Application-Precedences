concrete ExpFormal of Exp = {

param
  Connectivity = CInfixL | CPrefix | CPostfix | CNone ;
  Bool = True | False ;

oper

  ExpType = {s : Str ; c : Connectivity ; p : Prec} ;

  par : Prec -> ExpType -> Str =
    \p,exp -> case lessPrec exp.p p of {
      True => parenth exp.s ;
      False => parenthOpt exp.s
      -- False => exp.s
    } ;

  parenth : Str -> Str = \s -> "(" ++ s ++ ")" ;
  parenthOpt : Str -> Str = \s -> variants {s ; "(" ++ s ++ ")"} ;


  mkPrec : Connectivity -> Prec -> Str -> ExpType = \c,p,s ->
    {s = s ; p = p ; c = c} ;

  constant : Str -> ExpType = \s -> {s = s ; p = maxPrec ; c = CNone } ;

  maxPrec = 5 ;

  Prec : PType = Predef.Ints 5 ;

  lessPrec : Prec -> Prec -> Bool = \p,q ->
    case <<p,q> : Prec * Prec> of {
      <3,5> | <2,5> | <4,5> => True ;
      <3,4> | <2,3> | <2,4> => True ;
      <1,1> | <1,0> | <0,0> => False ;
      <1,_> | <0,_>         => True ;
      _ => False
    } ;

  nextPrec : Prec -> Prec = \p -> case <p : Prec> of {
    5 => 5 ;
    n => Predef.plus n 1
    } ;

lincat
  Exp = ExpType ;
  Var = ExpType ;

lin
  EVar v = v ;

  -- how to distinguish infixl vs infixr
  EApp f a = apply f a ;

oper

  apply : ExpType -> ExpType -> ExpType = \plus,a -> case <plus.c, a.c> of {
    <_,CPrefix|CInfixL>  -- either the operand is not CNone,
      | <CNone,_>          -- or the  operator is CNone
      -- => a ** {s = nonExist} ;
         => {s = "APPLY" ++ plus.s ++ a.s ; c = CNone ; p = 2} ;
    <CInfixL,_> => {s = par plus.p a ++ plus.s ; c = CPrefix ; p = plus.p } ;
    <CPrefix,_> => {s = plus.s ++ par (nextPrec plus.p) a ; c = a.c ; p = plus.p } ;
    <CPostfix,_> => {s = par (nextPrec plus.p) a ++ plus.s ; c = a.c ; p = plus.p }
    } ;

  ss : Str -> { s : Str } = \s -> { s = s } ;

  infixl : Str -> Prec -> ExpType = \str,pr -> {s = str ; c = CInfixL ; p = pr} ;

lin

  -- However, when using operator notation with a caret (^) or arrow (↑), there is no common standard.[19] For example, Microsoft Excel and computation programming language MATLAB evaluate a^b^c as (ab)c, but Google Search and Wolfram Alpha as a(bc). Thus 4^3^2 is evaluated to 4,096 in the first case and to 262,144 in the second case.
  -- additionally - can be interpreted differently depending on the programming language

  VAdd = {s = "+" ; c = CInfixL ; p = 0} ;
  VTimes = {s = "*" ; c = CInfixL ; p = 1} ;
  VExp = {s = "^" ; c = CInfixL ; p = 2} ;
  VNeg = {s = "-" ; c = CPrefix ; p = 3} ;
  VFac = {s = "!" ; c = CPostfix ; p = 4} ;

  VInt i = {s = i.s ; c = CNone ; p = 5} ;

}
