abstract Exp = {

flags startcat = Exp ;

cat
  Exp ;
  Var ;

fun
  EVar : Var -> Exp ;
  EApp : Exp -> Exp -> Exp ;

  VAdd : Var ;
  VTimes : Var ;
  VExp : Var ;
  VFac : Var ;
  VNeg : Var ;

  VInt : Int -> Var ;

}
