
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_mkdv.m *)

(* modified KdV lattice *)
(* Equation (A.2) in paper by Quispel, Capel, Papageorgiou, and Nijhoff *)
(* Physica A, vol. 173, pp. 243-266 (1991) *)
(* Special case of equation (2.13) in Project Thesis of Dinh Tran *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = mKdV;

(* for mKdV equation *)
fun1[p_,q_] :=  p;  
fun2[p_,q_] := -q;    
fun3[p_,q_] := -q;     

(* mKdV lattice *)

lattice[x_,x1_,x2_,x12_,p_,q_] := 
   fun1[p,q]*(x*x2-x1*x12)+fun2[p,q]*x*x1-fun3[p,q]*x2*x12 == 0;

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* *)
tchoice = 1/x; 
schoice = 1/x; 
(* *)

(* choice 2 -- do not forget to set byhand to True *)
(* 
tchoice = 1/x1; 
schoice = 1/x2; 
*)

(* choice 3 -- do not forget to set byhand to True *)
(* 
tchoice = 1/Sqrt[x*x1]; 
schoice = 1/Sqrt[x*x2]; 
*)

(* choice 4 -- do not forget to set byhand to True *)
(* 
tchoice = 1/Sqrt[(p^2-k^2)*(x*x1)]; 
schoice = 1/Sqrt[(q^2-k^2)*(x*x2)]; 
*)

(* L and M must be given in a form that is suitable for the *)
(* compatibility equation:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/(x)*
  {
  { -p*x,       k*x*x1 },
  { k,           -p*x1 }
  };

candidateM = 1/(x)*
  {
  { -q*x, k*x*x2 },
  { k,     -q*x2 }
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/(x1)*
  {
  { -p*x,       k*x*x1 },
  { k,           -p*x1 }
  };

candidateM = 1/(x2)*
  {
  { -q*x, k*x*x2 },
  { k,     -q*x2 }
  };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(*
candidateL = 1/(Sqrt[(p^2-k^2)]*Sqrt[x*x1])*
  {
  { -p*x,       k*x*x1 },
  { k,           -p*x1 }
  };

candidateM = 1/(Sqrt[(q^2-k^2)]*Sqrt[x*x2])*
  {
  { -q*x, k*x*x2 },
  { k,     -q*x2 }
  };
*)

(* choice 4 -- from paper Quispel et al. Physica A (1991) *)
(* do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/((k-p)*x)*
  {
  { -p*x,       x*x1 },
  { k^2,       -p*x1 }
  };

candidateM = 1/((k-q)*x)*
  {
  { -q*x,  x*x2 },
  { k^2,   -q*x2 }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, *) 
(* in the latter case the t and s must be given in the data file. *)
(* Set either byhand or detmethod to True, the other one must be set False. *)
(* Computation of t and s will only be carried out if LaxPairGiven is False. *)

byhand      = True;
detmethod   = False;

(* control for computation of Lax pair: either given in the data file or *)
(* computed with the algorithm (for lattices consistent around the cube) *)

LaxPairGiven = False; 
(* LaxPairGiven = True; *)

(* applying or skipping the test for consistency around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots. *)
(* They should have opposite Boolean values *)

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* control for testing of the compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control for computing the matrix N *)

controlcomputationmatrixN = True; 
(* controlcomputationmatrixN = False; *)

(* control for computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_mkdv.m *)

(* ################################################################ *)

