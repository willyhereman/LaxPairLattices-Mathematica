
(* Last modified: July 21, 2008 at 19:50 at home in Boulder *)

(* l_A1eps1.m *)

(* (A.1) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = A1;

(* if delta = 0 then equation has no rhs, called A1zero *)
(* if delta =!= 0 then equation has rhs, called A1rhs or A1 *)

(* delta = 1; *)

lattice[x_,x1_,x2_,x12_,p_,q_]:= p*(x+x2)*(x1+x12)-q*(x+x1)*(x2+x12)-
                                 delta^2*p*q*(p-q) == 0

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* DOES NOT WORK *)
(* 
tchoice = 1/(delta*p+(x+x1)); 
schoice = 1/(delta*q+(x+x2)); 
*)

(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* DOES NOT WORK *)
(* 
tchoice = 1/(delta*p-(x+x1)); 
schoice = 1/(delta*q-(x+x2)); 
*)

(* choice 3 -- do not forget to set byhand to True and detmethod to False *)
(* WORKS *)
(*  *)
tchoice = eps1/Sqrt[(delta*p+(x+x1))*(delta*p-(x+x1))]; 
schoice = eps2/Sqrt[(delta*q+(x+x2))*(delta*q-(x+x2))]; 
(* *)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* DOES NOT WORK *)
(*
candidateL = 1/(delta*p+(x+x1))*
  {
  {(k-p)*x1+k*x,   -p*(delta^2*k*(k-p)+x*x1) },
  {p,                       -((k-p)*x+k*x1) }
  };

candidateM = 1/(delta*q+(x+x2))*
  {
  {(k-q)*x2+k*x,   -q*(delta^2*k*(k-q)+x*x2) },
  {q,                       -((k-q)*x+k*x2) }
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* DOES NOT WORK *)
(* 
candidateL = 1/(delta*p-(x+x1))*
  {
  {(k-p)*x1+k*x,   -p*(delta^2*k*(k-p)+x*x1) },
  {p,                       -((k-p)*x+k*x1) }
  };

candidateM = 1/(delta*q-(x+x2))*
  {
  {(k-q)*x2+k*x,   -q*(delta^2*k*(k-q)+x*x2) },
  {q,                       -((k-q)*x+k*x2) }
  };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* WORKS *)
(* 
candidateL = 1/Sqrt[(delta*p+(x+x1))*(delta*p-(x+x1))]*
  {
  {(k-p)*x1+k*x,   -p*(delta^2*k*(k-p)+x*x1) },
  {p,                       -((k-p)*x+k*x1) }
  };

candidateM = 1/Sqrt[(delta*q+(x+x2))*(delta*q-(x+x2))]*
  {
  {(k-q)*x2+k*x,   -q*(delta^2*k*(k-q)+x*x2) },
  {q,                       -((k-q)*x+k*x2) }
  };
*)

(* choice 4 -- do not forget to set LaxPairGiven to True *)
(* close to choice 1 but different signs *)
(* DOES NOT WORK *)
(* 
AAA = -1; (* for flip of k-p to match result of Q1 *)
BBB = -1; (* for flip of x1 and x2 *)
candidateL = 1/(delta*p+(x+x1))*
  {
  {AAA*(k-p)*BBB*x1+k*x,   -p*(AAA*delta^2*k*(k-p)+x*BBB*x1) },
  {p,                       -(AAA*(k-p)*x+k*BBB*x1) }
  };

candidateM = 1/(delta*q+(x+x2))*
  {
  {AAA*(k-q)*BBB*x2+k*x,   -q*(AAA*delta^2*k*(k-q)+x*BBB*x2) },
  {q,                       -(AAA*(k-q)*x+k*BBB*x2) }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, in which case *) 
(* the t and s need to be given in this data file *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = True;
detmethod   = False;

(* True if Lax pair is given in this data file, False otherwise *)

LaxPairGiven = False; 
(* LaxPairGiven = True; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* they should have opposite Boolean values *)

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* control over the test of the compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)
(* Code needs more work for complicated lattices, *)
(* in particular, if square roots are present *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_A1eps1.m *)

(* ################################################################ *)

