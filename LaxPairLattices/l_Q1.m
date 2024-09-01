
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_Q1.m *)

(* (Q.1) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = Q1;

(* if delta = 0 then equation has no rhs, called Q1zero *)
(* if delta =!= 0 then equation has rhs, called Q1rhs *)
(* delta = 0; *)

(* delta = 1; *)

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
   p*(x-x2)*(x1-x12)-q*(x-x1)*(x2-x12)+delta^2*p*q*(p-q) == 0

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/(delta*p+(x-x1)); 
schoice = 1/(delta*q+(x-x2)); 
*)

(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/(delta*p-(x-x1)); 
schoice = 1/(delta*q-(x-x2)); 
*)

(* choice 3 -- do not forget to set byhand to True and detmethod to False *)
(* *)
tchoice = 1/Sqrt[(delta*p+(x-x1))*(delta*p-(x-x1))]; 
schoice = 1/Sqrt[(delta*q+(x-x2))*(delta*q-(x-x2))]; 
(* *)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = 1/(delta*p+(x-x1))*
  {
  {(p-k)*x1+k*x,   -p*(delta^2*k*(p-k)+x*x1) },
  {p,                       -((p-k)*x+k*x1) }
  };

candidateM = 1/(delta*q+(x-x2))*
  {
  {(q-k)*x2+k*x,   -q*(delta^2*k*(q-k)+x*x2) },
  {q,                       -((q-k)*x+k*x2) }
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/(delta*p-(x-x1))*
  {
  {(p-k)*x1+k*x,   -p*(delta^2*k*(p-k)+x*x1) },
  {p,                       -((p-k)*x+k*x1) }
  };

candidateM = 1/(delta*q-(x-x2))*
  {
  {(q-k)*x2+k*x,   -q*(delta^2*k*(q-k)+x*x2) },
  {q,                       -((q-k)*x+k*x2) }
  };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/Sqrt[(delta*p+(x-x1))*(delta*p-(x-x1))]*
  {
  {(p-k)*x1+k*x,   -p*(delta^2*k*(p-k)+x*x1) },
  {p,                       -((p-k)*x+k*x1) }
  };

candidateM = 1/Sqrt[(delta*q+(x-x2))*(delta*q-(x-x2))]*
  {
  {(q-k)*x2+k*x,   -q*(delta^2*k*(q-k)+x*x2) },
  {q,                       -((q-k)*x+k*x2) }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, that is *) 
(* giving a choice of tcondition and scondition in the code itself *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = True;
detmethod   = False;

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

(* test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_Q1.m *)

(* ################################################################ *)

