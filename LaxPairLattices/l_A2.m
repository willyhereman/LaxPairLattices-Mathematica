
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_A2.m *)

(* (A.2) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = A2;

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
(q^2-p^2)*(x*x1*x2*x12+1)+q*(p^2-1)(x*x2+x1*x12)-p*(q^2-1)*(x*x1+x2*x12) == 0

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* DID NOT WORK !!!! *)
(* 
tchoice = 1/(A*(p-x*x1)); 
schoice = 1/(B*(q-x*x2)); 
*)

(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* DID NOT WORK !!!! *)
(* 
tchoice = 1/(p*x*x1-1); 
schoice = 1/(q*x*x2-1); 
*)

(* choice 3 -- do not forget to set byhand to True and detmethod to False *)
(* DID WORK !!!! *)
(*  
tchoice = 1/Sqrt[(p-x*x1)*(p*x*x1-1)]; 
schoice = 1/Sqrt[(q-x*x2)*(q*x*x2-1)]; 
*)

(* choice 4 -- do not forget to set byhand to True and detmethod to False *)
(* DID NOT WORK !!!! *)
(* 
tchoice = 1/((p-x*x1)*(p*x*x1-1)); 
schoice = 1/((q-x*x2)*(q*x*x2-1)); 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = (1/(p-x*x1))*
  {
  {k*(p^2-1)*x,                   -(p^2-k^2+p*(k^2-1)*x*x1) },
  {p*(k^2-1)+(p^2-k^2)*x*x1,      -k*(p^2-1)*x1 }
  };

candidateM = (1/(q-x*x2))*
  {
  {k*(q^2-1)*x,                   -(q^2-k^2+q*(k^2-1)*x*x2) },
  {q*(k^2-1)+(q^2-k^2)*x*x2,      -k*(q^2-1)*x2 }
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = (1/(p*x*x1-1))*
  {
  {k*(p^2-1)*x,                   -(p^2-k^2+p*(k^2-1)*x*x1) },
  {p*(k^2-1)+(p^2-k^2)*x*x1,      -k*(p^2-1)*x1 }
  };

candidateM = (1/(q*x*x2-1))*
  {
  {k*(q^2-1)*x,                   -(q^2-k^2+q*(k^2-1)*x*x2) },
  {q*(k^2-1)+(q^2-k^2)*x*x2,      -k*(q^2-1)*x2 }
  };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = (1/Sqrt[(p-x*x1)*(p*x*x1-1)])*
  {
  {k*(p^2-1)*x,                   -(p^2-k^2+p*(k^2-1)*x*x1) },
  {p*(k^2-1)+(p^2-k^2)*x*x1,      -k*(p^2-1)*x1 }
  };

candidateM = (1/Sqrt[(q-x*x2)*(q*x*x2-1)])*
  {
  {k*(q^2-1)*x,                   -(q^2-k^2+q*(k^2-1)*x*x2) },
  {q*(k^2-1)+(q^2-k^2)*x*x2,      -k*(q^2-1)*x2 }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, in that case *) 
(* t and s must be given in this data file *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

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

(* control of test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control of computation of matrix N *)
(* Does not work for complicated lattices, in particular, if square roots *)
(* are present. Code needs more work *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_A2.m *)

(* ################################################################ *)
