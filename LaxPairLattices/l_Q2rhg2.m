
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_Q2rhg2.m *)

(* translated version of equation Q2rhs in file Q2rhsg.m *)
(* Same as Q2 but introducing coefficient delta and delta^2 *)
(* in last two terms *)

(* (Q.2) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = Q2rhg2;

(* delta = 1 is Q2 itself *)

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
  p*(x-x2)*(x1-x12)-q*(x-x1)*(x2-x12)+delta*p*q*(p-q)*(x+x1+x2+x12)+
  p*q*(p-q)*(epsilon^2-delta^2*(p^2-p*q+q^2))== 0;

(* data for Q2 equation itself: *)
(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/Sqrt[p^4-2*p^2*x+x^2-2*p^2*x1-2*x*x1+x1^2]; 
schoice = 1/Sqrt[q^4-2*q^2*x+x^2-2*q^2*x2-2*x*x2+x2^2]; 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* data for Q2 equation itself: *)
(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = (1/Sqrt[p^4-2*p^2*x+x^2-2*p^2*x1-2*x*x1+x1^2])*
  {
  {(k-p)*(k*p-x1)+k*x,   -p*(k*(k-p)*(k^2-k*p+p^2-x-x1)+x*x1) },
  {p,                                   -((k-p)*(k*p-x)+k*x1) }
  };

candidateM = (1/Sqrt[q^4-2*q^2*x+x^2-2*q^2*x2-2*x*x2+x2^2])*
  {
  {(k-q)*(k*q-x2)+k*x,   -q*(k*(k-q)*(k^2-k*q+q^2-x-x2)+x*x2) },
  {q,                                   -((k-q)*(k*q-x)+k*x2) }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, that is *) 
(* giving a t and s in this data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* Giving the Lax pair in the data file or not *)

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

controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_Q2rhg2.m *)

(* ################################################################ *)

