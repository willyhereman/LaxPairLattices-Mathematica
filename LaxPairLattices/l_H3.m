
(* Last modified: July 18, 2008 at 21:45 at home in Boulder *)

(* l_H3.m *)

(* (H.3) on page 9 in paper on ``Classification of integrable equations *)
(* on quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = H3;

(* if delta = 0   for the equation without rhs ---> H3zero *)
(* if delta =!= 0 for the equation with rhs    ---> H3rhs  *)
(* delta = 0; *)

delta = 1;

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
       p*(x*x1+x2*x12)-q*(x*x2+x1*x12)+delta*(p^2-q^2) == 0;

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(*
tchoice = 1/Sqrt[delta*p+x*x1]; 
schoice = 1/Sqrt[delta*q+x*x2]; 
*)

(* L and M must be given in a form that is suitable for the *)
(* compatibility equation:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/(Sqrt[delta*p+x*x1])*
  { 
  { k*x,       -delta*(p^2-k^2)-p*x*x1 },
  { p,                           -k*x1 }
  };

candidateM = 1/(Sqrt[delta*q+x*x2])*
  {
  { k*x, -delta*(q^2-k^2)-q*x*x2 },
  { q,                     -k*x2 }
  };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/(Sqrt[p^2-k^2]*Sqrt[delta*p+x*x1])*
  {
  { k*x,       -delta*(p^2-k^2)-p*x*x1 },
  { p,                           -k*x1 }
  };
candidateM = 1/(Sqrt[q^2-k^2]*Sqrt[delta*q+x*x2])*
  {
  { k*x, -delta*(q^2-k^2)-q*x*x2 },
  { q,     -k*x2 }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand in this *) 
(* data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* Lax pair is given in this data file or not *)

LaxPairGiven = False;
(* LaxPairGiven = True; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* They should have opposite Boolean values *)

(* controlpowerexpand    = True; *)
controlpowerexpand = False; 

(* controlrootrules    = True; *)
controlrootrules = False; 

(* control over test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over computation of matrix N *)
(* does not work for complicated lattices, needs furhter work *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_H3.m *)

(* ################################################################ *)

