
(* Last modified: July 18, 2008 at 21:45 at home in Boulder *)

(* l_H2.m *)

(* (H.2) on page 9 in paper on ``Classification of integrable equations *)
(* on quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = H2;

(* used for analysis of (alpha-beta)-equation *)
(* action = moebiustransformation; *)
(* action = translation; *)

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
      (x-x12)*(x1-x2)+(q-p)*(x+x1+x2+x12)+(q^2-p^2) == 0;

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/(Sqrt[p+x+x1]); 
schoice = 1/(Sqrt[q+x+x2]); 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = (1/Sqrt[p+x+x1])*
  {
  {p-k+x,   p^2-k^2+(p-k)*(x+x1)-x*x1},
  {1,                       -(p-k+x1)}
  };

candidateM = (1/Sqrt[q+x+x2])*
  {
  {q-k+x,   q^2-k^2+(q-k)*(x+x2)-x*x2},
  {1,                       -(q-k+x2)}
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand in this *) 
(* data ile *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* Lax pair given in this data file or not *)

LaxPairGiven = False;
(* LaxPairGiven = True; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* they should have opposite Boolean values *)

(* controlpowerexpand    = False; *)
controlpowerexpand = True; 

(* controlrootrules    = False; *)
controlrootrules = True; 

(* control over test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)
(* does not work for complicated lattices, needs more work *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_H2.m *)

(* ################################################################ *)

