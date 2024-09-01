
(* Last modified: July 18, 2008 at 21:50 at home Boulder *)

(* l_H1.m *)

(* (H.1) on page 9 in paper on ``Classification of integrable equations *)
(* on quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = H1;

(* used for analysis of (alpha-beta)-equation *)
(* action = moebiustransformation; *)
(* action = translation; *)

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
      (x-x12)*(x1-x2)+(q-p) == 0;

(* choice 1 -- do not forget to set byhand to True *)
(*
tchoice = 1; 
schoice = 1; 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = 
  {
  {x,   p-k-x*x1},
  {1,            -x1}
  };

candidateM = 
  {
  {x,     q-k-x*x2},
  {1,          -x2}
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or given t and s in *) 
(* this data file *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* Lax pair is given in this data file or is computed with the algorithm *)

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

controlcomputationmatrixN = True; 
(* controlcomputationmatrixN = False; *)

(* control of computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* H1.m *)

(* ################################################################ *)

