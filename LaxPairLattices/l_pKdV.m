
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_pkdv.m *)

(* Potential KdV equation as given in Project Thesis of Dinh Tran, *)
(* first equation in Section 2.5.3 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = pKdV;

lattice[x_,x1_,x2_,x12_,p_,q_] := (x-x12)*(x1-x2)-p^2+q^2 == 0;

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(*
tchoice = 1; 
schoice = 1; 
*)

(* L and M must be given in the format for testing the compatibility *)
(* condition L2.M - M1.L = 0 on lattice and computation of matrix N *)
(* so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* and testconsistencyoncube to False, if you want to skip it *)
(* 
candidateL = 
  {
  { x,p^2 -k^2-x*x1 },
  { 1,               -x1 }
  };

candidateM = 
  {
  { x ,    q^2-k^2-x*x2 },
  { 1,              -x2 }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or given in the data file.*) 
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation of t and s will only be done if LaxPairGiven is set to False *)

byhand      = False;
detmethod   = True;

(* control for computation of Lax pair: either given in the data file or *)
(* computed with the algorithm (for lattices consistent around the cube) *)

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

(* control for test of compatibility conditions *)
controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control for computation of matrix N *)
controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)
controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_pkdv.m *)

(* ################################################################ *)

