
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_gpKdV.m *)

(* General form of potential KdV equation as given in Project Thesis *)
(* of Dinh Tran but no longer assuming that form p^2 - q^2 is known *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = gpKdV;

(* lattice[x_,x1_,x2_,x12_,p_,q_]:= (x-x12)*(x1-x2)-p^2+q^2 == 0; *)

fun[p_,q_] := g[p]-g[q];

lattice[x_,x1_,x2_,x12_,p_,q_]:= (x-x12)*(x1-x2)+fun[p,q] == 0;

(* data for pKdV itself: *)
(* choice 1 -- do not forget to set byhand to TRUE and detmethod to FALSE *)
(*
tchoice = 1; 
schoice = 1; 
*)

(* L and M must be givin in the format for testing the compatibility *)
(* condition L2.M - M1.L = 0 on lattice and computation of matrix N *)
(* so that L2.M - M1.L = (lattice)*N *)

(* data for pKdV itself: *)
(* choice 1 -- do not forget to set LaxPairGiven to TRUE *)
(* and testconsistencyoncube to FALSE, if you want to skip it *)
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

(* Computation of t and s with determinant method or by hand, in that case *) 
(* t and s must be given in this data file. *)
(* Set either byhand or detmethod to true, the other one must be set false *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

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

(* control over testing the compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_gpKdV.m *)

(* ################################################################ *)

