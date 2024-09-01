
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_cmKdV.m *)

(* modified KdV lattice *)
(* Equation (A.2) in paper by Quispel, Capel, Papageorgiou, and Nijhoff *)
(* Physica A, vol. 173, pp. 243-266 (1991) *)
(* Special case of equation (2.13) in Project Thesis of Dinh Tran *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = cmKdV;

(* for traditional mKdV equation *)
(* 
fun1[p_,q_] :=  p  
fun2[p_,q_] := -q    
fun3[p_,q_] := -q     
*) 

(* generalization of mKdV lattice *)
lattice[x_,x1_,x2_,x12_,p_,q_] := 
   fun1[p,q]*(x*x2-x1*x12)+fun2[p,q]*x*x1-fun3[p,q]*x2*x12 == 0;

(* data for mkdv itself: *)
(* choice 1 -- do not forget to set byhand to True *)
(* tchoice = 1; *)
(* schoice = 1; *)

(* L and M must be in the form for using the compatibility equation in *)
(* the form L2.M - M1.L = (lattice)*N *)

(* data for mKdV itself: *)
(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 
  {
  {x,  p^2-k^2-x*x1 },
  {1,           -x1 }
  };
*)

(*
candidateM = 
  {
  {x,      q^2-k^2-x*x2 },
  {1,               -x2 }
  };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, in which *) 
(* case t and s must be given in this data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* True or False depending on whether or not the Lax pair is given in this *)
(* data file *)

LaxPairGiven = False;
(* LaxPairGiven = True; *)

(* applying or skipping the consistency test around the cube *)

testconsistencyoncube = True; 
(* testconsistencyoncube = False; *)

(* ****************************************************************** *)

(* use of powerexpand and simplification rules for roots *)
(* They should have opposite Boolean values *)

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over computation of matrix N *)
controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_cmKdV.m *)

(* ################################################################ *)

