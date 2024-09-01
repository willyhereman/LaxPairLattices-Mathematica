
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_abeq.m *)

(* (alpha-beta)-equation in Project Thesis of Dinh Tran: equation (4.13) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = abeq;

(* used in analysis of (alpha-beta)-equation *)
(* action = moebiustransformation; *)
(* action = translation; *)

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
((p-alpha)*x-(p+beta)*x1)*((p-beta)*x2-(p+alpha)*x12)-( 
((q-alpha)*x-(q+beta)*x2)*((q-beta)*x1-(q+alpha)*x12) ) == 0;

(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/((alpha-p)*x+(beta+p)*x1); 
schoice = 1/((alpha-q)*x+(beta+q)*x2); 
*)

(* choice 2 -- do not forget to set byhand to True and detmethod to False *)
(* 
tchoice = 1/((beta-p)*x+(alpha+p)*x1); 
schoice = 1/((beta-q)*x+(alpha+q)*x2); 
*)

(* L and M must be given in a form that is suitable for the *)
(* compatibility equation:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/((alpha-p)*x+(beta+p)*x1)*
 { 
 { (p-alpha)*(p-beta)*x+(k^2-p^2)*x1, -(k-alpha)*(k-beta)*x*x1 },
 { (k+alpha)*(k+beta),                -((p+alpha)*(p+beta)*x1+(k^2-p^2)*x) }
 };

candidateM = 1/((alpha-q)*x+(beta+q)*x2)*
 { 
 { (q-alpha)*(q-beta)*x+(k^2-q^2)*x2, -(k-alpha)*(k-beta)*x*x2 },
 { (k+alpha)*(k+beta),                -((q+alpha)*(q+beta)*x2+(k^2-q^2)*x) }
 };
*)

(* choice 2 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/((beta-p)*x+(alpha+p)*x1)*
 { 
 { (p-alpha)*(p-beta)*x+(k^2-p^2)*x1, -(k-alpha)*(k-beta)*x*x1 },
 { (k+alpha)*(k+beta),                -((p+alpha)*(p+beta)*x1+(k^2-p^2)*x) }
 };

candidateM = 1/((beta-q)*x+(alpha+q)*x2)*
 { 
 { (q-alpha)*(q-beta)*x+(k^2-q^2)*x2, -(k-alpha)*(k-beta)*x*x2 },
 { (k+alpha)*(k+beta),                -((q+alpha)*(q+beta)*x2+(k^2-q^2)*x) }
 };
*)

(* choice 3 -- do not forget to set LaxPairGiven to True *)
(* 
candidateL = 1/Sqrt[((beta-p)*x+(alpha+p)*x1)*((alpha-p)*x+(beta+p)*x1)]*
 { 
 { (p-alpha)*(p-beta)*x+(k^2-p^2)*x1, -(k-alpha)*(k-beta)*x*x1 },
 { (k+alpha)*(k+beta),                -((p+alpha)*(p+beta)*x1+(k^2-p^2)*x) }
 };

candidateM = 1/Sqrt[((beta-q)*x+(alpha+q)*x2)*((alpha-q)*x+(beta+q)*x2)]*
 { 
 { (q-alpha)*(q-beta)*x+(k^2-q^2)*x2, -(k-alpha)*(k-beta)*x*x2 },
 { (k+alpha)*(k+beta),                -((q+alpha)*(q+beta)*x2+(k^2-q^2)*x) }
 };
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, in which *) 
(* case t and s must be given in this data file *)
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

controlpowerexpand    = False;
(* controlpowerexpand = True; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* control for test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control for computation of matrix N *)
(* does not work for complicated lattices, needs more work *)

controlcomputationmatrixN = False;
(* controlcomputationmatrixN = True; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_abeq.m *)

(* ################################################################ *)

