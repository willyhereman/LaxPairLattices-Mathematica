
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_gcpKdV.m *)

(* General form of potential KdV equation with linear terms: *)
(* equation (1) in preprint INS #166 of Nijhoff, Papageorgiou, and Capel.*)
(* Simpler cases are in Project Thesis of Dinh Tran *)
(* but no longer assuming the forms p-q, p+q, and p^2 - q^2 (rhs) *)

(* Form of potential KdV equation with linear terms and arbitrary functions *)
(* See Project Thesis of Dinh Tran: equation (4.1) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = gcpKdV;

(* lattice[x_,x1_,x2_,x12_,p_,q_]:= (p-q+x2-x1)*(p+q-x12+x)-p^2+q^2 == 0; *)

(* in this case we do not assume these functional forms: *)
(* fun1[p_,q_] := p-q; *)
(* fun2[p_,q_] := p+q; *)
(* fun3[p_,q_] := -p*2+q^2; *)

(* for first run start with arbitrary fun1[p,q], etc. *)
(* and conclude from the results that one needs to following *)
(* separated functional forms *)

fun1[pp_,qq_] := f[pp]-f[qq];
fun2[pp_,qq_] := f[pp]+f[qq];
fun3[pp_,qq_] := g[pp]-g[qq];

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
       (fun1[p,q]+x2-x1)*(fun2[p,q]-x12+x)+fun3[p,q] == 0; 

(* data for pkdv itself: *)
(* choice 1 -- do not forget to set byhand to True and detmethod to False *)
(*
tchoice = 1; 
schoice = 1; 
*)

(* L and M must be givin in the format for testing the compatibility *)
(* condition L2.M - M1.L = 0 on lattice and computation of matrix N *)
(* so that L2.M - M1.L = (lattice)*N *)

(* data for pkdv itself: *)
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

(* Computation of t and s with determinant method or by hand, in which case *) 
(* t and s need to be given in this data file. *)
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* Set to True if Lax pair is given in this data file, False otherwise *)

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

(* control over test of compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* control over the computation of matrix N *)
(* works OK for simple lattices, does not work well for complicated lattices,*)
(* in particular, when square roots are present *)

controlcomputationmatrixN = True;
(* controlcomputationmatrixN = False; *)

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_gcpKdV.m *)

(* ################################################################ *)

