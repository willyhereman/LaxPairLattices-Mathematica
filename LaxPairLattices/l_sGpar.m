
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_sGpar.m *)

(* sine-Gordon lattice *)
(* Equation (2) in paper by Quispel, Capel, Papageorgiou, and Nijhoff *)
(* Physica A, vol. 173, pp. 243-266 (1991) *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = sGpar;

(* if term = 1 then sG as in equation (2) in paper Quispel et al. 1991 *)
(* if term = 0 then sG without constant term *)

(* term = 0; *)
(* sine-Gordon lattice in paper Quispel et al in Physica A, 1991 *)
(* lattice[x_,x1_,x2_,x12_,p_,q_] := 
        x*x1*x2*x12-p*q*(x*x12-x1*x2)-1*term == 0;  *)

(* More general sine-Gordon lattice in Project Thesis Tran: *)
(* equation (2.10) on page 13 *)

fun1[p_,q_] := 1;     
fun2[p_,q_] := p*q; 
fun3[p_,q_] := 1;    

lattice[x_,x1_,x2_,x12_,p_,q_] := 
   fun1[p,q]*(x*x1*x2*x12)-fun2[p,q]*(x*x12-x1*x2)-fun3[p,q] == 0;

(* SSS, TTT, UUU and VVV are parameters *)
(* choice 1 -- do not forget to set LaxPairGiven to True *)
(* *)
candidateL = 
  {
  {SSS,              -TTT*x1 },
  {-TTT*(1/x),    SSS*(x1/x) }
  };

candidateM = 
  {
  {UUU*(x2/x),    -VVV*(1/x) },
  {-VVV*x2,              UUU }
  };
(* *)

(* **************************************************************** *)

(* Control of either byhand if t and s are given in this data file. *)
(* Set detmethod to True if you want to compute the Lax pair with *)
(* the algorithm using the determinant method *)

byhand = False;
detmethod = True;

(* True if Lax pair is given in this data file, False otherwise *)

LaxPairGiven = True;
(* LaxPairGiven = False; *)

(* Testing the consistency around the cube or not *)

(* testconsistencyoncube = False; *)
testconsistencyoncube = True; 

(* ****************************************************************** *)

(* control of powerexpand or use of rootrules, True or False *)

controlpowerexpand    = False;
(* controlpowerexpand = False; *)

controlrootrules    = True;
(* controlrootrules = False; *)

(* control of testing of the compatibility conditions *)

controlcompatibilitytesting = True; 
(* controlcompatibitytesting = False; *)

(* Control for computation of matrix N with flag controlcomputationmatrixN *)
(* set to True or False *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* control of computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_sGpar.m *)

(* ################################################################ *)

