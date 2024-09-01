
(* Last modified: July 18, 2008 at 21:50 at home in Boulder *)

(* l_Q4rhs.m *)

(* (Q.4) in paper on ``Classification of integrable equations on *)
(* quadgraphs" by V.E. Adler, A.I. Bobenko, Yu. B. Suris *)
(* Comm. Math. Phys. vol. 233, pp. 513-543, 2003 *)

(* lattice[origin_,right_,up_,across_,parameterright_,parameterup_] *)

name = Q4rhs;

lattice[x_,x1_,x2_,x12_,p_,q_]:= 
a0*( x*x1*x2*x12 ) + a1*( x*x1*x2+x1*x2*x12+x*x2*x12+x*x1*x12 ) + 
a2*( x*x12+x1*x2) + a2bar*(x*x1+x2*x12 ) +
a2tilde*( x*x2+x1*x12 ) + a3*( x+x1+x2+x12 ) + a4 == 0 

(* !!! from old Q3 file, not correct for Q4 *)
(* only appropriate to show the format *)
(* If delta = 0 ---> Q3zero case, also in separate file *)
(* 
 If[delta===0, 
 lattice[x_,x1_,x2_,x12_,p_,q_]:= 
 (q^2-p^2)*(x*x12+x1*x2)+q*(p^2-1)*(x*x1+x2*x12)-p*(q^2-1)*(x*x2+x1*x12) == 0
];
*)

(* from old Q3 file, only left here to show the format *)
(* 
If[delta=!=0, 
 lattice[x_,x1_,x2_,x12_,p_,q_]:= 
 (4*p*q)*(
 (q^2-p^2)*(x*x12+x1*x2)+q*(p^2-1)*(x*x1+x2*x12)-p*(q^2-1)*(x*x2+x1*x12))-
 delta^2*(p^2-q^2)*(p^2-1)*(q^2-1) == 0
];
*)

(* from old Q3 file !!! kept for format only *)
(* choice 1 -- do not forget to set byhand to TRUE and detmethod to FALSE *)
(* 
tchoice = 1/Sqrt[delta^2-2*delta^2*p^2+delta^2*p^4+4*p^2*x^2-4*p*x*x1-
          4*p^3*x*x1+4*p^2*x1^2]; 

schoice = 1/Sqrt[delta^2-2*delta^2*q^2+delta^2*q^4+4*q^2*x^2-4*q*x*x2-
          4*q^3*x*x2+4*q^2*x2^2]; 
*)

(* L and M must be given in a format that allows one to test the *)
(* compatibility condition in the form:  L2.M - M1.L = 0 on lattice *)
(* and computation of matrix N so that L2.M - M1.L = (lattice)*N *)

(* from old Q3 file !!! kept for format only *)
(* choice 1 -- do not forget to set LaxPairGiven to TRUE *)
(* and testconsistencyoncube to FALSE, if you want to skip it *)
(* 
candidateL = 1/(Sqrt[delta^2-2*delta^2*p^2+delta^2*p^4+4*p^2*x^2-4*p*x*x1-
          4*p^3*x*x1+4*p^2*x1^2])*
{
{-4*k*p*(p*(k^2-1)*x+(p^2-k^2)*x1),  
-(p^2-1)*(delta^2*k^2-delta^2*k^4-delta^2*p^2+delta^2*k^2*p^2-4*k^2*p*x*x1) },
{-4*k^2*p*(p^2-1),                         4*k*p*(p*(k^2-1)*x1+(p^2-k^2)*x) }
};

candidateM = 1/(Sqrt[delta^2-2*delta^2*q^2+delta^2*q^4+4*q^2*x^2-4*q*x*x2-
          4*q^3*x*x2+4*q^2*x2^2])*
{
{-4*k*q*(q*(k^2-1)*x+(q^2-k^2)*x2),  
-(q^2-1)*(delta^2*k^2-delta^2*k^4-delta^2*q^2+delta^2*k^2*q^2-4*k^2*q*x*x2) },
{-4*k^2*q*(q^2-1),                         4*k*q*(q*(k^2-1)*x2+(q^2-k^2)*x) }
};
*)

(* **************************************************************** *)
(* CONTROL FLAGS *)

(* Computation of t and s with determinant method or by hand, that is *) 
(* t and s are given in this data file. *) 
(* Set either byhand or detmethod to True, the other one must be set False *)
(* Computation will only be carried out if LaxPairGiven is False !!! *)

byhand      = False;
detmethod   = True;

(* True if Lax pair is given in this data file, False otherwise *)

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

(* control over computation of matrix N *)
(* does not work for complicated lattices, in particular, if square roots *)
(* are present *)

(* controlcomputationmatrixN = True; *)
controlcomputationmatrixN = False; 

(* computation of gauge transformation *)

controlgaugetf = False; 
(* controlgaugetf = True; *)

(* l_Q4rhs.m *)

(* ################################################################ *)

