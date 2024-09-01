
(* Last updated: Tuesday, July 22, 2008 at 19:05 at home in Boulder *)

(* Cleaned-up version with menu system *)
(* File l0710-08.m based on l0703-08.m, *)
(* in turn based on l0628-08.m, l0617-08.m, and l1126-07.m *)

(* July 1, 2008: added numeric tests for verification of L and M *)
(* July 6, 2008: added numeric tests for t and s *)
(* July 9, 2008: took pieces slanted for REMOVAL out *)
(* July 10-12, 2008: added menu system *)
(* July 18, 2008: large range for random numbers because for the H2 equation *)
(*                division by zero occurs *)
(* July 21, 2008: for numerical tests, I introduced symbols eps1*t, eps2*s *)
(* in both the given t and s (byhand) and the computed t and s (detmethod) *)
(* ...then numerical tests do not fail *)
(* Needs further testing --> did not work either *)
(* July 21, 2008: added a absolute value test where the areguments of *)
(* the square roots are replace by absolute values *)
(* July 22, 2008: added a numerical/decimal test taken from PDEV2.m *)

(* Applying Moebius transformation to equation involving x13, etc *)
(* Under development for dealing better with Q3 and Q4 equations !!! *)

(* In code, fraction means the fraction of (t2*s)/(t*s1) *)
(* DO LATER: final test of L, M and ALSO N *)

(* November 24, 2007: *)
(* Use f[x,x1,x2,x12,p,q] (better order) instead of f[x,x1,x12,x2,p,q] *)
(* November 23, 2007: moved control flags into the data files *)
(* Potential of giving L and M and unravelling the compatibility conditions *)

(* TO DO: test factorization of the element of N by unvevaluatedlattice *)
(* TO DO: computation of N: investigate what happens when tcondition *)
(* and scondition are given by hand, because they are evaluated on lattice *)
(* Check how N changes for different choices of t and s *)

(* Controls must be set in data file as of version l1120-07.m *)

(* Control of either byhand (giving tcondition and scondition) with *)
(* byhand set to True or computing it with detmethod with detmethod by *)
(* setting detmethod to True *)
(* Control of powerexpand with controlpowerexpand *)
(* Control my simplification rules for roots with controlrootrules *)

(* Control for computation of matrix N with flag controlcomputationmatrixN *)
(* set to True or False *)

(* Control if testing the compatibility conditions *)
(* set to True or False *)

(* Control of applying the gauge transformation set to True or False *)

(* Has strategy for computing t and s via the determinants *)
(* Possible use of PowerExpand and my rootrules in various places *)

(* Typical data files:                                                 *)
(* l_pkdv.m, l_abeq.m, l_Q3zero, l_Q3rhs, l_sg, l_mkdv, etc.           *)
(* potential KdV (pKdV), (alpha,beta)-lattice, Q3 zero with rhs zero,  *)
(* Q3 lattice with rhs non-zero, sine-Gordon, mKdV lattices, etc.      *)

(*****************************************************************************)
(*                                                                           *)
(*          *** M A T H E M A T I C A   P R O G R A M ***                    *)
(*                                                                           *)
(*      SYMBOLIC COMPUTATION OF LAX PAIRS OF 2-DIMENSIONAL NONLINEAR         *)
(*                       PARTIAL DIFFERENCE EQUATIONS                        *)
(*                                                                           *)
(* program name: laxpair.m                                                   *)
(*                                                                           *)
(* purpose: symbolic computation of Lax pair for single nonlinear partial    *)
(*          difference equations in 2 dimensions (on quad-graphs)            *)
(*                                                                           *)
(* input to laxpair.m : single nonlinear partial difference equation in 2D   *)
(*                      parameters should be p, q, alpha, beta, ...          *)
(*                      Q(x,x1,x2,x12,p,q,alpha,beta,...) = 0                *)
(*                                                                           *)
(* output : Lax pair L and M (if it exists), and matrix N                    *)
(*          L2.M - M1.L = (lattice) N                                        *)
(*          L2.M - M1.L = 0 (if evaluated on the lattice)                    *)
(*                                                                           *)
(* tested on : various PCs with Mathematica 5.0 and 5.1                      *)
(*                                                                           *)
(* language : Mathematica 5.0 (not tested for version 6.0 yet)               *)
(*                                                                           *)
(* author:  Willy Hereman                                                    *)
(*          Department of Mathematical and Computer Sciences                 *)
(*          Colorado School of Mines                                         *)
(*          Golden, CO 80401-1887, U.S.A.                                    *)
(*                                                                           *)
(* colaborators: Reinout Quispel and Peter van der Kamp                      *)
(*               Department of Mathematics and Statistics                    *)
(*               La Trobe Universtiy, Bundoora (Melbourne) Australia         *)
(*                                                                           *)
(* Version 1: July 22, 2008                                                  *)
(*                                                                           *)
(* Copyright 2008                                                            *)
(*                                                                           *)
(*****************************************************************************)

(* ******************************************************************** *)
Clear["Global`*"];

(* ************************** debug flags ***************************** *)

numericTesttands = True;
debugNumericTesttands = False;

decimalNumericTesttands = False;
debugDecimalNumericTesttands = False;

(* ------------------------------------------------------------------------ *)

numericTestLaxPairGiven = True;
debugNumericTestLaxPairGiven = False;

decimalNumerTestLaxPairGiven = False;
debugDecimalNumericTestLaxPairGiven = False;

(* ------------------------------------------------------------------------ *)

numericTestLaxPairComputed = True;
debugNumericTestLaxPairComputed = False;

decimalNumerTestLaxPairComputed = False;
debugDecimalNumericTestLaxPairComputed = False;

(* ------------------------------------------------------------------------ *)

debugConsistencyTest = False;
debugBuildLaxPair = False;
debugSubstitutionInLaxEquation = False;
debugFinalSubstitutionInLaxEquation = False;
debugComputationOftAnds4s = False;
debugTestCompatibilitySquared = False;
debugComputationOfN = False;

(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* commentinter[]: prints a welcome message                                  *)
(*****************************************************************************)

(* begin of commentinter *)
commentinter[] := Block[{},
Print["********************************************************************"];
Print["         WELCOME TO THE MATHEMATICA PROGRAM              "];
Print["                  by WILLY HEREMAN                       "];
Print["     FOR THE COMPUTATION OF LAX PAIRS OF NONLINEAR       "];
Print["  PARTIAL DIFFERENCE EQUATIONS IN 2D (ON QUAD-GRAPHS)    "];
Print[" Collaborators: Reinout Quispel and Peter van der Kamp   "];
Print["           First released on July 22, 2008               "];
(* Print["           Last updated: July 22, 2008                   "]; *)
Print["                    Copyright 2008                       "];
Print["********************************************************************"]
]; (* end of commentinter *)

(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* cls: clears the screen                                                    *)
(*****************************************************************************)

(* 
cls := Print["\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"];
*)
(*****************************************************************************)
(* printpage[ , ]: a subroutine for menu                                     *)
(*****************************************************************************)
 
(* begin of printpage *)
printpage[n_,page_] := Module[{i,choice,lenpage},
cls; 
lenpage = Length[page[n]];
Print["***************** MENU INTERFACE *****************  (page: ",n,")"];
Print["---------------------------------------------------------------------"];
Print["ENTER YOUR CHOICE IN DIALOG BOX (may be hidden behind this window)"];
Print["---------------------------------------------------------------------"];
For[i=1,i <= lenpage,i++,
   Print[Part[Part[page[n],i],1]]
   ];
Print[" nn) Next Page"];
Print[" tt) Your System"];
Print[" qq) Exit the Program"];
Print["---------------------------------------------------------------------"];
choice = Input["ENTER YOUR CHOICE: "];
Return[choice] 
]; (* end of printpage *)

(*****************************************************************************)
(* menu: creates the menu                                                    *)
(*****************************************************************************)

(* begin of menu *)
menu := Module[
{counterpage = 1,menulist,numpages,page,
choice1,control,choice2,choice3,lenmenulist,i},
(* begin of menulist *)
menulist = {
{"  1) discrete potential KdV Equation (l_pKdV.m)"},
{"  2) discrete modified KdV Equation  (l_mKdV.m)"},
{"  3) discrete sine-Gordon Equation (l_sG.m)"},
{"  4) H1 equation (ABS classification) (l_H1.m)"},
{"  5) H2 equation (ABS classification) (l_H2.m)"},
{"  6) H3 equation (delta not 0) (ABS classification) (l_H3rhs.m)"},
{"  7) H3 equation (delta = 0) (ABS classification) (l_H3zero.m)"},
{"  8) Q1 equation (delta not 0) (ABS classification) (l_Q1rhs.m)"},
{"  9) Q1 equation (delta = 0) (ABS classification) (l_Q1zero.m)"},
{"  10) Q2 equation (delta not 0) (ABS classification) (l_Q2rhs.m)"},
{"  11) Q2 equation (delta = 0) (ABS classification) (l_Q2zero.m)"},
{"  12) Q3 equation (delta not 0) (ABS classification) (l_Q3rhs.m)"},
{"  13) Q3 equation (delta = 0) (ABS classification) (l_Q3zero.m)"},
{"  14) A1 equation (delta not 0) (ABS classification) (l_A1rhs.m)"},
{"  15) A1 equation (delta = 0) (ABS classification) (l_A1zero.m)"},
{"  16) A2 equation (ABS classification) (l_A2.m)"},
{"  17) (alpha,beta) equation (l_abeq.m)"},
{"  18) discrete sine-Gordon Equation (testing parameters) (l_sGpar.m)"},
{"  19) discrete generalized modified KdV Equation (l_gmKdV.m)"}, 
{"  20) Q2 equation with more general terms (l_Q2rhs3.m)"}
}; (* end of menulist *)

lenmenulist = Length[menulist];
numpages = Ceiling[lenmenulist/10];
For[i = 1,i <= numpages,i++,
   page[i] = If[lenmenulist >= (i*10),
               menulist[[Table[k,{k,(i-1)*10+1,i*10}]]],
               menulist[[Table[k,{k,(i-1)*10+1,lenmenulist}]]]]
   ];

choice1 = printpage[counterpage,page];

control := (
Switch[choice1,
tt,Print["Make sure that you have prepared the data file for the system"];
   Print["you want to test (similar to the data files we supplied)."];
   choice2 = Input["If your file is ready, press 1, else 2: "];
   If[choice2 === 1,
    choice3 = InputString["Enter the name of your data file: "];
    Get[choice3],
    Print["Aborting the computations!"];
    Print["Prepare your data file first, then start over."];
    Abort[] 
     ], 
nn,If[counterpage < numpages,counterpage++;
     choice1 = printpage[counterpage,page]; control,
     counterpage = 1; choice1 = printpage[1,page]; control],
qq,Print["Aborting the computations!"];Abort[],
1,<<l_pKdV.m,
2,<<l_mKdV.m,
3,<<l_sG.m,
4,<<l_H1.m,
5,<<l_H2.m,
6,<<l_H3rhs.m,
7,<<l_H3zero.m,
8,<<l_Q1rhs.m,
9,<<l_Q1zero.m,
10,<<l_Q2rhs.m,
11,<<l_Q2zero.m,
12,<<l_Q3rhs.m,
13,<<l_Q3zero.m,
14,<<l_A1rhs.m,
15,<<l_A1zero.m,
16,<<l_A2.m,
17,<<l_abeq.m,
18,<<l_sGpar.m,
19,<<l_gmKdV.m,
20,<<l_Q2rhs3.m,
_,
Print["Aborting the computations!"];
Abort[]
]; (* closes Switch *)
); (* end control *)
control; 
If[Not[StringQ[myfile]],
  myfile = InputString["Enter the name of the output file (e.g., xxxxx.out):"]]
]; (* end of menu *)

(* ------------------------------------------------------------------------- *)

(* set-up for collecting the data in a log file, transcript of computations *)
logfile="";
OpenLog[filename_String] := (logfile = OpenWrite[filename];
  If[logfile === $Failed, Return[]];
  AppendTo[$Echo,logfile]; AppendTo[$Output,logfile];);
CloseLog[] := ( $Echo = Complement[$Echo,{logfile}];
  $Output = Complement[$Output,{logfile}];
  Close[logfile]; );

(* ----------------------------------------------------------------------- *)

(* End of the auxiliary functions and procedures *)
(* Start of the executable code lines *)

Print["Loading code for computation of Lax pairs (July 22, 2008)"];

menu;
OpenLog[myfile];
commentinter[]; 

(* ------------------------------------------------------------------------- *)

(* ****************************************************************** *)
(* Input: lattice defined on front face (F)                           *)
(* ****************************************************************** *)

Print["Working with the discrete ",name," equation:"];
Print[lattice[x,x1,x2,x12,p,q]];

Print["********************** CONTROL SETTINGS *************************"];

(* ********************************************************************** *)

(* ALL CONTROLS have been moved into data files and must be set there *)

(* clear the variable x12, as precaution if the kernel was not quit between *)
(* successive runs *)
(* Clear[x12]; *)

(* printing the settings of the control flags *)
Print["LaxPairGiven (L and M are given in data file) is set to ",LaxPairGiven];
Print["t and s are given in data file (byhand) is set to: ",byhand];
Print["Determinant method to compute t and s is set to: ",detmethod];
Print["Test consistency on cube is set to ",testconsistencyoncube];
Print["Use of powerexpand is set to ",controlpowerexpand];
Print["Use of rootrules is set to ",controlrootrules];
(* Print["Evaluation on lattice is set to ",evaluateonlattice]; *)
Print["Computation of matrix N is set to: ",controlcomputationmatrixN];
Print["Application of gauge transformation is set to: ",controlgaugetf];
If[(byhand && detmethod) || (!byhand && !detmethod), 
  Print["Boolean values of byhand and detmethod cannot be BOTH set to True,"];
  Print["neither can the be BOTH set to False. "]
  ];

(* ********************************************************************** *)

(* For testing only *)
evaluateonlattice = False;
(* evaluateonlattice = True; *)
(* control evaluation on lattice: true or false *)
(*
If[evaluateonlattice, 
   tempx12 = x12, (* else *)
   tempx12 = tempx12
   ];   
*)

(* ******************************************************************* *)

Print["******************* START OF THE COMPUTATION ************************"];

(* rootrules needed to simplify expression involving roots *)

rootrule1    = {Sqrt[cat_]*Sqrt[dog_] -> Sqrt[cat*dog] };
rootrule1gen = {Sqrt[cat__]*Sqrt[dog__] -> Sqrt[cat*dog]};

(* FOR TESTING only since it ignores the signs ! *)
(* Note: Sqrt[a^2] -> a, but Sqrt[(-a)^2] -> -a which may lead to overall *)
(* negative sign *)

rootrule2    = {Sqrt[mouse_*cat_^2*dog_] -> cat*Sqrt[mouse*dog]};

(* FOR TESTING only since it ignores the signs *)
(* Note: Sqrt[a^2] -> a, but Sqrt[(-a)^2] -> -a which may lead to overall *)
(* negative sign *)

rootrule2gen = {Sqrt[mouse___*cat_^2*dog___] -> cat*Sqrt[mouse*dog]};

rootrule2abs = {Sqrt[mouse_*cat_^2*dog_] -> Abs[cat]*Sqrt[mouse*dog]};

rootrule3gen = {Sqrt[mouse___*(cat_^2)/dog___] -> cat*Sqrt[mouse/dog]};

rootrule3abs = {Sqrt[mouse___*(cat_^2)/dog___] -> Abs[cat]*Sqrt[mouse/dog]};

rootrule4abs = {Sqrt[mouse___*cat_^2*dog___] -> Abs[cat]*Sqrt[mouse*dog]};

rootrule5gen = {Sqrt[cat__]*Sqrt[dog__]*mouse___ -> mouse*Sqrt[cat*dog] };

(* rootrule6gen = {Sqrt[cat__*dog__] -> Sqrt[cat]*Sqrt[dog]}; *)

(* ********************************************************************** *)

If[name===cabeq, 
  Print["The data file l_cabeq.m was created to experiment with removing"];
  Print["constant terms in the lattice, or, with applying a"];
  Print["non-autonomous Moebius transformation to reduce the original"];
  Print["lattice to a lattice of type Q3"]; 
  Print["In the data file l_cabeq.m, set action = translation"];
  Print["to see the effect of removing the constant terms"];
  Print["In the data file l_cabeq.m, set action = moebiustransformation"];
  Print["to see the effect of applying a non-autonomous Moebius"];
  Print["transformation"]
  ];

(* NON-GENERIC CODE, only for cabeq lattice: translation issue! *)
(* Piece of code set up for applying translation to a specific equation *)
(* In the data file l_cabeq.m: set action = translation *)
(* Computation of the translated lattice *)

If[name === cabeq && action === translation, 
Print["********************* APPLICATION OF A TRANSLATION ******************"];
(* This is the lattice defined on the front (F) *)
latticefrontoriginal = lattice[y,y1,y2,y12,p,q]; 

Print["Lattice on front (F) before translation, latticefrontoriginal:"];
Print[latticefrontoriginal];

Print["Remove linear terms first!"];
con = 1/(alpha + beta);
Print["Set con = ", con];
translationrulefront = 
  {y -> x + con, y1 -> x1 + con, y2 -> x2 + con, y12 -> x12 + con};
Print["To the lattice on the front plane, apply the translationrulefront:"];
Print[translationrulefront];
latticefront = 
  Factor[Part[latticefrontoriginal,1] /. translationrulefront] == 0;
Print["Lattice on front (F) after translation, latticefront:"];
Print[latticefront];

(* Computing the lattice on the ground (G) *)
latticegroundoriginal = lattice[y,y1,y3,y13,p,k]; 
translationruleground = 
 {y -> x + con, y1 -> x1 + con, y3 -> x3 + con, y13 -> x13 + con};
Print["To the lattice on the ground, apply the translationruleground:"];
Print[translationruleground];
latticeground = 
  Factor[Part[latticegroundoriginal,1] /. translationruleground] == 0;

Print["Lattice on ground (G) after translation, latticeground:"];
Print[latticeground];

(* Computing the lattice on the left face (L) *)
latticeleftfaceoriginal = lattice[y,y2,y3,y23,q,k]; 
translationruleleftface = 
 {y -> x + con, y2 -> x2 + con, y3 -> x3 + con, y23 -> x23 + con};
Print["To the lattice on the left face, apply the translationruleleftface:"];
Print[translationruleleftface];
latticeleftface = 
  Factor[Part[latticeleftfaceoriginal,1] /. translationruleleftface] == 0;
Print["Lattice on left face (L) after translation, latticeleftface:"];
Print[latticeleftface];
Print["The translated lattice replaces the lattice given in the data file."];
Print["The computations will continue with the translated lattice"];
Print["*********************************************************************"];
]; (* end if name===cabeq && action===translation *)

(* NON-GENERIC CODE, only for cabeq lattice: Moebius transformation issue *)
(* In the data file l_cabeq.m: set action = moebiustransformation *)
(* Application of a non-autonomous Moebius transformation *)

If[name===cabeq && action===moebiustransformation, 
Print["****** APPLICATION OF A NON-AUTONOMOUS MOEBIUS TRANSFORMATION *******"];

(* This is the lattice defined on the front (F) *)
latticefrontoriginal = lattice[y,y1,y2,y12,p,q]; 

Print["Lattice on front plane (F) before the non-autonomous Moebius"];
Print["transformation, (latticefrontoriginal):"];
Print[latticefrontoriginal];

Print["Applying a non-autonomous Moebius transformation first!"];
tau = Sqrt[((p-alpha)*(p-beta))/((p+alpha)*(p+beta))];
sigma = Sqrt[((q-alpha)*(q-beta))/((q+alpha)*(q+beta))];
translationrulefront = 
  {y   ->  tau^l*sigma^m*x, 
   y1  ->  tau^(l+1)*sigma^m*x1, 
   y2  ->  tau^l*sigma^(m+1)*x2, 
   y12 ->  tau^(l+1)*sigma^(m+1)*x12};
Print["To the lattice (on the front plane), apply the translationrulefront:"];
Print[translationrulefront];
latticefront = 
  Factor[Part[latticefrontoriginal,1] /. translationrulefront];
Print["Lattice defined on front (F) after applying the non-autonomous"<>
      "Moebius transformation, latticefront:"];
Print[latticefront];
Print["************************** END OF COMPUTATION! **********************"];
Abort[]
]; (* name===cabeq && action===moebiustransformation *) 

(* GENERIC CODE for all lattices, except cabeq lattice where the linear *)
(* terms should be removed first, followed by a Moebius transformation *)

If[name=!=cabeq, (* begin if name not cabeq *)
(* This is the lattice defined on the front (F) *)
latticefront = lattice[x,x1,x2,x12,p,q]; 

Print["Working with lattice defined on front (F), latticefront:"];
Print[latticefront];

(* Computing the lattice on the ground (G) *)
latticeground = lattice[x,x1,x3,x13,p,k]; 

Print["Lattice on ground (G), latticeground:"];
Print[latticeground];

(* Computing the lattice on the left face (L) *)
latticeleftface = lattice[x,x2,x3,x23,q,k]; 

Print["Lattice on left face (L), latticeleftface:"];
Print[latticeleftface]
]; (* end if name not cabeq *)

(* ********************************************************************* *)
(* Experiment with Moebius transformations for x3, x13, and x23 *)

applymoebiusx3directionsimple = False;
If[applymoebiusx3directionsimple, 
  (* begin if applymoebiusx3directionsimple *)
  (* consider simple translation itself *)
  (* backrulesimple = {a -> 0, b -> 1, c -> 1, d -> 0}; *)

  moebiusx3simple = { x3 -> a+b*x3};
  Print["Simple Moebius transformation for x3:"];
  Print[moebiusx3simple];

  moebiusx13simple = { x13 -> (a1+b1*x13) };
  Print["Simple Moebius transformation for x13:"];
  Print[moebiusx13simple];

  latticeground = latticeground /. moebiusx3simple;
  latticeground = latticeground /. moebiusx13simple;
  Print["After simple Moebius transformation for x3 and x13, latticeground:"];
  Print[latticeground]
 ]; (* end if applymoebiusx3directionsimple *)

applymoebiusx3direction = False;
If[applymoebiusx3direction, 
   (* begin if applymoebiusx3direction *)

   backrule = {a -> 0, b -> 1, c -> 1, d -> 0,  
               a1 -> 0, b1 -> 1, c1 -> 1, d1 -> 0,  
               a2 -> 0, b2 -> 1, c2 -> 1, d2 -> 0};

   moebiusx3 = { x3 -> (a+b*x3)/(c+d*x3) };
   Print["Moebius transformation for x3:"];
   Print[moebiusx3];

   moebiusx13 = { x13 -> (a1+b1*x13)/(c1+d1*x13) };
   Print["Moebius transformation for x13:"];
   Print[moebiusx13];

   latticeground = latticeground /. moebiusx3;
   latticeground = latticeground /. moebiusx13;
   Print["After Moebius transformation for x3 and x13, latticeground:"];
   Print[latticeground]
 ]; (* end if applymoebiusx3direction *)

If[applymoebiusx3directionsimple, 
   (* begin if applymoebiusx3directionsimple *)

   moebiusx3simple = { x3 -> a+b*x3 };
   Print["Simple Moebius transformation for x3:"];
   Print[moebiusx3simple];

   moebiusx23simple = { x23 -> a2+b2*x23 };
   Print["Simple Moebius transformation for x23:"];
   Print[moebiusx23simple];

   latticeleftface = latticeleftface /. moebiusx3simple;
   latticeleftface = latticeleftface /. moebiusx23simple;
   Print["After simple Moebius transformation for x3 and x23, 
         latticeleftface:"];
   Print[latticeleftface]
 ]; (* end if applymoebiusx3directionsimple *)

If[applymoebiusx3direction, 
   (* begin if applymoebiusx3direction *)

   moebiusx3 = { x3 -> (a+b*x3)/(c+d*x3) };
   Print["Moebius transformation for x3:"];
   Print[moebiusx3];

   moebiusx23 = { x23 -> (a2+b2*x23)/(c2+d2*x23) };
   Print["Moebius transformation for x23:"];
   Print[moebiusx23];

   latticeleftface = latticeleftface /. moebiusx3;
   latticeleftface = latticeleftface /. moebiusx23;
   Print["After Moebius transformation for x3 and x23, latticeleftface:"];
   Print[latticeleftface]
  ]; (* end if applymoebiusx3direction *)

(* ******************************************************************* *)

Print["**************** TESTING CONSISTENCY AROUND THE CUBE ****************"];

(* GENERIC CODE for all lattices *)
(* Solving for the x12 *)

x12sol = Solve[latticefront, x12];
x12 = Factor[x12 /. Flatten[x12sol]];
Print["Computation  x12 ="];
Print[x12];

(* ******************************************************************** *)

(* flags for testconsistecyoncube are now given in the data file *)
(* testconsistecyoncube = True *)
(* testconsistecyoncube = False *)

If[testconsistencyoncube, (* begin if testconsistencyoncube *)

(* Solving for the x13 *)
x13sol = Solve[latticeground, x13];
x13 = Factor[x13 /. Flatten[x13sol]];
Print["Computation  x13 = "];
Print[x13];

(* Solving for the x23 *)
x23sol = Solve[latticeleftface, x23];
x23 = Factor[x23 /. Flatten[x23sol]];
Print["Computation  x23 = "];
Print[x23];

(* ***************************************************************** *)

Print["*****************************************************************"];

(* Computation of x123 in 3 ways *)
(* 
x123choice1form = x23 /. {x -> x1, x2 -> tempx12, x3 -> tempx13};
Print["Before factoring, computation of x123 using x23 (x123choice1form), "<> 
      "x123 ="];
Print[x123choice1form];
*)

x123choice1 = Factor[x23 /. {x -> x1, x2 -> x12, x3 -> x13}];
Print["After factoring, computation of x123 using x23 (x123choice1), x123 ="];
Print[x123choice1];

(* 
x123choice2form = x13 /. {x -> x2, x1 -> tempx12, x3 -> tempx23};
Print["Before factoring, computation of x123 using x13, (x123choice2form), "<>
      "x123 ="];
Print[x123choice2form];
*)

x123choice2 = Factor[x13 /. {x -> x2, x1 -> x12, x3 -> x23}];
Print["After factoring, computation of x123 using x13, (x123choice2), x123 ="];
Print[x123choice2];

(*
x123choice3form = x12 /. {x -> x3, x1 -> tempx13, x2 -> tempx23};
Print["Before factoring, computation of x123 using x12 (x123choice3form), "<>
      "x123 ="];
Print[x123choice3form];
*)

(* skip choice 3 
x123choice3 = Factor[x12 /. {x -> x3, x1 -> x13, x2 -> x23}];
Print["After factoring, computation of x123 using x12 (x123choice3), x123 ="];
Print[x123choice3];
end skip choice 3 *)

(* Testing if x123 is consistent based on the three choices *)

Print["********************************************************************"];

Print["As soon as one consistency condition is violated the computation"];
Print["will abort, unless candidates for L and M are given in the data file."];

diffx123choice1choice2 = Factor[x123choice1-x123choice2];
Print["Test if (x123choice1 - x123choice2) = 0, diffx123choice1choice2 ="];
If[debugConsistencyTest, 
  Print[diffx123choice1choice2]
  ];

numdiffx123choice1choice2 = Numerator[diffx123choice1choice2];
If[debugConsistencyTest, 
   Print["Numerator of diffx123choice1choice2 = "]
  ];
Print[numdiffx123choice1choice2];

If[diffx123choice1choice2=!=0, 
  Print["The equation is INCONSISTENT around the cube!"];
  Print["********* TEST RESULT IS NOT ZERO! ************ "<> 
        "numerator of diffx123choice1choice2 must be zero:"];
  Print[numdiffx123choice1choice2];
   If[!LaxPairGiven, 
     Print["A Lax pair may exist but can not be computed with this code!"];
     Print["Give candidates (L and M) for the Lax pair in the data file."];
     Abort[], (* else *)
     Print["Continue with the verification of the given Lax pair!"]
     ], (* else *)
  Print["*** TEST RESULT = ",diffx123choice1choice2," (SHOULD BE ZERO) ***"];
  Print["The equation is CONSISTENT around the cube!"]
  ];

(* CAN BE REMOVED LATER *)
(* begin skip second way to speed things up 

diffx123choice1choice3 = Factor[x123choice1-x123choice3];
Print["Test if (x123choice1 - x123choice3) = 0, diffx123choice1choice3 ="];
Print[diffx123choice1choice3];

numdiffx123choice1choice3 = Numerator[diffx123choice1choice3];
Print["Numerator of diffx123choice1choice3 = "];
Print[numdiffx123choice1choice3];

If[diffx123choice1choice3=!=0, 
  Print["The equation is INCONSISTENT around the cube!"];
  Print["**** TEST RESULT IS NOT ZERO! "<>
        "numerator of diffx123choice1choice3 must be zero:"];
  Print[numdiffx123choice1choice3];
   If[!LaxPairGiven, 
     Print["A Lax pair may exist but can not be computed with this code!"];
     Print["Give candidates (L and M) for the Lax pair in the data file."];
     Abort[], (* else *)
     Print["Continue with the verification of the given Lax pair!"]
     ], (* else *)
  Print["**** TEST RESULT = ",diffx123choice1choice3," (SHOULD BE ZERO) ****"];
  Print["The equation is CONSISTENT around the cube!"]
  ]
end skip second way *)

]; (* end test consistencyoncube *)

(* Print["Computation aborted on demand! After consistency test is done!"]; *)
(* Abort[]; *)

(* ******************************************************************* *)

If[!LaxPairGiven,
  Print["************* BUILDING THE LAX PAIR (L_c and M_c) *****************"];
  Print["(c refers to candidate matrix), normalization factors t and s will "];
  Print["be computed below, or must be given in the data file "]
  ];

If[!LaxPairGiven && testconsistencyoncube, 
   (* Replacement of x3 by f/g *)

x13hom = Factor[x13 /. {x3 -> f/g }];
Print["Substitution of x3 = f/g into x13, gives x13hom = "];
Print[x13hom];

x23hom = Factor[x23 /. {x3 -> f/g }];
Print["Substitution of x3 = f/g into x23, gives x23hom = "];
Print[x23hom];

(* Building the vector (f1, g1) *)

f1 = t*(Numerator[x13hom]);
If[debugBuildLaxPair, 
  Print["Computing f1 = "];
  Print[f1]
];

g1 = t*(Denominator[x13hom]);
If[debugBuildLaxPair, 
   Print["Computing g1 = "];
   Print[g1]
];

(* Building the vector (f2, g2) *)

f2 = s*(Numerator[x23hom]);
If[debugBuildLaxPair, 
   Print["Computing f2 = "];
   Print[f2]
   ];

g2 = s*(Denominator[x23hom]);
If[debugBuildLaxPair, 
   Print["Computing g2 = "];
   Print[g2]
   ]
]; (* end if !LaxPairGiven && testconsistencyoncube *)

(* ****************************************************************** *)

Print["*********************************************************************"];

(* Setting up the matrices L and M *)
(* Control flags are now given in the data file *)
(* LaxPairGiven = True *)
(* LaxPairGiven = False *)

If[!LaxPairGiven && !testconsistencyoncube,
   Print["Setting both LaxPairGiven and testconsistencyoncube to False"];
   Print["is not supported by the current code!"]; 
   Abort[]
   ];
   
If[!LaxPairGiven && testconsistencyoncube, (* begin if !LaxPairGiven *)
  candidateL = 
            {
            {Coefficient[Expand[f1],f,1],Coefficient[Expand[f1],g,1]},
            {Coefficient[Expand[g1],f,1],Coefficient[Expand[g1],g,1]}
            }
  ]; (* end if !LaxPairGiven *)

If[LaxPairGiven,
  Print["************* VERIFICATION OF THE GIVEN LAX PAIR ****************"];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ];

If[byhand, 
   Print["IMPORTANT NOTE:"];
   Print["Although t and s are given in the data file, they will only be"];
   Print["used LATER. Continuation with the next steps allows one to verify"];
   Print["if t and s are CONSISTENT with the steps of the algorithm"];
   Print["******************************************************************"]
   ];

If[LaxPairGiven, (* begin if LaxPairGiven *)
   Print["******* The matrix L was given in the data file! ******"]
   ]; (* end if LaxPairGiven *)

candidateL = Map[Factor,candidateL];
Print["Candidate Lax pair, matrix L (candidateL)"]; 
Print["L = "];
Print[candidateL // MatrixForm];

Print["*********************************************************************"];

(* Entries of candidateL are available separately *)

entrycandidateL11 = candidateL[[1]][[1]];
Print["Entry L11 of candidate L (entrycandidateL11) "];
Print["L11 = "];
Print[entrycandidateL11];

entrycandidateL12 = candidateL[[1]][[2]];
Print["Entry L12 of candidate L (entrycandidateL12) "];
Print["L12 = "];
Print[entrycandidateL12];

entrycandidateL21 = candidateL[[2]][[1]];
Print["Entry L21 of candidate L (entrycandidateL21) "];
Print["L21 = "];
Print[entrycandidateL21];

entrycandidateL22 = candidateL[[2]][[2]];
Print["Entry L22 of candidate L (entrycandidateL22) "];
Print["L22 = "];
Print[entrycandidateL22];

(* ************************************************************* *)

Print["*********************************************************************"];

If[!LaxPairGiven && testconsistencyoncube, (* begin if !LaxPairGiven *)
   candidateM = {
                {Coefficient[Expand[f2],f,1],Coefficient[Expand[f2],g,1]},
                {Coefficient[Expand[g2],f,1],Coefficient[Expand[g2],g,1]}
                }
  ]; (* begin if !LaxPairGiven *)

If[LaxPairGiven, (* begin if LaxPairGiven *)
   Print["****** The matrix M was given in the data file! ******"]
   ]; (* begin if LaxPairGiven *)

candidateM = Map[Factor,candidateM];
Print["Candidate Lax pair, matrix M (candidateM)"];
Print["M = "];
Print[candidateM // MatrixForm];

Print["*********************************************************************"];

(* Entries of candidateM are available separately *)

entrycandidateM11 = candidateM[[1]][[1]];
Print["Entry M11 of candidate M (entrycandidateM11) "];
Print["M11 = "];
Print[entrycandidateM11];

entrycandidateM12 = candidateM[[1]][[2]];
Print["Entry M12 of candidate M (entrycandidateM12) "];
Print["M12 = "];
Print[entrycandidateM12];

entrycandidateM21 = candidateM[[2]][[1]];
Print["Entry M21 of candidate M (entrycandidateM21) "];
Print["M21 = "];
Print[entrycandidateM21];

entrycandidateM22 = candidateM[[2]][[2]];
Print["Entry M22 of candidate M (entrycandidateM22) "];
Print["M22 = "];
Print[entrycandidateM22];

(* ********************************************************************* *)

If[debugComputationOftANDs,
   Print["************** SUBSTITUTION INTO THE LAX EQUATION ****************"]
  ];

candidateL2 = Map[Factor,candidateL /. 
                  {x -> x2, x1 -> tempx12, t -> t2}];
If[debugSubstitutionInLaxEquation, 
  Print["Candidate Lax pair, matrix L2  (candidateL2)"];
  Print["L2 = "];
  Print[candidateL2 // MatrixForm]
];

candidateM1 = Map[Factor,candidateM /. 
                  {x -> x1, x2 -> tempx12, s -> s1}];
If[debugSubstitutionInLaxEquation, 
   Print["Candidate Lax pair, matrix M1 (candidateM1)"];
   Print["M1 = "];
   Print[candidateM1 // MatrixForm]
  ];

(* Expressing the compatibility condition for the ``eigenfunctions" *)
(* (f1,g1)_2 = (f12, g12) = (f2,g2)_1 = (f21, g21) *) 

candidateL2M = Map[Factor,candidateL2.candidateM];
If[debugSubstitutionInLaxEquation, 
   Print["Product candidates L2.M = "];
   Print[candidateL2M // MatrixForm]
   ];

candidateM1L = Map[Factor,candidateM1.candidateL];
If[debugSubstitutionInLaxEquation, 
   Print["Product candidates M1.L = "];
   Print[candidateM1L // MatrixForm]
   ];

(* Computation of the determinants of the matrices L2.M and M1.L *)

determinantcandidateL2M = Factor[Det[candidateL2M]];
If[debugSubstitutionInLaxEquation, 
  Print["Determinant of (candidate) L2.M, det(L2.M) = "];
  Print[determinantcandidateL2M]
  ];

determinantcandidateM1L = Factor[Det[candidateM1L]];
If[debugSubstitutionInLaxEquation, 
   Print["Determinant of (candidate) M1.L, det(M1.L) = "];
   Print[determinantcandidateM1L]
   ];

(* Differences of determinants of candidates L2.M and M1.L *)
(* Det(L2.M) - Det(M1.L) *)

If[LaxPairGiven, 
  Print["******************************************************************"];
  Print["*********** COMPUTATION OF CONSTRAINTS FOR t AND s ***************"];
  Print["The subsequent computations might be slow for complicated lattices!"],
  (* else *)
  Print["*******************************************************************"];
  Print["************ COMPUTATION OF THE LAX PAIR CONTINUES ***************"];
  Print["************ COMPUTATION OF CONSTRAINTS FOR t AND s ***************"];
  Print["The subsequent computations might be slow for complicated lattices!"]
  ];

(* Equating the matrices and building the four compatibility equations *)

compatibilitymatrix = Map[Factor,candidateL2M-candidateM1L];
If[debugComputationOftAnds4s,
   Print["Compatibility matrix L2.M - M1.L, NOT evaluated ON LATTICE, "<>
         "compatibilitymatrix"];
   Print[compatibilitymatrix // MatrixForm]
  ];

If[debugComputationOftAnds4s,
 Print["Generating and simplifying the compatibility conditions."];
 Print["The subsequent computations might be slow for complicated lattices"]
 ];

compatibilityeq11 = MapAll[Factor,compatibilitymatrix[[1]][[1]]];

  If[LaxPairGiven && name =!= Q3rhs,
     If[controlrootrules, 
      compatibilityeq11 = 
      MapAll[Factor, 
      Numerator[compatibilityeq11 //. rootrule1 //. rootrule2abs]/
      Denominator[compatibilityeq11 //. rootrule1 //. rootrule2abs]]
      (* Print["Abs value replaced by Factor!"]; *)
      (* compatibilityeq11 = Factor[compatibilityeq11 /. {Abs -> Factor}] *)
     ]
    ];

If[debugComputationOftAnds4s,
   Print["Compatibility equation, (1-1) element, NOT evaluated ON LATTICE "<>
         "compatibilityeq11 = "];
   Print[compatibilityeq11 == 0]
  ];

compatibilityeq12 = MapAll[Factor,compatibilitymatrix[[1]][[2]]];

  If[LaxPairGiven && name =!= Q3rhs,
     If[controlrootrules, 
      compatibilityeq12 = 
      MapAll[Factor, 
      Numerator[compatibilityeq12 //. rootrule1 //. rootrule2abs]/
      Denominator[compatibilityeq12 //. rootrule1 //. rootrule2abs]]
      (* Print["Abs value replaced by Factor!"]; *)
      (* compatibilityeq12 = Factor[compatibilityeq12 /. {Abs -> Factor}] *)
     ]
    ];

If[debugComputationOftAnds4s,
   Print["Compatibility equation, (1-2)-element, NOT evaluated ON LATTICE,"];
   Print["compatibilityeq12 = "];
   Print[compatibilityeq12 == 0]
  ];

compatibilityeq21 = MapAll[Factor,compatibilitymatrix[[2]][[1]]];

  If[LaxPairGiven && name =!= Q3rhs,
   If[controlrootrules, 
     compatibilityeq21 = 
     MapAll[Factor, 
     Numerator[compatibilityeq21 //. rootrule1 //. rootrule2abs]/
     Denominator[compatibilityeq21 //. rootrule1 //. rootrule2abs]]
     (* Print["Abs value replaced by Factor!"]; *)
     (* compatibilityeq21 = Factor[compatibilityeq21 /. {Abs -> Factor}] *)
    ]
  ];

If[debugComputationOftAnds4s,
  Print["Compatibility equation, (2-1)-element, NOT evaluated ON LATTICE,"];
  Print["compatibilityeq21 = "];
  Print[compatibilityeq21 == 0]
  ];

compatibilityeq22 = MapAll[Factor,compatibilitymatrix[[2]][[2]]];

  If[LaxPairGiven && name =!= Q3rhs,
   If[controlrootrules, 
     compatibilityeq22 = 
     MapAll[Factor, 
     Numerator[compatibilityeq22 //. rootrule1 //. rootrule2abs]/
     Denominator[compatibilityeq22 //. rootrule1 //. rootrule2abs]]
     (* Print["Abs value replaced by Factor!"]; *)
     (* compatibilityeq22 = Factor[compatibilityeq22 /. {Abs -> Factor}] *)
    ]
   ];

If[debugComputationOftAnds4s,
  Print["Compatibility equation, (2-2)-element, NOT evaluated ON LATTICE,"];
  Print["compatibilityeq22 = "];
  Print[compatibilityeq22 == 0]
  ];

If[debugComputationOftAnds4s,
  Print["*******************************************************************"]
  ];

If[debugComputationOftAnds4s,
  Print["Start testing the four compatibility conditions for (t2*s)/(t*s1)."];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ];

If[name =!= Q3rhs, 
  compatibilitymatrixonlattice = 
  MapAll[Factor,compatibilitymatrix /. {tempx12 -> x12}];
  If[debugComputationOftAnds4s,
    Print["Compatibility matrix L2.M - M1.L, ON LATTICE, "<>
          "compatibilitymatrixonlattice"];
    Print[compatibilitymatrixonlattice // MatrixForm]
   ]
  ];

If[debugComputationOftAnds4s,
  Print["Testing the compatibility condition for (1-1) element."];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ];

compatibilityeq11onlattice = 
  MapAll[Factor,compatibilityeq11 /. {tempx12 -> x12}];

  If[LaxPairGiven, (* begin if laxpairgiven *)
   If[controlrootrules, 
     compatibilityeq11onlattice = 
     MapAll[Factor, 
     Numerator[compatibilityeq11onlattice //. rootrule1 //. rootrule2abs]/
     Denominator[compatibilityeq11onlattice //. rootrule1 //. rootrule2abs]];
   If[debugComputationOftAnds4s,
     Print["Abs value might have been replaced by Factor!"]
     ];
     compatibilityeq11onlattice = 
       Factor[compatibilityeq11onlattice /. {Abs -> Factor}]
    ] (* end if controlrootrules *)
   ]; (* end if laxpairgiven *)

  If[LaxPairGiven && name === Q3rhs, 
     Print["Using PowerExpand to simplify!"];
     compatibilityeq11onlattice = 
     MapAll[PowerExpand, Numerator[compatibilityeq11onlattice]]
    ];

If[debugComputationOftAnds4s || True,
   Print["Compatibility equation, (1-1) element ON LATTICE,"];
   Print["compatibilityeq11onlattice = "];
   Print[compatibilityeq11onlattice == 0]
  ];

If[debugComputationOftAnds4s,
  Print["Testing the compatibility condition for (1-2) element."];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ];

compatibilityeq12onlattice = 
  MapAll[Factor,compatibilityeq12 /. {tempx12 -> x12}];

  If[LaxPairGiven, (* begin if laxpairgiven *)
    If[controlrootrules, 
     compatibilityeq12onlattice = 
     MapAll[Factor, 
     Numerator[compatibilityeq12onlattice //. rootrule1 //. rootrule2abs]/
     Denominator[compatibilityeq12onlattice //. rootrule1 //. rootrule2abs]];
    If[debugComputationOftAnds4s,
       Print["Abs value might have been replaced by Factor!"]
      ];
     compatibilityeq12onlattice = 
       Factor[compatibilityeq12onlattice /. {Abs -> Factor}]
    ]
   ]; (* end if laxpairgiven *)

  If[LaxPairGiven && name === Q3rhs, 
     If[debugComputationOftAnds4s,
       Print["Using PowerExpand to simplify!"]
       ];
     compatibilityeq12onlattice = 
     MapAll[PowerExpand, Numerator[compatibilityeq12onlattice]]
    ];

If[debugComputationOftAnds4s || True,
   Print["Compatibility equation, (1-2) element ON LATTICE, "];
   Print["compatibilityeq12onlattice = "];
   Print[compatibilityeq12onlattice == 0]
  ];

If[debugComputationOftAnds4s,
  Print["Testing the compatibility condition for (2-1) element."];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ];

compatibilityeq21onlattice = 
  MapAll[Factor,compatibilityeq21 /. {tempx12 -> x12}];

  If[LaxPairGiven, (* begin if laxpairgiven *)
   If[controlrootrules, (* begin if controlrootrules *)
     compatibilityeq21onlattice = 
     MapAll[Factor, 
     Numerator[compatibilityeq21onlattice //. rootrule1 //. rootrule2abs]/
     Denominator[compatibilityeq21onlattice //. rootrule1 //. rootrule2abs]];
     If[debugComputationOftAnds4s,
        Print["Abs value might have been replaced by Factor!"]
       ];
     compatibilityeq21onlattice = 
       Factor[compatibilityeq21onlattice /. {Abs -> Factor}]
    ] (* end if controlrootrules *)
   ]; (* end if laxpairgiven *)

  If[LaxPairGiven && name === Q3rhs, 
     Print["Using PowerExpand to simplify!"];
     compatibilityeq21onlattice = 
     MapAll[PowerExpand, Numerator[compatibilityeq21onlattice]]
    ];

If[debugComputationOftAnds4s || True,
   Print["Compatibility equation, (2-1) element ON LATTICE, "];
   Print["compatibilityeq21onlattice = "];
   Print[compatibilityeq21onlattice == 0]
   ];

If[debugComputationOftAnds4s,
  Print["Testing the compatibility condition for (2-2) element."];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ];

compatibilityeq22onlattice = 
  MapAll[Factor,compatibilityeq11 /. {tempx12 -> x12}];

  If[LaxPairGiven, (* begin if laxpairgiven *)
   If[controlrootrules, (* begin if controlrootrules *)
     compatibilityeq22onlattice = 
     MapAll[Factor, 
     Numerator[compatibilityeq22onlattice //. rootrule1 //. rootrule2abs]/
     Denominator[compatibilityeq22onlattice //. rootrule1 //. rootrule2abs]];
     If[debugComputationOftAnds4s,
       Print["Abs value might have been replaced by Factor!"]
       ];
     compatibilityeq22onlattice = 
       Factor[compatibilityeq22onlattice /. {Abs -> Factor}]
    ] (* end if controlrootrules *)
   ]; (* end if laxpairgiven *)

  If[LaxPairGiven && name === Q3rhs, 
     Print["Using PowerExpand to simplify!"];
     compatibilityeq22onlattice = 
     MapAll[PowerExpand, Numerator[compatibilityeq22onlattice]]
    ];

If[debugComputationOftAnds4s || True,
   Print["Compatibility equation, (2-2) element ON LATTICE, "];
   Print["compatibilityeq22onlattice = "];
   Print[compatibilityeq22onlattice == 0]
  ];

(* ********************************************************************* *)

If[controlcompatibilitytesting && !LaxPairGiven, 
  (* begin if controlcompatibilitytesting && !LaxPairGiven *)
Print["********************************************************************"];

 (* begin if controlcompatibilitytesting *)
 Print["Compatibility testing of elements of matrix equation L2.M - M1.L = 0"];

 t2soleq11 = MapAll[Factor,Flatten[Solve[compatibilityeq11 == 0, t2]]];
If[debugComputationOftAnds4s,
  Print["Solution for t2 (using compatibilityeq11), t2soleq11 = "];
  Print[t2soleq11]
  ];

 candidatet2eq11 = Map[Factor, t2 /. t2soleq11];
If[debugComputationOftAnds4s,
  Print["Candidate t2 (from compatibilityeq11), candidatet2eq11 = "];
  Print[candidatet2eq11]
  ];

 fractioneq11 = MapAll[Factor, candidatet2eq11 /.
                       {s1 -> 1, t -> 1, s -> 1} ];

If[debugComputationOftAnds4s,
If[controlcomputationmatrixN, 
  Print["Fraction (t2 s)/(t s1) (fractioneq11, from "<>
        "compatibilityeq11 NOT evaluated on lattice)"];
  Print["(t2 s)/(t s1) = "];
  Print[fractioneq11]
  ]
 ];

 candidatet2onlatticeeq11 = MapAll[Factor,candidatet2eq11 /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
  Print["Candidate t2 (from compatibilityeq11 and "]; 
  Print["EVALUATED ON LATTICE) (candidatet2onlatticeeq11)"];
  Print["t2 = "];
  Print[candidatet2onlatticeeq11]
 ];

 fractiononlatticeeq11 = MapAll[Factor, candidatet2onlatticeeq11 /.
                                {s1 -> 1, t -> 1, s -> 1} ];
 Print["Fraction (t2 s)/(t s1) (fractiononlatticeeq11, from "];
 Print["compatibilityeq11 and EVALUATED ON LATTICE)"];
 Print["(t2 s)/(t s1) = "];
 Print[fractiononlatticeeq11];

 t2soleq12 = MapAll[Factor,Flatten[Solve[compatibilityeq12 == 0, t2]]];
 (*
 Print["Solution for t2 (using compatibilityeq12), t2soleq12 = "];
 Print[t2soleq12];
 *)

 candidatet2eq12 = Map[Factor, t2 /. t2soleq12];
 (*
 Print["Candidate t2 (from compatibilityeq12), candidatet2eq12 = "];
 Print[candidatet2eq12];
 *)

 fractioneq12 = MapAll[Factor, candidatet2eq12 /.
                       {s1 -> 1, t -> 1, s -> 1} ];

If[debugComputationOftAnds4s,
If[controlcomputationmatrixN, 
   Print["Fraction (t2 s)/(t s1) (fractioneq12, from compatibilityeq12 "<>
         "NOT evaluated on lattice)"];
   Print["(t2 s)/(t s1) = "];
   Print[fractioneq12]
  ]
 ];

 candidatet2onlatticeeq12 = MapAll[Factor,candidatet2eq12 /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
   Print["Candidate t2 (from compatibilityeq12 and "];
   Print["EVALUATED ON LATTICE) (candidatet2onlatticeeq12)"];
   Print["t2 = "];
   Print[candidatet2onlatticeeq12]
  ];

 fractiononlatticeeq12 = MapAll[Factor, candidatet2onlatticeeq12 /.
                                {s1 -> 1, t -> 1, s -> 1} ];
 Print["Fraction (t2 s)/(t s1) (fractiononlatticeeq12, from "];
 Print["compatibilityeq12 and EVALUATED ON LATTICE)"];
 Print["(t2 s)/(t s1) = "];
 Print[fractiononlatticeeq12];

 t2soleq21 = MapAll[Factor,Flatten[Solve[compatibilityeq21 == 0, t2]]];
 (*
 Print["Solution for t2 (using compatibilityeq21), t2soleq21 = "];
 Print[t2soleq21];
 *)

 candidatet2eq21 = Map[Factor, t2 /. t2soleq21];
 (*
 Print["Candidate t2 (from compatibilityeq21), candidatet2eq21 = "];
 Print[candidatet2eq21];
 *)

 fractioneq21 = MapAll[Factor, candidatet2eq21 /.
                       {s1 -> 1, t -> 1, s -> 1} ];
If[debugComputationOftAnds4s,
If[controlcomputationmatrixN, 
  Print["Fraction (t2 s)/(t s1) (fractioneq21, from compatibilityeq21 "<>
        "NOT evaluated on lattice)"];
  Print["(t2 s)/(t s1) = "];
  Print[fractioneq21]
  ]
 ];

 candidatet2onlatticeeq21 = MapAll[Factor,candidatet2eq21 /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
  Print["Candidate t2 (from compatibilityeq21 and "];
  Print["EVALUATED ON LATTICE) (candidatet2onlatticeeq21)"];
  Print["t2 = "];
  Print[candidatet2onlatticeeq21]
  ];

 fractiononlatticeeq21 = MapAll[Factor, candidatet2onlatticeeq21 /.
                                {s1 -> 1, t -> 1, s -> 1} ];
 Print["Fraction (t2 s)/(t s1) (fractiononlatticeeq21, from "];
 Print["compatibilityeq21 and EVALUATED ON LATTICE)"];
 Print["(t2 s)/(t s1) = "];
 Print[fractiononlatticeeq21];

 t2soleq22 = MapAll[Factor,Flatten[Solve[compatibilityeq22 == 0, t2]]];
 (*
 Print["Solution for t2 (using compatibilityeq22), t2soleq22 = "];
 Print[t2soleq22];
 *)

 candidatet2eq22 = Map[Factor, t2 /. t2soleq22];
 (*
 Print["Candidate t2 (from compatibilityeq22), candidatet2eq22 = "];
 Print[candidatet2eq22];
 *)

 fractioneq22 = MapAll[Factor, candidatet2eq22 /.
                       {s1 -> 1, t -> 1, s -> 1} ];

If[debugComputationOftAnds4s,
 If[controlcomputationmatrixN, 
   Print["Fraction (t2 s)/(t s1) (fractioneq22, from compatibilityeq22 "<>
         "NOT evaluated ON LATTICE)"];
   Print["(t2 s)/(t s1) = "];
   Print[fractioneq22]
   ]
 ];

 candidatet2onlatticeeq22 = MapAll[Factor,candidatet2eq22 /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
  Print["Candidate t2 (from compatibilityeq22 and"];
  Print["EVALUATED ON LATTICE) (candidatet2onlatticeeq22)"];
  Print["t2 = "];
  Print[candidatet2onlatticeeq22]
  ];

 fractiononlatticeeq22 = MapAll[Factor, candidatet2onlatticeeq22 /.
                                {s1 -> 1, t -> 1, s -> 1} ];
 Print["Fraction (t2 s)/(t s1) (fractiononlatticeeq22, from "];
 Print["compatibilityeq22 and EVALUATED ON LATTICE)"];
 Print["(t2 s)/(t s1) = "];
 Print[fractiononlatticeeq22]
 ]; (* end if controlcompatibilitytesting && !LaxPairGiven *)

(* *********************************************************************** *)

If[!LaxPairGiven, (* start if !LaxPairGiven *)
 If[debugComputationOftAnds4s,
    Print["******************************************************************"]
   ];

If[debugComputationOftAnds4s || True,
  If[detmethod,
   Print["******************************************************************"];
   Print["************* CRUCIAL STEP: COMPUTATION OF t AND s ***************"];
   Print["The subsequent computations might be slow for complicated lattices"]
   ];
  If[byhand,
  Print["******************************************************************"];
  Print["****** CRUCIAL STEP: TESTING t AND s GIVEN IN THE DATA FILE ******"];
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"]
  ]
 ];

(* Solving one of the compatibility equations for t2 *)
(* IMPORTANT: needs to be more generic, should solve the simplest equation *)

(* Pieces of algorithm to find t and s. Needs further work once the issues *)
(* with lattices A1 and A2 are resolved! *)

(* New strategy based on determinant of candidateL and candidateM *)

alllattices = True;
(* GENERIC CODE for all lattices, but with sub-branches inside for *)
(* different lattices and different choices of t and s *)
If[alllattices, (* start if alllattices *)
(* 
name===pKdV  || name==cpKdV  || name===gcpKdV || name===gpKdV  ||
name===abeq  || name===cabeq || name===Q3zero || name===Q3rhs  || 
name===sG    || name===mKdV  || name===Q1rhs  || name===Q1zero || 
name===Q2    || name===Q3rhs 
*)
(* needed for compatibility test *)
(* for examples considered the compatibility condition for (2-1) element *)
(* was simplest, i.e. least number of factors *)

t2sol = MapAll[Factor,Flatten[Solve[compatibilityeq21 == 0, t2]]];
If[debugComputationOftAnds4s,
  Print["Solution for t2 (using compatibilityeq21), t2sol = "];
  Print[t2sol]
  ];

candidatet2 = Map[Factor, t2 /. t2sol];
If[debugComputationOftAnds4s,
  Print["Candidate t2 (from compatibilityeq21), (candidatet2)"];
  Print["t2 = "];
  Print[candidatet2]
  ];

fraction = MapAll[Factor, candidatet2 /.
                  {s1 -> 1, t -> 1, s -> 1} ];
If[debugComputationOftAnds4s,
  Print["Fraction (t2 s)/(t s1) (fraction, from compatibilityeq21 "<>
        "NOT evaluated ON LATTICE)"];
  Print["(t2 s)/(t s1) = "];
  Print[fraction]
  ];

candidatet2onlattice = MapAll[Factor,candidatet2 /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
  Print["Candidate t2 (from compatibilityeq21 and "];
  Print["EVALUATED ON LATTICE) (candidatet2onlattice)"];
  Print["t2 = "];
  Print[candidatet2onlattice]
  ];

fractiononlattice = MapAll[Factor, candidatet2onlattice /.
                           {s1 -> 1, t -> 1, s -> 1} ];
If[debugComputationOftAnds4s,
  Print["Fraction (t2 s)/(t s1) (fractiononlattice, from "];
  Print["compatibilityeq21 and EVALUATED ON LATTICE)"];
  Print["(t2 s)/(t s1) = "];
  Print[fractiononlattice]
  ];

(* ********************************************************************* *)

If[debugComputationOftAnds4s,
  Print["*******************************************************************"]
  ];

(* GENERIC CODE for all lattice needed for computation of matrixN *)
(* and for comparison with tcondition and scondition given by hand *)

matrixLc = Map[Factor,candidateL/t];
If[debugComputationOftAnds4s,
  Print["Candidate matrix Lc (candidate L without factor t):"]; 
  Print["matrixLc = "];
  Print[matrixLc // MatrixForm]
  ];

Print["*********************************************************************"];
determinantmatrixLc = Factor[Det[matrixLc]];
If[debugComputationOftAnds4s || True,
  Print["Determinant of matrix Lc, det(matrixLc) = "];
  Print[determinantmatrixLc]
  ];

matrixMc = Map[Factor,candidateM/s];
If[debugComputationOftAnds4s,
  Print["Candidate matrix Mc (candidate M without factor s):"]; 
  Print["matrixMc = "];
  Print[matrixMc // MatrixForm]
  ];

determinantmatrixMc = Factor[Det[matrixMc]];
If[debugComputationOftAnds4s || True,
  Print["Determinant of matrix Mc, det(matrixMc) = "];
  Print[determinantmatrixMc]
  ];
Print["*********************************************************************"];

(* ********************************************************************** *)

If[debugComputationOftAnds4s,
  Print["********************************************************************"]
  ];

(* ALGORITHM to compute t and s *)
(* Computation of Det[matrixLc], matrixLc is candidateL without t *)
(* Computation of Det[matrixMc], matrixMc is candidateM without s *)

If[detmethod, (* begin if detmethod *)
  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"];
  tcondition = 1/Sqrt[determinantmatrixLc];
  (* WH 07-21-08 introduce a factor eps1 *)
  eps1 = 1;
  tcondition = eps1*tcondition;
 If[debugComputationOftAnds4s || True,
   Print["-----> Using algorithm (detmethod)! Coefficient t (tcondition)"];
(* Print["NOT evaluated ON LATTICE,"]; *)
   Print["t = "];
   Print[tcondition];
   Print["*******************************************************************"]
   ];

 scondition = 1/Sqrt[determinantmatrixMc];
  (* WH 07-21-08 introduce a factor eps2 *)
  eps2 = 1;
  scondition = eps2*scondition;
 If[debugComputationOftAnds4s || True,
   Print["------> Using algorithm (detmethod)! Coefficient s (scondition)"];
(* Print["NOT evaluated ON LATTICE,"]; *)
   Print["s = "];
   Print[scondition];
   Print["*******************************************************************"]
  ]
 ]; (* end if detmethod *)

(* begin if by hand *)
If[byhand, (* begin if by hand *)
  Print["----> Candidates for t and s are given in the data file!"];
  Print["In the data file, t = ", tchoice];
  Print["In the data file, s = ", schoice];
  numcandidatet2onlattice = Numerator[candidatet2onlattice /. 
                                      {s1 -> 1, t -> 1, s -> 1} ];
 If[debugComputationOftAnds4s,
  Print["Numerator candidate t2 (from compatibilityeq21 and "];
  Print["EVALUATED ON LATTICE)"];
  Print["numcandidatet2onlattice = "];
  Print[numcandidatet2onlattice];
  Print["INFO: If numcandidatet2onlattice can be factored then tchoice might"];
  Print["be the reciprical of one of the factors of numcandidatet2onlattice!"];
  Print["----> t is given in the data file (as tchoice)"]; 
  Print["t = ", tchoice];
  Print["tcondition is set equal to tchoice"]
  ];
 tcondition = tchoice;
 (* WH 07-21-08 introduce a factor eps1 *)
 eps1 = 1;
 tcondition = eps1*tcondition;
  If[debugComputationOftAnds4s, 
    Print["Introduce a factor eps1, tcondition =",tcondition]
    ];
 If[debugComputationOftAnds4s, 
    Print["tcondition =",tcondition]
   ];
 
 denomcandidatet2onlattice = Denominator[candidatet2onlattice /.
                                         {s1 -> 1, t -> 1, s -> 1}];
 If[debugComputationOftAnds4s,
  Print["Denominator candidate t2 (from compatibilityeq21 and " ];
  Print["EVALUATED ON LATTICE)"];
  Print["denomcandidatet2onlattice = "];
  Print[denomcandidatet2onlattice];
  Print["INFO: If denomcandidatet2onlattice can be factored then "];
  Print["schoice might be the reciprical of one of the factors"];
  Print["of denomcandidatet2onlattice!"];
  Print["----> s is given in the data file (as schoice)"]; 
  Print["s = ", schoice];
  Print["scondition is set equal to schoice"]
 ];
 scondition = schoice;
 (* WH 07-21-08 introduce a factor eps2 *)
 eps2 = 1;
 scondition = eps2*scondition;
  If[debugComputationOftAnds4s, 
    Print["Introduce a factor eps2, scondition =",scondition]
    ];
 If[debugComputationOftAnds4s,
  Print["scondition =",scondition];
  Print["Compatibility of t and s with the algorithm"];
  Print["will be checked!"]
  ]
 ]; (* end if by hand *)

(* ***************************************************************** *)

Print["************************* TESTING t AND s ***************************"];
Print["The subsequent computations might be slow for complicated lattices,"<>
      " in particular, if square roots are present!"];

(* GENERIC CODE for all lattices *)
(* executed for byhand as well as detmethod *)

t2condition = MapAll[Factor,tcondition /. {x -> x2, x1 -> tempx12}];
If[debugComputationOftAnds4s,
  Print["Condition for coefficient t2, NOT evaluated ON LATTICE"];
  Print["t2condition = "];
  Print[t2condition]
  ];

t2conditiononlattice = MapAll[Factor,t2condition /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
  Print["Condition for coefficient t2, ON LATTICE, t2conditiononlattice = "];
  Print[t2conditiononlattice]
  ];

s1condition = MapAll[Factor,scondition /. {x -> x1, x2 -> tempx12}];
If[debugComputationOftAnds4s,
  Print["Condition for coefficient s1, NOT evaluated ON LATTICE"];
  Print["s1condition = "];
  Print[s1condition]
  ];

s1conditiononlattice = MapAll[Factor, s1condition /. {tempx12 -> x12}];
If[debugComputationOftAnds4s,
  Print["Condition for coefficient s1, ON LATTICE, s1conditiononlattice = "];
  Print[s1conditiononlattice]
  ];

allstrules = 
  {s -> scondition, t -> tcondition, s1 -> s1condition, t2 -> t2condition};
If[debugComputationOftAnds4s,
  Print["Based on t and s, all substitution rules, "<>
        "NOT evaluated ON LATTICE,"];
  Print["allstrules = "];
  Print[allstrules]
  ];

allstrulesonlattice = 
  {s -> scondition, 
   t -> tcondition, 
   s1 -> s1conditiononlattice, 
   t2 -> t2conditiononlattice};
If[debugComputationOftAnds4s,
 Print["Based on t and s, all substitution rules,"];
 Print["evaluated ON LATTICE"];
 Print["allstrulesonlattice = "];
 Print[allstrulesonlattice]
 ];

(* check of compatibility -- compute the fraction (t2*s)/(t*s1) *)
(* but evaluated on lattice *)
(* Does not always simplify to correct expression due to lack of *)
(* simplifications of roots with Mathematica *)

If[debugComputationOftAnds4s,
  Print["Start test by computing ratio (t2 s)/(t s1) in two ways!"]
  ];

testfractiononlattice = Factor[MapAll[Factor, 
   (t2conditiononlattice*scondition)/(tcondition*s1conditiononlattice)]];
If[debugComputationOftAnds4s,
  Print["Based on s and t, testfraction (t2*s)/(t*s1) ON LATTICE,"];
  Print[testfractiononlattice]
  ];
(* First test without the extra simplifications *)
 If[debugComputationOftAnds4s,
   Print["testfractiononlattice = "];
   Print[testfractiononlattice];
   Print["This expression should MATCH fraction ON LATTICE:"];
   Print[fractiononlattice]
   ];
 diffbothfractions = Factor[fractiononlattice - testfractiononlattice];
(* WH: 07-19-08 *)
 diffbothfractionstobetested = diffbothfractions;
 If[diffbothfractions === 0, (* begin if diffbothfractions *)
    Print["Without introducing extra root rules or absolute value rules,"];
    Print["t and s PASS the SYMBOLIC TEST!"], (* else *)
    Print["t and s DO NOT PASS the SYMBOLIC TEST YET."];
    Print["Either t and s do not satisfy the condition(s),"];
    Print["OR, more likely, Mathematica could not simplify the expression."];
    (* start if numericTesttands *)
    If[numericTesttands, 
      Print["The result will be numerically tested, with 12 tests where "];
      Print["the symbols are replaced by randomly selected integers."];
      (* HERE 1 Test t and s *)
      resultNumericTests = {};
      Do[ (* begin do loop for numeric testing *)
         If[debugNumericTesttands, 
            Print["Start of numerical TEST ", kk]
           ];
(*
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,4}][[Random[Integer,{1,4}]]], 
        q -> Table[Prime[mm],{mm,5,8}][[Random[Integer,{1,4}]]], 
        k -> Table[Prime[mm],{mm,9,12}][[Random[Integer,{1,4}]]], 
        alpha -> Table[Prime[mm],{mm,13,16}][[Random[Integer,{1,4}]]], 
        beta -> Table[Prime[mm],{mm,17,20}][[Random[Integer,{1,4}]]], 
        delta -> Table[Prime[mm],{mm,21,24}][[Random[Integer,{1,4}]]],
        x -> Table[Prime[mm],{mm,25,28}][[Random[Integer,{1,4}]]],  
        x1 -> Table[Prime[mm],{mm,29,32}][[Random[Integer,{1,4}]]], 
        x2 -> Table[Prime[mm],{mm,33,36}][[Random[Integer,{1,4}]]] 
      };
*)
(*
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,6}][[Random[Integer,{1,6}]]], 
        q -> Table[Prime[mm],{mm,7,12}][[Random[Integer,{1,6}]]], 
        k -> Table[Prime[mm],{mm,13,18}][[Random[Integer,{1,6}]]], 
        alpha -> Table[Prime[mm],{mm,19,24}][[Random[Integer,{1,6}]]], 
        beta -> Table[Prime[mm],{mm,25,30}][[Random[Integer,{1,6}]]], 
        delta -> Table[Prime[mm],{mm,49,54}][[Random[Integer,{1,6}]]],
        x -> Table[Prime[mm],{mm,31,36}][[Random[Integer,{1,6}]]],  
        x1 -> Table[Prime[mm],{mm,37,42}][[Random[Integer,{1,6}]]], 
        x2 -> Table[Prime[mm],{mm,43,48}][[Random[Integer,{1,6}]]] 
      };
*)

(* now *)
      randomRules = 
        { 
        x -> Table[Prime[mm]+mm^3,{mm,1,8}][[Random[Integer,{1,8}]]], 
        p -> Table[Prime[mm]+mm,{mm,9,16}][[Random[Integer,{1,8}]]], 
        x2 -> Table[Prime[mm]+mm^2,{mm,17,24}][[Random[Integer,{1,8}]]], 
        x1 -> Table[Prime[mm],{mm,25,32}][[Random[Integer,{1,8}]]],  
        q -> Table[Prime[mm]+3*mm,{mm,33,40}][[Random[Integer,{1,8}]]], 
        k -> Table[Prime[mm],{mm,41,48}][[Random[Integer,{1,8}]]],
        alpha -> Table[Prime[mm],{mm,49,56}][[Random[Integer,{1,8}]]], 
        beta -> Table[Prime[mm],{mm,57,64}][[Random[Integer,{1,8}]]], 
        delta -> Table[Prime[mm],{mm,65,72}][[Random[Integer,{1,8}]]]
      };
(* now *)

(* was 
      randomRules = 
        { 
        x -> Table[Prime[mm],{mm,1,8}][[Random[Integer,{1,8}]]], 
        x1 -> Table[Prime[mm],{mm,9,16}][[Random[Integer,{1,8}]]], 
        x2 -> Table[Prime[mm],{mm,17,24}][[Random[Integer,{1,8}]]], 
        p -> Table[Prime[mm],{mm,25,32}][[Random[Integer,{1,8}]]],  
        q -> Table[Prime[mm],{mm,33,40}][[Random[Integer,{1,8}]]], 
        k -> Table[Prime[mm],{mm,41,48}][[Random[Integer,{1,8}]]],
        alpha -> Table[Prime[mm],{mm,49,56}][[Random[Integer,{1,8}]]], 
        beta -> Table[Prime[mm],{mm,57,64}][[Random[Integer,{1,8}]]], 
        delta -> Table[Prime[mm],{mm,65,72}][[Random[Integer,{1,8}]]]
      };
end was *)

      If[debugNumericTesttands, 
        Print["Selected numbers for the symbols, randomRules = "];
        Print[randomRules]
        ];

      (* Numerical test of result by replacing all the symbols with the *)
      (* randomly selected integers. *)
      numericTestValue = 
        Map[Factor,diffbothfractions /. randomRules];
      If[numericTestValue =!= 0, 
        numericTestValue = MapAll[Factor, numericTestValue]
        ];
       If[numericTestValue =!= 0, 
         numericTestValue = MapAll[Simplify, numericTestValue]
         ];
      If[debugNumericTesttands, 
        Print["The numerical test value, numericTestValue = "];
        Print[numericTestValue]
        ];

       (* WH: 07-22-08 added a decimal numerical test up to *)
       (* half the machine precision *)
       If[decimalNumericTesttands,
         decimalNumericTestValue = N[Abs[numericTestValue],10];
         If[debugDecimalNumericTesttands,
            Print["The decimal test value, decimalNumericTestValue = "];
            Print[decimalNumericTestValue]
            ];
         If[ decimalNumericTestValue < 10^(-$MachinePrecision/2), (* then *)
            If[debugDecimalNumericTesttands,
              CellPrint[Cell["The decimal test value did simplify to "<>
                    "less than 10^("<> ToString[-$MachinePrecision/2] <>")."]]
             ], (* else *)
            If[debugDecimalNumericTesttands,
              CellPrint[Cell["These decimal test value did not simplify to "<>
                   "less than 10^(" <> ToString[-$MachinePrecision/2] <>" )."]]
             ]
           ]
        ];

       (* WH: 07-21-08: introduce the absolute value rule for *)
       (* the arguments of the sqrt *)
       absvalueruleroot = {Sqrt[zzz__] -> Sqrt[Abs[zzz]]};
       If[numericTestValue =!= 0,
          absdiffbothfractions = diffbothfractions /. absvalueruleroot; 
          absnumericTestValue = 
          Map[Factor, absdiffbothfractions /. randomRules];
          If[debugNumericTesttands, 
            Print["After using the absolute value rule, absnumericTestValue="];
            Print[absnumericTestValue]
            ], (* else when zero *)
          absnumericTestValue = 0
          ];

      (* WH: 07-21-08: if both are zero (testing t and s) *)
      If[( numericTestValue =!= 0 && absnumericTestValue =!= 0 ),
        Print["Numerical test failed, check if result is free of parameters"];
        Print["numericTestValue = "];
        Print[numericTestValue];
        Print["Numerical (absolute value) test also failed,"];
        Print["Check if result is free of parameters"];
        Print["absnumericTestValue = "];
        Print[absnumericTestValue];
        If[numericTestValue === Indeterminate, 
           Print["Ignore the test that failed because the numericTestValue"];
           Print["was Indeterminate"]
          ]
        ];
      
      If[debugNumericTesttands, 
        Print["This is the result of the absolute numeric test,"<>
              " absnumericTestValue = "];
        Print[absnumericTestValue]
        ];
(* *)
      If[debugNumericTesttands, 
        Print["Before appending the list with absnumericTestValue, "<>
              "resultNumericTests = "];
        Print[resultNumericTests]
        ];
(* *)

(* WH: 07-21-08 change to absnumericTestValue *)
(*     resultNumericTests = Append[resultNumericTests, numericTestValue]; *)

       resultNumericTests = Append[resultNumericTests, absnumericTestValue];

(* *)
      If[debugNumericTesttands, 
        Print["After appending the list with absnumericTestValue, "<> 
              "resultNumericTests = "];
        Print[resultNumericTests]
        ];
(* *)

      resultNumericTests = Union[resultNumericTests];

(* *)
      If[debugNumericTesttands, 
        Print["After applying Union to the list, resultNumericTests = "];
        Print[resultNumericTests]
        ],
(* *)
       {kk, 1, 12}
      ]; (* end do loop *)
      If[resultNumericTests === {0}, 
       Print["Based on t and s (given in the data file),"];
       Print["the ratio (t2*s)/(t*s1) has been computed in two ways."];
       Print["The comparison PASSES the 12 NUMERIC TESTS"];
       Print["(based on randomly selected integers)."];
       Print["-----> t and s (given in the data file) appear to be CORRECT!"],
       (* else *)
(* *)       
      If[debugNumericTesttands, 
        Print["The list of numerical tests, resultNumericTests: "];
        Print[resultNumericTests]
        ]; 
(* *)
       intersec = Intersection[resultNumericTests, {0} ]; 
(* *)
      If[debugNumericTesttands, 
         Print["The intersection of the list with {0}, intersec = "];
         Print[intersec]
         ]; 
(* *)
       rest = Complement[resultNumericTests, intersec];
(* *)
      If[debugNumericTesttands, 
         Print["After removing the intersection from the list, rest = "];
         Print[rest]
        ]; 
(* *)
       lenrest = Length[rest];
       Print["Based on t and s (given in the data file),"];
       Print["the ratio (t2*s)/(t*s1) has been computed in two ways."];
       Print["The comparison FAILED ",lenrest," of 12 numeric tests"];
       Print["(based on randomly selected integers)."];
       Print["-----> and s (given in the data file) appear to be INCORRECT!"];
       Print["Ignore the tests that failed because the numericTestValue"];
       Print["was Indeterminate (due to division by zero)."];
       Print["Turn debug flags on (in the code) and retest t and s!"]
       ]
      ]; (* end if numericTesttands *)
    Print["If controlpowerexpand and/or controlrootrules are set to True"];
    Print["in the data file, then further simplifications will be attempted."]
    ]; (* end if diffbothfractions *)

If[controlpowerexpand, (* begin if controlpowerexpand *)
 If[debugComputationOftAnds4s,
   Print["!!! Using PowerExpand function to further simplify !!!"]
  ];
  testfractiononlattice = 
  Factor[PowerExpand[MapAll[Factor,testfractiononlattice]]];
 If[debugComputationOftAnds4s,
   Print["!!! Using rootrule1 to further simplify !!!"]
   ];
  testfractiononlattice = 
      MapAll[Factor,testfractiononlattice /. rootrule1]; 
 If[debugComputationOftAnds4s,
   Print["!!! Using rootrule2 to further simplify !!!"]
   ];
  testfractiononlattice = 
      MapAll[Factor,testfractiononlattice /. rootrule2];
 If[debugComputationOftAnds4s,
   Print["!!! After using the PowerExpand function !!!"];
   Print["Based on the given s and t, testfraction (t2*s)/(t*s1) ON LATTICE"];
   Print[testfractiononlattice]
  ]
 ]; (* end if controlpowerexpand *)

If[controlpowerexpand, 
  If[debugComputationOftAnds4s,
    Print["!!! Using PowerExpand function to further simplify numerator !!!"]
   ];
  testnumfractiononlattice = 
  Factor[PowerExpand[MapAll[Factor,Numerator[testfractiononlattice]]]];
  ];

If[controlrootrules, (* start if controlrootrules *)
  testnumfractiononlattice = 
  Numerator[Factor[MapAll[Factor,testfractiononlattice]]];
  If[debugComputationOftAnds4s,
    Print["!!! Using rootrule1 on numerator to further simplify !!!"]
   ];
  testnumfractiononlattice = 
      MapAll[Factor,testnumfractiononlattice //. rootrule1]; 
  If[debugComputationOftAnds4s,
    Print["!!! Using rootrule2abs on numerator to further simplify !!!"]
   ];
  testnumfractiononlattice = 
      MapAll[Factor,testnumfractiononlattice //. rootrule2abs];
  If[debugComputationOftAnds4s,
     Print["After using rootrule1 and rootrule2abs, testnumfractiononlattice"];
     Print[testnumfractiononlattice]
    ];
   If[name===Q3rhs || name===abeq, 
     testnumfractiononlattice = 
        Map[Factor,testnumfractiononlattice //. rootrule1gen];
     If[debugComputationOftAnds4s,
       Print["!!! After repeatedly using rootrule1gen !!!"];
       Print["testnumfractiononlattice = "];
       Print[testnumfractiononlattice]
(* 
Print["Expression should MATCH parts of numerator of fractiononlattice:"];
*)
(* Print[Numerator[fractiononlattice]] *)
     ] ] (* end if name===Q3rhs || name===abeq *)
 ];  (* end if controlrootrules *)

If[controlpowerexpand, 
If[debugComputationOftAnds4s,
  Print["!!! Using PowerExpand function to further simplify the "<>
        "denominator !!!"]
  ];
  testdenomfractiononlattice = 
  Factor[PowerExpand[MapAll[Factor,Denominator[testfractiononlattice]]]]
  ];

If[controlrootrules, (* begin of controlrootrules *)
  testdenomfractiononlattice = 
  Denominator[Factor[MapAll[Factor,testfractiononlattice]]];
 If[debugComputationOftAnds4s,
   Print["!!! Using rootrule1 to further simplify the denominator !!!"]
   ];
  testdenomfractiononlattice = 
      MapAll[Factor,testdenomfractiononlattice //. rootrule1]; 
  If[debugComputationOftAnds4s,
    Print["!!! Using rootrule2abs to further simplify the denominator !!!"]
    ];
  testdenomfractiononlattice = 
      MapAll[Factor,testdenomfractiononlattice //. rootrule2abs];
  If[debugComputationOftAnds4s, 
   Print["After using rootrule1 and rootrule2abs, testdenomfractiononlattice"];
   Print[testdenomfractiononlattice]
    ];
   If[name===Q3rhs || name===abeq, 
     testdenomfractiononlattice = 
        Map[Factor,testdenomfractiononlattice //. rootrule1gen];
    If[debugComputationOftAnds4s,
      Print["!!! After repeatedly using rootrule1gen !!!"];
      Print["testdenomfractiononlattice = "];
      Print[testdenomfractiononlattice]
     (* Print["Expression should MATCH parts of fractiononlattice:"]; *)
     (* Print[Denominator[fractiononlattice]] *)
     ] ] (* end if name===Q3rhs || name===abeq *)
  ]; (* end if controlrootrules *)

If[controlpowerexpand, 
   testfractiononlattice =     
   Factor[testnumfractiononlattice/testdenomfractiononlattice];
   If[debugComputationOftAnds4s,
     Print["!!! After using PowerExpand !!!"];
     Print["testfractiononlattice = "];
     Print[testfractiononlattice]
    ]
  ];

If[controlrootrules, 
   testfractiononlattice =     
   Factor[testnumfractiononlattice/testdenomfractiononlattice];
   If[debugComputationOftAnds4s,
     Print["!!! After using various rootrules !!!"];
     Print["testfractiononlattice = "];
     Print[testfractiononlattice];
     Print["This expression should MATCH fraction ON LATTICE:"];
     Print[fractiononlattice]
    ];
 diffbothfractions = Factor[fractiononlattice - testfractiononlattice];
 If[diffbothfractions === 0, 
    Print["After introducing rootrules to simplify the roots,"];
    Print["t and s PASS the SYMBOLIC TEST!"], (* else *)
    Print["After using various rules to simplify the roots (rootrules),"];
    Print["t and s DO NOT PASS the SYMBOLIC TEST YET."];
    Print["Either t and s do not satisfy the condition(s),"];
    Print["OR, Mathematica still could not simplify the expression."];
    Print["If controlpowerexpand is set to True in the data file, then"]; 
    Print["additional simplifications will be attempted."]
   ]
 ]; (* end if controlrootrules *)

If[controlpowerexpand, (* begin if controlpowerexpand *)
   If[debugComputationOftAnds4s,
     Print["FOR TESTING ONLY: Using PowerExpand to further simplify!"]
     ];
   newtestfractiononlattice= 
      MapAll[Factor,
             MapAll[PowerExpand,testnumfractiononlattice]/
             MapAll[PowerExpand,testdenomfractiononlattice]
             ];
   If[debugComputationOftAnds4s,
     Print["!!! After using PowerExpand !!!"];
     Print["newtestfractiononlattice = "];
     Print[newtestfractiononlattice];
     Print["This expression should MATCH fraction ON LATTICE:"];
     Print[fractiononlattice]
    ];
 diffbothfractions = Factor[fractiononlattice - newtestfractiononlattice];
 If[diffbothfractions === 0, 
    Print["After using powerexpand in various places,"];
    Print["t and s PASS the SYMBOLIC TEST!"], (* else *)
    Print["After using powerexpand in various places,"];
    Print["t and s DO NOT PASS the SYMBOLIC TEST YET."];
    Print["Either t and s do not satisfy the condition(s),"];
    Print["OR, Mathematica could not simplify the expression."];
    Print["A final set of simplifications will now be attempted."]
   ]
]; (* end if controlpowerexpand *)

If[controlpowerexpand, (* begin if controlpowerexpand *)
If[debugComputationOftAnds4s,
   Print["FOR TESTING ONLY:"]
   ];

(* EPSILON must be 1 or -1 *) 
If[debugComputationOftAnds4s,
  Print["!!! Replacing abs by factor (assuming arguments are positive) !!! "];
  Print["Introducing factor EPSILON (which should be 1 if sign is correct) "];
  Print["EPSILON will be -1 if sign is wrong (caused by PowerExpand)"]
  ];
testfractiononlatticeabsremoved = 
    Factor[testfractiononlattice /. {Abs -> Factor}];
epsilontestcompatibilityonlattice = 
  Factor[fractiononlattice - 
         EPSILON*testfractiononlatticeabsremoved (* /. EPSILON -> 1 *)
        ];
 If[debugComputationOftAnds4s,
   Print["After removing absolute values (Abs) "<>
         "(assuming arguments are postive)"];
   Print["Based on the given s and t, epsilontestcompatibilityonlattice "<>
         "ON LATTICE should be zero"];
   Print["If EPSILON is present, then EPSILON should be 1 (or -1)"];
   Print[epsilontestcompatibilityonlattice]
   ];

If[debugComputationOftAnds4s,
  Print["FOR TESTING ONLY: Using PowerExpand to simplify!"]
  ];
(* EPSILON must be 1 or -1 !!! *)
If[debugComputationOftAnds4s,
  Print["!!! Replacing abs by factor (assuming arguments are positive) !!! "];
  Print["Introducing factor EPSILON, which should be 1 if sign is correct "];
  Print["EPSILON will be -1 if sign is wrong (caused by PowerExpand) "]
  ];
newtestfractiononlatticeabsremoved = 
    Factor[newtestfractiononlattice /. {Abs -> Factor}];
epsilonnewtestcompatibilityonlattice = 
  Factor[fractiononlattice - 
         EPSILON*newtestfractiononlatticeabsremoved (* /. EPSILON -> 1 *)
        ];
If[debugComputationOftAnds4s,
  Print["After removing absolute values (Abs) "<>
        "(assuming arguments are postive)"];
  Print["Based on the given s and t, epsilonnewtestcompatibilityonlattice "<>
        "ON LATTICE should be zero"];
  Print["If EPSILON is present, then EPSILON should be 1 (or -1)"];
  Print[epsilonnewtestcompatibilityonlattice]
  ]
]; (* end if controlpowerexpand *)

If[controlpowerexpand, (* begin if controlpowerexpand *)
 If[debugComputationOftAnds4s,
  Print["!!! Using PowerExpand function to further simplify the numerator !!!"]
  ];
  epsilonnewtestcompatibilityonlattice = 
  Numerator[Factor[PowerExpand[
    MapAll[Factor,epsilonnewtestcompatibilityonlattice]]]];
 If[debugComputationOftAnds4s,
   Print["Based on the given s and t, epsilonnewtestcompatibilityonlattice "<>
         "ON LATTICE should be zero"];
   Print["Working with numerator of epsilonnewtestcompatibilityonlattice! "];
   Print["If EPSILON is present, then EPSILON should be 1"];
   Print[epsilonnewtestcompatibilityonlattice]
  ]
 ]; (* end if contolpowerexpand *)

If[controlpowerexpand, (* begin if controlpowerexpand *)
 (* Final test by setting EPSILON = 1 and EPSILON = -1 *)
 epsilononenewtestcompatibilityonlattice = 
   Factor[epsilonnewtestcompatibilityonlattice /. {EPSILON -> 1}];
 If[debugComputationOftAnds4s,
   Print["Test result after setting EPSILON = 1"];
   Print[epsilononenewtestcompatibilityonlattice]
   ];
 If[epsilononenewtestcompatibilityonlattice === 0, 
    Print["After introducing absolute values in various places,"];
    Print["t and s PASS the SYMBOLIC TEST!"], (* else *)
    Print["After introducing absolute values in various places,"];
    Print["t and s DO NOT PASS the SYMBOLIC TEST."];
    Print["Either t and s do not satisfy the condition(s),"];
    Print["OR, Mathematica could not simplify the expression."];
    Print["No further simplifications will be done."]
   ]
]; (* end if controlpowerexpand *)

Print["*********************************************************************"];

(* for NEW compatibility test in squared form *)
If[diffbothfractions=!=0, (* begin if diffbothfractions *)
  If[debugComputationOftAnds4s || True,
(* 
    Print["*****************************************************************"];
*)
    Print["******* START OF TEST OF THE CONSISTENCY IN SQUARED FORM *******"];
    Print["Software will verify if t2^2 = ( (t*s1)/s )^2 in two ways!"]
   ];
  t2conditionsquared = t2condition^2;
  t2conditiononlatticesquared = 
      Factor[t2conditionsquared /. {tempx12 -> x12}];
 If[debugTestCompatibilitySquared,
    Print["t2conditiononlatticesquared = "]; 
    Print[t2conditiononlatticesquared]
    ];
  evalcandidatet2onlattice = candidatet2onlattice /. 
                 {s -> scondition, t -> tcondition, s1 -> s1condition};
  evalcandidatet2onlattice = 
      Factor[evalcandidatet2onlattice /. {tempx12 -> x12}];
 If[debugTestCompatibilitySquared,
   Print["evalcandidatet2onlattice = "]; 
   Print[evalcandidatet2onlattice]
   ];
  evalcandidatet2onlatticesquared = evalcandidatet2onlattice^2;
 If[debugTestCompatibilitySquared,
    Print["evalcandidatet2onlatticesquared = "];
    Print[evalcandidatet2onlatticesquared]
    ];
  newtestcompatibilityonlatticesquared =   
  Factor[t2conditiononlatticesquared - evalcandidatet2onlatticesquared];
 If[debugTestCompatibilitySquared || True,
   Print["Based on the given s and t"];
   Print["newtestcompatibilityonlatticesquared ON LATTICE should be zero:"];
   Print["newtestcompatibilityonlatticesquared = "];
   Print[newtestcompatibilityonlatticesquared]
   ];
  If[newtestcompatibilityonlatticesquared=!=0, 
   If[debugTestCompatibilitySquared || True,
     Print["NEW compatibility test SQUARED for given t = ", tcondition];
     Print["and s =", scondition," is NOT SATISFIED!"];
     Print["If appropriate, apply simplification rules for (square) roots!"];
     Print["**** TEST RESULT IS NOT ZERO! **** "<>
           "newtestcompatibilityonlatticesquared must be zero:"];
     Print[newtestcompatibilityonlatticesquared]
     ];
    Abort[], (* else *)
   If[debugComputationOftAnds4s || True,
     Print["**** TEST RESULT = ",newtestcompatibilityonlatticesquared," ****"];
     Print["(SHOULD BE ZERO)"];
     Print["NEW compatibility TEST (squared) for t and s is SATISFIED!"]
    ]
   ];
 Print["*********************************************************************"]
   ] (* end of diffbothfractions *)
  ] (* end if alllattices *)
 ]; (* end if !LaxPairGiven *)

(* 
name===pKdV  || name==cpKdV  || name===gcpKdV || name===gpKdV  ||
name===abeq  || name===cabeq || name===Q3zero || name===Q3rhs  || 
name===sG    || name===mKdV  || name===Q1rhs  || name===Q1zero || 
name===Q2    || name===Q3rhs 
*)

(* ****************************************************************** *)

(* GENERIC CODE for all lattices *)
(* needed for computation of matrix N *)
(* ********************************************************************** *)

If[debugComputationOftAnds4s,
  Print["*******************************************************************"]
  ];

(* GENERIC CODE for all lattices *)
(* Computation of final L, evaluated ON LATTICE *)

If[!LaxPairGiven, (* begin if !LaxPairGiven *)
  Print["Using t = ", tcondition," and s = ",scondition];

 (* Form of Lax pair matrices, L and M for s = scondition, t = tcondition *)
 (* and consequences for s1, t2 *)

 matrixL = Map[Factor,candidateL /. allstrules]; 

 Print["Lax pair, matrix L (matrixL) for t = ", tcondition," "];
 Print["********************************************************************"];
 Print["L = "];
 Print[matrixL // MatrixForm];
 Print["********************************************************************"];

 (* Entries of matrixL are available separately *)
 Print["The elements of L below are for t = ", tcondition];

 entryL11 = matrixL[[1]][[1]];
 Print["Entry L11 (entryL11)"];
 Print["L11 = "];
 Print[entryL11];

 entryL12 = matrixL[[1]][[2]];
 Print["Entry L12 (entryL12)"];
 Print["L12 = "];
 Print[entryL12];

 entryL21 = matrixL[[2]][[1]];
 Print["Entry L21 (entryL21)"];
 Print["L21 = "];
 Print[entryL21];

 entryL22 = matrixL[[2]][[2]];
 Print["Entry L22 (entryL22)"];
 Print["L22 = "];
 Print[entryL22]
]; (* end if !LaxPairGiven *)

(* ******************************************************************** *)

(* GENERIC CODE for all lattices *)
(* Computation of final M, evaluated ON LATTICE *)

If[!LaxPairGiven, (* begin if !LaxPairGiven *)
 Print["********************************************************************"];
 matrixM = Map[Factor,candidateM /. allstrules]; 

 Print["Lax pair, matrix M (matrixM) for s = ",scondition," "];
 Print["********************************************************************"];
 Print["M = "];
 Print[matrixM // MatrixForm];
 Print["********************************************************************"];

 (* Entries of matrixM are available separately *)
 Print["The elements of M below are for s = ", scondition];

 entryM11 = matrixM[[1]][[1]];
 Print["Entry M11 (entryM11)"];
 Print["M11 = "];
 Print[entryM11];

 entryM12 = matrixM[[1]][[2]];
 Print["Entry M12 (entryM12)"];
 Print["M12 = "];
 Print[entryM12];

 entryM21 = matrixM[[2]][[1]];
 Print["Entry M21 (entryM21)"];
 Print["M21 = "];
 Print[entryM21];

 entryM22 = matrixM[[2]][[2]];
 Print["Entry M22 (entryM22)"];
 Print["M22 = "];
 Print[entryM22]
 ]; (* end if !LaxPairGiven *)

(* ******************************************************************** *)

(* GENERIC CODE for all lattices *)
(* Direct verification of computed Lax pair mattrices L and M *)

If[!LaxPairGiven, (* start of Not LaxPairGiven *)
   Print["******************************************************************"];
   Print["************** FINAL TEST OF THE COMPUTED LAX PAIR ***************"];
   matrixL2 = Map[Factor, matrixL /. {x -> x2, x1 -> tempx12}];
   productL2M = Map[Factor, matrixL2.matrixM];
   If[debugFinalSubstitutionInLaxEquation, 
     Print["Product final L2.M, NOT yet evaluated ON LATTICE, L2.M = "];
     Print[productL2M // MatrixForm]
     ];

   matrixM1 = Map[Factor, matrixM /. {x -> x1, x2 -> tempx12}];
   productM1L = Map[Factor, matrixM1.matrixL];
   If[debugFinalSubstitutionInLaxEquation, 
      Print["Product final M1.L, NOT yet evaluated ON LATTICE, M1.L = "];
      Print[productM1L // MatrixForm]
     ];

  finalcompatibilitymatrix = Map[Factor,productL2M-productM1L];

   If[debugFinalSubstitutionInLaxEquation,
    Print["Compatibility matrix L2.M - M1.L, NOT yet evaluated ON LATTICE,"];
    Print["finalcompatibilitymatrix = "];
    Print[finalcompatibilitymatrix // MatrixForm]
    ];

  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"];
  finalcompatibilitymatrixonlattice = 
       MapAll[Factor,finalcompatibilitymatrix /. {tempx12 -> x12}];

(*   
  finalcompatibilitymatrixonlattice = 
       finalcompatibilitymatrixonlattice /. rootrule1; 
*)

(* PowerExpand is not reliable: may change the signs !!! *)
(*
  If[controlpowerexpand, 
     finalcompatibilitymatrixonlattice = 
          MapAll[PowerExpand,finalcompatibilitymatrixonlattice]
    ];
*)
  If[debugFinalSubstitutionInLaxEquation || True,
    Print["Compatibility matrix L2.M - M1.L, ON LATTICE"];
    Print["finalcompatibilitymatrixonlattice ="];
    Print[finalcompatibilitymatrixonlattice // MatrixForm]
    ];

    If[( finalcompatibilitymatrixonlattice =!= {{0,0},{0,0}} 
       && numericTestLaxPairComputed ), 
      (* start if numericTestLaxPairComputed *)
      Print["Either the Lax pair computed by the software does not satisfy"];
      Print["the Lax equation OR, more likely, Mathematica could not "];
      Print["simplify the resulting matrix."];
      Print["Matrix will be tested with 12 numeric tests where the symbols"];
      Print["are replaced by randomly selected integers."];
      (* HERE 2 Lax Pair not Given *)
      resultNumericTests = {};
      Do[ (* begin do loop for numeric testing *)
         If[debugNumericLaxPairComputed, 
            Print["Start of numerical TEST ", kk]
            ];
(*
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,4}][[Random[Integer,{1,4}]]], 
        q -> Table[Prime[mm],{mm,5,8}][[Random[Integer,{1,4}]]], 
        k -> Table[Prime[mm],{mm,9,12}][[Random[Integer,{1,4}]]], 
        alpha -> Table[Prime[mm],{mm,13,16}][[Random[Integer,{1,4}]]], 
        beta -> Table[Prime[mm],{mm,17,20}][[Random[Integer,{1,4}]]], 
        delta -> Table[Prime[mm],{mm,21,24}][[Random[Integer,{1,4}]]],
        x -> Table[Prime[mm],{mm,25,28}][[Random[Integer,{1,4}]]],  
        x1 -> Table[Prime[mm],{mm,29,32}][[Random[Integer,{1,4}]]], 
        x2 -> Table[Prime[mm],{mm,33,36}][[Random[Integer,{1,4}]]] 
      };
*)
(* 
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,4}][[Random[Integer,{1,4}]]], 
        x -> Table[Prime[mm],{mm,5,8}][[Random[Integer,{1,4}]]], 
        alpha -> Table[Prime[mm],{mm,9,12}][[Random[Integer,{1,4}]]], 
        k -> Table[Prime[mm],{mm,13,16}][[Random[Integer,{1,4}]]], 
        x1 -> Table[Prime[mm],{mm,17,20}][[Random[Integer,{1,4}]]], 
        delta -> Table[Prime[mm],{mm,21,24}][[Random[Integer,{1,4}]]],
        q -> Table[Prime[mm],{mm,25,28}][[Random[Integer,{1,4}]]],  
        beta -> Table[Prime[mm],{mm,29,32}][[Random[Integer,{1,4}]]], 
        x2 -> Table[Prime[mm],{mm,33,36}][[Random[Integer,{1,4}]]] 
      };
*)
(* 
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,6}][[Random[Integer,{1,6}]]], 
        x -> Table[Prime[mm],{mm,7,12}][[Random[Integer,{1,6}]]], 
        alpha -> Table[Prime[mm],{mm,13,18}][[Random[Integer,{1,6}]]], 
        k -> Table[Prime[mm],{mm,19,24}][[Random[Integer,{1,6}]]], 
        x1 -> Table[Prime[mm],{mm,25,30}][[Random[Integer,{1,6}]]], 
        delta -> Table[Prime[mm],{mm,49,54}][[Random[Integer,{1,6}]]],
        q -> Table[Prime[mm],{mm,31,36}][[Random[Integer,{1,6}]]],  
        beta -> Table[Prime[mm],{mm,37,42}][[Random[Integer,{1,6}]]], 
        x2 -> Table[Prime[mm],{mm,43,48}][[Random[Integer,{1,6}]]] 
      };
*)
(*
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,6}][[Random[Integer,{1,6}]]], 
        q -> Table[Prime[mm],{mm,7,12}][[Random[Integer,{1,6}]]], 
        k -> Table[Prime[mm],{mm,13,18}][[Random[Integer,{1,6}]]], 
        alpha -> Table[Prime[mm],{mm,19,24}][[Random[Integer,{1,6}]]], 
        beta -> Table[Prime[mm],{mm,25,30}][[Random[Integer,{1,6}]]], 
        delta -> Table[Prime[mm],{mm,49,54}][[Random[Integer,{1,6}]]],
        x -> Table[Prime[mm],{mm,31,36}][[Random[Integer,{1,6}]]],  
        x1 -> Table[Prime[mm],{mm,37,42}][[Random[Integer,{1,6}]]], 
        x2 -> Table[Prime[mm],{mm,43,48}][[Random[Integer,{1,6}]]] 
      };
*)

(* now *)
      randomRules = 
        { 
        x -> Table[Prime[mm]+mm^3,{mm,1,8}][[Random[Integer,{1,8}]]], 
        p -> Table[Prime[mm]+mm,{mm,9,16}][[Random[Integer,{1,8}]]], 
        x2 -> Table[Prime[mm]+mm^2,{mm,17,24}][[Random[Integer,{1,8}]]], 
        x1 -> Table[Prime[mm],{mm,25,32}][[Random[Integer,{1,8}]]],  
        q -> Table[Prime[mm]+3*mm,{mm,33,40}][[Random[Integer,{1,8}]]], 
        k -> Table[Prime[mm],{mm,41,48}][[Random[Integer,{1,8}]]],
        alpha -> Table[Prime[mm],{mm,49,56}][[Random[Integer,{1,8}]]], 
        beta -> Table[Prime[mm],{mm,57,64}][[Random[Integer,{1,8}]]], 
        delta -> Table[Prime[mm],{mm,65,72}][[Random[Integer,{1,8}]]]
      };
(* now *)

(* was 
      randomRules = 
        { 
        x -> Table[Prime[mm],{mm,1,8}][[Random[Integer,{1,8}]]], 
        x1 -> Table[Prime[mm],{mm,9,16}][[Random[Integer,{1,8}]]], 
        x2 -> Table[Prime[mm],{mm,17,24}][[Random[Integer,{1,8}]]], 
        p -> Table[Prime[mm],{mm,25,32}][[Random[Integer,{1,8}]]],  
        q -> Table[Prime[mm],{mm,33,40}][[Random[Integer,{1,8}]]], 
        k -> Table[Prime[mm],{mm,41,48}][[Random[Integer,{1,8}]]],
        alpha -> Table[Prime[mm],{mm,49,56}][[Random[Integer,{1,8}]]], 
        beta -> Table[Prime[mm],{mm,57,64}][[Random[Integer,{1,8}]]], 
        delta -> Table[Prime[mm],{mm,65,72}][[Random[Integer,{1,8}]]]
      };
end was *)

      If[debugNumericLaxPairComputed, 
        Print["Selected numbers for the symbols, randomRules = "];
        Print[randomRules]
        ];

      (* Numerical test of result by replacing all the symbols with the *)
      (* randomly selected integers. *)
      (* Can not be done after powerexpand was applied because the signs *)
      (* my be incorrect *)
      numericTestValue = 
        Map[Factor,finalcompatibilitymatrixonlattice /. randomRules];
      If[numericTestValue =!= {{0,0},{0,0}}, 
        numericTestValue = MapAll[Factor, numericTestValue]
        ];
       If[numericTestValue =!= {{0,0},{0,0}}, 
         numericTestValue = MapAll[Simplify, numericTestValue]
         ];
      If[debugNumericTestLaxPairComputed,
         Print["After using the absolute value rule, numericTestValue : "];
         Print[numericTestValue]
         ];

       (* WH: 07-22-08 added a decimal numerical test up to *)
       (* half the machine precision *)
       If[decimalNumericTestLaxPairComputed,
        decimalNumericTestValue = N[Abs[numericTestValue],10] /. List -> Plus;
        If[debugDecimalNumericTestLaxPairComputed,
          Print["The decimal value, decimalNumericTestValue = "];
          Print[decimalNumericTestValue]
          ];
        If[ decimalNumericTestValue < 10^(-$MachinePrecision/2), (* then *)
        If[debugDecimalNumericTestLaxPairComputed,          
           CellPrint[Cell["The decimal test value did simplify to "<>
                  "less than 10^("<> ToString[-$MachinePrecision/2] <>")."]]
          ], (* else *)
          If[debugDecimalNumericTestLaxPairComputed,
            CellPrint[Cell["These decimal test value did not simplify to "<>
                 "less than 10^(" <> ToString[-$MachinePrecision/2] <>" )."]]
            ]
          ]
        ];

       (* WH: 07-21-08: introduce the absolute value rule for *)
       (* the arguments of the sqrt *)
       absvalueruleroot = {Sqrt[zzz__] -> Sqrt[Abs[zzz]]};
       If[numericTestValue =!= {{0,0},{0,0}},
          absfinalcompatibilitymatrixonlattice = 
            finalcompatibilitymatrixonlattice /. absvalueruleroot; 
          absnumericTestValue = 
          Map[Factor,absfinalcompatibilitymatrixonlattice /. randomRules];
          If[debugNumericTestLaxPairComputed,
            Print["After using the absolute value rule, absnumericTestValue="];
            Print[absnumericTestValue]
            ], (* else *)
          absnumericTestValue = {{0,0},{0,0}}
          ];

      (* WH: 07-21-08: if both are zero (Lax pair not given routine) *)
      If[( numericTestValue =!= {{0,0},{0,0}} &&
           absnumericTestValue =!= {{0,0},{0,0}} ),
        Print["Numerical test failed, check if result is free of parameters"];
        Print["numericTestValue = "];
        Print[numericTestValue];
        Print["Numerical (absolute value) test also failed"];
        Print["Check if result is free of parameters"];
        Print["absnumericTestValue = "];
        Print[absnumericTestValue];

        If[numericTestValue === Indeterminate, 
           Print["Ignore the test that failed because the numericTestValue"];
           Print["was Indeterminate (due to division by zero)."]
          ]
        ];
      
      If[debugNumericTestLaxPairComputed, 
        Print["This is the result of the absolute value numerical test, "<>
              "absnumericTestValue = "];
        Print[absnumericTestValue]
        ];

(* *)
      If[debugNumericTestLaxPairComputed, 
        Print["Before appending the list with absnumericTestValue, "<>
              " resultNumericTests = "];
        Print[resultNumericTests]
        ];
(* *)

(* WH: 07-21-08 change to absnumericTestValue *)
(* resultNumericTests = Append[resultNumericTests, numericTestValue]; *)

   resultNumericTests = Append[resultNumericTests, absnumericTestValue];

(* *)
      If[debugNumericTestLaxPairComputed, 
        Print["After appending the list with absnumericTestValue, "<>
              "resultNumericTests = "];
        Print[resultNumericTests]
        ];
(* *)

      resultNumericTests = Union[resultNumericTests];

(* *)
      If[debugNumericTestLaxPairComputed, 
        Print["After applying Union to the list, resultNumericTests = "];
        Print[resultNumericTests]
        ],
(* *)
       {kk, 1, 12}
      ]; (* end do loop *)
      If[resultNumericTests === { {{0,0}, {0,0}} }, 
       Print["Upon substitution into the Lax equation, the computed Lax pair"];
       Print["PASSES the 12 NUMERIC TESTS"];
       Print["(based on randomly selected integers)."];
       Print["The computed Lax pair SATISFIES the Lax equation"],
       (* else *)
       intersec = Intersection[resultNumericTests,{ {{0,0},{0,0}} }];
       If[debugNumericTestLaxPairComputed,
          Print["The intersection of the list with {{0,0},{0,0}} "];
          Print["intersec = "];
          Print[intersec]
          ];
       rest = Complement[resultNumericTests, intersec];
       If[debugNumericTestLaxPairComputed,
          Print["After removing the intersection from the list, rest = "];
          Print[rest]
          ];
       lenrest = Length[rest];
       Print["Upon substitution into the Lax equation, the computed Lax pair"];
       Print["FAILED ",lenrest," of the 12 numeric tests"];
       Print["(based on randomly selected integers)."];
       Print["Ignore the tests that failed because the numericTestValue"];
       Print["was Indeterminate (due to division by zero)."];
       Print["Turn debug flags on (in the code) and retest the Lax pair!"]
       ]
      ] (* end if numericTestLaxPairComputed *)
   ]; (* end if Not LaxPairGiven *)

(* Direct verification of Lax pair L and M given in the data file *)
If[LaxPairGiven, (* start of LaxPairGiven *)
   Print["******************************************************************"];
   Print["************** FINAL TEST OF THE COMPUTED LAX PAIR ***************"];
   candidateL2 = Map[Factor, candidateL /. {x -> x2, x1 -> tempx12}];
   productcandidateL2M = Map[Factor, candidateL2.candidateM];
   If[debugFinalSubstitutionInLaxEquation, 
     Print["Product candidates L2.M, NOT yet evaluated ON LATTICE, L2.M = "];
     Print[productcandidateL2M // MatrixForm]
     ];

   candidateM1 = Map[Factor, candidateM /. {x -> x1, x2 -> tempx12}];
   productcandidateM1L = Map[Factor, candidateM1.candidateL];
   If[debugFinalSubstitutionInLaxEquation, 
      Print["Product candidates M1.L, NOT yet evaluated ON LATTICE, M1.L = "];
      Print[productcandidateM1L // MatrixForm]
     ];
  candidatecompatibilitymatrix = 
       Map[Factor,productcandidateL2M-productcandidateM1L];

(* root rules are not reliable *)
(*
  candidatecompatibilitymatrixonlattice = 
       candidatecompatibilitymatrixonlattice /. rootrule1;
*)
(* PowerExpand is not reliable *)
(*
  If[controlpowerexpand, 
     candidatecompatibilitymatrixonlattice = 
          MapAll[PowerExpand,candidatecompatibilitymatrixonlattice]
    ];
*)

   If[debugFinalSubstitutionInLaxEquation,
    Print["Candidate compatibility matrix L2.M - M1.L,"];
    Print["NOT yet evaluated ON LATTICE, finalcompatibilitymatrix ="];
    Print[candidatecompatibilitymatrix // MatrixForm]
    ];

  Print["The subsequent computations might be slow for complicated lattices,"<>
        " in particular, if square roots are present!"];
  candidatecompatibilitymatrixonlattice = 
       MapAll[Factor,candidatecompatibilitymatrix /. {tempx12 -> x12}];
  If[debugFinalSubstitutionInLaxEquation || True,
    Print["Candidate compatibility matrix L2.M - M1.L, ON LATTICE,"];
    Print["candidatecompatibilitymatrixonlattice ="];
    Print[candidatecompatibilitymatrixonlattice // MatrixForm]
   ];

    If[candidatecompatibilitymatrixonlattice =!= {{0,0},{0,0}} && 
       numericTestLaxPairGiven, 
     (* start if numericTestLaxPairGiven *)
     Print["Either the Lax pair (given in the data file) does not satisfy"];
     Print["the Lax equation OR, more likely, Mathematica could not simplify"];
     Print["the resulting matrix."];
     Print["Matrix will be tested with 12 tests where the symbols"];
     Print["are replaced by randomly selected integers."];

      (* HERE 3 Lax Pair Given *)
      resultNumericTests = {};
      Do[ (* begin do loop for numeric testing *)
         If[debugNumericLaxPairGiven, 
            Print["Start of numerical TEST ", kk]
           ];
(* 
      randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,4}][[Random[Integer,{1,4}]]], 
        q -> Table[Prime[mm],{mm,5,8}][[Random[Integer,{1,4}]]], 
        k -> Table[Prime[mm],{mm,9,12}][[Random[Integer,{1,4}]]], 
        alpha -> Table[Prime[mm],{mm,13,16}][[Random[Integer,{1,4}]]], 
        beta -> Table[Prime[mm],{mm,17,20}][[Random[Integer,{1,4}]]], 
        delta -> Table[Prime[mm],{mm,21,24}][[Random[Integer,{1,4}]]],
        x -> Table[Prime[mm],{mm,25,28}][[Random[Integer,{1,4}]]],  
        x1 -> Table[Prime[mm],{mm,29,32}][[Random[Integer,{1,4}]]], 
        x2 -> Table[Prime[mm],{mm,33,36}][[Random[Integer,{1,4}]]] 
      };
*)
(*
        randomRules = 
        { 
        p -> Table[Prime[mm],{mm,1,6}][[Random[Integer,{1,6}]]], 
        q -> Table[Prime[mm],{mm,7,12}][[Random[Integer,{1,6}]]], 
        k -> Table[Prime[mm],{mm,13,18}][[Random[Integer,{1,6}]]], 
        alpha -> Table[Prime[mm],{mm,19,24}][[Random[Integer,{1,6}]]], 
        beta -> Table[Prime[mm],{mm,25,30}][[Random[Integer,{1,6}]]], 
        delta -> Table[Prime[mm],{mm,49,54}][[Random[Integer,{1,6}]]],
        x -> Table[Prime[mm],{mm,31,36}][[Random[Integer,{1,6}]]],  
        x1 -> Table[Prime[mm],{mm,37,42}][[Random[Integer,{1,6}]]], 
        x2 -> Table[Prime[mm],{mm,43,48}][[Random[Integer,{1,6}]]] 
        };
*)

(* now *)
      randomRules = 
        { 
        x -> Table[Prime[mm]+mm^3,{mm,1,8}][[Random[Integer,{1,8}]]], 
        p -> Table[Prime[mm]+mm,{mm,9,16}][[Random[Integer,{1,8}]]], 
        x2 -> Table[Prime[mm]+mm^2,{mm,17,24}][[Random[Integer,{1,8}]]], 
        x1 -> Table[Prime[mm],{mm,25,32}][[Random[Integer,{1,8}]]],  
        q -> Table[Prime[mm]+3*mm,{mm,33,40}][[Random[Integer,{1,8}]]], 
        k -> Table[Prime[mm],{mm,41,48}][[Random[Integer,{1,8}]]],
        alpha -> Table[Prime[mm],{mm,49,56}][[Random[Integer,{1,8}]]], 
        beta -> Table[Prime[mm],{mm,57,64}][[Random[Integer,{1,8}]]], 
        delta -> Table[Prime[mm],{mm,65,72}][[Random[Integer,{1,8}]]]
      };
(* now *)

(* was 
      randomRules = 
        { 
        x -> Table[Prime[mm],{mm,1,8}][[Random[Integer,{1,8}]]], 
        x1 -> Table[Prime[mm],{mm,9,16}][[Random[Integer,{1,8}]]], 
        x2 -> Table[Prime[mm],{mm,17,24}][[Random[Integer,{1,8}]]], 
        p -> Table[Prime[mm],{mm,25,32}][[Random[Integer,{1,8}]]],  
        q -> Table[Prime[mm],{mm,33,40}][[Random[Integer,{1,8}]]], 
        k -> Table[Prime[mm],{mm,41,48}][[Random[Integer,{1,8}]]],
        alpha -> Table[Prime[mm],{mm,49,56}][[Random[Integer,{1,8}]]], 
        beta -> Table[Prime[mm],{mm,57,64}][[Random[Integer,{1,8}]]], 
        delta -> Table[Prime[mm],{mm,65,72}][[Random[Integer,{1,8}]]]
      };
end was *)

      If[debugNumericTestLaxPairGiven, 
        Print["Selected random values for the symbols, randomRules = "];
        Print[randomRules]
        ];
      (* Numerical test of result by replacing all the symbols with the *)
      (* randomly selected integers. *)
      numericTestValue = 
        Map[Factor,candidatecompatibilitymatrixonlattice /. randomRules];
      If[numericTestValue =!= { {0,0},{0,0} }, 
         numericTestValue = MapAll[Factor, numericTestValue]
         ];
      If[numericTestValue =!= { {0,0},{0,0} }, 
         numericTestValue = MapAll[Simplify, numericTestValue]
         ];
       If[debugNumericTestLaxPairGiven,
         Print["This is the numerical test value, numericTestValue = "];
         Print[numericTestValue]
         ];
        (* WH: 07-22-08 added a decimal numerical test up to *)
        (* half the machine precision *)
       If[decimalNumericTestLaxPairGiven,
         decimalNumericTestValue = N[Abs[numericTestValue],10] /. List -> Plus;
         If[debugDecimalNumericTestLaxPairGiven,
            Print["This is the decimal test value, decimalNumericTestValue ="];
            Print[decimalNumericTestValue]
           ];
         If[ decimalNumericTestValue < 10^(-$MachinePrecision/2), (* then *)
            If[debugDecimalNumericTestLaxPairGiven,
               CellPrint[Cell["The decimal test value did simplify to "<>
                   "less than 10^("<> ToString[-$MachinePrecision/2] <>")."]]
              ], (* else *)
            If[debugDecimalNumericTestLaxPairGiven,
               CellPrint[Cell["These decimal test value did not simplify to "<>
                  "less than 10^(" <> ToString[-$MachinePrecision/2] <>" )."]]
               ]
          ]
        ];

       (* WH: 07-21-08: introduce the absolute value rule for *)
       (* the arguments of the sqrt *)
       absvalueruleroot = {Sqrt[zzz__] -> Sqrt[Abs[zzz]]};
       If[numericTestValue =!= {{0,0},{0,0}},
          absfinalcompatibilitymatrixonlattice = 
            finalcompatibilitymatrixonlattice /. absvalueruleroot; 
          absnumericTestValue = 
          Map[Factor,absfinalcompatibilitymatrixonlattice /. randomRules];
         If[debugNumericTestLaxPairGiven,
            Print["After uing the absolute value rule, absnumericTestValue= "];
            Print[absnumericTestValue]
            ], (* else *)
            absnumericTestValue = {{0,0},{0,0}}
         ];

      (* WH: 07-21-08: if both are zero  (Lax pair given routine) *)
      If[( numericTestValue =!= {{0,0},{0,0}} &&
           absnumericTestValue =!= {{0,0},{0,0}} ),
        Print["Numerical test failed, check if result is free of parameters"];
        Print["numericTestValue = "];
        Print[numericTestValue];
        Print["Numerical (absolute value) test also failed"];
        Print["Check if result is free of parameters"];
        Print["absnumericTestValue = "];
        Print[absnumericTestValue];

        If[numericTestValue === Indeterminate, 
           Print["Ignore the test that failed because the numericTestValue"];
           Print["was Indeterminate (due to division by zero)."]
          ]
        ];

      If[debugNumericTestLaxPairGiven, 
        Print["This is the result of the absolute value numerical test, "<>
              "absnumericTestValue = "];
        Print[absnumericTestValue]
        ];

(* *)
      If[debugNumericTestLaxPairGiven, 
        Print["Before appending the list with absnumericTestValue, "<>
              "resultNumericTests = "];
        Print[resultNumericTests]
        ];
(* *)
(* WH: 07-21-08 change to absnumericTestValue *)
(* resultNumericTests = Append[resultNumericTests, numericTestValue]; *)

   resultNumericTests = Append[resultNumericTests, absnumericTestValue];

(* *)
      If[debugNumericTestLaxPairGiven, 
        Print["After appending the list with absnumericTestValue, "<>
              "resultNumericTests = "];
        Print[resultNumericTests]
        ];
(* *)

      resultNumericTests = Union[resultNumericTests];

(* *)
      If[debugNumericTestLaxPairGiven, 
        Print["After applying Union to the list, resultNumericTests = "];
        Print[resultNumericTests]
        ],
(* *)
        {kk, 1, 12}
      ]; (* end do loop for numeric testing *)
      If[resultNumericTests === { {{0,0}, {0,0}} }, 
        Print["Upon substitution into the Lax equation, the given Lax pair"];
        Print["PASSES the 12 NUMERIC TESTS"];
        Print["(based on randomly selected integers)."];
        Print["The Lax pair (given in data file) SATISFIES the Lax equation"],
        (* else *)
        intersec = Intersection[resultNumericTests,{ {{0,0},{0,0}} }];
        If[debugNumericTestLaxPairGiven,
          Print["The intersection of the list with {{0,0},{0,0}} "];
          Print["intersec = "]; 
          Print[intersec]
          ];
        rest = Complement[resultNumericTests, intersec];
        If[debugNumericTestLaxPairGiven,
          Print["After removing the intersection from the list, rest = "];
          Print[rest]
          ];
        lenrest = Length[rest];
        Print["Upon substitution into the Lax equation, the Lax pair"];
        Print["(given in the data file), FAILED ",lenrest," of the 12"]; 
        Print["numeric tests (based on randomly selected integers)."];
        Print["Ignore the tests that failed because the numericTestValue"];
        Print["was Indeterminate."];
        Print["Turn debug flags on (in the code) and retest the Lax pair!"]
        ]
     ] (* end if numericTestLaxPairGiven *)
 ]; (* end of LaxPairGivenGiven *)

(* GENERIC CODE for all lattices *)
(* *********************************************************************** *)

(* 
Print["********************************************************************"]; 
*)

(* BOTH ROUTINES THAT ARE ACTUALLY USED FOR THE COMPUTATION OF MATRIX N *)
If[controlcomputationmatrixN, (* begin if controlcomputationmatrixN *)

 (* GENERIC CODE for all lattices *)
 Print["********************************************************************"];
 Print["****************** START COMPUTATION OF MATRIX N *******************"];
 Print["********* THIS PIECE OF THE CODE IS STILL UNDER DEVELOPMENT ********"];

 (* Computation of the matrix N. *)
 (* We are assuming that factor in front of N in rhs is lattice itself *)
 (* in polynomial form, but not a function of the lattice *) 
 (* like Sqrt[lattice] or lattice^2, etc. *)

 (* Compatibility matrix L2.M - M1.L = (lattice)*N *)
 (* This is an identity which holds without evaluation on lattice. *)
 (* Key issue for coding: do not replace x12 from the lattice use *)
 (* tempx12 instead *)

 (* TO DO: investigate what happens when you give tcondition and scondition *)
 (* by hand, because these are already evaluated on lattice *)

 (* lattice has format lhs == 0 *)
 unevaluatedlattice = Expand[Part[lattice[x,x1,x2,tempx12,p,q],1]]; 

If[debugComputationOfN,
 Print["Check is lattice is in polynomial form!"];
 Print["(unevaluated) lattice, unevaluatedlattice = "];
 Print[unevaluatedlattice]
 ];

 (* Consider two separate routines: one for when LaxPairGiven, *)
 (* the other one for LaxPairNotGiven *)

 ForceCorrectRoutineLaxPairGiven = False; 

 (* If Lax pair is given in data file *)
 (* apply this routine when LaxPairGiven *)
 (* checking if candidateL and candidateM have common denominators *)

 If[LaxPairGiven, (* begin if LaxPairGiven *)
    Print["LaxPairGiven is true, given candidate for L (candidateL) = "];
    Print["L = "];
    Print[candidateL // MatrixForm];
    Print["LaxPairGiven is true, given candidate for M (candidateM) = "];
    Print["M = "];
    Print[candidateM // MatrixForm];
    (* Branch according to cases where square roots are present or not *)   
    (* Cases without roots first *)
    (* end if test for presence of roots or ForceCorrectRoutineLaxPairGiven *)
    If[(FreeQ[candidateL, Rational[1,2]] && 
        FreeQ[candidateL, Rational[-1,2]] &&
        FreeQ[candidateM, Rational[1,2]] &&
        FreeQ[candidateM, Rational[-1,2]]) || ForceCorrectRoutineLaxPairGiven, 
        (* then do simple routine never replacing x12 from the lattice *)
       If[ForceCorrectRoutineLaxPairGiven,         
        If[debugComputationOfN,
          Print["!!! LaxPairGiven and forcing the software to find N !!!"], 
          Print["NO SQUARE ROOTS ARE PRESENT in the given matrices L and M"]
          ]
         ]; 
        candidateL2 = candidateL /. {x -> x2, x1 -> tempx12};
        candidateM1 = candidateM /. {x -> x1, x2 -> tempx12};
        candidateL2M = Map[Factor,candidateL2.candidateM];
        If[debugComputationOfN,
         Print["Product candidates L2.M = "];
         Print[candidateL2M // MatrixForm]
         ];
        candidateM1L = Map[Factor,candidateM1.candidateL];
        If[debugComputationOfN,
          Print["Product candidates M1.L = "];
          Print[candidateM1L // MatrixForm]
          ];
        prodlatticematrixN = Map[Factor,candidateL2M-candidateM1L];
        If[debugComputationOfN,
          Print["Differences matrix L2.M - M1.L, "<>
                "NOT evaluated ON LATTICE, prodlatticematrixN"];
          Print[prodlatticematrixN // MatrixForm]
          ];
(*
        Print["AT PT 1234, "<>
              "Trying to replace the unevaluatedlattice by name of lattice" ];
*)
        prodnamematrixN = prodlatticematrixN /. {unevaluatedlattice -> name};
        prodnamematrixN = prodnamematrixN /. {-unevaluatedlattice -> -name}; 
        If[debugComputationOfN || True,
           Print["prodnamematrixN = "];
           Print[prodnamematrixN // MatrixForm];
           Print["******** CHECK BY INSPECTION ******* NOT FULLY CODED YET! "];
           Print["Check if prodnamematrixN (above) has the "<>
                 "common factor: ",name]
         ];
        matrixN = prodlatticematrixN/unevaluatedlattice;
        matrixN = MapAll[Factor, matrixN];

        Print["This is the matrix N (matrixN)"];
        Print["N = "];
        Print[matrixN // MatrixForm];

Print["*********************************************************************"];

        (* Entries of matrixN are available separately *)

         entryN11 = matrixN[[1]][[1]];
         Print["N11 = "];
         Print[entryN11];

         entryN12 = matrixN[[1]][[2]];
         Print["N12 = "];
         Print[entryN12];

         entryN21 = matrixN[[2]][[1]];
         Print["N21 = "];
         Print[entryN21];

         entryN22 = matrixN[[2]][[2]];
         Print["N22 = "];
         Print[entryN22];
 
(*        Print["Aborted on demand!"]; *)
Print["********************** END OF THE COMPUTATIONS! *********************"];
Print["*********************************************************************"];
        Abort[], (* end simple routine *)
        (* else if square roots are present in candidateL or candidateM *)
    Print["SQUARE ROOTS ARE PRESENT in the given L and M!"]; 
    candidatematrixLc = Transpose[Thread[Numerator[candidateL]]];
    If[debugComputationOfN,
      Print["From given candidateL, matrixLc = "];
      Print[candidatematrixLc // MatrixForm]
      ];
    matrixLc = candidatematrixLc;
    matrixLc2 = matrixLc /. {x -> x2, x1 -> tempx12};
    If[debugComputationOfN,
      Print["Matrix Lc2, (matrixLc2) NOT evaluated ON LATTICE:"];
      Print[matrixLc2 // MatrixForm]
     ];
    candidaterectlist = Thread[Denominator[candidateL]];
    candidaterectlist = Union[Flatten[candidaterectlist]];

  If[debugComputationOfN,
     Print["Candidaterectlist should have one element"];
     Print[candidaterectlist]
    ];
    If[Length[candidaterectlist] === 1, (* if length is one, then *)
       candidaterect = Part[candidaterectlist,1]; 
      If[debugComputationOfN,
        Print["Common denominator in elements of candidateL = "];
        Print[candidaterect]
       ]; 
       tcondition = 1/candidaterect;
       If[debugComputationOfN,
        Print["tcondition = "];
        Print[tcondition]
        ]; 
       t2condition = tcondition /. {x -> x2, x1 -> tempx12};
       If[debugComputationOfN,
         Print["t2condition = "];
         Print[t2condition]
         ], 
         (* else if length is not one *)
       If[debugComputationOfN,
         Print["TO BE DONE DIFFERENTLY!"]
         ]; 
       ]; (* end if length is one *)
    Print["LaxPairGiven is true, given candidate for M (candidateM) = "];
    Print[candidateM // MatrixForm];
    candidatematrixMc = Transpose[Thread[Numerator[candidateM]]];
   If[debugComputationOfN,
      Print["From given matrixM, matrixMc = "];
      Print[candidatematrixMc // MatrixForm]
      ];
    matrixMc = candidatematrixMc;
    matrixMc1 = matrixMc /. {x -> x1, x2 -> tempx12};
    If[debugComputationOfN,
      Print["Matrix Mc1, (matrixMc1) NOT evaluated ON LATTICE"];
      Print[matrixMc1 // MatrixForm]
      ];
    candidaterecslist = Thread[Denominator[candidateM]];
    candidaterecslist = Union[Flatten[candidaterecslist]];

    If[debugComputationOfN,
      Print["Candidaterecslist should have one element "];
      Print[candidaterecslist]
      ];
    If[Length[candidaterecslist] === 1, (* if length is one, then *)
       candidaterecs = Part[candidaterecslist,1]; 
    If[debugComputationOfN,
       Print["Common denominator in elements of candidateM = "];
       Print[candidaterecs]
       ]; 
       scondition = 1/candidaterecs;
       If[debugComputationOfN,
         Print["scondition = "];
         Print[scondition]
         ];
       s1condition = scondition /. {x -> x1, x2 -> tempx12};
       If[debugComputationOfN,
          Print["s1condition = "];
          Print[s1condition]
          ]; 
       fraction = (t2condition*scondition)/(tcondition*s1condition);
       If[debugComputationOfN,
          Print["NOT evaluated ON LATTICE, fraction = "];
          Print[fraction]
         ];
       fractiononlattice = fraction /. {tempx12 -> x12};
       If[debugComputationOfN || True,
         Print["WARNING!"];
         Print["If fraction is not evaluated on the lattice and roots appear"];
         Print["then Mathematica cannot factor out the lattice "<>
               "in front of N !!!"];
         Print["NEEDS MORE WORK!"]
        ];
       If[debugComputationOfN,
         Print["The fraction (t2*s)/(t*s1) is evaluated on lattice"];
         Print["i.e. in (t2*s)/(t*s1), x12 is replaced from the lattice!"];
         Print["LaxPairGiven is true, fraction evaluated ON LATTICE, "<>
               "fractiononlattice ="];
         Print[fractiononlattice]
         ];
       fractiononlattice = MapAll[Factor, fractiononlattice];
       If[debugComputationOfN,
         Print["After factoring, fraction evaluated ON LATTICE, "<> 
               "fractiononlattice ="];
         Print[fractiononlattice];
         Print["Application of rootrule3gen! Careful with Abs! -- fix later"]
         ];
       numfractiononlattice = MapAll[Factor, MapAll[Factor, 
         Numerator[fractiononlattice] /. rootrule1] /. rootrule3gen];
       If[debugComputationOfN,
         Print["numfractiononlattice ="];
         Print[numfractiononlattice]
         ];
       denomfractiononlattice = MapAll[Factor, MapAll[Factor, 
         Denominator[fractiononlattice] /. rootrule1] /. rootrule3gen];
       If[debugComputationOfN,
          Print["denomfractiononlattice ="];
          Print[denomfractiononlattice]
          ];
       fractiononlattice = 
         MapAll[Factor, numfractiononlattice/denomfractiononlattice];
       If[debugComputationOfN || True,
         Print["WARNING!"];
         Print["If fraction is not evaluated on the lattice and roots appear"];
         Print["then Mathematica cannot factor out the lattice "<>
               "in front of N !!!"];
         Print["NEEDS MORE WORK!"]
         ];
       If[debugComputationOfN,
         Print["The fraction (t2*s)/(t*s1) is evaluated on lattice"];
         Print["i.e. in (t2*s)/(t*s1), x12 is replaced from the lattice!"];
         Print["After simplification, fraction evaluated ON LATTICE, "<>
               "fractiononlattice ="];
         Print[fractiononlattice]
         ];
       factorts1 = MapAll[Factor, tcondition*s1condition]; 
       If[debugComputationOfN,
         Print["factor tcondition*s1condition is factorts1 = "];
         Print[factorts1]
         ];

       prodlatticematrixNalt =
       factorts1*(fractiononlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc));
(* 
       Print["Before factoring, lattice times matrix N "<>
             "(prodlatticematrixNalt)"];
       Print["product (lattice times N) = "];
       Print[prodlatticematrixNalt // MatrixForm];
*)
 Print["********************************************************************"];
       prodlatticematrixNalt = MapAll[Factor, prodlatticematrixNalt];
       Print["After factoring, "<> 
             "lattice times matrix N (prodlatticematrixNalt)"];
       Print["product (lattice times N) = "];
       Print[prodlatticematrixNalt // MatrixForm];

  Print["*******************************************************************"];
       (* see what happens to (1-1) element *)    
       prodlatticeN11alt = Part[Part[prodlatticematrixNalt,1],1];
       prodlatticeN11alt = MapAll[Factor, prodlatticeN11alt];
(*
       Print["AT PT 500a, prodlatticeN11alt = "];
       Print[prodlatticeN11alt];
*)
      prodlatticeN11alt = MapAll[Factor, prodlatticeN11alt /. rootrule1];
      prodlatticeN11alt = MapAll[Factor, prodlatticeN11alt /. rootrule3abs];
      prodlatticeN11alt = MapAll[Factor, prodlatticeN11alt /. {Abs -> Factor}];
    If[debugComputationOfN,
     Print["If square roots are present then Abs might have been replaced "<> 
           "by Factor"];
     Print[prodlatticeN11alt]
    ];
(*
       Print["AT PT 600a, After simplification, prodlatticeN11alt = "];
       Print[prodlatticeN11alt];
*)
(*
       Print["AT PT 700a, Trying to replace the unevaluatedlattice by name "<>
             "of lattice "];
*)
       prodnameN11alt = prodlatticeN11alt /. {unevaluatedlattice -> name};
       prodnameN11alt = prodnameN11alt /. {-unevaluatedlattice -> -name};
       Print["prodnameN11alt = "];
       Print[prodnameN11alt];
       If[debugComputationOfN || True,
        Print["****** CHECK BY INSPECTION ****** NOT FULLY CODED YET! "];
        Print["Check if prodnameN11alt (above) has the common factor: ",name];
        N11alt = Map[Factor,prodlatticeN11alt/unevaluatedlattice]; 
        Print["----> After division by lattice (N11alt) "];
        Print["N11 = "];
        Print[N11alt]
        ];
       If[debugComputationOfN,
          Print["Applying PowerExpand to further simplify --- change later!"]
         ];
       N11altpowerexpand = MapAll[PowerExpand,N11alt];
       If[debugComputationOfN,
         Print["Applying rootrule5gen to numerator and denominator."]
        ];        
       N11altpowerexpand = MapAll[Factor,
         (Numerator[N11altpowerexpand] //. rootrule5gen)/
         (Denominator[N11altpowerexpand] //. rootrule5gen)];
       N11altpowerexpand = MapAll[PowerExpand,N11altpowerexpand];
       If[debugComputationOfN,
         Print["After using PowerExpand -- change later!," <>
               "(N11altpowerexpand) = "];
         Print["N11alt = "];
         Print[N11altpowerexpand];
  Print["********************************************************************"]
        ];
       (* see what happens to (1-2) element *)    
       prodlatticeN12alt = Part[Part[prodlatticematrixNalt,1],2];
       prodlatticeN12alt = MapAll[Factor, prodlatticeN12alt];
(*
       Print["AT PT 500b, prodlatticeN12alt = "];
       Print[prodlatticeN12alt];
*)
      prodlatticeN12alt = MapAll[Factor, prodlatticeN12alt /. rootrule1];
      prodlatticeN12alt = MapAll[Factor, prodlatticeN12alt /. rootrule3abs];
      prodlatticeN12alt = MapAll[Factor, prodlatticeN12alt /. {Abs -> Factor}];
    If[debugComputationOfN,
     Print["If square roots are present then Abs might have been replaced "<>
           "by Factor"];
     Print[prodlatticeN12alt]
    ];
(*
       Print["AT PT 600b, After simplification, prodlatticeN12alt = "];
       Print[prodlatticeN12alt];
*)
(*
       Print["AT PT 700b, Trying to replace the unevaluatedlattice "<>
             "by name of lattice "];
*)
       prodnameN12alt = prodlatticeN12alt /. {unevaluatedlattice -> name};
       prodnameN12alt = prodnameN12alt /. {-unevaluatedlattice -> -name};
       If[debugComputationOfN,
         Print["prodnameN12alt = "];
         Print[prodnameN12alt];
         Print["****** CHECK BY INSPECTION ****** NOT FULLY CODED YET! "];
         Print["Check if prodnameN12alt (above) has the common factor: ",name]
         ];
       N12alt = Map[Factor,prodlatticeN12alt/unevaluatedlattice];
       If[debugComputationOfN,
         Print["----> After division by lattice (N12alt) "];
         Print["N12 = "];
         Print[N12alt];
         Print["Applying PowerExpand to further simplify --- change later!"]
         ];
       N12altpowerexpand = MapAll[PowerExpand,N12alt];
       If[debugComputationOfN,
        Print["Applying rootrule5gen to numerator and denominator."]
        ];        
        N12altpowerexpand = MapAll[Factor,
         (Numerator[N12altpowerexpand] //. rootrule5gen)/
         (Denominator[N12altpowerexpand] //. rootrule5gen)];
       N12altpowerexpand = MapAll[PowerExpand,N12altpowerexpand];
       If[debugComputationOfN,
         Print["After using PowerExpand -- change later!, "<>
               "(N12altpowerexpand) = "];
         Print["N12alt = "];
         Print[N12altpowerexpand];
  Print["********************************************************************"]
       ];
       (* see what happens to (2-1) element *)    
       prodlatticeN21alt = Part[Part[prodlatticematrixNalt,2],1];
       prodlatticeN21alt = MapAll[Factor, prodlatticeN21alt];
(*
       Print["AT PT 500c, prodlatticeN21alt = "];
       Print[prodlatticeN21alt];
*)
      prodlatticeN21alt = MapAll[Factor, prodlatticeN21alt /. rootrule1];
      prodlatticeN21alt = MapAll[Factor, prodlatticeN21alt /. rootrule3abs];
      prodlatticeN21alt = MapAll[Factor, prodlatticeN21alt /. {Abs -> Factor}];
    If[debugComputationOfN,
     Print["If square roots are present then Abs might have been replaced "<>
           "by Factor"];
     Print[prodlatticeN21alt]
    ];
(*
       Print["AT PT 600c, After simplification, prodlatticeN21alt = "];
       Print[prodlatticeN21alt];
*)
(* 
       Print["AT PT 700c, Trying to replace the unevaluatedlattice "<>
             "by name of lattice "];
*)
       prodnameN21alt = prodlatticeN21alt /. {unevaluatedlattice -> name};
       prodnameN21alt = prodnameN21alt /. {-unevaluatedlattice -> -name};
       If[debugComputationOfN,
        Print["prodnameN21alt = "];
        Print[prodnameN21alt];
        Print["****** CHECK BY INSPECTION ****** NOT FULLY CODED YET! "];
        Print["Check if prodnameN21alt (above) has the common factor: ",name]
        ];
       N21alt = Map[Factor,prodlatticeN21alt/unevaluatedlattice];
       If[debugComputationOfN,
         Print["----> After division by lattice (N21alt) "];
         Print["N21 = "];
         Print[N21alt];
         Print["Applying PowerExpand to further simplify --- change later!"]
         ];
       N21altpowerexpand = MapAll[PowerExpand,N21alt];
       If[debugComputationOfN,
         Print["Applying rootrule5gen to numerator and denominator."]
         ];        
         N21altpowerexpand = MapAll[Factor,
         (Numerator[N21altpowerexpand] //. rootrule5gen)/
         (Denominator[N21altpowerexpand] //. rootrule5gen)];
       N21altpowerexpand = MapAll[PowerExpand,N21altpowerexpand];
       If[debugComputationOfN,
          Print["After using PowerExpand -- change later!, "<>
                "(N21altpowerexpand) = "];
          Print["N21alt = "];
          Print[N21altpowerexpand];
  Print["********************************************************************"]
        ];
       (* see what happens to (2-2) element *)    
       prodlatticeN22alt = Part[Part[prodlatticematrixNalt,2],2];
       prodlatticeN22alt = MapAll[Factor, prodlatticeN22alt];
(*
       Print["AT PT 500d, prodlatticeN22alt = "];
       Print[prodlatticeN22alt];
*)
      prodlatticeN22alt = MapAll[Factor, prodlatticeN22alt /. rootrule1];
      prodlatticeN22alt = MapAll[Factor, prodlatticeN22alt /. rootrule3abs];
      prodlatticeN22alt = MapAll[Factor, prodlatticeN22alt /. {Abs -> Factor}];
   If[debugComputationOfN,
     Print["If square roots are present then Abs might have been replaced "<>
           "by Factor"];
     Print[prodlatticeN22alt]
     ];
(*
       Print["AT PT 600d, After simplification, prodlatticeN22alt = "];
       Print[prodlatticeN22alt];
*)
(*
       Print["AT PT 700d, Trying to replace the unevaluatedlattice "<>
             "by name of lattice "];
*)
       prodnameN22alt = prodlatticeN22alt /. {unevaluatedlattice -> name};
       prodnameN22alt = prodnameN22alt /. {-unevaluatedlattice -> -name};
       If[debugComputationOfN,
         Print["prodnameN22alt = "];
         Print[prodnameN22alt];
         Print["****** CHECK BY INSPECTION ****** NOT FULLY CODED YET! "];
         Print["Check if prodnameN22alt (above) has the common factor: ",name]
         ];
       N22alt = Map[Factor,prodlatticeN22alt/unevaluatedlattice];
       If[debugComputationOfN,
          Print["----> After division by lattice (N22alt) "];
          Print["N22 = "];
          Print[N22alt];
          Print["Applying PowerExpand to further simplify --- change later!"]
          ];
       N22altpowerexpand = MapAll[PowerExpand,N22alt];
       If[debugComputationOfN,
         Print["Applying rootrule5gen to numerator and denominator."]
         ];        
       N22altpowerexpand = MapAll[Factor,
         (Numerator[N22altpowerexpand] //. rootrule5gen)/
         (Denominator[N22altpowerexpand] //. rootrule5gen)];
       N22altpowerexpand = MapAll[PowerExpand,N22altpowerexpand];
       If[debugComputationOfN,
          Print["After using PowerExpand -- change later!, "<>
                "(N22altpowerexpand) = "];
          Print["N22alt = "];
          Print[N22altpowerexpand];
       Print["***************************************************************"]
        ];
       (* now assembling the matrix N *)
       matrixN = {{N11altpowerexpand,N12altpowerexpand},
                  {N21altpowerexpand,N22altpowerexpand} };
(*
      Print["AT PT 800, ----> Matrix N after division by lattice (matrixN) "];
*)

      Print["N = "];
      Print[matrixN // MatrixForm];
      Print["***************************************************************"];

       Print[" *** "], (* else if length is not one *)
       Print["TO BE DONE DIFFERENTLY:"]; 
       ] (* end if length is one *)
     ] (* end if test for presence of roots or 
          ForceCorrectRoutineLaxPairGiven *)
  ]; (* end if LaxPairGiven *)

 (* routine when LaxPairNotGiven *)
 ForceCorrectRoutineLaxPairNotGiven = True; 

 (* If Lax pair is not given in data file *)
 If[!LaxPairGiven, (* begin if !LaxPairGiven *)
  If[debugComputationOfN,
    Print["*****************************************************************"];
    Print["Computation of the matrix N."]
    ]; 

  Print["Matrix Lc, (matrixLc):"];
  Print[matrixLc // MatrixForm];

  matrixLc2 = matrixLc /. {x -> x2, x1 -> tempx12};
  If[debugComputationOfN,
    Print["Matrix Lc2, (matrixLc2) NOT evaluated ON LATTICE:"];
    Print[matrixLc2 // MatrixForm]
    ];

  Print["Matrix Mc, (matrixMc):"];
  Print[matrixMc // MatrixForm];

  matrixMc1 = matrixMc /. {x -> x1, x2 -> tempx12};
  If[debugComputationOfN,
    Print["Matrix Mc1, (matrixMc1) NOT evaluated ON LATTICE"];
    Print[matrixMc1 // MatrixForm]
    ];

 (* 
 Print["AT PT 6666, this is (t2*s)/(t*s1), fractioneq21 = "];
 Print[fractioneq21];
 Print["AT PT 7777, this is fractiononlatticeeq21 = "];
 Print[fractiononlatticeeq21];
 Print["AT PT 8888, this is fractiononlattice = "];
 Print[fractiononlattice];
 *)

 (* Note: fractiononlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc) = *)
 (* (lattice*N)/(tcondition*s1condition) *)

 factorts1 = MapAll[Factor, tcondition*s1condition]; 

(*
 Print["AT PT 9999, the factor tcondition*s1condition is factorts1 = "];
 Print[factorts1];
*)

 (* do not evaluate on the lattice !!!! 
 prodlatticematrixNalt =
   factorts1*(fractiononlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc));
 *)

 (*
 Print["AT PT 1000, t2condition = "];
 Print[t2condition];
 Print["AT PT 1001, s1condition = "];
 Print[s1condition];
 *)

 fractionnotonlattice = (t2condition*scondition)/(tcondition*s1condition);

 (*
 Print["AT PT 1002, fractionnotonlattice = "];
 Print[fractionnotonlattice];
 *)

 (* begin ifFreeQ... or ForceCorrectRoutineLaxPairNotGiven *)
 If[(FreeQ[candidateL, Rational[1,2]] &&
     FreeQ[candidateL, Rational[-1,2]] &&
     FreeQ[candidateM, Rational[1,2]] &&
     FreeQ[candidateM, Rational[-1,2]]) || ForceCorrectRoutineLaxPairNotGiven, 
     (* then do simple routine never replacing x12 from the lattice *)
     If[ForceCorrectRoutineLaxPairNotGiven,         
       If[debugComputationOfN,
         Print["!!! Lax pair not given and forcing the software to find N !!!"]
        ], 
        (* else *)
        Print["NO SQUARE ROOTS ARE PRESENT in the computed L and M"]
       ]; 
     (* Note: fractiononlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc) = *)
     (* (lattice*N)/(tcondition*s1condition) *)
     prodlatticematrixN =
    factorts1*(fractionnotonlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc));
     prodlatticematrixN = MapAll[Factor, prodlatticematrixN];
    If[debugComputationOfN,
       Print["Differences matrix L2.M - M1.L, NOT evaluated ON LATTICE, "<>
             "prodlatticematrixN"];
       Print[prodlatticematrixN // MatrixForm]
     ];
(*
     Print["AT PT 4567, Trying to replace the unevaluatedlattice "<>
           "by name of lattice" ];
*)
     prodnamematrixN = prodlatticematrixN /. {unevaluatedlattice -> name};
     prodnamematrixN = prodnamematrixN /. {-unevaluatedlattice -> -name}; 
     If[debugComputationOfN || True,
       Print["prodnamematrixN = "];
       Print[prodnamematrixN // MatrixForm];
       Print[" **** CHECK BY INSPECTION **** NOT FULLY CODED YET! "];
       Print["Check if prodnamematrixN (above) has the common factor: ",name]
      ];
     matrixN = prodlatticematrixN/unevaluatedlattice;
     matrixN = MapAll[Factor, matrixN];
     Print["This is the matrix N (matrixN)"];
     Print["N = "];
     Print[matrixN // MatrixForm];

  Print["*******************************************************************"];
     (* Entries of matrixN are available separately *)

         entryN11 = matrixN[[1]][[1]];
         Print["N11 = "];
         Print[entryN11];

         entryN12 = matrixN[[1]][[2]];
         Print["N12 = "];
         Print[entryN12];

         entryN21 = matrixN[[2]][[1]];
         Print["N21 = "];
         Print[entryN21];

         entryN22 = matrixN[[2]][[2]];
         Print["N22 = "];
         Print[entryN22];
 
(*        Print["Aborted on demand!"]; *)
Print["******************* END OF THE COMPUTATIONS! ************************"];
Print["*********************************************************************"];
        Abort[], (* end simple routine *)
        (* else if square roots are present in candidateL or candidateM *)
 If[debugComputationOfN || True,
   Print["SQUARE ROOTS ARE PRESENT in the computed L and M!"]; 
   Print["WARNING!"];
   Print["If fraction is not evaluated on the lattice and square roots "<>
         "are present"];
   Print["then Mathematica cannot factor out the lattice in front of N !!!"];
   Print["NEEDS MORE WORK!"]
  ];
 If[debugComputationOfN,
   Print["The fraction (t2*s)/(t*s1) is evaluated on lattice"];
   Print["i.e. in (t2*s)/(t*s1), x12 is replaced from the lattice!"];
   Print["Use fractiononlattice instead"]
  ];
(*
  Print["AT PT 1003, fractiononlattice = "]; 
  Print[fractiononlattice];
*)

  prodlatticematrixNalt =
   factorts1*(fractiononlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc));
  (* should use fractionnotonlattice but then the lattice cannot be *)
  (* factored out in front of N *)
  (* 
    prodlatticematrixNalt =
    factorts1*(fractionnotonlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc));
  *)

  (* 
  Print["This is prodlatticematrixNalt = "];
  Print[prodlatticematrixNalt // MatrixForm];
  *)

  (* 
  newfractionnotonlattice = fractioneq11;
  Print["AT PT 1003, newfractionnotonlattice = "];
  Print[newfractionnotonlattice];
  *) 
  (* 
  prodlatticematrixNalt =
     factorts1*(newfractionnotonlattice*(matrixLc2.matrixMc)-(matrixMc1.matrixLc));
  *)
  (* equivalent way *)
  (* 
  prodlatticematrixNalt =
              (t2condition*scondition)*(matrixLc2.matrixMc)-
              (tcondition*s1condition)*(matrixMc1.matrixLc);
  *)
  (*
  Print["Before factoring, lattice times matrix N (prodlatticematrixNalt)"];
  Print["product (lattice times N) = "];
  Print[prodlatticematrixNalt // MatrixForm];
  *)

 Print["********************************************************************"];
 prodlatticematrixNalt = MapAll[Factor, prodlatticematrixNalt];

 Print["After factoring, lattice times matrix N (prodlatticematrixNalt)"];
 Print["product (lattice times N) = "];
 Print[prodlatticematrixNalt // MatrixForm];

 (* applying simplification rules *)
 elementN11alt = Part[Part[prodlatticematrixNalt,1],1];
 elementN11alt = MapAll[Factor, elementN11alt];
 (*
 Print["AT PT 1004, elementN11alt = "];
 Print[elementN11alt];
 *)

 If[controlrootrules, 
   elementN11alt = MapAll[Factor, 
     Numerator[elementN11alt //. rootrule1 //. rootrule2abs]/
     Denominator[elementN11alt //. rootrule1 //. rootrule2abs]
     ];
    If[debugComputationOfN,
     Print["Abs value might have been replaced by Factor!"]
     ]
   ];
 (*
 Print["AT PT 1005, After simplification, elementN11alt = "];
 Print[elementN11alt];
 *)

If[debugComputationOfN,
  Print["This is prodlatticematrixNalt = "];
  Print[prodlatticematrixNalt // MatrixForm]
  ];

 If[debugComputationOfN || True,
   Print["****** CHECK BY INSPECTION ****** NOT FULLY CODED YET! "];
   Print["Check if prodlatticematrixNalt (above) has the common factor :"];
   Print[unevaluatedlattice]
  ];

 (*
 Print["AT PT 1006, Trying to replace the unevaluatedlattice "<>
       "by name of lattice "];
 *)
 prodnamematrixNalt = prodlatticematrixNalt /. {unevaluatedlattice -> name};
 prodnamematrixNalt = prodnamematrixNalt /. {-unevaluatedlattice -> -name}; 
 If[debugComputationOfN,
   Print["prodnamematrixNalt = "];
   Print[prodnamematrixNalt // MatrixForm]
  ];

 If[debugComputationOfN || True,
   Print["****** CHECK BY INSPECTION ****** NOT FULLY CODED YET! "];
   Print["Check if prodnamematrixN (above) has the common factor :",name]
  ];

 Print["********************************************************************"];

 (* ******************************************************************* *)

 matrixNalt = prodlatticematrixNalt/unevaluatedlattice;
 matrixNalt = MapAll[Factor, matrixNalt];
 Print["MatrixN, after division by lattice (matrixNalt)"];
 Print["N = "];
 Print[matrixNalt // MatrixForm];

 Print["********************************************************************"];

 (* Entries of matrixNalt are available separately *)

 entryNalt11 = matrixNalt[[1]][[1]];
 (*  
 Print["Entry N11 for t = ",tcondition," and s = ",scondition," <>
       "(entryNalt11)"];
 *)
 Print["N11 = "];
 Print[entryNalt11];

 entryNalt12 = matrixNalt[[1]][[2]];
 (*
 Print["Entry N12 for t = ",tcondition," and s = ",scondition," <>
       "(entryNalt12)"];
 *)
 Print["N12 = "];
 Print[entryNalt12];

 entryNalt21 = matrixNalt[[2]][[1]];
 (*
 Print["Entry N21 for t = ",tcondition," and s = ",scondition," <>
       "(entryNalt21)"];
 *)
 Print["N21 = "];
 Print[entryNalt21];

 entryNalt22 = matrixNalt[[2]][[2]];
 (*
 Print["Entry N22 for t = ",tcondition," and s = ",scondition," <>
       "(entryNalt22)"];
 *)
 Print["N22 = "];
 Print[entryNalt22];

 Print["********************************************************************"];

 (* Print["Set matrixN = matrixNalt to perform rest of test --- skipped "]; *)
 (* Print["for now!"]; *)
 (* matrixN = matrixNalt *)
 ] (* end ifFreeQ testing ... roots or ForceCorrectRoutineLaxPairGiven *)
 ]; (* end if !LaxPairGiven *)
]; (* end if controlcomputationmatrixN *)

(* ****************************************************************** *)

Print["*********************************************************************"];

(* GENERIC CODE for all lattices *)
(* Applying a general gauge transformation *)

If[controlgaugetf, (* begin if controlgaugetf *)
 gaugematrixG = {{a,b},{c,d}};
 Print["Gauge matrix G, gaugematrixG = "];
 Print[gaugematrixG // MatrixForm];

 gaugematrixG1 = gaugematrixG /. {a -> a1, b -> b1, c -> c1, d -> d1};
 Print["Gauge matrix G1, gaugematrixG1 = "];
 Print[gaugematrixG1 // MatrixForm];

 gaugematrixG2 = gaugematrixG /. {a -> a2, b -> b2, c -> c2, d -> d2};
 Print["Gauge matrix G2, gaugematrixG2 = "];
 Print[gaugematrixG2 // MatrixForm];

 matrixgaugeL = Map[Factor,gaugematrixG1.matrixL.Inverse[gaugematrixG]]; 
 Print["Lax pair: matrix L under gauge transformation, matrixgaugeL = "];
 Print[matrixgaugeL // MatrixForm];

(* Entries of matrixgaugeL are available separately *)

 entrygaugeL11 = matrixgaugeL[[1]][[1]];
 Print["Entry gauge L11 for t = ", tcondition,", entrygaugeL11 = "];
 Print[entrygaugeL11];

 entrygaugeL12 = matrixgaugeL[[1]][[2]];
 Print["Entry gauge L12 for t = ", tcondition,", entrygaugeL12 = "];
 Print[entrygaugeL12];

 entrygaugeL21 = matrixgaugeL[[2]][[1]];
 Print["Entry gauge L21 for t = ", tcondition,", entrygaugeL21 = "];
 Print[entrygaugeL21];

 entrygaugeL22 = matrixgaugeL[[2]][[2]];
 Print["Entry gauge L22 for t = ", tcondition,", entrygaugeL22 = "];
 Print[entrygaugeL22];

 matrixgaugeM = Map[Factor,gaugematrixG2.matrixM.Inverse[gaugematrixG]]; 
 Print["Lax pair: matrix M under gauge transformation, matrixgaugeM = "];
 Print[matrixgaugeM // MatrixForm];

 (* Entries of matrixgaugeM are available separately *)

 entrygaugeM11 = matrixgaugeM[[1]][[1]];
 Print["Entry gauge M11 for t = ", tcondition,", entrygaugeM11 = "];
 Print[entrygaugeM11];

 entrygaugeM12 = matrixgaugeM[[1]][[2]];
 Print["Entry gauge M12 for t = ", tcondition,", entrygaugeM12 = "];
 Print[entrygaugeM12];

 entrygaugeM21 = matrixgaugeM[[2]][[1]];
 Print["Entry gauge M21 for t = ", tcondition,", entrygaugeM21 = "];
 Print[entrygaugeM21];

 entrygaugeM22 = matrixgaugeM[[2]][[2]];
 Print["Entry gauge M22 for t = ", tcondition,", entrygaugeM22 = "];
 Print[entrygaugeM22]
]; (* end if controlgaugetf *)

(* Print["********************* BEGIN SUMMARY **********************"]; *)

Print["Total CPU time used in the current session is ",Round[TimeUsed[]],
      " seconds."];

CloseLog[];

Print["Code for computation of Lax pairs (July 22, 2008) was successfully "<>
      "loaded."];

(* ******************************* END ************************************* *)

