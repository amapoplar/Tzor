(* ::Package:: *)

$MatrixAndTensorLists = {epsilon,delta,lambda,deltaAdj,structF}


NColorFactors::usage = "caculate the value of color factors"


Begin["`Private`"]


(*epsilon\:4e0edelta\:7684\:6837\:5f0f*)
Format[epsilon[a_, b_, c_], TraditionalForm] := 
DisplayForm[SuperscriptBox["\[Epsilon]", RowBox[{a, b, c}]]]
Format[delta[a_, b_], TraditionalForm] := 
DisplayForm[SuperscriptBox["\[Delta]", RowBox[{a, b}]]]
 Format[deltaAdj[a_, b_], TraditionalForm] := 
 DisplayForm[SuperscriptBox["\[Delta]", RowBox[{a, b}]]]
Format[lambda[a_,b_,n_],TraditionalForm]:=
 DisplayForm[Subsuperscript["\[Lambda]", RowBox[{a,b}],RowBox[{n}]]]
Format[structF[a_,b_,c_],TraditionalForm]:=
 DisplayForm[Superscript["\[Lambda]", RowBox[{a,b,c}]]]


color[DE[{ferm_, ferm_}, {x_ , y_}] [ci[ci_],si[si_]]]:=ci 
color[DE[{ferm_, ferm_}, {x_ , y_}] [ci[ci_]]]:=ci 
color[a_[ci[ci_]]]:=ci
color[a_[si[si_],ci[ci_]]]:=ci
color[a_[si[si_]]]:= Nothing
color[delta[a_,b_]]:={a,b}
color[deltaAdj[a_,b_]]:={a,b}
color[epsilon[a_,b_,c_]]:={a,b,c}
color[lambda[a_,b_,n_]]:={a,b,n}
color[structF[a_,b_,c_]]:={a,b,c}
color[Trans[a_]]:=Reverse[color[a]]
color[a_]:=Nothing
uncolor[con_]:=Module[{cons = con},cons /.{a_[ci[ci_]]:>a,DE[a__][ci[ci_],si[sis_]]:>DE[a][si[sis]]}]


parameter[delta[a_, b_]]:={{a,3},{b,3}}
parameter[deltaAdj[a_, b_]]:={{a,8},{b,8}}
parameter[epsilon[a_,b_,c_]]:={{a,3},{b,3},{c,3}}
parameter[lambda[a_,b_,n_]]:={{a,3},{b,3},{n,8}}
parameter[structF[a_, b_, c_]]:= {{a,8},{b,8},{c,8}}
parameters[f_]:=parameter[f]
parameters[Times[f_,g_]]:=Union[ parameter[f],parameters[Times[g]]];


(*\:8272\:6307\:6807\:8ba1\:7b97*)
filename = "Algebra/factors.csv";
$ColorCalcDirectory = DirectoryName[$InputFileName];
caculateColor[hashStr_,exp_]:= Module[{indexOfSum =  parameters[exp],expused=exp,log="",
eps = Flatten[Table[LeviCivitaTensor[3][[i,j,k]],{i,1,3},{i,1,3},{j,1,3},{k,1,3}],1],
del = {{1,0,0},{0,1,0},{0,0,1}},
Gellman = {{{0,1,0},{1,0,0},{0,0,0}},
	{{0,-I,0},{I,0,0},{0,0,0}},
	{{1,0,0},{0,-1,0},{0,0,0}},
	{{0,0,1},{0,0,0},{1,0,0}},
	{{0,0,-I},{0,0,0},{I,0,0}},
	{{0,0,0},{0,0,1},{0,1,0}},
	{{0,0,0},{0,0,-I},{0,I,0}},
	1/Sqrt[3] {{1,0,0},{0,1,0},{0,0,-2}}},
deladj = IdentityMatrix[8],
sF
},

sF = 1/(4I) Table[Sum[Gellman[[b,i,j]]Gellman[[c,j,k]]Gellman[[a,k,i]]-Gellman[[c,i,j]]Gellman[[b,j,k]]Gellman[[a,k,i]],{i,3},{j,3},{k,3}],{a,8},{b,8},{c,8}];
log = \!\(TraditionalForm\`File[FileNameJoin[{$ColorCalcDirectory, filename}]]\);
expused = expused/.{epsilon[a_,b_,c_]:> eps[[a,b,c]],delta[a_,b_]:> del[[a,b]],lambda[a_,b_,n_]:>Gellman[[n,a,b]],deltaAdj[a_, b_]:> deladj[[a,b]],structF[a_,b_,c_]:> sF[[a,b,c]]};

expused = Fold[Sum[#1,#2]&,expused,indexOfSum];
OpenAppend[log];
If[NumericQ[expused],WriteString[log,hashStr,",",ToString[expused]<>"\n"]];
Close[log];
expused
]//Quiet
NColorFactors[0]:= 0
NColorFactors[Times[ factor_,factor1__]]:= Block[{ fac =SortBy[Select[{factor,factor1},Length[color[#]]>1&],First],
													fac1 =Times@@Select[{factor,factor1},Length[color[#]]<=1&],
													facs = ToExpression[Import[NotebookDirectory[]<>"facs5.csv","Data"]],str},
	
	facs = Association[Table[facs[[i,1]]->facs[[i,2]],{i,Length[facs]}]];
	
	str = ToString[Join[Select[fac,Head[#]===epsilon&],Select[fac,Head[#]=!=epsilon&]]];
	If[!KeyExistsQ[facs, Hash[str]],caculateColor[Hash[str],Times@@fac]*fac1,facs[Hash[str]]fac1]
]//Quiet
SetAttributes[NColorFactors,Listable]



End[]
