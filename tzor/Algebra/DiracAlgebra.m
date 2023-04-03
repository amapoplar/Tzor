(* ::Package:: *)

$DiracLists = {sigma,slash};
$SpinSpace = {fourVector,gamma,gamma5,metric,dim};


CJM::usage = "CJM is charge conjugation matrix";
indexNew::usage = "indexNew will give you a new variable never apear before. And Options EndQ means that variable never apeared agin";
resetIndex::usage = "reset indx of symbol "
epsilon4::usage = "epsilon4 is 4-diemsions Levie-Cita tensor."
diracContract::usage = "diracContract will do Dirac algebra."
trackContract::usage = "trackContract will simplfiy Track"


Begin["`Private`"]


Format[sigma[a_,b_],TraditionalForm]:=
 DisplayForm[SuperscriptBox["\[Sigma]", RowBox[{a, b}]]]
Format[slash[a_],TraditionalForm]:=
 DisplayForm[OverHat[a]]
Format[gamma[index_],TraditionalForm]:=DisplayForm[Superscript["\[Gamma]",index]]
Format[epsilon4[\[Mu]_,\[Nu]_,\[Lambda]_,\[Rho]_],TraditionalForm]:=DisplayForm[SuperscriptBox["\[Epsilon]",RowBox[{\[Mu],\[Nu],\[Lambda],\[Rho]}]]]
Format[gamma5,TraditionalForm]:=DisplayForm[SuperscriptBox["\[Gamma]","5"]]


Format[CJM,TraditionalForm]:= DisplayForm[
  Style["C",Italic]]


slash[-x_]:=-slash[x]
slash[x_+y_]:= slash[x]+slash[y]


(*\:65b0\:6307\:6807*)
$tzorcounts = {tzorindices = Association[]}
indexNew[\[Mu]_?StringQ,OptionsPattern["EndQ"-> False]]:= Module[{index = If[MissingQ[#],AppendTo[tzorindices,\[Mu]->0]; 0,#]&[tzorindices[\[Mu]]]},
	If[OptionValue["EndQ"],
	index =index +1;
tzorindices[\[Mu]]=index;
	ToExpression[ToString[\[Mu]]<>ToString[index -1]],
		ToExpression[ToString[\[Mu]]<>ToString[index ]]]];
resetIndex[sym_?StringQ]:=Block[ {},tzorindices[sym] = 0;]


gammaQ[a_gamma]:=True
gammaQ[a_+b_]:=Or[gammaQ[a],gammaQ[b]]
gammaQ[a_]:=False
gammaQ[a_ b_]:=Or[gammaQ[a],gammaQ[b]]
gammaQ[Dot[a_ ,b_]]:=Or[gammaQ[a],gammaQ[b]]
gammaQ[gamma5]:=True


repacleGamma5[algebra0_Dot]:=Module[{algebra = Flatten[algebra0]/.Dot->List,lists ,Dotalgebra = {Dot[a___,b_ c_?gammaQ,d___]:> b Dot[a,c,d],Dot[a___,b_?(!gammaQ[#]&),d___]:> b Dot[a,d],Dot[a___,b_+c_,d___]:>Dot[a,b,d]+Dot[a,c,d]}},
lists = Join[Select[algebra,!SameQ[gamma5,#]&],Select[algebra,SameQ[gamma5,#]&] ];
(-1)^Plus@@Flatten[Position[lists,gamma5]-Position[algebra,gamma5]]Dot@@lists//.Join[{gamma5 . gamma5->1},Dotalgebra]
];
repacleGamma5[Times[a__] b_Dot]:=Times[a] repacleGamma5[b]
repacleGamma5[Track[Times[a__] b_Dot]]:=Times[a] Track[repacleGamma5[b]]
repacleGamma5[Times[a__] Track[b_Dot]]:=Times[a] Track[repacleGamma5[b]]
repacleGamma5[Track[algebra0_Dot]]:=Track[repacleGamma5[algebra0]]
repacleGamma5[gamma5]:=gamma5
repacleGamma5[a_]:=a
repacleGamma5[a_+b_]:= repacleGamma5[a]+repacleGamma5[b]
repacleGamma5[gamma5-Track[gamma5 . gamma[\[Mu]] . gamma5 . gamma[\[Nu]] . gamma5 . gamma[\[Nu]]]+dim dim gamma[\[Mu]] . gamma5 . gamma[\[Mu]]]
repacleGamma5[dim]


diracContract[a_+b_]:=diracContract[a]+diracContract[b]
diracContract[algebra0_]:=Module[{algebra = algebra0/.{sigma[\[Mu]_,\[Nu]_]:>I metric[\[Mu],\[Nu]]-I gamma[\[Nu]] . gamma[\[Mu]],slash[x_]:> fourVector[x,indexNew["slash\[Mu]"]]gamma[indexNew["slash\[Mu]","EndQ"->True]]},
timesalgebra = {Times[a___,b_+c_,d_]:>Times[a,b,d]+Times[a,c,d]},
Dotalgebra = {Dot[a___,b_ c_?gammaQ,d___]:> b Dot[a,c,d],Dot[a___,b_?(!gammaQ[#]&),d___]:> b Dot[a,d],Dot[a___,b_+c_,d___]:>Dot[a,b,d]+Dot[a,c,d]},
diracalgebra = {gamma[\[Mu]_] . gamma[\[Nu]_] . gamma[\[Rho]_]:> metric[\[Mu],\[Nu]]gamma[\[Rho]]+metric[\[Nu],\[Rho]]gamma[\[Mu]]-metric[\[Mu],\[Rho]]gamma[\[Nu]]-I epsilon4[\[Mu],\[Nu],\[Rho],indexNew["\[Sigma]"]]gamma[indexNew["\[Sigma]","EndQ"->True]] . gamma5,
				gamma[\[Mu]_] . gamma[\[Mu]_]:> dim,
				gamma5 . gamma5->1},
lorezalgebra = {epsilon4[___,\[Nu]_,___,\[Nu]_,___]:>0,
metric[\[Mu]_,\[Nu]_]metric[\[Mu]_,\[Lambda]_]:>metric[\[Nu],\[Lambda]],
metric[\[Mu]_,\[Nu]_]^2:>dim,
metric[\[Mu]_,\[Nu]_]epsilon4[a___,\[Mu]_,l___]:>epsilon4[a,\[Nu],l],metric[\[Mu]_,\[Mu]_]:> dim,
metric[\[Mu]_,\[Nu]_]gamma[\[Mu]_]:>gamma[\[Nu]],
metric[\[Mu]_,\[Nu]_]Dot[a___,gamma[\[Mu]_],l___]:>Dot[a,gamma[\[Nu]],l]}},
algebra = FixedPoint[repacleGamma5[(#//.Join[timesalgebra,Dotalgebra,diracalgebra,lorezalgebra])]&,algebra//.Join[timesalgebra,Dotalgebra,diracalgebra,lorezalgebra]];
algebra  = algebra //.Join[timesalgebra,Dotalgebra,diracalgebra,lorezalgebra,{epsilon4[\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_]epsilon4[\[Mu]_,\[Nu]_,\[Lambda]_,\[Rho]_]:>-Det[Table[metric[i,j],{j,{\[Alpha],\[Beta],\[Gamma],\[Delta]}},{i,{\[Mu],\[Nu],\[Lambda],\[Rho]}}]]}];
algebra = algebra//.{epsilon4[a__,\[Mu]_,c___]gamma[\[Mu]_]:>epsilon4[a,gamma,c],
		epsilon4[a__,\[Mu]_,c___]gamma[\[Mu]_] . gamma5:>epsilon4[a,gamma,c]gamma5,
		epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Mu]_] . gamma[\[Nu]_]:>epsilon4[a,gamma,c,gamma,d],
		epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Nu]_] . gamma[\[Mu]_]:>- epsilon4[a,gamma,c,gamma,d],
		epsilon4[w__]:> Signature[Flatten[DeleteDuplicates[Position[Sort[{w}],#]&/@{w}]]]epsilon4@@Sort[{w}],
		epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Mu]_] . gamma[\[Nu]_] . gamma5:> epsilon4[a,gamma,c,gamma,d]gamma5,
		epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Nu]_] . gamma[\[Mu]_] . gamma5:>- epsilon4[a,gamma,c,gamma,d]gamma5};
 algebra//.{fourVector[x_,\[Mu]_]Dot[a___,gamma[\[Mu]_],b___]:>Dot[a,slash[x],b]}
		
]


indicesOfgamma[gamma[\[Mu]_]]:=\[Mu]
indicesOfgamma[gamma5]:=Nothing
SetAttributes[indicesOfgamma,Listable]


trackContractMaster[Times[a__]Track[Dot[gammas___]]]:=Times[a] trackContractMaster[Track[Dot[gammas]]]
trackContractMaster[a_+b_]:=trackContractMaster[a]+trackContractMaster[b]
trackContractMaster[Track[Dot[gammas___]]]:=Module[{matrices = gammas/.Dot->List,indices = indicesOfgamma[gammas/.Dot->List],rules,j = 0},
If[OddQ[Length[indices]],0,
If[Length[indices]===2,
		dim  metric[indices[[1]],indices[[2]]],
Plus@@Table[-(-1)^(j = j + 1)metric[First[indices],i]trackContractMaster[Track[Dot@@gamma/@DeleteCases[#,i]]],{i,#}]&[Delete[indices,1]]//Expand
]
]
]


trackContract[Track[Times[a__]alge0_Dot]]:=Times[a] trackContract[Track[alge0]]
trackContract[Times[a__]Track[alge0_Dot]]:=Times[a] trackContract[Track[alge0]]
trackContract[Times[a__]Track[]]:=Times[a] trackContract[Track[]]
trackContract[Track[]]:=dim
trackContract[Track[Times[a__]]]:=Times[a] trackContract[Track[]]
trackContract[Track[alge0_Dot]]:=Module[{alge=alge0/.{sigma[\[Mu]_,\[Nu]_]:>I metric[\[Mu],\[Nu]]-I gamma[\[Nu]] . gamma[\[Mu]],slash[x_]:>fourVector[x,indexNew["slash\[Mu]"]]gamma[indexNew["slash\[Mu]"]]},
timesalgebra = {Times[a___,b_+c_,d_]:>Times[a,b,d]+Times[a,c,d]},
Dotalgebra = {Dot[a___,b_ c_?gammaQ,d___]:> b Dot[a,c,d],
				Dot[a___,b_?(!gammaQ[#]&),d___]:> b Dot[a,d],
				Dot[a___,b_+c_,d___]:>Dot[a,b,d]+Dot[a,c,d]},
dotlist = alge0/.{Dot->List},
lorezalgebra = {epsilon4[___,\[Nu]_,___,\[Nu]_,___]:>0,
			metric[\[Mu]_,\[Nu]_]metric[\[Mu]_,\[Lambda]_]:>metric[\[Nu],\[Lambda]],
			metric[\[Mu]_,\[Nu]_]^2:>dim,metric[\[Mu]_,\[Nu]_]epsilon4[a___,\[Mu]_,l___]:>epsilon4[a,\[Nu],l],
			metric[\[Mu]_,\[Mu]_]:> dim,metric[\[Mu]_,\[Nu]_]gamma[\[Mu]_]:>gamma[\[Nu]],
			metric[\[Mu]_,\[Nu]_]Dot[a___,gamma[\[Mu]_],l___]:>Dot[a,gamma[\[Nu]],l],
			metric[\[Mu]_,\[Nu]_]fourVector[x_,\[Mu]_]:>fourVector[x,\[Nu]]}},
alge = alge//.Join[timesalgebra,Dotalgebra];

alge = Track[alge] //.Track[a_ + b_]:>Track[a]+Track[b];
alge = repacleGamma5[alge]//.Join[{gamma5-> -(1/4!)epsilon4[indexNew["eps1"],indexNew["eps2"],indexNew["eps3"],indexNew["eps4"]] . gamma[indexNew["eps1"]] . gamma[indexNew["eps2"]] . gamma[indexNew["eps3"]] . gamma[indexNew["eps4","EndQ"->True]]},
Dotalgebra];
(trackContractMaster[alge]//Expand) //.Join[lorezalgebra ,{
			epsilon4[a__,\[Mu]_,c___]gamma[\[Mu]_]:>epsilon4[a,gamma,c],
			epsilon4[a__,\[Mu]_,c___]gamma[\[Mu]_] . gamma5:>epsilon4[a,gamma,c]gamma5,
			epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Mu]_] . gamma[\[Nu]_]:>epsilon4[a,gamma,c,gamma,d],
			epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Nu]_] . gamma[\[Mu]_]:>- epsilon4[a,gamma,c,gamma,d],
			epsilon4[w__]:> Signature[Flatten[DeleteDuplicates[Position[Sort[{w}],#]&/@{w}]]]epsilon4@@Sort[{w}],
			epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Mu]_] . gamma[\[Nu]_] . gamma5:>epsilon4[a,gamma,c,gamma,d]gamma5,
			epsilon4[a__,\[Mu]_,c___,\[Nu]_,d___]gamma[\[Nu]_] . gamma[\[Mu]_] . gamma5:>- epsilon4[a,gamma,c,gamma,d]gamma5}]
]


End[]
