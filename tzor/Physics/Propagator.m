(* ::Package:: *)

$porgator = {QPropagatorx,QPropagatorp,GPropagatorx,GPropagatorp}
$heavyQuarks = {ToExpression["b"],ToExpression["c"],ToExpression["Q"]}


FeynAmpD::usage = "FeynAmpD is the external form of Feynmann Ampalitue Denominator and denotes an inverse propagator.";
diracDelta::usage = "diracDelta is the delta function.";
color::usage = "";
lorenz::usage = "";
symbol::usage = "symbol will give a variable's symbol.";
replace::usage = "replace will replace propagators by order.";


TzorDeclareHeader[FileNameJoin[{$TzorDirectory,"Algebra/LorenzAlgebra.m"}]];
TzorDeclareHeader[FileNameJoin[{$TzorDirectory,"Algebra/ColorAlgebra.m"}]];
TzorDeclareHeader[FileNameJoin[{$TzorDirectory,"Algebra/DiracAlgebra.m"}]];


Begin["`Private`"]


Format[diracDelta[list__],TraditionalForm]:=DisplayForm[RowBox[Flatten[{"\[Delta]","(",Riffle[Flatten[{list}],"+"],"=","0",")"}]]]
symbol[a_]:=If[NumberQ[First[Flatten[Level[a,{0,Infinity}]]]],Sign[First[Flatten[Level[a,{0,Infinity}]]]],1];


Unprotect[Times];
Format[PD[{q_,m_,n_}],TraditionalForm]:=DisplayForm[Superscript[RowBox[{"(",q^2-m^2,")"}],n]]
Format[PD[{q_,m_,1}],TraditionalForm]:=DisplayForm[RowBox[{"(",q^2-m^2,")"}]]
Format[PD[{q_,m_}],TraditionalForm]:=DisplayForm[RowBox[{"(",q^2-m^2,")"}]]
Format[PD[q_],TraditionalForm]:=DisplayForm[RowBox[{q^2}]]
Format[FeynAmpD[q__],TraditionalForm]:=DisplayForm[FractionBox["1",RowBox[Riffle[PD[#]&/@{q},"\[CenterDot]"]]]]
FeynAmpD[q1__]FeynAmpD[q2__]:=FeynAmpD[q1,q2]
FeynAmpD[]:=1
SetAttributes[FeynAmpD,Orderless]
Protect[Times];


color[DE[{ferm_, ferm_}, {x_ , y_} ] [ci[cis_],si[sis_]]]:=cis
color[DE[{ferm_, ferm_}, {x_ , y_} ] [ci[cis_]]]:=cis
color[GE[GField[n1_,x_][li[{\[Mu]1_,\[Nu]1_}]],GField[n2_,y_][li[{\[Mu]2_,\[Nu]2_}]]]]:={n1,n2}
color[a_[ci[ci_]]]:=ci
color[a_[si[sis_],ci[ci_]]]:=ci
color[a_[si[sis_]]]:= {}
color[a_[li[sis_],ci[ci_]]]:=ci
color[a_[li[sis_]]]:= {}
color[delta[a_,b_]]:={a,b}
color[deltaAdj[a_,b_]]:={a,b}
color[epsilon[a_,b_,c_]]:={a,b,c}
color[lambda[a_,b_,n_]]:={a,b,n}
color[structF[a_,b_,c_]]:={a,b,c}
color[Trans[a_]]:=color[a]
color[a_^n_]:=color[a]
color[a_]:={}
color[a_ + b_]:=Union[color[a],color[b]]
color[a_ b_]:=Union[color[a],color[b]]
color[a_ . b_]:=Union[color[a],color[b]]
color[Track[a___]]:=color[a]

lorenz[gamma[\[Mu]_]]:={\[Mu]}
lorenz[a_^n_]:=lorenz[a]
lorenz[sigma[\[Mu]_,\[Nu]_]]:={\[Mu],\[Nu]}
lorenz[Trans[a_]]:=Reverse[lorenz[a]]
lorenz[a_]:={}
lorenz[a_ + b_]:= Union[lorenz[a],lorenz[b]]
lorenz[a_ b_]:= Union[lorenz[a],lorenz[b]]
lorenz[a_ . b_]:= Union[lorenz[a],lorenz[b]]
lorenz[Track[a___]]:=lorenz[a]

uncolor[con_]:=Module[{cons = con},cons /.{a_[ci[ci_]]:>a,DE[a__][ci[cis_],si[si_]]:>DE[a][ci[cis]]}]

posandflovor[DE[{q_,q_},{y_,x_}][___]]:={q,x-y}
posandflovor[a___]:={}

SetAttributes[{color,posandflovor,lorenz},Listable]


(*x\:7a7a\:95f4\:8f7b\:5938\:514bd\:7ef4\:4f20\:64ad\:5b50*)
(*m,n,o,p\:5bf9\:5e94\:7684\:662f\:60ac\:6302\:80f6\:5b50\:7ebf*)
(*factorsx[a_,b_,q_,x_,key_]:= Block[{fa},fa = {
	{"a",(I Gamma[dim/2])/(2 \[Pi]^(dim/2) (-1)^(dim/2)(x^2)^(dim/2))*delta[a,b]*slash[x]},
	{"b",1/2 lambda[a,b,indexNew["\[Eta]"]]*(-I gs Gamma[dim/2-1] GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]])/(32 \[Pi]^(dim/2) (-1)^(dim/2-1) (x^2)^(dim/2-1))*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[x]+slash[x] . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]])},
	{"c",-(1/12) chiral[q]*delta[a,b]},
	{"d",1/192 x^2 hybrid[q]*delta[a,b]},
	{"e",-(I (gs^2 x^2 chiral[q]^2)/7776)*delta[a,b]*slash[x]},
	{"f",-((x^4 \[LeftAngleBracket]gs^2 "G"^2\[RightAngleBracket] chiral[q])/27648)*delta[a,b]},
	{"g",(-1)^(dim/2-1) Gamma[dim/2-1] (mass[q]/(4 \[Pi]^(dim/2) (x^2)^(dim/2-1)))*delta[a,b]},
	{"h",-(gs Gamma[dim/2-2] GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]](-1)^(2-dim/2) (x^2)^(2-dim/2) mass[q])/(32 \[Pi]^(dim/2))*sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]*1/2*lambda[a,b,indexNew["\[Eta]"]]},
	{"i",(-1)^(3-dim/2) Gamma[dim/2-3](((x^2)^(3-dim/2) doubleG mass[q])/(1536 \[Pi]^(dim/2)))*delta[a,b]},
	{"j",1/48 I chiral[q] mass[q]*slash[x]*delta[a,b]},
	{"k",-((I x^2 hybrid[q] mass[q])/1152)*delta[a,b]*slash[x]},{"l",-((gs^2 x^4 chiral[q]^2 mass[q])/31104)*delta[a,b]},
	{"m",(-I 1/2 lambda[a,b,indexNew["\[Eta]"]] Gamma[dim/2-1])/(32 \[Pi]^(dim/2) (-1)^(dim/2-1) (x^2)^(dim/2-1))*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[x]+slash[x] . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]])},
	{"n",-(Gamma[dim/2-2] (-1)^(2-dim/2) (x^2)^(2-dim/2) mass[q])/(32 \[Pi]^(dim/2))*sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]*1/2*lambda[a,b,indexNew["\[Eta]"]]},
	{"o",-(1/(2^6*3))hybrid[q]*sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]*1/2 lambda[a,b,indexNew["\[Eta]"]]},
	{"p",(I mass[q])/(2^8*3) hybrid[q]*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[x]+slash[x] . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]])*1/2 lambda[a,b,indexNew["\[Eta]"]]}};
fa = Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]] *)
QPropagatorx[q_,x_,y_,a_,b_,key_]:= Block[{},
Switch[key,
		"a", (I Gamma[dim/2])/(2 \[Pi]^(dim/2) (-1)^(dim/2)(scalarP[x-y,x-y])^(dim/2))*delta[a,b]*slash[x-y],
		"b", 1/2 lambda[a,b,indexNew["\[Eta]"]]*(-I gs Gamma[dim/2-1] GField[indexNew["\[Eta]","EndQ"->True],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]])/(32 \[Pi]^(dim/2) (-1)^(dim/2-1) (scalarP[x-y,x-y])^(dim/2-1))*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[x-y]+slash[x-y] . sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]]),
		"c",-(1/12) chiral[q]*delta[a,b],
		"d",1/192  scalarP[x-y,x-y] hybrid[q]*delta[a,b],
		"e",-(I (gs^2 scalarP[x-y,x-y] chiral[q]^2)/7776)*delta[a,b]*slash[x-y],
		"f",-(((scalarP[x-y,x-y])^2 \[LeftAngleBracket]gs^2 "G"^2\[RightAngleBracket] chiral[q])/\!\(TraditionalForm\`27648\))*delta[a,b],
		"g",(-1)^(dim/2-1) Gamma[dim/2-1] (mass[q]/(4 \[Pi]^(dim/2) (scalarP[x-y,x-y])^(dim/2-1)))*delta[a,b],
		"h",-(gs Gamma[dim/2-2] GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]](-1)^(2-dim/2) (scalarP[x-y,x-y])^(2-dim/2) mass[q])/(32 \[Pi]^(dim/2))*sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]]*1/2*lambda[a,b,indexNew["\[Eta]","EndQ"->True]],
		"i",(-1)^(3-dim/2) Gamma[dim/2-3](((scalarP[x-y,x-y])^(3-dim/2) doubleG mass[q])/(1536 \[Pi]^(dim/2)))*delta[a,b],
		"j",1/48 I chiral[q] mass[q]*slash[x-y]*delta[a,b],
		"k",-((I scalarP[x-y,x-y] hybrid[q] mass[q])/1152)*delta[a,b]*slash[x-y],
		"l",-((gs^2 (scalarP[x-y,x-y])^2 chiral[q]^2 mass[q])/31104)*delta[a,b],
		"m",(-I 1/2 lambda[a,b,indexNew["\[Eta]","EndQ"->True]] Gamma[dim/2-1])/(32 \[Pi]^(dim/2) (-1)^(dim/2-1) (scalarP[x-y,x-y])^(dim/2-1))*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[x-y]+slash[x-y] . sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]]),
		"n",-(Gamma[dim/2-2] (-1)^(2-dim/2) (scalarP[x-y,x-y])^(2-dim/2) mass[q])/(32 \[Pi]^(dim/2))*sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]]*1/2*lambda[a,b,indexNew["\[Eta]","EndQ"->True]],
		"o",-(1/(2^6*3))hybrid[q]*sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]]*1/2 lambda[a,b,indexNew["\[Eta]","EndQ"->True]],
		"p",(I mass[q])/(2^8*3) hybrid[q]*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[x-y]+slash[x-y] . sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]])*1/2 lambda[a,b,indexNew["\[Eta]","EndQ"->True]]
		]
]



(*lfactorsp[a_,b_,q_,k_,key_]:= Block[{fa, knew = k},fa = {
  {"a",I*(slash[knew])delta[a,b]FeynAmpD[{knew,0,1}]},
  {"b",-I gs (sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[knew]+slash[knew] . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]) lambda[a,b,indexNew["\[Eta]"]] GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]]FeynAmpD[{knew,0,2}]/2},
  {"c",0},
  {"d",0},
  {"e",0},
  {"f",0},
  {"g",I*(mass[q])delta[a,b]FeynAmpD[{knew,0,1}]},
  {"h",-I gs sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]mass[q]lambda[a,b,indexNew["\[Eta]"]] GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]]FeynAmpD[{knew,0,2}]},
  {"i",I*doubleG delta[a,b] mass[q] FeynAmpD[{knew,0,3}]/12},
  {"j",0},
  {"k",0},
  {"l",0},
  {"m",-I gs (sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[knew]+slash[knew] . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]) lambda[a,b,indexNew["\[Eta]"]]FeynAmpD[{knew,0,2}]/8},
  {"n",0},
  {"o",-I gs mass[q](sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]) lambda[a,b,indexNew["\[Eta]"]]FeynAmpD[{knew,0,2}]/4},
  {"p",0}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]] *)
QPropagatorp[q_,x_,y_,a_,b_,k_ ,key_]:=Block[{},
Switch[key,
	"a",I*(slash[k])delta[a,b]FeynAmpD[{k,mass[q],1}]*Exp[-I scalarP[k,x-y]],
	"b",-I gs (sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . slash[k]+slash[k] . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]) lambda[a,b,indexNew["\[Eta]"]] GField[indexNew["\[Eta]","EndQ"->True],0][li[{indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]}]]FeynAmpD[{k,mass[q],2}]/2 *Exp[-I scalarP[k,x-y]],
	"c",0,
	"d",0,
	"e",0,
	"f",0,
	"g",0,
	"h",0,
	"i",I*doubleG delta[a,b] mass[q] (scalarP[k,k] + mass[q] slash[k])FeynAmpD[{k,mass[q],4}]/12*Exp[-I scalarP[k,x-y]],
	"j",0,
	"k",0,
	"l",0,
	"m",-I gs (sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . (mass[q]+slash[k])+(mass[q]+slash[k]) . sigma[indexNew["\[Mu]","EndQ"->True],indexNew["\[Nu]","EndQ"->True]]) lambda[a,b,indexNew["\[Eta]","EndQ"->True]]FeynAmpD[{k,mass[q],2}]/8*Exp[-I scalarP[k,x-y]],
	"n",0,
	"o",0,
	"p",0
	]
]


(*factorsp[a_,b_,q_,k_,key_]:= Block[{fa, knew = k},fa = {
  {"a",I*(slash[knew]+ mass[q])delta[a,b]FeynAmpD[{knew,mass[q],1}]},
  {"b",-I gs (sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . (mass[q]+slash[knew])+(mass[q]+slash[knew]) . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]) lambda[a,b,indexNew["\[Eta]"]] GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]]FeynAmpD[{knew,mass[q],2}]/2},
  {"c",0},
  {"d",0},
  {"e",0},
  {"f",0},
  {"g",0},
  {"h",0},
  {"i",I*doubleG delta[a,b] mass[q] (scalarP[knew,knew] + mass[q] slash[knew])FeynAmpD[{knew,mass[q],4}]/12},
  {"j",0},
  {"k",0},
  {"l",0},
  {"m",-I gs (sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]] . (mass[q]+slash[knew])+(mass[q]+slash[knew]) . sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]) lambda[a,b,indexNew["\[Eta]"]]FeynAmpD[{knew,mass[q],2}]/8},
  {"n",0},
  {"o",0},
  {"p",0}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]] *)




(*factorsxG[a_,b_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,x_,key_]:=Block[{fa},fa ={{"a",x}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]]*)
GPropagatorp[x_,y_,a_,b_,\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_ ,key_]:=Block[{},
	Switch[key,
	"a", 1
	]
]


(*factorspG[a_,b_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,k_,key_]:=Block[{fa,p = k,\[Mu] = \[Mu]1, \[Nu] = \[Nu]1,\[Alpha] = \[Mu]2, \[Beta] = \[Nu]2},
	fa ={
	{"a",-I deltaAdj[a,b]FeynAmpD[{p,0,1}](fourVector[p,\[Mu]]fourVector[p,\[Alpha]]metric[\[Nu],\[Beta]]-fourVector[p,\[Nu]]fourVector[p,\[Alpha]]metric[\[Mu],\[Beta]]-fourVector[p,\[Mu]]fourVector[p,\[Beta]]metric[\[Nu],\[Alpha]]+fourVector[p,\[Nu]]fourVector[p,\[Beta]]metric[\[Mu],\[Alpha]])},
	{"b",-(1/96) doubleG deltaAdj[a,b] (metric[\[Alpha],\[Nu]] metric[\[Beta],\[Mu]]-metric[\[Alpha],\[Mu]] metric[\[Beta],\[Nu]])},
	{"c",3/32*I*gs^2*doubleG*deltaAdj[a,b]FeynAmpD[{p,0,2}](metric[\[Alpha],\[Nu]]metric[\[Beta],\[Mu]]-metric[\[Alpha],\[Mu]]metric[\[Beta],\[Nu]])-I/16*gs^2*doubleG*deltaAdj[a,b]FeynAmpD[{p,0,3}](fourVector[p,\[Alpha]]fourVector[p,\[Mu]]metric[\[Beta],\[Nu]]-fourVector[p,\[Beta]]fourVector[p,\[Mu]]metric[\[Alpha],\[Nu]]-fourVector[p,\[Alpha]]fourVector[p,\[Nu]]metric[\[Beta],\[Mu]]+fourVector[p,\[Beta]]fourVector[p,\[Nu]]metric[\[Alpha],\[Mu]])},
	{"d",1/384*gs*tripleG*deltaAdj[a,b] x^2(metric[\[Alpha],\[Mu]]metric[\[Beta],\[Nu]]-metric[\[Alpha],\[Nu]]metric[\[Beta],\[Mu]])},
	{"e",1/(2^4 3^4)*gs^2*deltaAdj[a,b]chiral[q]^2 x^2 (metric[\[Alpha],\[Mu]]metric[\[Beta],\[Nu]]-metric[\[Alpha],\[Nu]]metric[\[Beta],\[Mu]])+
1/(2^4 3^4)*gs^2*deltaAdj[a,b]chiral[q]^2 (fourVector[x,\[Alpha]]fourVector[x,\[Mu]]metric[\[Beta],\[Nu]]-
fourVector[x,\[Beta]]fourVector[x,\[Mu]]metric[\[Alpha],\[Nu]]-
fourVector[x,\[Alpha]]fourVector[x,\[Nu]]metric[\[Beta],\[Mu]]+
fourVector[x,\[Beta]]fourVector[x,\[Nu]] metric[\[Alpha],\[Mu]])}
	};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]]*)

GPropagatorp[x_,y_,a_,b_,\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,p_ ,key_]:=Block[{},
Switch[key,
		"a",-I deltaAdj[a,b]FeynAmpD[{p,0,1}](fourVector[p,\[Mu]]fourVector[p,\[Alpha]]metric[\[Nu],\[Beta]]-fourVector[p,\[Nu]]fourVector[p,\[Alpha]]metric[\[Mu],\[Beta]]-fourVector[p,\[Mu]]fourVector[p,\[Beta]]metric[\[Nu],\[Alpha]]+fourVector[p,\[Nu]]fourVector[p,\[Beta]]metric[\[Mu],\[Alpha]])*Exp[-I scalarP[p,x-y]],
		"b",-(1/96) doubleG deltaAdj[a,b] (metric[\[Alpha],\[Nu]] metric[\[Beta],\[Mu]]-metric[\[Alpha],\[Mu]] metric[\[Beta],\[Nu]]) *Exp[-I scalarP[p,x-y]],
		"c",(3/32*I*gs^2*doubleG*deltaAdj[a,b]FeynAmpD[{p,0,2}](metric[\[Alpha],\[Nu]]metric[\[Beta],\[Mu]]-metric[\[Alpha],\[Mu]]metric[\[Beta],\[Nu]])-I/16*gs^2*doubleG*deltaAdj[a,b]FeynAmpD[{p,0,3}](fourVector[p,\[Alpha]]fourVector[p,\[Mu]]metric[\[Beta],\[Nu]]-fourVector[p,\[Beta]]fourVector[p,\[Mu]]metric[\[Alpha],\[Nu]]-fourVector[p,\[Alpha]]fourVector[p,\[Nu]]metric[\[Beta],\[Mu]]+fourVector[p,\[Beta]]fourVector[p,\[Nu]]metric[\[Alpha],\[Mu]]))*Exp[-I scalarP[p,x-y]],
		"d",1/384*gs*tripleG*deltaAdj[a,b] scalarP[x-y,x-y](metric[\[Alpha],\[Mu]]metric[\[Beta],\[Nu]]-metric[\[Alpha],\[Nu]]metric[\[Beta],\[Mu]])*Exp[-I scalarP[p,x-y]],
		"e",(1/(2^4 3^4)*gs^2*deltaAdj[a,b]chiral[q]^2 scalarP[x-y,x-y] (metric[\[Alpha],\[Mu]]metric[\[Beta],\[Nu]]-metric[\[Alpha],\[Nu]]metric[\[Beta],\[Mu]])+
		1/(2^4 3^4)*gs^2*deltaAdj[a,b]chiral[q]^2 (fourVector[x-y,\[Alpha]]fourVector[x-y,\[Mu]]metric[\[Beta],\[Nu]]-
		fourVector[x-y,\[Beta]]fourVector[x-y,\[Mu]]metric[\[Alpha],\[Nu]]-
		fourVector[x-y,\[Alpha]]fourVector[x-y,\[Nu]]metric[\[Beta],\[Mu]]+
		fourVector[x-y,\[Beta]]fourVector[x-y,\[Nu]] metric[\[Alpha],\[Mu]]))*Exp[-I scalarP[p,x-y]],
		"f",0,
		"h",0,
		"i",0,
		"j",0,
		"k",0,
		"l",0,
		"m",0,
		"n",0,
		"o",0,
		"p",0
		]
]


(*\:6307\:5b9a\:60ac\:6302\:80f6\:5b50\:7ebf\:7684\:56fe*)
$danglingGluon ={"m","n","o","p"};
(*\:6307\:5b9a\:60ac\:6302\:80f6\:5b50\:7ebf\:7684\:56fe*)
$quantumPropagators ={"a","b","g","h","i","m","o"};


(*\:4f20\:64ad\:5b50\:66ff\:6362*)
colrelator[]:=1
colrelatorG[]:=1
replace[cont_,digq_,OptionsPattern[{"HeavyQuarks"-> $heavyQuarks,"MomentumSpaceG"-> True}]] :=
	replaces[cont,digq,{"a"},"HeavyQuarks"->OptionValue["HeavyQuarks"],"MomentumSpaceG"->OptionValue["MomentumSpaceG"]]
replace[cont_,digq_,digg_,OptionsPattern[{"HeavyQuarks"-> $heavyQuarks,"MomentumSpaceG"-> True}]] :=
	replaces[cont,digq,digg,"HeavyQuarks"->OptionValue["HeavyQuarks"],"MomentumSpaceG"->OptionValue["MomentumSpaceG"]]


replaces[exp_, listq_?ListQ, listG_?ListQ, OptionsPattern[{"HeavyQuarks"-> {},"MomentumSpaceG"-> True}]]:= Module[{cont= exp, 
lorenzin = lorenz@(exp//.{Dot->Times, Track[a___]:>a, Trans[a_]:>a}), colorin = color@(exp//.{Dot->Times, Track[a___]:>a, Trans[a_]:>a}),
lorenzout={}, colorout = {},
tempResult = 1,j =0, k =0,m = 0
},

tempResult = cont/.{DE[{q_,q_},{y_,x_}][ci[{ci1_,ci2_}]]:> colrelator[q,y,x,ci1,ci2,j=j+1](*,DE[{q_,q_},{y_,x_}][ci[{ci1_,ci2_}]][listD_?ListQ]:> colrelator[q,y,x,ci1,ci2,j=j+1,listD]*)};
tempResult = tempResult/.GE[GField[n1_,x_][li[{\[Mu]1_,\[Nu]1_}]],GField[n2_,y_][li[{\[Mu]2_,\[Nu]2_}]]]:> colrelatorG[n1,n2,\[Mu]1,\[Nu]1,\[Mu]2,\[Nu]2,y,x,k=k+1];
tempResult = tempResult /.{colrelator[q_?(!MemberQ[OptionValue["HeavyQuarks"],#]&),y_,x_,ci1_,ci2_,order_]:> QPropagatorx[q,x,y,ci1,ci2, listq[[order]]],
colrelator[q_?(MemberQ[OptionValue["HeavyQuarks"],#]&),y_,x_,ci1_,ci2_,order_]:> QPropagatorp[q,x,y,ci1,ci2,ToExpression["kq"<>ToString[order]], listq[[order]]](*,
colrelator[q_?(!MemberQ[OptionValue["HeavyQuarks"],#]&),y_,x_,ci1_,ci2_,order_,Dlist_]:> Fold[DLorenz[#1,Splice[#2,DLorenz]]&,QPropagatorx[q,x,y,ci1,ci2, listq[[order]]],Dlist],
colrelator[q_?(MemberQ[OptionValue["HeavyQuarks"],#]&),y_,x_,ci1_,ci2_,order_\:ff0cDlist_]:> Fold[DLorenz[#1,Splice[#2,DLorenz]]&,QPropagatorp[q,x,y,ci1,ci2, listq[[order]],ToExpression["kq"<>ToString[order]]],Dlist]*)
};
If[OptionValue["MomentumSpaceG"],
	tempResult =tempResult /.colrelatorG[n1_,n2_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,x_,order_]:> GPropagatorp[n1,n2,\[Mu]1,\[Nu]1,\[Mu]2,\[Nu]2,x,ToExpression["kg"<>ToString[order]], listq[[order]]],								
tempResult  =tempResult /.colrelatorG[n1_,n2_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,x_,order_]:> GPropagatorx[n1,n2,\[Mu]1,\[Nu]1,\[Mu]2,\[Nu]2,x,listq[[order]]]
		];
tempResult  =tempResult //.{Dot[a___,b_?NumericQ c_,d___]:> b Dot[a,c,d],
					Dot[a___,b_?NumericQ ,d___]:>b Dot[a,d],
					Dot[a___,b_?CQ c_,d___]:> b Dot[a,c,d],
					Dot[a___,b_?CQ,c___]:> b Dot[a,c]
};

colorout= Complement[color[tempResult],colorin];
lorenzout= Complement[lorenz[tempResult],lorenzin];

If[Length[Intersection[$danglingGluon,listq]]===0,colorout={};lorenzout = {}];
	resetIndex/@{"\[Mu]","\[Nu]","\[Eta]"};
	Times@@(deltaAdj[#[[1]],#[[2]]]&/@Partition[colorout,2])Times@@(metric[#[[1]],#[[2]]]&/@Partition[lorenzout,2])tempResult(*//.{
					Dot[a___,b_?NumericQ c_,d___]:> b Dot[a,c,d],
					Dot[a___,b_?NumericQ ,d___]:>b Dot[a,d],
					Dot[a___,b_?CQ c_,d___]:> b Dot[a,c,d],
					Dot[a___,b_?CQ,c___]:> b Dot[a,c],
                    Dot[a___,CJM,CJM,b___]:>-Dot[a,b], 
			        Dot[a___,c_+d_,b___]:>Dot[a,c,b]+ Dot[a,d,b]}*)
]

	(*Module[{func =  cont,mydig = digq,mydigg = digg,j=0, k = 0, m = 0, ps = 1,colorin = Flatten[color[First@Transpose[FactorList[cont/.{Dot->Times,Trans->Times,Track->Times}]]]],
	lorenin = Flatten[lorenz[First@Transpose[FactorList[cont/.{Dot->Times,Trans->Times,Track->Times}]]]],colorout = {},lorenout = {},deltalists = {},itemp},
	ps = cont/.DE[{q_,q_},{y_,x_}][ci[{ci1_,ci2_}]]:> colrelator[q,x-y,ci1,ci2,j=j+1];
	ps = ps/.GE[GField[n1_,x_][li[{\[Mu]1_,\[Nu]1_}]],GField[n2_,y_][li[{\[Mu]2_,\[Nu]2_}]]]:> colrelatorG[n1,n2,\[Mu]1,\[Nu]1,\[Mu]2,\[Nu]2,x-y,k=k+1];
	ps =ps/.{colrelator[q_?(MemberQ[OptionValue["HeavyQuarks"],#]&),x_,ci1_,ci2_,l_]:>factorsp[ci1,ci2, q,
																							itemp = If[NumberQ[First[Level[#,{0,Infinity}]]],-1,1]&[x]ToExpression["k"<>ToString[m=m+1]];
																	AppendTo[deltalists,itemp];itemp,mydig[[l]]],
							 colrelator[q_?(!MemberQ[OptionValue["HeavyQuarks"],#]&),x_,ci1_,ci2_,l_]:>factorsx[ci1,ci2, q,x,mydig[[l]]]};

	If[OptionValue["MomentumSpaceG"],
	
		ps =ps/.colrelatorG[n1_,n2_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,x_,l_]:> factorspG[n1,n2,\[Mu]1,\[Nu]1,\[Mu]2,\[Nu]2,itemp = If[NumberQ[First[Level[#,{0,Infinity}]]],-1,1]&[x]ToExpression["k"<>ToString[m=m+1]];
																										AppendTo[deltalists,itemp];
																										itemp,mydigg[[l]]],
																										
		ps =ps/.colrelatorG[n1_,n2_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,x_,l_]:> factorsxG[n1,n2,\[Mu]1,\[Nu]1,\[Mu]2,\[Nu]2,x,mydigg[[l]]]
		];
		(*Dot \:4ee3\:6570*)
		ps = ps//.{Dot[a___,b_?NumericQ c_,d___]:> b Dot[a,c,d],
					Dot[a___,b_?NumericQ ,d___]:>b Dot[a,d],
					Dot[a___,b_?CQ c_,d___]:> b Dot[a,c,d],
					Dot[a___,b_?CQ,c___]:> b Dot[a,c]};
		(*\:8865\:8db3\:54d1\:6307\:6807*)
	colorout= Complement[Flatten[color[First@Transpose[FactorList[ps/.{Dot->Times,Trans->Times,Track->Times}]]]],colorin];
	lorenout = Complement[Flatten[lorenz[First@Transpose[FactorList[ps/.{Dot->Times,Trans->Times,Track->Times}]]]],lorenin];
	If[Length[Intersection[$danglingGluon,mydig]]===0,colorout={};lorenout = {}];
	If[Length[deltalists]===0,1,(diracDelta@@Join[deltalists,{-ToExpression["p"]}]) ]Times@@(delta[#[[1]],#[[2]]]&/@Partition[colorout,2])Times@@(metric[#[[1]],#[[2]]]&/@Partition[lorenout,2])ps
];
*)


End[]
