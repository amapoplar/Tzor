(* ::Package:: *)

BeginPackage["Tzor`"];


CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = TraditionalForm
Print[" Tz\[OAcute]r for ",Style["T",Red],"heori",Style["z",Red],"ed package ",Style["o",Red],"f sum ",Style["r",Red],"ules."]
Print[" SumRules of QCD"]
Print["Get Latest Version : ",Hyperlink["Tzor@github", "https://github.com/amapoplar/Tzor"]]
Hyperlink["Tzor@github", "https://github.com/amapoplar/Tzor"]
Print[" Author: poplar "]
Print[" Version: alpha "]
Print[" Lastest Veresion : This Version"]


$IndicesLists = {li,ci}
$MatrixAndTensorLists = {eps,delta,lambda,sigma,slash}
$SpinAndColor = {spin, color, unspin, uncolor, posandflovor,lorenz}
$SpinSpace = {fourVector,gamma,metric}
$Consendate = {mass,chiral,hybrid,doubleG,tripleG}
$porgator = {factorsx,factorsp,factorsxG,factorspG}


QField::usage = "QuarkQField[f,a,\[Alpha],x] is a flavor f quark filed at postion x with color a and Lonrtez index \[Alpha]";
QFieldB::usage= "QuarkQQFieldB[f,a,\[Alpha],x] is a flavor f anti quark filed at postion x with color a and Lonrtez index \[Alpha]";
GField::usage = "GluonQField[n,li[{\[Mu],\[Nu]}],x]is a gluon tensor QField at postion x with color n, \[Mu] and \[Nu] are Lonrtez indices";
DE::usage = "DE is a quark progrator";
GE::usage = "GE is a gluon progrator";
Track::usage = "Track is trace of matrices";
CJM::usage = "CJM is charge conjugation matrix";
Trans::usage = "Trans is Transpose of something";
bar::usage = "bar is anti-particle of one particle";
wickContract::usage = "wickContract will give a Wick Contract of some operation"
traceContract::usage = "traceContract will give a dot time of matrices"
feynCalc::usage = "give leagal Feynmman diagram of kth dimenson"
NColorFactors::usage = "caculate the value of color factors"
indexNew::usage = "indexNew will give you a new variable never apear before. And Options EndQ means that variable never apeared agin"
replace::usage = "replace Wick contract by diffrent progrator in diffrent postion"
operators::usage = "give vacuum condesion of diffrent experssion"
Einsum::usage = "caclulate the value by Einstein summation convention"
ToTeXForm::usage = "LatexForm of experssion"
fourierTransform::usage = "caclulate the Fourier transform from x-space to p-sapce"
borelTransform::usage="cacluate the Borel transform form q to M"
gs::usage = "strong coupling"
dimension::usage = "dimension of consedate."


Begin["`Private`"];


Format[QField[u_, a_, \[Alpha]_, x_], TraditionalForm] := DisplayForm[RowBox[{SubsuperscriptBox[u, \[Alpha], a], "(", x, ")"}]]
Format[QFieldB[u_, a_, \[Alpha]_, x_], TraditionalForm] := DisplayForm[RowBox[{SubsuperscriptBox[OverBar[u], \[Alpha], a], "(", x, ")"}]] 
Format[GField[n_,x_],TraditionalForm]:= DisplayForm[RowBox[{SuperscriptBox["G", n], "(", x, ")"}]]
Format[gs,TraditionalForm]:= DisplayForm[SubscriptBox["g", "s"]]
Format[GField[n_,x_][li[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[RowBox[{SubsuperscriptBox["G",RowBox[{\[Mu],\[Nu]}],n], "(", x, ")"}]]
Format[GField[n_][li[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[SubsuperscriptBox["G",RowBox[{\[Mu],\[Nu]}],n]]
Format[DE[{ferm_, ferm_}, {x_ , y_}], TraditionalForm] := DisplayForm[RowBox[{SuperscriptBox["S",ferm], "(", y-x, ")"}]]
Format[DE[{ferm_,ferm_},{x_,y_}][li[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[RowBox[{SubsuperscriptBox["S",RowBox[{\[Mu],\[Nu]}],ferm], "(", y-x, ")"}]]
Format[DE[{ferm_,ferm_},{x_,y_}][ci[{\[Mu]_,\[Nu]_}]],TraditionalForm]:=DisplayForm[RowBox[{SubsuperscriptBox["S",RowBox[{\[Mu],\[Nu]}],ferm], "(", y-x, ")"}]]
Format[DE[{ferm_,ferm_},{x_,y_}][ci[{a_,b_}],li[{\[Mu]_,\[Nu]_}]],TraditionalForm]:= DisplayForm[RowBox[{SubsuperscriptBox["S", RowBox[{\[Mu], \[Nu]}], RowBox[{ferm, ",", RowBox[{a, b}]}]], "(",y-x, ")"}]]
Format[GE[GField[n1_,x_][li[{\[Mu]1_,\[Nu]1_}]],GField[n2_,y_][li[{\[Mu]2_,\[Nu]2_}]]],TraditionalForm]:= DisplayForm[RowBox[{"\[LeftAngleBracket]",SubsuperscriptBox["G",RowBox[{\[Mu]1,\[Nu]1}],n1],"(", x, ")",SubsuperscriptBox["G",RowBox[{\[Mu]2,\[Nu]2}],n2],"(", y, ")", "\[RightAngleBracket]"}]]
SetAttributes[GE,Orderless]


Format[mass[q_], TraditionalForm] := DisplayForm[SubscriptBox["m", RowBox[{q}]]]
Format[chiral[q_],TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]",OverscriptBox[q,"_"],q,"\[RightAngleBracket]"}]]
Format[hybrid[q_],TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]",gs,OverscriptBox[q,"_"],"\[Sigma]","\[CenterDot]","G",q,"\[RightAngleBracket]"}]]
Format[doubleG,TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]",SuperscriptBox[gs,2],SuperscriptBox["G",2],"\[RightAngleBracket]"}]]
Format[tripleG,TraditionalForm]:=DisplayForm[RowBox[{"\[LeftAngleBracket]","f",SuperscriptBox["G",3],"\[RightAngleBracket]"}]]


dimension[mass[q_]]:=1
dimension[chiral[q_]]:=3
dimension[hybrid[q_]]:=5
dimension[doubleG]:=4
dimension[tripleG]:=6
dimension[a_ b_]:=dimension[a]+dimension[b]
dimension[a_^n_]:=n dimension[a]
dimension[a_]:=0


(*\:77e9\:9635\:7684\:6837\:5f0f*)
MakeBoxes[(h_)[li[{a_, b_}]], TraditionalForm] := 
 SubscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]

MakeBoxes[(h_)[ci[{a_, b_}]], TraditionalForm] := 
 SuperscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]



(*epsilon\:4e0edelta\:7684\:6837\:5f0f*)
Format[eps[a_, b_, c_], TraditionalForm] := 
 DisplayForm[SuperscriptBox["\[Epsilon]", RowBox[{a, b, c}]]]
Format[delta[a_, b_], TraditionalForm] := 
 DisplayForm[SuperscriptBox["\[Delta]", RowBox[{a, b}]]]
 Format[lambda[a_,b_,n_],TraditionalForm]:=
 DisplayForm[Subsuperscript["\[Lambda]", RowBox[{a,b}],RowBox[{n}]]]
Format[sigma[a_,b_],TraditionalForm]:=
 DisplayForm[SuperscriptBox["\[Sigma]", RowBox[{a, b}]]]
Format[slash[a_],TraditionalForm]:=
 DisplayForm[OverHat[a]]


Format[Track[func_], 
  TraditionalForm] := 
 DisplayForm[RowBox[{"tr(",func,")"}]]
Format[CJM,TraditionalForm]:= DisplayForm[
  Style["C",Italic]]
Format[Trans[p_],TraditionalForm]:= DisplayForm[
  SuperscriptBox[p,"T"]]
Format[bar[q_],TraditionalForm]:=DisplayForm[OverscriptBox[q,"_"]]


slash[-a_]:=-slash[a]
Protect[QField,QFieldB,GField,DE]
Protect[li,ci]
Protect[eps,delta,lambda,sigma,slash]


NM[a___, -c__, b___] := -NM[a, c, b]
NM[a___, -1, b___] := -NM[a, b]
SetAttributes[NM,{OneIdentity,Flat}]


Track[a___,b_?NumericQ c_,d___]:=b Track[a,c,d];
Track[a___,b_?NumericQ,d___]:=b Track[a,d];
CQ[expr_]:=(Head[expr]=!=gamma)\[And](Head[expr]=!=sigma)\[And](Head[expr]=!=slash);
CQ[Trans[expr_]]:=CQ[expr]
CQ[CJM]:= False;
CQ[a_ b_]:=CQ[a]\[And]CQ[b];
CQ[a_+b_]:=CQ[a]\[And]CQ[b];
CQ[a_ .b_]:=CQ[a]\[And]CQ[b];
Trans[Trans[a_]]:= a
Trans[a_+b_]:= Trans[a]+Trans[b]
Trans[a_ b_?CQ]:=b Trans[a]
Trans[1]:= 1
Trans[a_?NumberQ]:= a
Track[a_?CQ b_]:= a Track[b]


scalarQ[expr_]:=Head[expr]=!=(QField\[Or]QFieldB);
quarkQ[expr_]:= (Head[expr] === QField) || (Head[expr] === QFieldB);
gluonQ[expr_ ]:=Head[Head[expr]]===GField;
qorgQ[expr_]:= quarkQ[expr]||gluonQ[expr];
ProgQ[pro_]:= Head[Head[pro]]===DE;
SetAttributes[ProgQ,Listable]


tempContract[]:= 1
tempT[]:= 1
lign[exp1_ , exp2_]:= Flatten[Position[exp1,#]&/@exp2]; 
wickContract[expr_] := 
 Block[{res}, res = expr/. NonCommutativeMultiply -> NM;
NM[NM@@(DeleteCases[res, _?qorgQ, \[Infinity]]), 
    tempContract[Cases[res, _?qorgQ, \[Infinity]]]]/.NM->Times];
tempContract[cur_]:= Module[{exp =cur,qcurent= 0,gcurent =0, quarks = Select[cur,Head[#]==QField&],qbars = Select[cur,Head[#]==QFieldB&],gluons = Select[cur,gluonQ],
erros = 1,erromassages = {},qtemps = 0,gtemps = 0,res = 0},
erromassages = {"\:8ba1\:7b97\:6210\:529f","\:6b63\:53cd\:5938\:514b\:6570\:76ee\:4e0d\:4e00\:81f4\:ff0c\:8bf7\:68c0\:67e5\:6d41\:5f62\:5f0f","\:5176\:4ed6\:9519\:8bef"};
If[Length[quarks]!= Length[qbars],erros =2];
qcurent = Riffle[quarks,#]&/@Permutations[qbars];
qtemps =Plus@@(((Signature@lign[exp,#])*(tempT@@#)&/@qcurent)//.tempT[arg1___,QField[q1_,ci1_,li1_,x_],QFieldB[q2_,ci2_,li2_,y_],arg2___]:> I DE[{q1,q2},{y,x}][ci[{ci1,ci2}],li[{li1,li2}]]*tempT[arg1,arg2])/.{DE[{ferm1_, ferm2_}, __][___] /; ferm1 =!= ferm2 :>  0,DE[{___}, {x_,y_}][___] /; x ==y  :>  0};
Plus@@(qtemps*tempT@@@Permutations[gluons]//.tempT[arg1___,GField[n1_,x_][li[{li1_,li2_}]],GField[n2_,y_][li[{li3_,li4_}]],arg2___]:> GE[GField[n1,x][li[{li1,li2}]],GField[n2,y][li[{li3,li4}]]]*tempT[arg1,arg2])/.{GE[GField[_,x][___],GField[_,x][___]]:>  0}
];


(*\:5224\:65ad\:81ea\:65cb\:4e0e\:989c\:8272\:6307\:6807*)
spin[DE[{ferm_, ferm_}, {x_ , y_} ] [ci_,li[si_]]]:=si
spin[a_[li[si_]]]:=si
spin[eps[a_,b_,c_]]:=Nothing
spin[Trans[a_]]:=Reverse[spin[a]]
spin[a_]:=Nothing
spin[a_?scalarQ]:=Nothing
spin[GE[__]]:=Nothing
unspin[con_]:=Module[{cons = con},cons /.{a_[li[si_]]:>a,DE[a__][ci[cis_],li[si_]]:>DE[a][ci[cis]]}]
color[DE[{ferm_, ferm_}, {x_ , y_} ] [li[ci_],li[si_]]]:=ci 
color[DE[{ferm_, ferm_}, {x_ , y_} ] [ci[ci_]]]:=ci 
color[a_[ci[ci_]]]:=ci
color[a_[li[si_],ci[ci_]]]:=ci
color[a_[li[si_]]]:= Nothing
color[delta[a_,b_]]:={a,b}
color[eps[a_,b_,c_]]:={a,b,c}
color[lambda[a_,b_,n_]]:={a,b,n}
color[Trans[a_]]:=color[a]
color[a_]:=Nothing
uncolor[con_]:=Module[{cons = con},cons /.{a_[ci[ci_]]:>a,DE[a__][ci[ci_],li[si_]]:>DE[a][li[ci]]}]
posandflovor[DE[{q_,q_},{y_,x_}][___]]:={q,x-y}
posandflovor[a___]:=Nothing
lorenz[gamma[\[Mu]_]]:={\[Mu]}
lorenz[sigma[\[Mu]_,\[Nu]_]]:={\[Mu],\[Nu]}
lorenz[Trans[a_]]:=Reverse[lorenz[a]]
lorenz[a_]:=Nothing
SetAttributes[{color,spin,posandflovor,lorenz},Listable]


(*\:7565\:53bb\:6d1b\:4f26\:5179\:6307\:6807*)
givePointsByEdge[UndirectedEdge[a_,b_]]:= {a,b}
SetAttributes[givePointsByEdge,Listable]
TransQ[h_]:=Head[h]==Trans
Tracktemp[Dot[conj__]]:=If[MatchQ[Table[conj [[i]],{i,Length[conj]}],{__?TransQ}],Track[Dot@@Table[Trans[conj [[i]]],{i,Length[conj]}]],Track[Dot@@RotateLeft[#,Position[ProgQ[#],True][[1,1]]-1]&@Table[conj [[i]],{i,Length[conj]}]]]
findMatrixsWithIndex[list_,index_]:=Union[Select[list,SameQ[spin[#],index]&],Trans/@Select[list,SameQ[spin[#],Reverse[index]]&]]
traceContract[mycon_]:=Module[{mylist= Transpose[FactorList[mycon]][[1]],fac= Times@@Transpose[FactorList[mycon]][[2]]},
fac = fac*Times@@Select[mylist,spin[#]===Nothing&];
mylist = Select[mylist,spin[#]=!=Nothing&];
fac*join[mylist]
]//Quiet;
setGraphWithIndices[index_?ListQ]:=(#[[1]]\[UndirectedEdge]#[[2]])&/@index
join[con_]:=Block[{list = con,spins = spin/@con,graph = Null,temps = Null,templist,TrQ=Null,reverse = {},reverseQ = {}},
graph = setGraphWithIndices[spins];
graph = ConnectedGraphComponents[graph];
templist = If[AcyclicGraphQ[#],temps = FindHamiltonianPath[#];reverse ={First[temps],Last[temps]};
reverseQ =  Select[Flatten[spins],MemberQ[reverse ,#]&];
If[EvenQ[First[First[Position[Flatten[spins],First[temps]]]]],temps = Reverse[temps]];{{Table[{temps[[i-1]],temps[[i]]},{i,2,Length[temps]}]},0},{givePointsByEdge[FindPostmanTour[#]],1}]&/@graph;
TrQ = Last[Transpose[templist]];
templist =Table[Dot@@Flatten[findMatrixsWithIndex[list,#]&/@templist[[i,1,1]]],{i,Length[templist]}];
Times@@Table[If[TrQ[[i]]==1,Tracktemp[templist[[i]]],templist[[i]]],{i,Length[TrQ]}]//unspin
]//Quiet;



(*\:65b0\:6307\:6807*)
$counts = {index=0}
indexNew[\[Mu]_?StringQ,OptionsPattern["EndQ"-> False]]:= Module[{},If[OptionValue["EndQ"],index=index+1;ToExpression[ToString[\[Mu]]<>ToString[index-1]],ToExpression[ToString[\[Mu]]<>ToString[index]]]];


(*x\:7a7a\:95f4\:4f20\:64ad\:5b50*)
factorsx[a_,b_,q_,x_,key_]:= Block[{fa},fa = {{"a",I/(2 \[Pi]^2 x^4)*delta[a,b]*slash[x]},
{"b",1/2 lambda[a,b,indexNew["\[Eta]"]]*(I gs GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]])/(32 \[Pi]^2 x^2)*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]].slash[x]+slash[x].sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]])},
{"c",-(1/12) chiral[q]*delta[a,b]},
{"d",1/192 x^2 hybrid[q]*delta[a,b]},
{"e",-(I (gs^2 x^2 chiral[q]^2)/7776)*delta[a,b]*slash[x]},
{"f",-((x^4 \[LeftAngleBracket]gs^2 "G"^2\[RightAngleBracket] chiral[q])/\!\(TraditionalForm\`27648\))*delta[a,b]},
{"g",-(mass[q]/(4 \[Pi]^2 x^2))*delta[a,b]},
{"h",(gs GField[indexNew["\[Eta]"],0][li[{indexNew["\[Mu]"],indexNew["\[Nu]"]}]]Log[-x^2] mass[q])/(32 \[Pi]^2)*sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]*lambda[a,b,indexNew["\[Eta]"]]},
{"i",-((x^2 doubleG Log[-x^2] mass[q])/(1536 \[Pi]^2))*delta[a,b]},
{"j",1/48 I chiral[q] mass[q]*slash[x]*delta[a,b]},
{"k",-((I x^2 hybrid[q] mass[q])/1152)*delta[a,b]*slash[x]},{"l",-((gs^2 x^4 chiral[q]^2 mass[q])/31104)*delta[a,b]},
{"m",(I)/(32 \[Pi]^2 x^2)*1/2 *(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]].slash[x]+slash[x].sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]])lambda[a,b,indexNew["\[Eta]"]]},
{"n",(Log[-x^2] mass[q])/(32 \[Pi]^2)*lambda[a,b,indexNew["\[Eta]"]]*sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]},
{"o",-(1/(2^6*3))hybrid[q]*sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]]*1/2 lambda[a,b,indexNew["\[Eta]"]]},
{"p",(I mass[q])/(2^8*3) hybrid[q]*(sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]].slash[x]+slash[x].sigma[indexNew["\[Mu]"],indexNew["\[Nu]"]])*1/2 lambda[a,b,indexNew["\[Eta]"]]}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]] 


factorsp[a_,b_,q_,k_,key_]:= Block[{fa, knew = indexNew[ToString[k]]},fa = {
  {"a",I*slash[knew]/(knew^2-mass[q]^2)},
  {"b",0},
  {"c",0},
  {"d",0},
  {"e",0},
  {"f",0},
  {"g",I*mass[q]/(knew^2-mass[q]^2)},
  {"h",0},
  {"i",0},
  {"j",0},
  {"k",0},
  {"l",0},
  {"m",0},
  {"n",0},
  {"o",0},
  {"p",0}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]] 


factorsxG[a_,b_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,x_,key_]:=Block[{fa},fa ={{"a",x}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]]


factorspG[a_,b_,\[Mu]1_,\[Nu]1_,\[Mu]2_,\[Nu]2_,k_,key_]:=Block[{fa,knew = indexNew[ToString[k]]},fa ={{"a",knew}};
fa= Association[Table[fa[[i,1]]-> fa[[i,2]],{i,Length[fa]}]];
indexNew["TZOR","EndQ"->True];
fa[key]]


(*\:6307\:5b9a\:60ac\:6302\:80f6\:5b50\:7ebf\:7684\:56fe*)
$hangGluon ={"m","n","o","p"}


(*\:4f20\:64ad\:5b50\:66ff\:6362*)
colrelator[]:=1
colrelatorG[]:=1
replace[cont_,digq_,OptionsPattern[{"HeavyQuarks"-> {ToExpression["b"],ToExpression["c"],ToExpression["Q"]},"MomentumSpaceG"-> False}]] := replaces[cont,digq,{"a"},"HeavyQuarks"->OptionValue["HeavyQuarks"],"MomentumSpaceG"->OptionValue["MomentumSpaceG"]]
replace[cont_,digq_,digg_,OptionsPattern[{"HeavyQuarks"-> {},"MomentumSpaceG"-> False}]] := replaces[cont,digq,digg,"HeavyQuarks"->OptionValue["HeavyQuarks"],"MomentumSpaceG"->OptionValue["MomentumSpaceG"]]
replaces[cont_,digq_?ListQ,digg_?ListQ, OptionsPattern[{"HeavyQuarks"-> {},"MomentumSpaceG"-> False}]]:=Module[{func =  cont,mydig = digq,mydigg = digg,j=0,k = 0,ps = 1,colorin = Flatten[color[First@Transpose[FactorList[cont/.{Dot->Times,Trans->Times,Track->Times}]]]],
lorenin = Flatten[lorenz[First@Transpose[FactorList[cont/.{Dot->Times,Trans->Times,Track->Times}]]]],colorout = {},lorenout = {}},
ps = cont/.DE[{q_,q_},{y_,x_}][ci[{ci1_,ci2_}]]:> colrelator[q,x-y,ci1,ci2,j=j+1];
ps = ps/.GE[GField[n1_,x_][li[{\[Mu]1_,\[Nu]1_}]],GField[n2_,y_][li[{\[Mu]2_,\[Nu]2_}]]]:> colrelatorG[n1,n2,\[Mu]1,\[Mu]2,\[Nu]1,\[Nu]2,x-y,k=k+1];
ps = ps/.{colrelator[q_?(MemberQ[OptionValue["HeavyQuarks"],#]&),x_,ci1_,ci2_,l_]:>factorsp[ci1,ci2, q,ToExpression["k"],mydig[[l]]],colrelator[q_?(!MemberQ[OptionValue["HeavyQuarks"],#]&),x_,ci1_,ci2_,l_]:>factorsx[ci1,ci2, q,x,mydig[[l]]]};
If[OptionValue["MomentumSpaceG"]===True,ps =ps/.colrelatorG[n1_,n2_,\[Mu]1_,\[Mu]2_,\[Nu]1_,\[Nu]2_,x_,m_]:> factorspG[n1,n2,\[Mu]1,\[Mu]2,\[Nu]1,\[Nu]2,ToExpression["k"],mydigg[[m]]],ps =ps/.colrelatorG[n1_,n2_,\[Mu]1_,\[Mu]2_,\[Nu]1_,\[Nu]2_,x_,m_]:> factorsxG[n1,n2,\[Mu]1,\[Mu]2,\[Nu]1,\[Nu]2,x,mydigg[[m]]]];
ps = ps//.{Dot[a___,b_?NumericQ c_,d___]:> b Dot[a,c,d],Dot[a___,b_?NumericQ ,d___]:>b Dot[a,d],Dot[a___,b_?CQ c_,d___]:> b Dot[a,c,d],Dot[a___,b_?CQ,c___]:> b Dot[a,c]};
colorout= Complement[Flatten[color[First@Transpose[FactorList[ps/.{Dot->Times,Trans->Times,Track->Times}]]]],colorin];
lorenout = Complement[Flatten[lorenz[First@Transpose[FactorList[ps/.{Dot->Times,Trans->Times,Track->Times}]]]],lorenin];
If[Length[Intersection[$hangGluon,mydig]]===0,colorout={};lorenout = {}];
Times@@(delta[#[[1]],#[[2]]]&/@Partition[colorout,2])Times@@(metric[#[[1]],#[[2]]]&/@Partition[lorenout,2])ps
];


(*\:8272\:6307\:6807\:8ba1\:7b97*)
filename = "factors.csv"
parameter[f_[a__]]:=Partition[Riffle[{a},Table[3,3]],2]
parameter[lambda[a_,b_,n_]]:={{a,3},{b,3},{n,8}}
parameters[f_]:=parameter[f]
parameters[Times[f_,g_]]:=Union[ parameter[f],parameters[Times[g]]];
caculateColor[hashStr_,exp_]:= Module[{indexOfSum =  parameters[exp],expused=exp,log="",
epslion = Flatten[Table[LeviCivitaTensor[3][[i,j,k]],{i,1,3},{i,1,3},{j,1,3},{k,1,3}],1],
del = {{1,0,0},{0,1,0},{0,0,1}},Gellman = {{{0,1,0},{1,0,0},{0,0,0}},{{0,-I,0},{I,0,0},{0,0,0}},{{1,0,0},{0,-1,0},{0,0,0}},{{0,0,1},{0,0,0},{1,0,0}},{{0,0,-I},{0,0,0},{I,0,0}} ,{{0,0,0},{0,0,1},{0,1,0}},{{0,0,0},{0,0,-I},{0,I,0}},1/Sqrt[3] {{1,0,0},{0,1,0},{0,0,-2}}}
},
SetDirectory[NotebookDirectory[]];
log = \!\(TraditionalForm\`File[FileNameJoin[{NotebookDirectory[], filename}]]\);
expused = expused/.{eps[a_,b_,c_]:> epslion[[a,b,c]],delta[a_,b_]:> del[[a,b]],lambda[a_,b_,n_]:>Gellman[[n,a,b]]};
expused = Fold[Sum[#1,#2]&,expused,indexOfSum];
OpenAppend[log];
If[NumberQ[expused],WriteString[log,hashStr,",",ToString[expused]<>"\n"]];
Close[log];
expused
]//Quiet
NColorFactors[0]:= 0
NColorFactors[Times[ factor_,factor1__]]:= Block[{fac =SortBy[Select[{factor,factor1},Length[color[#]]>1&],First],fac1 =Times@@Select[{factor,factor1},Length[color[#]]<=1&],facs = ToExpression[Import[NotebookDirectory[]<>"facs5.csv","Data"]],str},
facs = Association[Table[facs[[i,1]]->facs[[i,2]],{i,Length[facs]}]];
str = ToString[Join[Select[fac,Head[#]===eps&],Select[fac,Head[#]=!=eps&]]];
If[Head[facs[Hash[str]]]===Missing,caculateColor[Hash[str],Times@@fac]*fac1,facs[Hash[str]]fac1]
]//Quiet
SetAttributes[NColorFactors,Listable]


(*\:63d0\:53d6\:7b97\:7b26*)



(*\:6c42\:548c\:7ea6\:5b9a*)
Einsum1[forum_,{var_, upper_}]:=Module[{fumar=forum,x= var, index = upper},
rule = {x->#}&/@Table[i,{i,index}];
Plus@@(fumar/.rule)];

Einsum[forum_,{var__, upper_}]:=Module[{fumar=forum,table= var, index = upper},
While[table !=  {},
fumar = Einsum1[fumar,{table[[1]],upper}];
table = Delete[table,1];
 Einsum[fumar,{table,upper}]];
fumar
]


ToTeXForm[expr_]:=Block[{},
part1=expr/.{
(H_)[ci[a__],si[b__]]/;Head[H]=!=DE :>TForm[H[ci[a],si[b]]],
(H_)[si[b__]]/;H=!=DE :>TForm[H[si[b]]],
(H_)[ci[a__]]/;H=!=DE :>TForm[H[ci[a]]]
};
WriteString["stdout",ToString[part1,TeXForm]]
]


(*\:56db\:77e2\:548c\:5ea6\:89c4\:7684\:683c\:5f0f*)
Format[fourVector[v_,index_],TraditionalForm]:=DisplayForm[Subscript[v,index]]
Format[metric[index1_,index2_],TraditionalForm]:=DisplayForm[Subscript["g",RowBox[{index1,index2}]]]
Format[gamma[index_],TraditionalForm]:=DisplayForm[Superscript["\[Gamma]",index]]
SetAttributes[metric,Orderless]
(*\:56db\:7ef4\:504f\:5bfc\:6027\:8d28\:548c\:5b9a\:4e49*)
DLorenz[fourVector[x_,index1_],x_,index2_]:=metric[index1,index2]
DLorenz[metric[index1_,index2_],x_,index3_]:=0
DLorenz[fun1_ fun2_,x0_,index_]:=DLorenz[fun1,x0,index]fun2+fun1 DLorenz[fun2,x0,index]
DLorenz[fun1_ +fun2_,x0_,index_]:=DLorenz[fun1,x0,index]+DLorenz[fun2,x0,index]
DLorenz[fun0_?(!MemberQ[#,fourVector]&),x0_,index_]:=Module[{\[Nu] = Exponent[fun0,x0]},If[\[Nu]==2,2 fourVector[x0,index],(D[fun0/.{x0->Sqrt[x0]},x0]/.{x0->x0^2})DLorenz[x0^2,x0,index]]]
(*\:8f85\:52a9\:51fd\:6570*)
giveIndice[fourVector[a_,index_],a_]:=index
giveIndice[a_,x_]:=Nothing
giveIndice[a_ b_,x_]:=Flatten[{giveIndice[a,x],giveIndice[b,x]}]

(*\:5085\:91cc\:53f6\:4e3b\:79ef\:5206\:51fd\:6570*)
fourierMasterFunction[s_,q_]:=- I 2^(4-2s) \[Pi]^2 (-q^2)^(s-2) Gamma[2-s]/Gamma[s]
(*\:5085\:91cc\:53f6\:53d8\:6362*)
fourierTransform[a_?NumericQ,x0_,p0_]:=a (2\[Pi])^4 delta[p0]
fourierTransform[func1_+func2_,x0_,p0_]:=fourierTransform[func1,x0,p0]+fourierTransform[func2,x0,p0]
fourierTransform[func0_,x0_,p0_]:=Module[{fun = func0,s=-Exponent[func0,x0]/2,x=x0,p=p0,nslash = MemberQ[func0,slash[x0],Infinity],nlog =Exponent[func0,Log[-x0^2]],slashindex = Nothing,nLorenz =giveIndice[func0,x0] ,tempresult=0,usedfunction = fourierMasterFunction,arg},
usedfunction=(-1)^nlog D[fourierMasterFunction[arg,p],{arg,nlog}];
tempresult=SeriesCoefficient[usedfunction/.{arg->s+\[Epsilon]},{\[Epsilon],0,0}]//Expand;
tempresult = If[Head[tempresult]=!=  Plus,tempresult,Select[tempresult,(MemberQ[#,Log[-p^2],Infinity])\[Or](Exponent[#,p]<0)&]];
fun = fun/.{fourVector[x,q_]:>1,Log[-x^2]->1};
tempresult = If[Length[nLorenz]>0,Fold[ Expand[-I DLorenz[#1,p,#2]]&,tempresult*(-x^2)^s*fun,nLorenz ],tempresult*(-x^2)^s*fun];
If[nslash,slashindex = ToExpression["\[Alpha]"<>Fold[StringDelete,DateString[],{" ",":"}]];
Expand[-I DLorenz[tempresult,p,slashindex]]/.{fourVector[p,slashindex]->1,slash[x]-> slash[p],metric[index_,slashindex]:>  gamma[index]},tempresult ]
]


(*borel\:53d8\:6362*)
ff[s_]:=ToExpression["Q"]^(s-4) 2^(2-s) Gamma[2-s/2]/Gamma[s/2]
gg[s_]:=ToExpression["M"]^(s-2) 2^(2-s) 1/Gamma[s/2]
borelTransform[Log[Q_],Q_,M_]:=Log[Q]:> -M^2/2
borelTransform[0,Q_,M_]:={q_:> q}
borelTransform[ss_,Q_,M_]:=Module[
{k= Exponent[ss,Q]+4,n=4,jj},
jj=(Table[{SeriesCoefficient[Series[ff[k+s],{s,0,n}],j],SeriesCoefficient[Series[gg[k+s],{s,0,n}],j]},{j,-1,n}]//Expand);
jj = jj/.{Log[q_]^l_ q_^k_:> m[q,k,l],Log[q_]^l_/q_^k_:> m[q,-k,l],Log[q_]q_^k_:> m[q,k,1],Log[q_]/q_^k_:> m[q,-k,1]};

jj = jj/.{Log[q_]^l_:> m[q,0,l],Log[q_]:> m[q,0,1],1/q_^k_:> m[q,-k,0],q_^k_:> m[q,k,0]};

jj = Solve[Table[jj[[i,1]]==jj[[i,2]],{i,1,Length[jj]}]][[1]];
jj/.m[q_,k_,l_]:> Log[q]^l q^k//FullSimplify
]
SetAttributes[fourierTransform,Listable]


End[];


EndPackage[];
