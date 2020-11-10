(* ::Package:: *)

(* Author: poplar *)
(* Date: 19.3.12 *)
(* Version: alpha *) 


BeginPackage["SumRules`"]
CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = TraditionalForm
Print["SumRules of QCD"]
Print[" Author: poplar "]
Print[" Version: alpha "]

(* \:9884\:52a0\:8f7d *)


(*\:8bbe\:7f6e\:8f93\:51fa\:6837\:5f0f*)

(*\:5938\:514b\:7684\:6837\:5f0f*)
Format[Field[u_, a_, \[Alpha]_, x_], TraditionalForm] := 
 DisplayForm[RowBox[{SubsuperscriptBox[u, \[Alpha], a], "(", x, ")"}]]

Format[FieldB[u_, a_, \[Alpha]_, x_], TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[OverBar[u], \[Alpha], a], "(", x, ")"}]]
  
(*\:4f20\:64ad\:5b50\:7684\:6837\:5f0f*)
Format[DE[{ferm_, ferm_}, {x_ , y_} ] [CI[ {ci1_, ci2_}],SI[ {si1_, si2_}]], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[S, RowBox[{si1, si2}], 
     RowBox[{ferm, ",", RowBox[{ci1, ci2}]}]], "(",y-x, ")"}]]

Format[DEInverse[{ferm_, ferm_}, {x_ , y_} ] [CI[ {ci1_, ci2_}],SI[ {si1_, si2_}]], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[S, RowBox[{si1, si2}], 
     RowBox[{"-1",ferm, ",", RowBox[{ci1, ci2}]}]], "(", y-x, ")"}]]

Format[DE[{ferm_, ferm_}, {x_ , y_} ], 
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SuperscriptBox[S,  
     ferm], "(", y-x, ")"}]]
 
Format[DE[{ferm_, ferm_}, {x_ , y_} ] [CI[ {ci1_, ci2_}]],
  TraditionalForm] := 
 DisplayForm[
  RowBox[{SubsuperscriptBox[S, ferm, 
      RowBox[{ci1, ci2}]], "(",y-x, ")"}]]
(*\:77e9\:9635\:7684\:6837\:5f0f*)
MakeBoxes[(h_)[SI[{a_, b_}]], TraditionalForm] := 
 SubscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]

MakeBoxes[(h_)[CI[{a_, b_}]], TraditionalForm] := 
 SuperscriptBox[ToBoxes[h,TraditionalForm], RowBox[{ToString[a], "", ToString[b]}]]
 
(*epsilon\:4e0edelta\:7684\:6837\:5f0f*)
Format[DD[a_, b_], TraditionalForm] := 
 DisplayForm[SubscriptBox["\[Delta]", RowBox[{a, b}]]]
Format[eps[a_, b_, c_], TraditionalForm] := 
 DisplayForm[SuperscriptBox[\[Epsilon], RowBox[{a, b, c}]]]
Format[delta[a_, b_], TraditionalForm] := 
 DisplayForm[SuperscriptBox[\[Delta], RowBox[{a, b}]]]
 Format[lambda[a_,b_,n_],TraditionalForm]:=
 DisplayForm[Subsuperscript[\[Lambda], RowBox[{a,b}],RowBox[{n}]]]
Format[sigma[a_,b_],TraditionalForm]:=
 DisplayForm[SuperscriptBox[\[Sigma], RowBox[{a, b}]]]
Format[slash[a_],TraditionalForm]:=
 DisplayForm[OverHat[a]]


SetAttributes[NM, {OneIdentity, Flat}]
NM[a___, -c__, b___] := -NM[a, c, b]
NM[a___, -1, b___] := -NM[a, b]


(*\:5224\:65ad\:5938\:514b\:4e0e\:6807\:91cf*)
scalarQ[expr_]:=Head[expr]=!=(Field\[Or]FieldB);
quarkQ[expr_]:= (Head[expr] === Field) || (Head[expr] === FieldB);
ProgQ[pro_]:= Head[Head[pro]]===DE;
SetAttributes[ProgQ,Listable]


Format[Track[func__], 
  TraditionalForm] := 
 DisplayForm[
tr[func]]
Format[CJM,TraditionalForm]:= DisplayForm[
  Style["C",Italic]]
Format[Trans[p_],TraditionalForm]:= DisplayForm[
  SuperscriptBox[p,"T"]]
Format[bar[q_],TraditionalForm]:=DisplayForm[OverscriptBox[q,"_"]]


Unprotect[Dot];
Dot[s___,a_ b_,c___]:=a Dot[s,b,c];
Dot[s___,1,c___]:= Dot[s,c];
Protect[Dot];
Track[a___,b_?NumberQ c_,d___]:=b Track[a,c,d];
Trans[Trans[a_]]:= a


(*\:7ef4\:514b\:6536\:7f29*)
tempContract[]:= 1
tempT[]:= 1
sign[exp1_ , exp2_]:= Flatten[Position[exp1,#]&/@exp2]; 
wickContract[expr_] := 
 Block[{res}, res = expr /. NonCommutativeMultiply -> NM;
  NM[NM@@(DeleteCases[res, _?quarkQ, \[Infinity]]), 
    tempContract[Cases[res, _?quarkQ, \[Infinity]]]]/.NM->Times];
tempContract[cur_]:= Module[{exp =cur,curent= 0, quarks = Select[cur,Head[#]==Field&],qbars = Select[cur,Head[#]==FieldB&],erros = 1,erromassages = {},res = 0},
erromassages = {"\:8ba1\:7b97\:6210\:529f","\:6b63\:53cd\:5938\:514b\:6570\:76ee\:4e0d\:4e00\:81f4\:ff0c\:8bf7\:68c0\:67e5\:6d41\:5f62\:5f0f","\:5176\:4ed6\:9519\:8bef"};
If[Length[quarks]!= Length[qbars],erros =2];
curent = Riffle[quarks,#]&/@Permutations[qbars];
Plus@@(Signature@sign[exp,#]*tempT@@#&/@curent//.tempT[arg1___,Field[q1_,ci1_,si1_,x_],FieldB[q2_,ci2_,si2_,y_],arg2___]:> I DE[{q1,q2},{y,x}][CI[{ci1,ci2}],SI[{si1,si2}]]*tempT[arg1,arg2])/.{DE[{ferm1_, ferm2_}, __][___] /; ferm1 =!= ferm2 :>  0,DE[{___}, {x_,y_}][___] /; x ==y  :>  0}

];


(*\:5224\:65ad\:81ea\:65cb\:4e0e\:989c\:8272\:6307\:6807*)
spin[DE[{ferm_, ferm_}, {x_ , y_} ] [ci_,SI[si_]]]:=si 
spin[a_[SI[si_]]]:=si
spin[eps[a_,b_,c_]]:=0
spin[Trans[a_]]:=Reverse[spin[a]]
spin[a_]:=If[NumberQ[a],0]
unspin[con_]:=Module[{cons = con},cons /.{a_[SI[si_]]:>a,DE[a__][CI[ci_],SI[si_]]:>DE[a][CI[ci]]}]
color[DE[{ferm_, ferm_}, {x_ , y_} ] [CI[ci_],SI[si_]]]:=ci 
color[DE[{ferm_, ferm_}, {x_ , y_} ] [CI[ci_]]]:=ci 
color[a_[CI[ci_]]]:=ci
color[a_[SI[si_],CI[ci_]]]:=ci
color[a_[SI[si_]]]:= {0,0}
color[delta[a_,b_]]:={a,b}
color[eps[a_,b_,c_]]:={a,b,c}
color[lambda[a_,b_,n_]]:={a,b,n}
color[Trans[a_]]:=color[a]
color[a_]:=If[NumberQ[a],0]
uncolor[con_]:=Module[{cons = con},cons /.{a_[CI[ci_]]:>a,DE[a__][CI[ci_],SI[si_]]:>DE[a][SI[ci]]}]
posandflovor[DE[{q_,q_},{y_,x_}][___]]:={q,x-y}
posandflovor[a___]:=0
SetAttributes[{color,spin,posandflovor},Listable]


(*\:7565\:53bb\:6d1b\:4f26\:5179\:6307\:6807*)
Tracktemp[Dot[conj_]]:=Track[Dot@@RotateLeft[#,Position[ProgQ[#],True][[1,1]]-1]&@Table[conj [[i]],{i,Length[conj]}]]
join[con_]:=Block[{list = con,templist = {},pointer = Null,postion = {1,2},flag = Range[Length[con]],Qslash = False,element = 1,result = {},resulttemp = {},transQ = Table[0,Length[con]],n=0},
templist = spin/@list;
 If[Mod[Length[Select[list,ProgQ]],2]==1,Qslash =True];
If[Qslash,element = First[FirstCase[Sort[Tally[Flatten[templist]]],{_,1}]];
postion = First[Position[templist,element]];
postion = {postion[[1]],If[postion[[2]]==1,2,1]}];
While[(Length[flag]>0),

If[FreeQ[flag,postion[[1]]],postion = {First[flag],2} ;result = Append[result,resulttemp];resulttemp = {}];
resulttemp = Append[resulttemp,postion[[1]]];
flag = DeleteCases[flag,postion[[1]]];
pointer = templist[[postion[[1]],postion[[2]]]];
postion = Flatten[DeleteCases[Position[templist,pointer],postion]];

postion = {postion[[1]],If[postion[[2]]==1,2,transQ [[postion[[1]]]]=1;1]};
n++];

result = Append[result,resulttemp];

list = Table[If[transQ[[i]]==1,Trans[list[[i]]],list[[i]]],{i,Length[list]}];

result = Table[list[[result[[i]]]],{i,Length[result]}];
If[Qslash,Times@@Flatten[{Dot@@First[result],Table[Tracktemp[result[[i]]],{i,2,Length[result]}]}],Times@@Table[Tracktemp[result[[i]]],{i,1,Length[result]}]]//unspin
]//Quiet;
traceContract[mycon_]:=Module[{mylist= Transpose[FactorList[mycon]][[1]],fac= Times@@Transpose[FactorList[mycon]][[2]]},
fac = fac*Times@@Select[mylist,spin[#]==0&];
mylist = Select[mylist,spin[#]=!=0&];
fac*join[mylist]
]//Quiet;


(*\:56fe\:5408\:6cd5\:6027\:5224\:65ad*)
reset["b"]:="m";
reset["h"]:="n";
reset["c"]:="o";
reset["j"]:="p";
Protect[reset];
test[myset_]:=Block[{set=progator[#][[4]]&/@myset,sets = myset},If[Count[set,"G"]==1,sets[[FirstPosition[set,"S"][[1]]]]=reset[sets[[FirstPosition[set,"S"][[1]]]]];sets[[FirstPosition[set,"G"][[1]]]]=reset[sets[[FirstPosition[set,"G"][[1]]]]];sets,sets]
]
mark = Alphabet["Latin"][[;;12]];
expoent = {-3,-1,0,2,3,4,-2,\[Epsilon],2+\[Epsilon],1,3,4};
condension = {0,1,2,0,0,0,0,1,0,2,0,0};
mass = {0,0,0,0,0,0,1,1,1,1,1,1};
cons = {"N","G","S","N","N","N","N","G","N","S","N","N"};
progator = Association[Table[mark[[i]]-> {expoent[[i]],condension[[i]],mass[[i]],cons[[i]]},{i,Length[mark]}]];
legalQ[set_,k_]:= (Plus@@(progator[#][[1]]&/@set)===k)\[And](Plus@@(progator[#][[3]]&/@set)<= 1)\[And]((Length[Select[(progator[#][[2]]&/@set),#==1&]]<= 2)\[And]!((FreeQ[(progator[#][[2]]&/@set),2])\[And]Length[Select[(progator[#][[2]]&/@set),#==1&]]==1));

feynCalc[n_,k_]:= Module[{set = Select[Union[Sort/@Tuples[mark,n]],legalQ[#,k]&]},
set = Table[test[set[[i]]],{i,Length[set]}];
Flatten[Table[Permutations[set[[i]]],{i,Length[set]}],1]
 ];
contentQ[myset_,mycase_]:=Module[{set = myset, case = mycase},
And@@(!FreeQ[case,#]&/@set)]


(*\:4f20\:64ad\:5b50*)
colors[a_,b_,key_]:= Block[{ss},
ss= {{"a",delta[a,b]},{"b",1/2 lambda[a,b,\[Eta]]},{"c",delta[a,b]},{"d",delta[a,b]},{"e",delta[a,b]},{"f",delta[a,b]},{"g",delta[a,b]},{"h", lambda[a,b,\[Eta]]},{"i",delta[a,b]},{"j",delta[a,b]},{"k",delta[a,b]},{"l",delta[a,b]},{"m",1/2 lambda[a,b,\[Eta]]},{"n",lambda[a,b,\[Eta]]},{"o",1/2 lambda[a,b,\[Eta]]},{"p",1/2 lambda[a,b,\[Eta]]}};
ss= Association[Table[ss[[i,1]]-> ss[[i,2]],{i,Length[ss]}]];
ss[key]]
factors[q_,x_,key_]:= Block[{ss},
ss= {{"a",I/(2 \[Pi]^2 x^4)},{"b",(I gc Gn)/(32 \[Pi]^2 x^2)},{"c",-(1/12) \[LeftAngleBracket]q bar[q]\[RightAngleBracket]},{"d",1/192 x^2 \[LeftAngleBracket]G gc q \[Sigma] bar[q]\[RightAngleBracket]},{"e",-(I (gc^2 x^2 \[LeftAngleBracket]q bar[q]\[RightAngleBracket]^2)/7776)},{"f",-((x^4 \[LeftAngleBracket]gc^2 G^2\[RightAngleBracket] \[LeftAngleBracket]q bar[q]\[RightAngleBracket])/\!\(TraditionalForm\`27648\))},{"g",-(Subscript[M, q]/(4 \[Pi]^2 x^2))},{"h",(gc Gn log[-x^2] Subscript[M, q])/(32 \[Pi]^2)},{"i",-((x^2 \[LeftAngleBracket]G^2 gc^2\[RightAngleBracket] log[-x^2] Subscript[M, q])/(1536 \[Pi]^2))},{"j",1/48 I \[LeftAngleBracket]q bar[q]\[RightAngleBracket] Subscript[M, q]},{"k",-((I x^2 \[LeftAngleBracket]G gc q \[Sigma] bar[q]\[RightAngleBracket] Subscript[M, q])/1152)},{"l",-((gc^2 x^4 \[LeftAngleBracket]q bar[q]\[RightAngleBracket]^2 Subscript[M, q])/31104)},{"m",(I)/(32 \[Pi]^2 x^2)},{"n",(log[-x^2] Subscript[M, q])/(32 \[Pi]^2)},{"o",-(1/(2^6*3))\[LeftAngleBracket]G gc q \[Sigma] bar[q]\[RightAngleBracket]},{"p",(I Subscript[M, q])/(2^8*3) \[LeftAngleBracket]G gc q \[Sigma] bar[q]\[RightAngleBracket]}};
ss= Association[Table[ss[[i,1]]-> ss[[i,2]],{i,Length[ss]}]];
ss[key]]


(*\:8272\:6307\:6807\:8ba1\:7b97*)

filename = "facs5.csv";
parameter[f_[a___]]:={a}
parameters[f_]:=parameter[f]
parameters[Times[f_,g__]]:=Union[ parameter[f],parameters[Times[g]]];
epslion = Flatten[Table[LeviCivitaTensor[3][[i,j,k]],{i,1,3},{i,1,3},{j,1,3},{k,1,3}],1];
del = {{1,0,0},{0,1,0},{0,0,1}};
\[Lambda]1={{0,1,0},{1,0,0},{0,0,0}} ;
\[Lambda]2={{0,-I,0},{I,0,0},{0,0,0}} ;
\[Lambda]3={{1,0,0},{0,-1,0},{0,0,0}} ;
\[Lambda]4={{0,0,1},{0,0,0},{1,0,0}};
\[Lambda]5={{0,0,-I},{0,0,0},{I,0,0}} ;
\[Lambda]6={{0,0,0},{0,0,1},{0,1,0}};
\[Lambda]7={{0,0,0},{0,0,-I},{0,I,0}} ;
\[Lambda]8=1/Sqrt[3] {{1,0,0},{0,1,0},{0,0,-2}} ;
Gellman = {\[Lambda]1,\[Lambda]2,\[Lambda]3,\[Lambda]4,\[Lambda]5,\[Lambda]6,\[Lambda]7,\[Lambda]8};
\[Epsilon][a_,b_,c_]:=epslion[[a,b,c]];
\[Delta][a_,b_]:=del[[a,b]];
\[Lambda][a_,b_,n_]:=Gellman[[n,a,b]];
caculateColor[hashStr_,exp_]:=Module[{indexOfSum=parameters[exp],expused=exp,log=""},
SetDirectory[ NotebookDirectory[]];
log =File[ \!\(TraditionalForm\`FileNameJoin[{NotebookDirectory[], filename}]\)];
expused=If[FreeQ[indexOfSum,\[Eta]],Einsum[expused/.{eps->\[Epsilon],delta->\[Delta]},{indexOfSum,3}],Sum[Einsum[expused/.{eps->\[Epsilon],delta->\[Delta],lambda->\[Lambda]},{Cases[indexOfSum,Except[\[Eta]]],3}],{\[Eta],8}]];
OpenAppend[log];
If[NumberQ[expused],WriteString[log,hashStr,",",ToString[expused]<>"\n"]];
Close[log];
expused
];//Quiet
NColorFactors[Times[ factor_,factor1__]]:= Block[{fac =SortBy[Select[{factor,factor1},Length[color[#]]>1&],First],fac1 =Times@@Select[{factor,factor1},Length[color[#]]<=1&],facs = ToExpression[Import[NotebookDirectory[]<>filename,"Data"]],str},
facs = Association[Table[facs[[i,1]]->facs[[i,2]],{i,Length[facs]}]];
str = ToString[Join[Select[fac,Head[#]===eps&],Select[fac,Head[#]=!=eps&]]];
If[facs[Hash[str]]===Missing["KeyAbsent",Hash[str]],caculateColor[Hash[str],Times@@fac]*fac1,facs[Hash[str]]*fac1]
];//Quiet
SetAttributes[NColorFactors,Listable]


(*\:4f20\:64ad\:5b50\:66ff\:6362*)
colrelator[]:=1
replace[cont_,dig_]:=Block[{func =  cont,mydig = dig,j=0,ps = 1,fac = 1},
ps = cont/.DE[{q_,q_},{y_,x_}][CI[{ci1_,ci2_}]]:> colrelator[q,x-y,ci1,ci2,j=j+1];
fac = Times@@Select[Transpose[FactorList[ps/.{Track->Times,Dot->Times,Trans[x_]:> x}]][[1]],Head[#]==colrelator&]/.colrelator[q_,x_,ci1_,ci2_,l_]:>factors[q,x,dig[[l]]]*colors[ci1,ci2,dig[[l]]];
fac*(ps/.colrelator[q_,x_,ci1_,ci2_,l_]:> de[x,dig[[l]]])
]


(*\:63d0\:53d6\:7b97\:7b26*)
operator[0]:=0
operator[\[LeftAngleBracket]a_\[RightAngleBracket]]:=\[LeftAngleBracket]a\[RightAngleBracket]
operator[Subscript[M_, q_]]:=Subscript[M, q]
operator[\[LeftAngleBracket]a_\[RightAngleBracket]^p_]:=\[LeftAngleBracket]a\[RightAngleBracket]^p
operator[Times[a_,f__]]:=Module[{xxx= Select[{a,f},Head[#]===AngleBracket\[Or]Head[#]===Subscript\[Or]!FreeQ[Head/@Level[#,1],AngleBracket]&]},Times@@xxx/.{s->q,u->q,d->q,c->Q,b->Q}]
operators[Plus[s1_,s2__]]:=(operator[s1]+operators[Plus[s2]])/2
operators[p_]:=If[Head[p]=!=Plus,operator[p]]
SetAttributes[operators,Listable]


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
(H_)[CI[a__],SI[b__]]/;Head[H]=!=DE :>TForm[H[CI[a],SI[b]]],
(H_)[SI[b__]]/;H=!=DE :>TForm[H[SI[b]]],
(H_)[CI[a__]]/;H=!=DE :>TForm[H[CI[a]]]
};
WriteString["stdout",ToString[part1,TeXForm]]
]


(*\:5085\:91cc\:53f6\:53d8\:6362*)
furier[s_]:=-I 2^(4-2s) \[Pi]^2 (-p^2)^(s-2) Gamma[2-s]/Gamma[s]

furierLog[s_]:=-(1/(p^4 Gamma[s])) 4^(2-s) (-p^2)^s \[Pi]^2 Gamma[2-s] (2 I Log[2]-I Log[-p^2]+I PolyGamma[0,2-s]+I PolyGamma[0,s])
fourierT[0,x_,p_]:=0
fourierT[ss_,x_,p_]:= Module[{\[Nu]=-Exponent[ss,x]/2, ee =0,slashQ =MemberQ[ss,slash[x],Infinity],logQ =MemberQ[ss,log[-x^2],Infinity],temp},
temp =If[logQ,
temp =SeriesCoefficient[Series[furierLog[\[Nu]+s],{s,0,1}],0]//Expand;If[\[Nu]==1,temp,Select[temp,(MemberQ[#,Log[-p^2],Infinity])\[Or](Exponent[#,p]<0)&]],
temp = SeriesCoefficient[Series[furier[\[Nu]+s],{s,0,1}],0]//Expand;If[\[Nu]==1,temp,Select[temp,(MemberQ[#,Log[-p^2],Infinity])\[Or](Exponent[#,p]<0)&]]
];

temp = If[slashQ,-I ss*D[temp,p]*(-x^2)^\[Nu]*(slash[p]/(p slash[x])),ss*temp*(-x^2)^\[Nu]]//Simplify;
If[logQ,temp*Log[-p^2]/log[-x^2],temp]//Simplify
]
SetAttributes[fourierT,Listable]


(*borel\:53d8\:6362*)
ff[s_]:=Q^(s-4) 2^(2-s) Gamma[2-s/2]/Gamma[s/2]
gg[s_]:=M^(s-2) 2^(2-s) 1/Gamma[s/2]
borelT[Log[Q],Q,M]:=Log[Q]:> -M^2/2
borelT[0,Q_,M_]:={q_:> q}
borelT[ss_,Q_,M_]:=Module[
{k= Exponent[ss,Q]+4,n=4,jj},
jj=(Table[{SeriesCoefficient[Series[ff[k+s],{s,0,n}],j],SeriesCoefficient[Series[gg[k+s],{s,0,n}],j]},{j,-1,n}]//Expand);
jj = jj/.{Log[q_]^l_ q_^k_:> m[q,k,l],Log[q_]^l_/q_^k_:> m[q,-k,l],Log[q_]q_^k_:> m[q,k,1],Log[q_]/q_^k_:> m[q,-k,1]};

jj = jj/.{Log[q_]^l_:> m[q,0,l],Log[q_]:> m[q,0,1],1/q_^k_:> m[q,-k,0],q_^k_:> m[q,k,0]};

jj = Solve[Table[jj[[i,1]]==jj[[i,2]],{i,1,Length[jj]}]][[1]];
jj/.m[q_,k_,l_]:> Log[q]^l q^k//FullSimplify
]
SetAttributes[fourierT,Listable]


EndPackage[]
