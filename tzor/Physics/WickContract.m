(* ::Package:: *)

(* Wolfram Language package *)
wickContract::usage = "wickContract will give a Wick Contract of some operation"
traceContract::usage = "traceContract will give a dot time of matrices"


TzorDeclareHeader[$TzorDirectory<>"Algebra/Matrix.m"]


Begin["`Private`"]


NM[a___, -c__, b___] := -NM[a, c, b]
NM[a___, -1, b___] := -NM[a, b]
SetAttributes[NM,{OneIdentity,Flat}]



scalarQ[expr_]:=Head[expr]=!=(QField\[Or]QFieldB);
quarkQ[expr_]:= (Head[expr] === QField) || (Head[expr] === QFieldB);
gluonQ[expr_ ]:=Head[Head[expr]]===GField;
qorgQ[expr_]:= quarkQ[expr]||gluonQ[expr];
ProgQ[pro_]:= Head[Head[pro]]===DE;
SetAttributes[ProgQ,Listable]


tempContract[]:= 1
tempT[]:= 1
sign[exp1_ , exp2_]:= Flatten[Position[exp1,#]&/@exp2]; 
wickContract[expr_] := Block[{res}, 
	res = expr/. NonCommutativeMultiply -> NM;
	NM[NM@@(DeleteCases[res, _?qorgQ, \[Infinity]]), 
    tempContract[Cases[res, _?qorgQ, \[Infinity]]]]/.NM->Times
    ];
tempContract[cur_]:= Module[{exp =cur,qcurent= 0,gcurent =0, quarks = Select[cur,Head[#]==QField&],
qbars = Select[cur,Head[#]==QFieldB&],gluons = Select[cur,gluonQ],
erros = 1,erromassages = {},qtemps = 0,gtemps = 0,res = 0},
	erromassages = {"\:8ba1\:7b97\:6210\:529f","\:6b63\:53cd\:5938\:514b\:6570\:76ee\:4e0d\:4e00\:81f4\:ff0c\:8bf7\:68c0\:67e5\:6d41\:5f62\:5f0f","\:80f6\:5b50\:6570\:76ee\:5e94\:4e3a\:5076\:6570","\:5176\:4ed6\:9519\:8bef"};
	If[Length[gluons]=!= 0 ,If[EvenQ[Length[gluons]],gluons = Partition[gluons,Length[gluons]/2],erros = 3]];
	If[Length[quarks]!= Length[qbars],erros =2];
	qcurent = Riffle[quarks,#]&/@Permutations[qbars];
	qtemps =Plus@@((
	(Signature@sign[exp,#])*(tempT@@#)&/@qcurent)
		//.tempT[arg1___,QField[q1_,ci1_,si1_,x_],QFieldB[q2_,ci2_,si2_,y_],arg2___]:> I DE[{q1,q2},{y,x}][ci[{ci1,ci2}],si[{si1,si2}]]*tempT[arg1,arg2])
		/.{DE[{ferm1_, ferm2_}, __][___] /; ferm1 =!= ferm2 :>  0,
	DE[{___}, {x_,y_}][___] /; x ===y  :>  0};

	If[gluons =!= {}, 
		Plus@@(qtemps*tempT@@@(Riffle[First[gluons],#]&/@Permutations[Last[gluons]])
		//.tempT[arg1___,GField[n1_,x_][li[{si1_,si2_}]],GField[n2_,y_][li[{si3_,si4_}]],arg2___]
				:> I GE[GField[n1,x][li[{si1,si2}]], GField[n2,y][li[{si3,si4}]]]*tempT[arg1,arg2])
				/.{GE[GField[_,x_][___],GField[_,y_][___]]/;x===y:>  0},
		qtemps]
];



(*\:6784\:9020\:56fe*)
givePointsByEdge[UndirectedEdge[a_,b_]]:= {a,b}
SetAttributes[givePointsByEdge,Listable]
TransQ[h_]:=Head[h]==Trans
Tracktemp[Dot[conj__]]:=
	If[MatchQ[Table[conj [[i]],{i,Length[conj]}],{__?TransQ}],
		Track[Dot@@Table[Trans[Reverse[conj][[i]]],{i,Length[conj]}]],
		Track[Dot@@RotateLeft[#,Position[ProgQ[#],True][[1,1]]-1]&@Table[conj [[i]],{i,Length[conj]}]]]
findMatrixsWithIndex[list_,index_]:=Union[Select[list,SameQ[spin[#],index]&],Trans/@Select[list,SameQ[spin[#],Reverse[index]]&]]

traceContract[exp1_+exp2_]:=traceContract[exp1]+traceContract[exp2]

traceContract[mycon_]:=Module[{mylist= Transpose[FactorList[mycon]][[1]],fac= Times@@Transpose[FactorList[mycon]][[2]]},

fac = fac*Times@@Select[mylist,spin[#]===Nothing&];

mylist = Select[mylist,spin[#]=!=Nothing&];

fac*join[mylist]
]//Quiet;
setGraphWithIndices[index_?ListQ]:=(#[[1]]\[UndirectedEdge]#[[2]])&/@index


(*\:5224\:65ad\:81ea\:65cb\:6307\:6807*)
spin[DE[{ferm_, ferm_}, {x_ , y_} ] [ci_,si[si_]]]:=si
spin[a_[si[si_]]]:=si
spin[eps[a_,b_,c_]]:=Nothing
spin[Trans[a_]]:=Reverse[spin[a]]
spin[a_]:=Nothing
spin[a_?scalarQ]:=Nothing
spin[GE[__]]:=Nothing
unspin[con_]:=Module[{cons = con},cons /.{a_[si[si_]]:>a,DE[a__][ci[cis_],si[si_]]:>DE[a][ci[cis]]}]
SetAttributes[spin,Listable]


(*\:7565\:53bb\:6d1b\:4f26\:5179\:6307\:6807*)

join[con_]:=Block[{list = con,spins = spin/@con,graph = Null,temps = Null,templist,TrQ=Null,reverse = {},reverseQ = {}},

graph = setGraphWithIndices[spins];
graph = ConnectedGraphComponents[graph];
templist = If[AcyclicGraphQ[#],
				temps = FindHamiltonianPath[#];
				reverse ={First[temps],Last[temps]};
				reverseQ =  Select[Flatten[spins],MemberQ[reverse ,#]&];
				If[EvenQ[First[First[Position[Flatten[spins],First[temps]]]]],
						temps = Reverse[temps]];
				{{Table[{temps[[i-1]],temps[[i]]},{i,2,Length[temps]}]},0},
				{givePointsByEdge[FindPostmanTour[#]],1}]&/@graph;
TrQ = Last[Transpose[templist]];

templist =Table[Dot@@Flatten[findMatrixsWithIndex[list,#]&/@templist[[i,1,1]]],{i,Length[templist]}];

Times@@Table[If[TrQ[[i]]==1,Tracktemp[templist[[i]]],templist[[i]]],{i,Length[TrQ]}]//unspin

]//Quiet;



End[]
