(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 2022.10.1 *)

BeginPackage["tzor`"]
(* Exported symbols added here with SymbolName::usage *) 
CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = TraditionalForm
Print[" Tz\[OAcute]r for ",Style["T",Red],"heori",Style["z",Red],"ed package ",Style["o",Red],"f sum ",Style["r",Red],"ules."]
Print[" Get Lastest Version : ",Hyperlink["Tzor@github", "https://github.com/amapoplar/Tzor"]]
Hyperlink["Tzor@github", "https://github.com/amapoplar/Tzor"]
Print[" Author : poplar, xlchen, zzchen, dklian"]
Print[" Version : alpha "]
Print[" Lastest Veresion : This Version"]


TzorDeclareHeader::usage = 


Begin["`Private`"]
(* Implementation of the package *)

TzorDeclareHeader[file_, type_String:"file"] :=
	Module[ {strm, einput, moreLines = True},
		Switch[
			type,
			"file",
				strm = OpenRead[file],
			"string",
				strm = StringToStream[file],
			_,
			Print["Tzor: TzorDeclareHeader: Unknown input type. Evaluation aborted"];
			Abort[];
		];

		If[ Head[strm] =!= InputStream,
			Return[$Failed]
		];
		While[
			moreLines,
			einput = Read[strm, Hold[Expression]];
			ReleaseHold[einput];
			If[ einput === $Failed || MatchQ[einput, Hold[_End]],
				moreLines = False
			]
		];
		Close[strm]
	];


End[]


$TzorDirectory = DirectoryName[$InputFileName]


$TzorAddOns = {"Physics","Algebra","Compute"}
loadfiles = Flatten[FileNames["*.m",#]&/@(FileNameJoin[{$TzorDirectory,#}]&/@$TzorAddOns)]


TzorDeclareHeader[#]&/@loadfiles



EndPackage[]

