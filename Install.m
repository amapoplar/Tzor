(* ::Package:: *)

InstallTzor[]:=Module[{$downLoadURL = "https://codeload.github.com/amapoplar/Tzor/zip/refs/heads/main",zipFile, unzipFile},

PrintTemporary["Downloading tzor from github to "<>$UserDocumentsDirectory<>"..."];
zipFile =URLDownload[$downLoadURL ,FileNameJoin[{$UserDocumentsDirectory,"tzor.zip"}]];
PrintTemporary["Unzipping tzor.zip..."];
unzipFile=If[Or@@(StringContainsQ[#,"Tzor-main"]&/@FileNames[All,$UserDocumentsDirectory]),
				If[               ChoiceDialog["There has been a FileDirectory named Tzor-main in "<>$UserDocumentsDirectory<>", do you want overwrite it or qiut installation?",{"OverWrite"->True,"Quit"->False}],
						ExtractArchive[zipFile,                        OverwriteTarget -> True],
						DeleteFile[Information[zipFile,"FileName"]]; Return[]]
,
ExtractArchive[zipFile]];
PrintTemporary["Copying tzor from "<>$UserDocumentsDirectory<>" to"<>$UserBaseDirectory<>"..."];
If[Length[FileNames["*",FileNameJoin[{$UserBaseDirectory,"Applications/tzor"}]]]=!=0,
		If[ChoiceDialog["There has been a FileDirectory named Tzor in "<>$UserBaseDirectory<>", do you want overwrite it or qiut installation?",{"OverWrite"->True,"Quit"->False}],
			DeleteDirectory[FileNameJoin[{$UserBaseDirectory,"Applications/tzor"}],DeleteContents->True],
			CopyDirectory[FileNameJoin[{$UserDocumentsDirectory,"Tzor-main/tzor"}], FileNameJoin[{$UserBaseDirectory,"Applications/tzor"}]],
			DeleteDirectory[FileNameJoin[{$UserDocumentsDirectory,"Tzor-main"}],DeleteContents->True];Return[]
		];
CopyDirectory[FileNameJoin[{$UserDocumentsDirectory,"Tzor-main/tzor"}], FileNameJoin[{$UserBaseDirectory,"Applications/tzor"}]]];
PrintTemporary["Deleting files under" <>$UserDocumentsDirectory<>"..."];

DeleteFile[Information[zipFile,"FileName"]];
DeleteDirectory[FileNameJoin[{$UserDocumentsDirectory,"Tzor-main"}],DeleteContents->True];
Get["tzor`"];
]
