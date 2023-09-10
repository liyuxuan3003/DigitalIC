#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter03A_01.fig.wls";
nbPDFName="build/"<>StringReplace[nbFileName,"wls"->"pdf"];
nbPDFNameList=(StringReplace[nbPDFName,".fig"->#<>".fig"]&)/@Alphabet[];
Export[nbPDFName,""]


mTable={1/3,1/2}
mStrTable={{"\:7ebf\:6027\:7ed3","1/3"},{"\:7a81\:53d8\:7ed3","1/2"}}
fig=
{
	Plot[(Exp[VD/(0.026)]-1),{VD,-1,1},
		Frame->True,
		PlotRange->{-0.5 10^13,8 10^13},
		GridLines->All,
		ImageSize->Automatic->300,
		PlotStyle->{Black},
		PlotPoints->500,
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{1,1}]],Magnification->0.5]},{x,-1.0,1.0,0.2}],
			Table[{x 10^13,MaTeX[ToString[x]<>"\\times 10^{13}",Magnification->0.5]},{x,0,8,1}]},
		FrameLabel->{
			MaTeX["V_D (\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["I_D/I_S",Magnification->0.8]}
	],
	Plot[Evaluate@Table[1/(1-VD/0.7)^m,{m,mTable}],{VD,-4,1},
		Frame->True,
		PlotRange->{0,2},
		GridLines->All,
		ImageSize->Automatic->300,
		PlotStyle->{Blue,Red},
		PlotPoints->500,
		FrameTicks->{
			Table[{x,MaTeX[ToString[PaddedForm[x,{1,1}]],Magnification->0.5]},{x,-4.0,1.0,0.5}],
			Table[{x,MaTeX[ToString[PaddedForm[x,{2,1}]],Magnification->0.5]},{x,0,2,0.2}]},
		FrameLabel->{
			MaTeX["V_D (\\si{V})",Magnification->0.8,"Preamble"->{"\\usepackage{siunitx}"}],
			MaTeX["C_j/C_{j0}",Magnification->0.8]},
		PlotLegends->Placed[
			LineLegend[
				(MaTeX["m="<>#[[2]]<>"\\quad",Magnification->0.7]Style[#[[1]],FontSize->7]&)/@ mStrTable,
				LegendFunction->"Panel",
				LegendLayout -> {"ReversedColumn",1}
			],{Left,Top}]
	]
}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]]]]&)/@figWithName]


(*-*)
