#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"]


SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter05D_01.fig.wls";
nbPDFName="build/"<>StringReplace[nbFileName,"wls"->"pdf"];
nbPDFNameList=(StringReplace[nbPDFName,".fig"->#<>".fig"]&)/@Alphabet[];
Export[nbPDFName,""]


xLaTeX=(MaTeX[
	#1,Magnification->#2,
	"Preamble"->{"\\usepackage{siunitx}","\\usepackage{physics}","\\usepackage{upgreek}"}])&;


xLabelFont={FontColor->Black,FontFamily->"Latin Modern",FontSize->5}


xFigConfig={
	Frame->True,GridLines->Automatic,ImageSize->Automatic->300,
	FrameStyle->BlackFrame,PlotStyle->ColorData[10],BaseStyle->xLabelFont};
xFigTicks=(FrameTicks->{
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}],
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#4[[1]],#4[[2]]}]],0.5]},{x,#3[[1]],#3[[2]],#3[[3]]}]})&;
xFigLabel=(FrameLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8]})&;


xTicks=(Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog=(Table[{10^x,xLaTeX["10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog1=(Table[{10^x,xLaTeX["1\\times 10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog2=(Table[{2 10^x,xLaTeX["2\\times 10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog5=(Table[{5 10^x,xLaTeX["5\\times 10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;


xGrids=(Table[x,{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xGridsLog=(Flatten[Table[Table[{If[#2[[1]]<=(n 10^x)<=#2[[2]],(n 10^x),{}]},{n,1,9,1}],{x,#1[[1]],#1[[2]],#1[[3]]}]])&;


tpHL=Function[{VDD},Log[2]CL VDD/(kn VDSATn (VDD-VTn-VDSATn))];
valList={kn->115 10^(-6),VDSATn->0.63,VTn->0.43,CL->6 10^(-15)};


fig1=Plot[(tpHL[VDD]/tpHL[2.5])/.valList,{VDD,1.2,2.5},
	PlotStyle->Black,
	PlotRange->Automatic,
	Evaluate@xFigTicks[{1.2,2.5,0.1},{2,1},{1.0,3.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{DD}","t_p/t_p|_{V_{DD}=2.5\\si{V}}"],
	Evaluate@xFigConfig]


xf=Function[{gamma},f/.(First@Solve[f==Exp[1+gamma/f],f])]
N[xf[1],4]
fig2=Plot[f/.{f->xf[gamma]},{gamma,0,3},
	PlotRange->{2.3,5.2},
	PlotStyle->Black,
	Epilog->{
		Gray,Thickness[0.002],Dashed,
		Line[{{0,xf[1]},{1,xf[1]},{1,0}}],
		Red,PointSize[0.015],Point[{1,xf[1]}],
		Text[xLaTeX["f_{opt}(\\gamma=1)\\approx"<>ToString[N[xf[1],4]],0.8],{0.65,3.75}]
	},
	Evaluate@xFigLabel["\\gamma","f_{opt}(\\gamma)"],
	Evaluate@xFigTicks[{0.0,3.0,0.5},{2,1},{2.5,5.0,0.5},{2,1}],
	Evaluate@xFigConfig]


xtp=Function[{Nx,F},Nx(1+F^{1/Nx}/gamma)]
fig3Func=Function[{n,Nx},tp/.{tp->xtp[Nx,F]}/.{F->10^n}/.{f->xf}/.{gamma->1}]
fig3=LogPlot[Evaluate@Table[fig3Func[n,Nx],{n,0,3,1}],
	{Nx,0,8},
	PlotRange->{0.5,200},
	PlotLegends->Placed[LineLegend[
		Table[xLaTeX["F=1\\times 10^{"<>ToString[n]<>"}",0.6],{n,0,3,1}],
		LegendFunction->"Panel",
		LegendLayout -> {"Row",2}],{Right,Top}],
	Epilog->{
		PointSize[0.01],Blend[{Red,White},0.3],
		Point[{{1.80,Log@8.27},{3.60,Log@16.54},{5.40,Log@24.81}}],
		PointSize[0.01],Blend[{Blue,White},0.3],
		Point[{{1,Log@2},{2,Log@8.32},{4,Log@16.65},{5,Log@24.91}}]
	},
	GridLines->{xGrids[{0,8,1}],xGridsLog[{0,3,1},{1,200}]},
	FrameTicks->{xTicks[{0,8,1},{1,1}],Join[xTicksLog1[{0,2,1}],xTicksLog5[{0,1,1}]]},
	Evaluate@xFigLabel["N","t_{p}(N,F)/t_{p0}"],
	Evaluate@xFigConfig]


xNopt=Function[{F},Log[F]/Log[f]]
fig4Func=Function[{F},Nopt/.{Nopt->xNopt[F]}/.{f->xf[1]}]
fig4=LogLinearPlot[fig4Func[F],{F,1,1000},
	PlotRange->{-0.5,6.5},
	PlotStyle->Black,
	GridLines->{xGridsLog[{0,3,1},{1,1000}],xGrids[{0,6,1}]},
	FrameTicks->{Join[xTicksLog1[{0,3,1}],xTicksLog5[{0,2,1}]],xTicks[{0,6,1},{1,1}]},
	Epilog->{
		PointSize[0.01],Blend[{Red,White},0.3],
		Point[Table[{Log@(10^p),fig4Func[10^(p)]},{p,1,3,1}]],
		Table[Text[xLaTeX[ToString[PaddedForm[N[fig4Func[10^(p[[1]])]],{4,2}]]<>"\\to"<>ToString[p[[2]]],0.4],{Log@(10^p[[1]])-0.35,fig4Func[10^(p[[1]])]}],{p,{{1,2},{2,4},{3,5}}}],
		PointSize[0.01],Blend[{Blue,White},0.5],
		Point[{{Log@10,2},{Log@100,4},{Log@1000,5}}]
	},
	Evaluate@xFigLabel["F","N_{opt}"],
	Evaluate@xFigConfig]


fig5Func=Function[{F},tpopt/.{tpopt->First@xtp[Nopt,F]}/.{Nopt->xNopt[F]}/.{f->xf[gamma]}/.{gamma->1}]
fig5=LogLogPlot[fig5Func[F],{F,1,1000},
	PlotRange->{0.5,200},
	PlotStyle->Black,
	FrameTicks->{
		Join[xTicksLog1[{0,3,1}],xTicksLog5[{0,2,1}]],
		Join[xTicksLog1[{0,2,1}],xTicksLog5[{0,1,1}]]
	},
	Epilog->{
		PointSize[0.01],Blend[{Red,White},0.3],
		Point[Table[{Log@(10^p),Log@fig5Func[10^(p)]},{p,1,3,1}]],
		Table[Text[xLaTeX[ToString[PaddedForm[N[fig5Func[10^(p)]],{4,2}]],0.4],{Log@(10^p),0.2+Log@fig5Func[10^(p)]}],{p,1,3,1}]
	},
	GridLines->{xGridsLog[{0,3,1},{1,1000}],xGridsLog[{0,2,1},{0.5,200}]},
	Evaluate@xFigLabel["F","t_{p}(N_{opt},F)/t_{p0}"],
	Evaluate@xFigConfig]


fig6=LogLogPlot[Evaluate@Table[tp/.{tp->xtp[Nx,F]}/.{gamma->1},{Nx,1,5,1}],{F,1,1000},
	PlotRange->{0.5,200},
	FrameTicks->{
		Join[xTicksLog1[{0,3,1}],xTicksLog5[{0,2,1}]],
		Join[xTicksLog1[{0,2,1}],xTicksLog5[{0,1,1}]]
	},
	PlotLegends->Placed[LineLegend[
		Table[xLaTeX["N="<>ToString[Nx],0.55],{Nx,1,5,1}],
		LegendFunction->"Panel",
		LegendLayout -> {"Column",1}],{Right,Bottom}],
	GridLines->{xGridsLog[{0,3,1},{1,1000}],xGridsLog[{0,2,1},{0.5,200}]},
	Evaluate@xFigLabel["F","t_{p}(N,F)/t_{p0}"],
	Evaluate@xFigConfig]


ft=Function[{F,Nx},tp/.{tp->First@xtp[Nx,F]}/.{gamma->1}]
fts=Function[{Nx},If[Nx==0,0,a/.First@Solve[ft[a,Nx]==ft[a,Nx+1]]]]
fts[1]


fig7=LogLogPlot[Evaluate@Table[Piecewise[{{ft[F,Nx],fts[Nx-1]<=F<=fts[Nx]},{Undefined,True}}],{Nx,1,5,1}],{F,1,1000},
	PlotRange->{0.5,200},
	FrameTicks->{
		Join[xTicksLog1[{0,3,1}],xTicksLog5[{0,2,1}]],
		Join[xTicksLog1[{0,2,1}],xTicksLog5[{0,1,1}]]
	},
	Epilog->{
		PointSize[0.015],Black,
		Point[Table[{Log@fts[Nx],Log@ft[fts[Nx],Nx]},{Nx,1,4,1}]],
		PointSize[0.01],Blend[{Blue,White},0.5],
		Point[Table[{Log@(10^p[[1]]),Log@ft[10^(p[[1]]),p[[2]]]},{p,{{0,1},{1,2},{2,4},{3,5}}}]],
		Table[Text[xLaTeX[ToString[PaddedForm[N[ft[10^(p[[1]]),p[[2]]]],{4,2}]],0.4],{Log@(10^p[[1]]),0.2+Log@ft[10^(p[[1]]),p[[2]]]}],{p,{{0,1},{1,2},{2,4},{3,5}}}]
	},
	GridLines->{xGridsLog[{0,3,1},{1,1000}],xGridsLog[{0,2,1},{0.5,200}]},
	Evaluate@xFigLabel["F","t_{p}(N,F)/t_{p0}"],
	Evaluate@xFigConfig]


fig={fig1,fig2,fig3,fig4,fig5,fig6,fig7}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]
