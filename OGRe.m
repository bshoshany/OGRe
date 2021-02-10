(* ::Package:: *)


(* OGRe(TM): An (O)bject-oriented (G)eneral (Re)lativity (T)oolkit for (M)athematica *)
(*                By Barak Shoshany (baraksh@gmail.com) (baraksh.com)                *)
(*                         https://github.com/bshoshany/OGRe                         *)


BeginPackage["OGRe`"];


(* Check if the package has already been loaded, in case Get was used instead of Needs. *)
If[
    ValueQ[OGRe`Private`AlreadyLoaded],
(* Then *)
    If[
        !ValueQ[OGRe`Private`DebugMode],
    (* Then *)
        (* Do not allow the package to be loaded more than once in the same session. *)
        Print["Package already loaded. Aborting."];
        Abort[],
    (* Else *)
        (* Debug mode: Unprotect and clear all symbols (except TensorData) so they can be redefined during a debug session. Triggered if the symbol OGRe`Private`DebugMode is defined. *)
        Unprotect["OGRe`*"];
        Unprotect["OGRe`Private`*"];
        OGReTemp`TensorData = OGRe`Private`TensorData;
        ClearAll["OGRe`*"];
        ClearAll["OGRe`Private`*"];
        OGRe`Private`TensorData = OGReTemp`TensorData;
        Remove[OGReTemp`TensorData];
        OGRe`Private`DebugMode = True;
        OGRe`Private`AlreadyLoaded = True;
        Print["Debug mode enabled. Package reloaded."];
    ],
(* Else *)
    OGRe`Private`AlreadyLoaded = True;
    (* Print welcome message on first load. *)
    CellPrint[ExpressionCell[
        Column[{
            Row[{Superscript["OGRe", "TM"], ": An (O)bject-oriented (G)eneral (Re)lativity (T)oolkit for (M)athematica"}],
            Row[{"By Barak Shoshany (", Hyperlink["baraksh@gmail.com", "mailto:baraksh@gmail.com"], ") (", Hyperlink["baraksh.com", "https://baraksh.com/"], ")"}],
            Row[{"v1.0 (February 10, 2021)"}],
            Row[{"To download the latest version, visit ", Hyperlink["https://github.com/bshoshany/OGRe"], "."}],
            Row[{"To list all available modules, type ", Style["?OGRe`*", "Input"], ". "}],
            Row[{"To get help on a particular module, type ", Style["?", "Input"], " followed by the module name."}]
        }],
        "Output"]
    ];
    (* Initialize the symbol TensorData, which is used to store the data for the tensor objects. This is done here since we do not want to delete the defined tensors during a debug session. *)
    OGRe`Private`TensorData = Association[];
];


(* A dirty trick to make the package's public modules globally visible without defining their usage messages in advance. I prefer to define each usage message at the same time as the module itself, so it can also serve as documentation for the code. *)
Null[{
    TAddCoordTransformation,
    TCalc,
    TChangeDefaultCoords,
    TChangeDefaultIndices,
    TChangeID,
    TChangeSymbol,
    TChristoffel,
    TCovariantD,
    TDelete,
    TEinsteinTensor,
    TExport,
    TExportAll,
    TImport,
    TImportAll,
    TIndexLetters,
    TInitializeSymbols,
    TList,
    TNewCoordinates,
    TNewMetric,
    TNewTensor,
    TPartialD,
    TRicciScalar,
    TRicciTensor,
    TRiemannTensor,
    TShow,
    TSimplify,
    TSimplifyAssumptions
}];


Begin["`Private`"];


(* Creates a nicely-formatted usage message. *)
CreateUsageMessage[f_, args_List, msg_String, additional_List : {}] := (
    Evaluate[f::usage] = StringReplace["\!\(\*RowBox[{\"" <> ToString[f] <> "\", \"[\", StyleBox[\"" <> StringRiffle[args, ","] <> "\", \"TI\"], \"]\"}]\) " <> StringReplace[msg, MapIndexed["`" <> ToString[#2[[1]]] <> "`" -> "\!\(\*StyleBox[\"" <> #1 <> "\", \"TI\"]\)" &, ToString /@ Join[args, additional]]], {" " -> "\[NonBreakingSpace]"}];
);
Attributes[UsageMessage] = HoldAll;


(* ===================================================
   Public modules (accessible to the user) start here.
   =================================================== *)

CreateUsageMessage[TAddCoordTransformation, {sourceID, targetID, rules}, "adds a transformation from the coordinate system `1` to the coordinate system `2`.
The argument `3` must be a list of transformation rules. For example, {x \[Rule] r Sin[\[Theta]] Cos[\[Phi]], y \[Rule] r Sin[\[Theta]] Sin[\[Phi]], z \[Rule] r Cos[\[Theta]]} is a transformation from Cartesian to spherical coordinates."];
TAddCoordTransformation::ErrorRulesForm = "The transformation rules must be a list of rules of the form x \[Rule] y.";
TAddCoordTransformation::ErrorDifferentCoords = "The source and target coordinate systems must be different.";
TAddCoordTransformation::ErrorNotSameDim = "The source and target coordinate systems must be of the same dimension.";
TAddCoordTransformation[sourceID_String, targetID_String, rules_List] := (
    (* Check that the tensor object sourceID exists. *)
    CheckIfTensorExists[sourceID];
    (* Check that the rules are of the correct form. *)
    If[
        !AllTrue[rules, MatchQ[#, _->_] &],
    (* Then *)
        Message[TAddCoordTransformation::ErrorRulesForm];
        Abort[];
    ];
    (* Check that both tensor objects represents coordinate systems. *)
    CheckIfCoordinates[sourceID];
    CheckIfCoordinates[targetID];
    (* Check that the source and target coordinate systems are different. *)
    If[
        sourceID === targetID,
    (* Then *)
        Message[TAddCoordTransformation::ErrorDifferentCoords];
        Abort[];
    ];
    (* Check that the source and target coordinate systems are of the same dimension. *)
    If[
        Length[TensorData[sourceID]["Components"][{{1}, sourceID}]] != Length[TensorData[targetID]["Components"][{{1}, targetID}]],
    (* Then *)
        Message[TAddCoordTransformation::ErrorNotSameDim];
        Abort[];
    ];
    (* Add the transformation to the CoordTransformations key of the source object, or create it if it doesn't already exist. *)
    If[
        KeyExistsQ[TensorData[sourceID], "CoordTransformations"] && AssociationQ[TensorData[sourceID]["CoordTransformations"]],
    (* Then *)
        ChangeTensorKey[sourceID, "CoordTransformations", Append[TensorData[sourceID]["CoordTransformations"], targetID -> rules]],
    (* Else *)
        ChangeTensorKey[sourceID, "CoordTransformations", Association[targetID -> rules]];
    ];
    Return[sourceID];
);


DefaultResultID = "Result";
DefaultSymbol = "\[DottedSquare]";
CreateUsageMessage[TCalc, {LHSTensorID[LHSIndices], RHSExpression, symbol}, "calculates a tensor formula.
`2` may include any number of tensors in the format `4`[`5`], where `4` is a tensor object and `5` is a string representing the order of indices, along with any combination of the following operations:
\[Bullet] Addition: For example, \"A\"[\"\[Mu]\[Nu]\"] + \"B\"[\"\[Mu]\[Nu]\"].
\[Bullet] Contraction: For example, \"A\"[\"\[Mu]\[Lambda]\"] . \"B\"[\"\[Lambda]\[Nu]\"].
\[Bullet] Multiplication by scalar: For example, 2 * \"A\"[\"\[Mu]\[Nu]\"].
`6` specifies the ID of the tensor object in which to store the result. If omitted, the ID \"" <> DefaultResultID <> "\" will be used.
`7` specifies the order of indices of the resulting tensor. The indices must be a permutation of the free indices of `2`. If omitted, the indices will be in the same order as they appear in `2`. If `6` is omitted, then `7` must be omitted as well.
`3` specifies the symbol to use for the result. If omitted, the placeholder symbol " <> DefaultSymbol <> " will be used.", {ID, indices, LHSTensorID, LHSIndices}];
TCalc::ErrorIndices = "The LHS index specification \"`1`\" and the RHS index specification \"`2`\" must be the same up to permutation.";
TCalc::ErrorResult = "Invalid tensor expression entered. Tensor expressions may only contain tensor references of the form \"ID\"[\"indices\"] combined using addition, contraction (dot product), or multiplication by scalar."
TCalc[RHSExpression_, symbol_String : DefaultSymbol] := TCalc[DefaultResultID[""], RHSExpression, symbol];
TCalc[LHSTensorID_String, RHSExpression_, symbol_String : DefaultSymbol] := TCalc[LHSTensorID[""], RHSExpression, symbol];
TCalc[LHSTensorID_String[LHSIndices_String], RHSExpression_, symbol_String : DefaultSymbol] := Module[
    {
        LHSVars,
        RHSVars,
        allVars,
        components,
        newComponents,
        newIndices,
        result,
        resultID,
        resultIndices,
        useCoords,
        useIndices
    },
    (* Repeatedly replace tensor operations with their results until they have all been calculated. *)
    result = RHSExpression //. {
        (* Trace *)
        ID_String[indices_String /; !DuplicateFreeQ[Characters[indices]]] :>
            TensorTrace[ID[indices]],
        (* Tensor addition *)
        firstID_String[firstIndices_String] + secondID_String[secondIndices_String] :>
            AddTensors[TensorTrace[firstID[firstIndices]], TensorTrace[secondID[secondIndices]]],
        (* Multiplication of tensor by scalar *)
        scalar_ * ID_String[indices_String] :>
            TensorByScalar[TensorTrace[ID[indices]], scalar],
        (* Contraction of tensors *)
        firstID_String[firstIndices_String] . secondID_String[secondIndices_String] :>
            ContractTensors[TensorTrace[firstID[firstIndices]], TensorTrace[secondID[secondIndices]]],
        (* Partial derivative *)
        TPartialD[derivativeIndex_String] . tensorID_String[tensorIndices_String] :>
            DivOrGrad[derivativeIndex, TensorTrace[tensorID[tensorIndices]]],
        (* Covariant derivative *)
        TCovariantD[derivativeIndex_String] . tensorID_String[tensorIndices_String] :>
            CovariantDivOrGrad[derivativeIndex, TensorTrace[tensorID[tensorIndices]]]
    };
    (* Check that the result is valid, i.e. of the form "tensorID"["indices"]. *)
    If[
        !MatchQ[result, _String[_String]],
    (* Then *)
        Message[TCalc::ErrorResult];
        (* Clear the temporary tensors that were created for the purpose of the calculation. *)
        ClearTemp[];
        Abort[];
    ];
    resultID = result[[0]];
    resultIndices = result[[1]];
    (* Get the indices, coordinates, and components of the result. *)
    useIndices = TensorData[resultID]["DefaultIndices"];
    useCoords = TensorData[resultID]["DefaultCoords"];
    components = TensorData[resultID]["Components"][{useIndices, useCoords}];
    If[
        LHSIndices === "",
    (* Then *)
        (* Either a scalar, or no rearranging of indices is desired. Store the result directly in a new tensor object. *)
        SetTensorID[LHSTensorID, Association[
            "Components" -> Association[{useIndices, useCoords} -> components],
            "DefaultCoords" -> useCoords,
            "DefaultIndices" -> useIndices,
            "Metric" -> TensorData[resultID]["Metric"],
            "Role" -> "General",
            "Symbol" -> symbol
        ]],
    (* Else *)
        (* Check that the LHS and RHS index specifications are the same up to permutation. *)
        If[
            Sort[Characters[LHSIndices]] != Sort[Characters[resultIndices]],
        (* Then *)
            Message[TCalc::ErrorIndices, LHSIndices, resultIndices];
            (* Clear the temporary tensors that were created for the purpose of the calculation. *)
            ClearTemp[];
            Abort[];
        ];
        (* Collect the variables to be used for rearranging the indices. Both LHSVars and RHSVars will be the same set of variables, but potentially in a different order. *)
        allVars = Association[];
        Scan[(allVars[#] = Unique["var"]) &, Characters[LHSIndices]];
        LHSVars = allVars[#]& /@ Characters[LHSIndices];
        RHSVars = allVars[#]& /@ Characters[resultIndices];
        (* Rearrange the components and indices to allow for a LHS with a different index order than the RHS. *)
        newComponents = Table[
            components[[Sequence @@ RHSVars]],
            Evaluate[Sequence @@ ({#, 1, Length[components]} & /@ LHSVars)]
        ];
        newIndices = Table[
            useIndices[[StringPosition[resultIndices, Characters[LHSIndices][[n]]][[1, 1]]]],
            {n, 1, StringLength[LHSIndices]}
        ];
        (* Store the result in a new tensor object. *)
        SetTensorID[LHSTensorID, Association[
            "Components" -> Association[{newIndices, useCoords} -> newComponents],
            "DefaultCoords" -> useCoords,
            "DefaultIndices" -> newIndices,
            "Metric" -> TensorData[resultID]["Metric"],
            "Role" -> "General",
            "Symbol" -> symbol
        ]];
    ];
    (* Clear the temporary tensors that were created for the purpose of the calculation. *)
    ClearTemp[];
    Return[LHSTensorID];
];


CreateUsageMessage[TChangeDefaultCoords, {tensorID,  coordinatesID}, "changes the default coordinate system of the tensor object `1` to `2`."];
TChangeDefaultCoords::ErrorCoordTensor = "Cannot change the default coordinate system for a tensor object representing a coordinate system."
TChangeDefaultCoords[tensorID_String, coordinatesID_String] := (
    (* Check that the tensor objects sourceID and coordinatesID exist. *)
    CheckIfTensorExists[tensorID];
    CheckIfTensorExists[coordinatesID];
    (* Check that the tensor object tensorID does not itself represents a coordinate system. *)
    If[
        TensorData[tensorID]["Role"] === "Coordinates",
    (* Then *)
        Message[TChangeDefaultCoords::ErrorCoordTensor];
        Abort[];
    ];
    (* Check that the tensor object coordinatesID represents a coordinate system. *)
    CheckIfCoordinates[coordinatesID];
    (* Add a representation to the tensor in the new coordinate system with the default indices, if it doesn't already exist. *)
    AddRepresentation[tensorID, TensorData[tensorID]["DefaultIndices"], coordinatesID];
    (* Change the DefaultCoords key. *)
    ChangeTensorKey[tensorID, "DefaultCoords", coordinatesID];
    Return[tensorID];
);


CreateUsageMessage[TChangeDefaultIndices, {ID, indices}, "changes the default index configuration of the tensor object `1` to `2`.
`2` is a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index."];
TChangeDefaultIndices::ErrorCoords = "Cannot change the default index configuration for a tensor object representing a coordinate system."
TChangeDefaultIndices::ErrorMetric = "Cannot change the default index configuration for a tensor object representing a metric."
TChangeDefaultIndices[ID_String, indices_List?VectorQ] := (
    (* Check that the tensor object ID exists. *)
    CheckIfTensorExists[ID];
    (* Check that the tensor object does not represent a coordinate system. *)
    If[
        TensorData[ID]["Role"] === "Coordinates",
    (* Then *)
        Message[TChangeDefaultIndices::ErrorCoords];
        Abort[];
    ];
    (* Check that the tensor object does not represent a metric. *)
    If[
        TensorData[ID]["Role"] === "Metric",
    (* Then *)
        Message[TChangeDefaultIndices::ErrorMetric];
        Abort[];
    ];
    (* Check that the list of indices is of the correct form. *)
    CheckIndicesForm[indices];
    (* Add a representation to the tensor with the new indices in the default coordinate system, if it doesn't already exist. *)
    AddRepresentation[ID, indices, TensorData[ID]["DefaultCoords"]];
    (* Change the DefaultIndices key. *)
    ChangeTensorKey[ID, "DefaultIndices", indices];
    Return[ID];
);


CreateUsageMessage[TChangeID, {oldID, newID}, "changes the ID of the tensor object `1` to `2`.
If the tensor is a metric or a coordinate system, all currently defined tensors will be scanned, and any references to `1` will be replaced with `2`. If a tensor with the ID `2` already exists, it will be overwritten."];
TChangeID[oldID_String, newID_String] := (
    (* Check that the tensor object oldID exists. *)
    CheckIfTensorExists[oldID];
    (* Copy the old tensor data to the new ID and then remove the old ID. *)
    SetTensorID[newID, TensorData[oldID]];
    RemoveTensorID[oldID];
    (* If the tensor is a metric, replace all references to it. *)
    If[
        this["Role"] === "Metric",
    (* Then *)
        Scan[
            If[TensorData[#]["Metric"] === oldID, ChangeTensorKey[#, "Metric", newID]] &,
            Keys[TensorData]
        ];
    ];
    (* If the tensor is a coordinate system, replace all references to it. *)
    If[
        this["Role"] === "Coordinates",
    (* Then *)
        Scan[
            ChangeCoordinateID[#, oldID, newID] &,
            Keys[TensorData]
        ];
    ];
    Return[newID];
);


CreateUsageMessage[TChangeSymbol, {ID, symbol}, "changes the symbol of the tensor object `1` to `2`."];
TChangeSymbol[ID_String, symbol_String] := (
    CheckIfTensorExists[ID];
    ChangeTensorKey[ID, "Symbol", symbol];
    Return[ID];
);


CreateUsageMessage[TChristoffel, {metricID}, "calculates the Christoffel symbols from the metric `1` and stores the result in a new tensor object with ID \"`1`Christoffel\"."];
TChristoffel::ErrorNotMetric = "The Christoffel symbols can only be calculated from a tensor object representing a metric.";
TChristoffel[metricID_String] := Module[
    {
        christoffelID,
        inverseMetricID = NewTempID[]
    },
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TChristoffel::ErrorNotMetric];
        Abort[];
    ];
    (* Create a temporary tensor for the inverse metric, with two upper indices as its default configuration, to force the Christoffel symbols to have the correct index configuration. *)
    SetTensorID[inverseMetricID, Association[
        "Components" -> TensorData[metricID]["Components"],
        "DefaultCoords" -> TensorData[metricID]["DefaultCoords"],
        "DefaultIndices" -> {1, 1},
        "Metric" -> TensorData[metricID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> DefaultSymbol
    ]];
    (* Calculate the Christoffel symbols, and give the tensor object the correct ID and symbol. *)
    christoffelID = TCalc[
        (metricID <> "Christoffel")["\[Lambda]\[Mu]\[Nu]"],
        1/2 inverseMetricID["\[Lambda]\[Sigma]"] . (
            TPartialD["\[Mu]"] . metricID["\[Nu]\[Sigma]"] +
            TPartialD["\[Nu]"] . metricID["\[Sigma]\[Mu]"] -
            TPartialD["\[Sigma]"] . metricID["\[Mu]\[Nu]"]
        ),
        "\[CapitalGamma]"
    ];
    Return[christoffelID];
];


CreateUsageMessage[TDelete, {ID}, "deletes the tensor object `1`.
WARNING: The tensor data will be lost forever and cannot be recovered.
If the tensor is a metric or coordinate system, it cannot be deleted unless all tensors referring to it have been deleted first."];
TDelete::ErrorMetric = "The metric \"`1`\" cannot be deleted, as it has been used to define the tensor \"`2`\". To delete the metric, first delete \"`2`\" and any other tensors defined using this metric.";
TDelete::ErrorCoords = "The coordinate system \"`1`\" cannot be deleted, as it is the default coordinate system of the tensor \"`2`\". To delete the coordinate system, first change the default coordinate system of \"`2`\" and any other relevant tensors.";
TDelete[ID_String] := (
    (* Check that the tensor object ID exists. *)
    CheckIfTensorExists[ID];
    (* If this tensor represents a metric, check that no tensors are defined using this metric. *)
    If[
        TensorData[ID]["Role"] === "Metric",
    (* Then *)
        Scan[
            If[
                TensorData[#]["Metric"] === ID && # =!= ID,
                (* Then *)
                Message[TDelete::ErrorMetric, ID, #];
                Abort[];
            ] &,
            Keys[TensorData]
        ];
    ];
    (* If this tensor represents a coordinate system, check that no tensors are using it as their default coordinate system. *)
    If[
        TensorData[ID]["Role"] === "Coordinates",
    (* Then *)
        Scan[
            If[
                TensorData[#]["DefaultCoords"] === ID && # =!= ID,
                (* Then *)
                Message[TDelete::ErrorCoords, ID, #];
                Abort[];
            ] &,
            Keys[TensorData]
        ];
    ];
    RemoveTensorID[ID];
);


CreateUsageMessage[TEinsteinTensor, {metricID}, "calculates the Einstein tensor from the metric `1` and stores the result in a new tensor object with ID \"`1`Einstein\".
If a tensor with ID \"`1`RicciTensor\" exists, it will be assumed to be the Ricci tensor of the metric, and will be used in the calculation. Otherwise, \"`1`RicciTensor\" will be created using TRicciTensor[ ]."];
TEinsteinTensor::ErrorNotMetric = "The Einstein tensor can only be calculated from a tensor object representing a metric.";
TEinsteinTensor[metricID_String] := (
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TEinsteinTensor::ErrorNotMetric];
        Abort[];
    ];
    (* If the Ricci tensor was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "RicciTensor"],
    (* Then *)
        TRicciTensor[metricID];
    ];
    (* Calculate the Einstein tensor, and give the tensor object the correct ID and symbol. *)
    Return[TCalc[
        (metricID <> "Einstein")["\[Mu]\[Nu]"],
        (metricID <> "RicciTensor")["\[Mu]\[Nu]"] - 1/2 metricID["\[Mu]\[Nu]"] . (metricID <> "RicciTensor")["\[Rho]\[Rho]"],
        "G"
    ]];
);


CreateUsageMessage[TExport, {ID}, "exports the raw tensor data for the tensor object `1` as an Association."];
TExport[ID_String] := (
    CheckIfTensorExists[ID];
    Return[Association[
        ID -> TensorData[ID]
    ]];
);


CreateUsageMessage[TExportAll, {filename}, "exports the raw tensor data for all tensors defined in the current session as an Association.
`1` is optional. If specified, the data is exported to a file with this name. If a full path is not given, the file will be created in the current working directory, as given by Directory[ ]. Note that the file will be overwritten if it already exists."];
TExportAll[] := TensorData;
TExportAll[filename_String] := Module[
    {
        stream
    },
    stream = OpenWrite[filename];
    Write[stream, TensorData];
    Close[stream];
    Print["Exported all tensor data to ", filename, "."];
];


CreateUsageMessage[TImport, {data}, "imports a tensor that has been exported using TExport[ ].
WARNING:
1. The data is assumed to not have been manually modified by the user, so it is NOT checked for errors or inconsistencies. Importing tensor data that has been manually modified may cause errors or unexpected results, and should be avoided.
2. The ID of the tensor will be taken from the name of the (single) key of the Association being imported. If a tensor with the same ID already exists, it will be overwritten."];
TImport[data_Association] := (
    SetTensorID[Keys[data][[1]], Values[data][[1]]];
    Return[Keys[data][[1]]];
);


CreateUsageMessage[TImportAll, {source}, "imports tensor data that has been exported using TExportAll[ ].
If `1` is an Association, imports the data directly.
If `1` is a file name, imports the data from that file. If a full path is not given, the file should be located in the current working directory, as given by Directory[ ].
WARNING:
1. The data is assumed to not have been manually modified by the user, so it is NOT checked for errors or inconsistencies. Importing tensor data that has been manually modified may cause errors or unexpected results, and should be avoided.
2. This will irreversibly delete ALL of the tensors already defined in the current session."];
TImportAll::ErrorFile = "The file `1` does not exist.";
TImportAll[data_Association] := ImportTensorData[data];
TImportAll[filename_String] := Module[
    {
        data,
        stream
    },
    If[
        !FileExistsQ[filename],
    (* Then *)
        Message[TImportAll::ErrorFile, filename];
        Abort[];
    ];
    stream = OpenRead[filename];
    data = Read[stream];
    ImportTensorData[data];
    Close[stream];
    Print["Imported all tensor data from ", filename, "."];
];


DefaultLettersInit = "\[Mu]\[Nu]\[Rho]\[Sigma]\[Kappa]\[Lambda]\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Epsilon]\[Theta]\[Iota]\[Xi]\[Pi]\[Tau]\[Phi]\[Chi]\[Psi]\[Omega]";
DefaultLetters = DefaultLettersInit;
CreateUsageMessage[TIndexLetters, {letters}, "changes the letters to be used when displaying indices.
If `1` is omitted, shows the index letters currently in use.
If `1` is set to Automatic, resets the index letters to the default."];
TIndexLetters[] := DefaultLetters;
TIndexLetters[letters_String] := (
    Unprotect[DefaultLetters];
    DefaultLetters = letters;
    Protect[DefaultLetters];
);
TIndexLetters[Automatic] := TIndexLetters[DefaultLettersInit];


CreateUsageMessage[TInitializeSymbols, {symbol1, symbol2, "..."}, "clears any definitions previously used for the given symbols and protects them against future changes.
Useful for making sure coordinate variables, constants, and abstract functions used in tensors do not accidentally change their definitions and break the code."];
TInitializeSymbols[symbols__] := (
    Unprotect[symbols];
    ClearAll[symbols];
    Protect[symbols];
);
Attributes[TInitializeSymbols] = HoldAll;


CreateUsageMessage[TList, {ID, indices, coordinatesID}, "lists the unique, non-zero components of the tensor object `1` with the index configuration `2` and in the coordinate system `3`.
`2` should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
If the index configuration and/or coordinate system are omitted, the default ones will be used."];
TList[ID_String, indices_List : {"UseDefault"}, coordinatesID_String : "UseDefault"] := ShowList[ID, indices, coordinatesID, "List"];


CreateUsageMessage[TNewCoordinates, {coordinatesID, symbols}, "creates a new tensor object representing a coordinate system.
`1` is a string that will be used to identify the new object, and must be unique. If a tensor object with the same ID already exists, it will be overwritten.
`2` are the coordinate symbols, e.g. {t, x, y, z}. They will automatically be cleared and protected against future changes using TInitializeSymbols[ ]."];
TNewCoordinates::ErrorEmptyList = "The coordinate symbols cannot be an empty list. At least one coordinate symbol must be specified.";
TNewCoordinates[coordinatesID_String, coordinates_List?VectorQ] := (
    (* Check that the coordinates are not an empty list. *)
    If[coordinates == {}, Message[TNewCoordinates::ErrorEmptyList]; Abort[]];
    (* Clear any definitions previously used for the coordinate symbols and protect them against future changes. *)
    TInitializeSymbols @@ Unevaluated[coordinates];
    (* Create a new tensor object for the coordinates with the desired ID. *)
    SetTensorID[coordinatesID, Association[
        "Components" -> Association[{{1}, coordinatesID} -> coordinates],
        "DefaultCoords" -> coordinatesID,
        "DefaultIndices" -> {1},
        "Role" -> "Coordinates",
        "Symbol" -> "x"
    ]];
    Return[coordinatesID];
);
Attributes[TNewCoordinates] = HoldRest;


CreateUsageMessage[TNewMetric, {metricID, coordinatesID, components, symbol}, "creates a new tensor object representing a metric.
`1` is a string that will be used to identify the new object, and must be unique. If a tensor object with the same ID already exists, it will be overwritten.
`2` is the unique ID of a tensor object representing a coordinate system, created using TNewCoordinates[ ].
`3` is a square matrix representing the metric with two lower indices in that coordinate system.
`4` will be used to represent the metric in formulas. If not given, \"g\" will be used."];
TNewMetric::ErrorIncorrectDim = "The metric components must be a square matrix with the same dimension as the coordinates.";
TNewMetric[metricID_String, coordinatesID_String, components_List?SquareMatrixQ, symbol_String : "g"] := Module[
    {
        dim = Length[components],
        simplified
    },
    (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
    CheckIfTensorExists[coordinatesID];
    CheckIfCoordinates[coordinatesID];
    (* Check that the metric components have the same dimension as the coordinates. *)
    If[
        dim != Length[TensorData[coordinatesID]["Components"][{{1}, coordinatesID}]],
    (* Then *)
        Message[TNewMetric::ErrorIncorrectDim];
        Abort[];
    ];
    (* Simplify the components. *)
    simplified = TensorSimplify[components];
    (* Create a new tensor object for the metric with the desired ID. The components of the matrix in every possible index configuration will be calculated in advance in the default coordinate system, to improve performance. *)
    SetTensorID[metricID, Association[
        "Components" -> Association[
            {{-1, -1}, coordinatesID} -> simplified,
            {{+1, +1}, coordinatesID} -> TensorSimplify[Inverse[simplified]],
            {{+1, -1}, coordinatesID} -> IdentityMatrix[dim],
            {{-1, +1}, coordinatesID} -> IdentityMatrix[dim]
        ],
        "DefaultCoords" -> coordinatesID,
        "DefaultIndices" -> {-1, -1},
        "Metric" -> metricID,
        "Role" -> "Metric",
        "Symbol" -> symbol
    ]];
    Return[metricID];
];


CreateUsageMessage[TNewTensor, {tensorID, metricID, indices, components, symbol}, "creates a new tensor object.
`1` is a string that will be used to identify the new object, and must be unique. If a tensor object with the same ID already exists, it will be overwritten.
`2` is the unique ID of a tensor object representing a metric, created using TNewMetric[ ]. The metric will be used to raise and lower indices for the new tensor.
`3` is a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
`4` is a list specifying the representation of the tensor, with the specified index configuration, in the default coordinate system of `2`.
`5` will be used to represent the tensor in formulas. If not given, the placeholder " <> DefaultSymbol <> " will be used."];
TNewTensor::ErrorDimension = "The components must have the same dimension as the coordinate system.";
TNewTensor::ErrorRank = "The number of indices must match the rank of the components.";
TNewTensor[tensorID_String, metricID_String, indices_List?VectorQ, components_List, symbol_String : DefaultSymbol] := Module[
    {
        useCoords
    },
    (* Check that the tensor object metricID exists and represents a metric. *)
    CheckIfTensorExists[metricID];
    CheckIfMetric[metricID];
    (* Check that the list of indices is of the correct form. *)
    CheckIndicesForm[indices];
    (* The components for the new tensor should be given in the default coordinate system of its metric. *)
    useCoords = TensorData[metricID]["DefaultCoords"];
    (* Validate the input. This is done differently depending on whether the tensor is a scalar or not. *)
    If[
        Length[components] * ArrayDepth[components] != 1,
    (* Then *)
        (* Not a scalar. Check that the dimension of the given components matches the dimension of the coordinates being used. *)
        If[
            Length[components] != Length[TensorData[useCoords]["Components"][{{1}, useCoords}]],
        (* Then *)
            Message[TNewTensor::ErrorDimension];
            Abort[];
        ];
        (* Check that the rank of the given components matches the number of given indices. *)
        If[
            Length[indices] != ArrayDepth[components],
        (* Then *)
            Message[TNewTensor::ErrorRank];
            Abort[];
        ],
    (* Else *)
        (* Is a scalar. Check that the number of indices for the scalar is zero. *)
        If[
            Length[indices] != 0,
        (* Then *)
            Message[TNewTensor::ErrorRank];
            Abort[];
        ];
    ];
    (* Create a new tensor object for the tensor with the desired ID. *)
    SetTensorID[tensorID, Association[
        "Components" -> Association[{indices, useCoords} -> TensorSimplify[components]],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> indices,
        "Metric" -> metricID,
        "Role" -> "General",
        "Symbol" -> symbol
    ]];
    Return[tensorID];
];


CreateUsageMessage[TRicciScalar, {metricID}, "calculates the Ricci scalar from the metric `1` and stores the result in a new tensor object with ID \"`1`RicciScalar\".
If a tensor with ID \"`1`RicciTensor\" exists, it will be assumed to be the Ricci tensor of the metric, and will be used in the calculation. Otherwise, \"`1`RicciTensor\" will be created using TRicciTensor[ ]."];
TRicciScalar::ErrorNotMetric = "The Ricci scalar can only be calculated from a tensor object representing a metric.";
TRicciScalar[metricID_String] := (
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TRicciScalar::ErrorNotMetric];
        Abort[];
    ];
    (* If the Ricci tensor was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "RicciTensor"],
    (* Then *)
        TRicciTensor[metricID];
    ];
    (* Calculate the Ricci scalar, and give the tensor object the correct ID and symbol. *)
    Return[TCalc[
        (metricID <> "RicciScalar")[""],
        (metricID <> "RicciTensor")["\[Mu]\[Mu]"],
        "R"
    ]];
);


CreateUsageMessage[TRicciTensor, {metricID}, "calculates the Ricci tensor from the metric `1` and stores the result in a new tensor object with ID \"`1`RicciTensor\".
If a tensor with ID \"`1`Riemann\" exists, it will be assumed to be the Riemann tensor of the metric, and will be used in the calculation. Otherwise, \"`1`Riemann\" will be created using TRiemannTensor[ ]."];
TRicciTensor::ErrorNotMetric = "The Ricci tensor can only be calculated from a tensor object representing a metric.";
TRicciTensor[metricID_String] := (
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TRicciTensor::ErrorNotMetric];
        Abort[];
    ];
    (* If the Riemann tensor was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Riemann"],
    (* Then *)
        TRiemannTensor[metricID];
    ];
    (* Calculate the Ricci tensor, and give the tensor object the correct ID and symbol. *)
    Return[TCalc[
        (metricID <> "RicciTensor")["\[Mu]\[Nu]"],
        (metricID <> "Riemann")["\[Lambda]\[Mu]\[Lambda]\[Nu]"],
        "R"
    ]];
);


CreateUsageMessage[TRiemannTensor, {metricID}, "calculates the Riemann tensor from the metric `1` and stores the result in a new tensor object with ID \"`1`Riemann\".
If a tensor with ID \"`1`Christoffel\" exists, it will be assumed to be the Christoffel symbols of the metric, and will be used in the calculation. Otherwise, \"`1`Christoffel\" will be created using TChristoffel[ ]."];
TRiemannTensor::ErrorNotMetric = "The Riemann tensor can only be calculated from a tensor object representing a metric.";
TRiemannTensor[metricID_String] := (
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TRiemannTensor::ErrorNotMetric];
        Abort[];
    ];
    (* If the Christoffel symbols were not already calculated, calculate them now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Christoffel"],
    (* Then *)
        TChristoffel[metricID];
    ];
    (* Calculate the Riemann tensor, and give the tensor object the correct ID, symbol, and default index configuration. *)
    Return[TChangeDefaultIndices[
        TCalc[
            (metricID <> "Riemann")["\[Rho]\[Sigma]\[Mu]\[Nu]"],
            TPartialD["\[Mu]"] . (metricID <> "Christoffel")["\[Rho]\[Nu]\[Sigma]"] -
            TPartialD["\[Nu]"] . (metricID <> "Christoffel")["\[Rho]\[Mu]\[Sigma]"] +
            (metricID <> "Christoffel")["\[Rho]\[Mu]\[Lambda]"] . (metricID <> "Christoffel")["\[Lambda]\[Nu]\[Sigma]"] -
            (metricID <> "Christoffel")["\[Rho]\[Nu]\[Lambda]"] . (metricID <> "Christoffel")["\[Lambda]\[Mu]\[Sigma]"],
            "R"
        ],
        {1,-1,-1,-1}
    ]];
);


CreateUsageMessage[TShow, {ID, indices, coordinatesID}, "shows the components of the tensor object `1` with the index configuration `2` and in the coordinate system `3`, in vector or matrix form when applicable.
`2` should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
If the index configuration and/or coordinate system are omitted, the default ones will be used."];
TShow[ID_String, indices_List : {"UseDefault"}, coordinatesID_String : "UseDefault"] := ShowList[ID, indices, coordinatesID, "Show"];


CreateUsageMessage[TSimplify, {ID}, "simplifies all previously-calculated representations of the tensor object `1` based on the user-defined simplification assumptions set using TSimplifyAssumptions[ ]. To be used if the assumptions have changed after the components have already been calculated."];
TSimplify[ID_String] := (
    CheckIfTensorExists[ID];
    ChangeTensorKey[ID, "Components", TensorSimplify /@ TensorData[ID]["Components"]];
    Return[ID];
);


CreateUsageMessage[TSimplifyAssumptions, {assumptions}, "sets the assumptions to be used when simplifying expressions.
If `1` is omitted, displays the currently used assumptions instead.
Use TSimplifyAssumptions[None] to clear previously set assumptions."];
TSimplifyAssumptions[] := UserAssumptions;
TSimplifyAssumptions[assumptions_] := (
    Unprotect[UserAssumptions];
    UserAssumptions = assumptions;
    Protect[UserAssumptions];
);
Attributes[TSimplifyAssumptions] = HoldAll;


(* =================================================== *)
(* Private modules (for internal use only) start here. *)
(* =================================================== *)


(* Add a new representation with a specific index configuration and coordinate system to a tensor object, if it does not already exist. Returns the components of the representation. *)
AddRepresentation::ErrorCoordinatesCoord = "Cannot transform coordinates for a tensor object representing a coordinate system.";
AddRepresentation::ErrorCoordinatesIndex = "Cannot lower index for a tensor object representing a coordinate system.";
AddRepresentation[ID_String, indices_List?VectorQ, coordinatesID_String] := Module[
    {
        defCoords = TensorData[ID]["DefaultCoords"],
        defIndices = TensorData[ID]["DefaultIndices"],
        i,
        oldIndices
    },
    (* If the representation has already been calculated, simply return the components. *)
    If[
        KeyExistsQ[TensorData[ID]["Components"], {indices, coordinatesID}],
    (* Then *)
        Return[TensorData[ID]["Components"][{indices, coordinatesID}]]
    ];
    (* Check that we are not trying to represent a coordinate tensor in a different coordinate system. *)
    If[
        TensorData[ID]["Role"] === "Coordinates" && coordinatesID =!= ID,
    (* Then *)
        Message[AddRepresentation::ErrorCoordinatesCoord];
        Abort[];
    ];
    (* Check that we are not trying to represent a coordinate tensor with a lower index. *)
    If[
        TensorData[ID]["Role"] === "Coordinates" && indices != {1},
    (* Then *)
        Message[AddRepresentation::ErrorCoordinatesIndex];
        Abort[];
    ];
    (* Raise or lower indices if required, one by one. We only need to do this if the tensor is not a metric, since metric tensors already have their components pre-calculated with all possible index configurations. *)
    If[
        TensorData[ID]["Role"] =!= "Metric",
    (* Then *)
        oldIndices = defIndices;
        Do[
            oldIndices = RaiseLower[ID, defCoords, oldIndices, i, indices[[i]]],
            {i, 1, Length[defIndices]}
        ];
    ];
    (* Transform the tensor to different coordinates if required. *)
    TransformCoordinates[ID, indices, defCoords, coordinatesID];
    (* Return the new components. *)
    Return[TensorData[ID]["Components"][{indices, coordinatesID}]];
];


(* Add two tensor objects. *)
AddTensors::ErrorCoords = "The tensor \"`1`\" cannot be added to another tensor, as it represents a coordinate system.";
AddTensors::ErrorIndicesSame = "The tensors \"`1`\"[\"`2`\"] and \"`3`\"[\"`4`\"] cannot be added, as their index specifications must be the same up to permutation."
AddTensors::ErrorMetricsMatch = "The tensors \"`1`\" and \"`2`\" cannot be added, as they are associated with different metrics, \"`3`\" and \"`4`\" respectively.";
AddTensors::ErrorRanksMatch = "The tensors \"`1`\" and \"`2`\" cannot be added, as they have different ranks.";
AddTensors[firstID_String[firstIndices_String], secondID_String[secondIndices_String]] := Module[
    {
        allVars,
        firstComponents,
        firstVars,
        i,
        newID,
        secondComponents,
        secondVars,
        sumComponents,
        useCoords,
        useIndices,
        useSecondIndices
    },
    (* Check that both tensors exist. *)
    CheckIfTensorExists[firstID];
    CheckIfTensorExists[secondID];
    (* Check that neither of the tensors is a coordinate system. *)
    Scan[
        If[
            TensorData[#]["Role"] === "Coordinates",
        (* Then *)
            Message[AddTensors::ErrorCoords, #];
            Abort[];
        ] &,
        {firstID, secondID}
    ];
    (* Check that both tensors are associated with the same metric. *)
    If[
        TensorData[firstID]["Metric"] =!= TensorData[secondID]["Metric"],
    (* Then *)
        Message[AddTensors::ErrorMetricsMatch, firstID, secondID, TensorData[firstID]["Metric"], TensorData[secondID]["Metric"]];
        Abort[];
    ];
    (* Check that both tensors have the same rank. *)
    If[
        Length[TensorData[firstID]["DefaultIndices"]] =!= Length[TensorData[secondID]["DefaultIndices"]],
    (* Then *)
        Message[AddTensors::ErrorRanksMatch, firstID, secondID];
        Abort[];
    ];
    (* Check that the index strings match the ranks of the tensors. *)
    CheckIndicesRank[firstIndices, firstID];
    CheckIndicesRank[secondIndices, secondID];
    (* Check that the index strings of both tensors are the same up to permutation. *)
    If[
        Sort[Characters[firstIndices]] != Sort[Characters[secondIndices]],
    (* Then *)
        Message[AddTensors::ErrorIndicesSame, firstID, firstIndices, secondID, secondIndices];
        Abort[];
    ];
    (* The components that will be added are the ones corresponding to the default representation of the first tensor. *)
    useIndices = TensorData[firstID]["DefaultIndices"];
    useCoords = TensorData[firstID]["DefaultCoords"];
    firstComponents = TensorData[firstID]["Components"][{useIndices, useCoords}];
    (* The index configuration for the second tensor will be rearranged to correctly calculate expressions like T^a_b + T_b^a.  *)
    useSecondIndices = Table[
        useIndices[[StringPosition[firstIndices, Characters[secondIndices][[i]]][[1, 1]]]],
        {i, 1, StringLength[firstIndices]}
    ];
    (* Add the appropriate index configuration to the second tensor if it does not already exist. *)
    secondComponents = AddRepresentation[secondID, useSecondIndices, useCoords];
    (* Collect the variables to be used in the addition. Both firstVars and secondVars will be the same set of variables, but potentially in a different order. *)
    allVars = Association[];
    Scan[(allVars[#] = Unique["var"]) &, Characters[firstIndices]];
    firstVars = allVars[#]& /@ Characters[firstIndices];
    secondVars = allVars[#]& /@ Characters[secondIndices];
    (* Add the two tensors by summing over their components using the appropriate variables. *)
    sumComponents = Table[
        firstComponents[[Sequence @@ firstVars]] + secondComponents[[Sequence @@ secondVars]],
        Evaluate[Sequence @@ ({#, 1, Length[firstComponents]} & /@ firstVars)]
    ];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{useIndices, useCoords} -> TensorSimplify[sumComponents]],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> useIndices,
        "Metric" -> TensorData[firstID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> DefaultSymbol
    ]];
    Return[newID[firstIndices]];
];


(* Go over the component definitions of a given tensor and change every reference to a coordinate system to a different name. Note that this also means changing the reference in the default coordinate system of a tensor and in any coordinate transformation rules. Used by TChangeID. *)
ChangeCoordinateID[tensorID_String, oldCoordsID_String, newCoordsID_String] := Module[
    {
        newComponents = Association[],
        oldComponents = TensorData[tensorID]["Components"],
        transf
    },
    (* For each key of the old components, if oldCoordsID is the coordinate system, convert it to newCoordsID. *)
    Scan[
        If[
            #[[2]] === oldCoordsID,
        (* Then *)
            newComponents[{#[[1]], newCoordsID}] = oldComponents[#],
        (* Else *)
            newComponents[#] = oldComponents[#];
        ] &,
        Keys[oldComponents]
    ];
    (* Store the new components. *)
    ChangeTensorKey[tensorID, "Components", newComponents];
    (* If the default coordinate system is oldCoordsID, change it to newCoordsID. *)
    If[
        TensorData[tensorID]["DefaultCoords"] === oldCoordsID,
    (* Then *)
        ChangeTensorKey[tensorID, "DefaultCoords", newCoordsID];
    ];
    (* If the tensor object is itself a coordinate system, change oldCoordsID to newCoordsID in any coordinate transformations stored inside it. *)
    If[
        TensorData[tensorID]["Role"] === "Coordinates" && KeyExistsQ[TensorData[tensorID], "CoordTransformations"],
    (* Then *)
        transf = TensorData[tensorID]["CoordTransformations"];
        If[
            KeyExistsQ[transf, oldCoordsID],
        (* Then *)
            transf[newCoordsID] = transf[oldCoordsID];
            KeyDropFrom[transf, oldCoordsID];
            ChangeTensorKey[tensorID, "CoordTransformations", transf];
        ]
    ];
];


(* Change a particular key for the tensor object with the given ID. *)
ChangeTensorKey[ID_String, key_String, value_] := (
    Unprotect[TensorData];
    TensorData[ID][key] = value;
    Protect[TensorData];
);


(* Check if the tensor with the given ID represents a coordinate system. *)
CheckIfCoordinates::ErrorNotCoordinates = "The tensor \"`1`\" does not represent a coordinate system.";
CheckIfCoordinates[ID_String] := If[
    TensorData[ID]["Role"] =!= "Coordinates",
(* Then *)
    Message[CheckIfCoordinates::ErrorNotCoordinates, ID];
    Abort[];
];


(* Check if the tensor with the given ID represents a metric. *)
CheckIfMetric::ErrorNotMetric = "The tensor \"`1`\" does not represent a metric.";
CheckIfMetric[ID_String] := If[
    TensorData[ID]["Role"] =!= "Metric",
(* Then *)
    Message[CheckIfMetric::ErrorNotMetric, ID];
    Abort[];
];


(* Check if a tensor with the given ID exists. *)
CheckIfTensorExists::ErrorDoesNotExist = "The tensor \"`1`\" does not exist.";
CheckIfTensorExists[ID_String] := If[
    !KeyExistsQ[TensorData, ID],
(* Then *)
    Message[CheckIfTensorExists::ErrorDoesNotExist, ID];
    Abort[];
];


(* Check that an index list has been entered correctly: a one-dimensional list with all its components either plus or minus 1. *)
CheckIndicesForm::ErrorIncorrectForm = "The indices must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.";
CheckIndicesForm[indices_List] := If[
    !VectorQ[indices] || !AllTrue[indices, (#^2 == 1) &],
(* Then *)
    Message[CheckIndicesForm::ErrorIncorrectForm];
    Abort[];
];


(* Check that an index list matches the tensor's rank. *)
CheckIndicesRank::ErrorIncorrectRank = "The index configuration `1` does not match the rank of the tensor \"`2`\". The number of indices should be `3`.";
CheckIndicesRank[indices_List, ID_String] := If[
    Length[indices] != Length[TensorData[ID]["DefaultIndices"]],
(* Then *)
    Message[CheckIndicesRank::ErrorIncorrectRank, indices, ID, Length[TensorData[ID]["DefaultIndices"]]];
    Abort[];
];
CheckIndicesRank[indices_String, ID_String] := If[
    StringLength[indices] != Length[TensorData[ID]["DefaultIndices"]],
(* Then *)
    Message[CheckIndicesRank::ErrorIncorrectRank, "\"" <> indices <> "\"", ID, Length[TensorData[ID]["DefaultIndices"]]];
    Abort[];
];


(* Clear all temporary tensors created by TCalc, and reset the counter. *)
ClearTemp[] := (
    ImportTensorData[Select[TensorData, #["Role"] =!= "Temporary" &]];
    Unprotect[TempID];
    TempID = 0;
    Protect[TempID];
);


(* Clear all previously defined tensor objects. *)
ClearTensorData[] := (
    Unprotect[TensorData];
    Clear[TensorData];
    TensorData = Association[];
    Protect[TensorData];
);


(* Contract two tensors. *)
ContractTensors::ErrorMetricsMatch = "The tensors \"`1`\" and \"`2`\" cannot be contracted, as they are associated with different metrics, \"`3`\" and \"`4`\" respectively.";
ContractTensors[firstID_String[firstIndices_String], secondID_String[secondIndices_String]] := Module[
    {
        allVars,
        firstComponents,
        firstVars,
        i,
        newComponents,
        newID,
        newIndices,
        outIndices,
        outVars,
        removeIndices,
        secondComponents,
        secondVars,
        sumVars,
        useCoords,
        useFirstIndices,
        useSecondIndices
    },
    (* Check that both tensors exist. *)
    CheckIfTensorExists[firstID];
    CheckIfTensorExists[secondID];
    (* Check that both tensors are associated with the same metric. *)
    If[
        TensorData[firstID]["Metric"] =!= TensorData[secondID]["Metric"],
    (* Then *)
        Message[ContractTensors::ErrorMetricsMatch, firstID, secondID, TensorData[firstID]["Metric"], TensorData[secondID]["Metric"]];
        Abort[];
    ];
    (* Check that the index strings match the ranks of the tensors. *)
    CheckIndicesRank[firstIndices, firstID];
    CheckIndicesRank[secondIndices, secondID];
    (* We perform the calculation in the default coordinate system of the first tensor. *)
    useCoords = TensorData[firstID]["DefaultCoords"];
    (* We start with the default index configuration of both tensors, but then rearrange the indices on the second tensor so that any contracted indices will be one upper, one lower. *)
    useFirstIndices = TensorData[firstID]["DefaultIndices"];
    useSecondIndices = TensorData[secondID]["DefaultIndices"];
    (* If either of the tensors is a scalar, simply do multiplication of tensor by scalar. *)
    If[
        Length[useFirstIndices] == 0,
        (* Then *)
        Return[TensorByScalar[secondID[secondIndices], TensorData[firstID]["Components"][{useFirstIndices, useCoords}][[1]]]]
    ];
    If[
        Length[useSecondIndices] == 0,
        (* Then *)
        Return[TensorByScalar[firstID[firstIndices], TensorData[secondID]["Components"][{useSecondIndices, useCoords}][[1]]]]
    ];
    (* newIndices will be the index configuration of the newly created tensor. We start from the default indices of the first tensor, remove any indices that were contracted, and add the remaining free indices of the second tensor. *)
    newIndices = useFirstIndices;
    removeIndices = {};
    Do[
        If[
            # =!= {},
        (* Then *)
            useSecondIndices[[i]] = -useFirstIndices[[#[[1, 1]]]];
            AppendTo[removeIndices, {#[[1, 1]]}],
        (* Else *)
            AppendTo[newIndices, useSecondIndices[[i]]];
        ]& @ StringPosition[firstIndices, Characters[secondIndices][[i]]],
        {i, 1, Length[useSecondIndices]}
    ];
    newIndices = Delete[newIndices, removeIndices];
    (* The components of the first tensor will be taken in the default index representation. *)
    firstComponents = TensorData[firstID]["Components"][{useFirstIndices, useCoords}];
    (* The components of the second tensor will be taken in the index representation that matches the contraction, with indices raised or lowered to match the corresponding indices in the first tensor. *)
    secondComponents = AddRepresentation[secondID, useSecondIndices, useCoords];
    (* Collect the variables to be used for contracting the indices.*)
    allVars = Association[];
    (* allVars will contain all of the variables for both the first and the second tensor. *)
    Scan[(allVars[#] = Unique["var"]) &, Characters[firstIndices <> secondIndices]];
    (* firstVars will contain the variables to be used as indices for the first tensor. *)
    firstVars = allVars[#]& /@ Characters[firstIndices];
    (* secondVars will contain the variables to be used as indices for the second tensor. *)
    secondVars = allVars[#]& /@ Characters[secondIndices];
    (* sumVars will contain the variables to be used in the sum. *)
    sumVars = {};
    (* outVars will contain the variables to be used in the output tensor. *)
    outVars = {};
    (* outIndices will be the index string of the output tensor. *)
    outIndices = "";
    If[
        Length[Cases[Characters[firstIndices <> secondIndices], #]] == 2,
    (* Then *)
        AppendTo[sumVars, allVars[#]],
    (* Else *)
        AppendTo[outVars, allVars[#]];
        outIndices = outIndices <> #;
    ]& /@ Keys[allVars];
    (* Calculate the components of the new tensor by summing over the contracted variables. *)
    newComponents = Table[
        If[
            Length[sumVars] > 0,
        (* Then *)
            Sum[
                firstComponents[[Sequence @@ firstVars]] * secondComponents[[Sequence @@ secondVars]],
                Evaluate[Sequence @@ ({#, 1, Length[firstComponents]} & /@ sumVars)]
            ],
        (* Else *)
            firstComponents[[Sequence @@ firstVars]] * secondComponents[[Sequence @@ secondVars]]
        ],
        Evaluate[
            If[
                Length[outVars] > 0,
            (* Then *)
                Sequence @@ ({#, 1, Length[firstComponents]} & /@ outVars),
            (* Else *)
                1
            ]
        ]
    ];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{newIndices, useCoords} -> TensorSimplify[newComponents]],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> newIndices,
        "Metric" -> TensorData[firstID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> DefaultSymbol
    ]];
    Return[newID[outIndices]];
];


(* Take the covariant derivative of a tensor. *)
CreateUsageMessage[TCovariantD, {index}, "represents the covariant derivative when used in a tensor expression given to TCalc[ ].
If a tensor with ID \"`2`Christoffel\" exists, where `2` is the metric associated with the tensor the derivative is acting upon, then it will be assumed to be the Levi-Civita connection of the metric, and will be used in the calculation. Otherwise, \"`2`Christoffel\" will be created using TChristoffel[ ].", {metricID}];
CovariantDivOrGrad[derivativeIndex_String, tensorID_String[tensorIndices_String]] := Module[
    {
        myChristoffel,
        newComponents,
        newID,
        out,
        useCoords,
        useIndices
    },
    (* If the Christoffel symbols were not already calculated, calculate them now. *)
    myChristoffel = TensorData[tensorID]["Metric"] <> "Christoffel";
    If[
        !KeyExistsQ[TensorData, myChristoffel],
    (* Then *)
        TChristoffel[TensorData[tensorID]["Metric"]];
    ];
    (* The simplest way to calculate the covariant derivative is to do it for the representation of the tensor with all of its indices raised. Otherwise we have to worry about minus signs and different index order in the Christoffel symbols attached to the tensor's lower indices. So we create a new temporary tensor with all upper indices as its default index configuration. This guarantees that no raising or lowering of indices will take place when contracting the tensor with the partial derivative and the Christoffel symbols. *)
    useIndices = 1 & /@ TensorData[tensorID]["DefaultIndices"];
    useCoords = TensorData[tensorID]["DefaultCoords"];
    (* Add a representation with all upper indices to the tensor if it doesn't already exist. *)
    newComponents = AddRepresentation[tensorID, useIndices, useCoords];
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{useIndices, useCoords} -> newComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> useIndices,
        "Metric" -> TensorData[tensorID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> DefaultSymbol
    ]];
    (* The first term is just the partial derivative. *)
    out = DivOrGrad[derivativeIndex, newID[tensorIndices]];
    (* The next terms add one Christoffel symbol per index, contracted with that index. The Unicode symbol \|040200 is used as the summation index, to prevent collisions with indices given by the user. *)
    Scan[
        (out = AddTensors[out,
            ContractTensors[TensorTrace[myChristoffel[# <> derivativeIndex <> "\|040200"]],
            newID[StringReplace[tensorIndices, # -> "\|040200"]]]
        ]) &,
        Characters[tensorIndices]
    ];
    Return[out];
];


(* Take the divergence or gradient of a tensor. *)
CreateUsageMessage[TPartialD, {index}, "represents the partial derivative when used in a tensor expression given to TCalc[ ]."];
DivOrGrad::OneIndex = "The index specification of the partial derivative must be a string with exactly one character. If the character matches a character in the index specification of the tensor, it will be contracted with it to produce a divergence. Otherwise, the gradient will be calculated.";
DivOrGrad[derivativeIndex_String, tensorID_String[tensorIndices_String]] := Module[
    {
        allVars,
        components,
        coordinateSymbols,
        firstVars,
        newComponents,
        newID,
        newIndices,
        outIndices,
        outVars,
        pos,
        secondVars,
        sumVars,
        useCoords,
        useIndices
    },
    (* Check that the tensor exists. *)
    CheckIfTensorExists[tensorID];
    (* Check that the derivative has exactly once index. *)
    If[
        StringLength[derivativeIndex] != 1,
    (* Then *)
        Message[DivOrGrad::OneIndex];
        Abort[];
    ];
    (* Check that the index string matches the rank of the tensor. *)
    CheckIndicesRank[tensorIndices, tensorID];
    useIndices = TensorData[tensorID]["DefaultIndices"];
    useCoords = TensorData[tensorID]["DefaultCoords"];
    (* If the derivative's index matches one of the tensor's indices, we are calculating a divergence, so raise that index on the tensor (useIndices) to match the lower index on the derivative, and remove that index from the result (newIndices) since we are contracting it. Otherwise, we are calculating a gradient, so prepend a lower index to the result (newIndices) to account for the derivative's index. *)
    newIndices = useIndices;
    pos = StringPosition[tensorIndices, derivativeIndex];
    If[
        pos =!= {},
    (* Then *)
        useIndices[[pos[[1, 1]]]] = 1;
        newIndices = Delete[newIndices, pos[[1, 1]]],
    (* Else *)
        PrependTo[newIndices, -1];
    ];
    (* Get the tensor's components in the desired representation, adding it if it does not already exist. *)
    components = AddRepresentation[tensorID, useIndices, useCoords];
    (* Collect the variables to be used for contracting the indices. See ContractTensors for more details on how this part works. *)
    allVars = Association[];
    Scan[(allVars[#] = Unique["var"]) &, Characters[derivativeIndex <> tensorIndices]];
    firstVars = allVars[#]& /@ Characters[derivativeIndex];
    secondVars = allVars[#]& /@ Characters[tensorIndices];
    sumVars = {};
    outVars = {};
    outIndices = "";
    If[
        Length[Cases[Characters[derivativeIndex <> tensorIndices], #]] == 2,
    (* Then *)
        AppendTo[sumVars, allVars[#]],
    (* Else *)
        AppendTo[outVars, allVars[#]];
        outIndices = outIndices <> #;
    ]& /@ Keys[allVars];
    (* Collect the coordinate symbols, since we are taking derivatives with respect to them. *)
    coordinateSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* Calculate the components of the new tensor by summing over the contracted variables and taking the derivatives of the tensor's components. *)
    newComponents = Table[
        If[
            Length[sumVars] > 0,
        (* Then *)
            Sum[
                D[components[[Sequence @@ secondVars]], coordinateSymbols[[firstVars[[1]]]]],
                Evaluate[Sequence @@ ({#, 1, Length[components]} & /@ sumVars)]
            ],
        (* Else *)
            D[components[[Sequence @@ secondVars]], coordinateSymbols[[firstVars[[1]]]]]
        ],
        Evaluate[
            If[
                Length[outVars] > 0,
            (* Then *)
                Sequence @@ ({#, 1, Length[components]} & /@ outVars),
            (* Else *)
                1
            ]
        ]
    ];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{newIndices, useCoords} -> TensorSimplify[newComponents]],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> newIndices,
        "Metric" -> TensorData[tensorID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> DefaultSymbol
    ]];
    Return[newID[outIndices]];
];


(* Replace TensorData with the given Association. All previously defined tensor objects will be erased. *)
ImportTensorData[data_Association] := (
    Unprotect[TensorData];
    TensorData = data;
    Protect[TensorData];
);


(* Convert an index definition (i.e. a List with +1 for an upper index and -1 for a lower index) into letters. Used for displaying tensors using either letters or coordinates as indices. If the first argument is +1, returns a row with the upper indices, and empty spaces to account for the lower indices. If the first argument is -1, do the same with the lower indices. *)
IndicesToLetters[lookFor_Integer, upperLower_List, letters_String] :=
    Row[MapIndexed[If[
        #1 == lookFor,
    (* Then *)
        Characters[letters][[First[#2]]],
    (* Else *)
        Invisible[Characters[letters][[First[#2]]]]
    ] &, upperLower]];


(* Ensure that the tensor IDs given to temporary tensors are unique. *)
TempID = 0;
NewTempID[] := (
    Unprotect[TempID];
    TempID++;
    Protect[TempID];
    Return["_TCalcTemp" <> ToString[TempID] <> "_"];
);


(* Display an expression using the DisplayFormula style. The user must define what this style means manually in the notebook style sheet. *)
Nice[expression_] := Style[expression, "DisplayFormula"];


(* Change one of a tensor's indices to the desired position (upper = +1 or lower = -1), starting from a specific index configuration (assumed to be already calculated). The new components are then saved as a separate representation within the tensor object. Returns the new index configuration. *)
RaiseLower[ID_String, coordinatesID_String, oldIndices_List?VectorQ, indexPos_Integer, upperLower_Integer] := Module[
    {
        allComponents,
        components = TensorData[ID]["Components"][{oldIndices, coordinatesID}],
        dim,
        metricID = TensorData[ID]["Metric"],
        newComponents,
        newIndices,
        newVars,
        oldVars,
        raiseVar,
        sumVar,
        useMetric
    },
    dim = Length[components];
    (* If the metric used to raise and lower indices for this tensor has not yet had its components calculated in the desired coordinate system, calculate them now. *)
    If[
        !KeyExistsQ[TensorData[metricID]["Components"], {{-1, -1}, coordinatesID}],
    (* Then *)
        TransformCoordinates[metricID, {-1, -1}, TensorData[metricID]["DefaultCoords"], coordinatesID]
    ];
    Which[
        oldIndices[[indexPos]] == +1 && upperLower == -1,
            (* If lowering the index, use the metric (with two lower indices). *)
            useMetric = TensorData[metricID]["Components"][{{-1, -1}, coordinatesID}],
        oldIndices[[indexPos]] == -1 && upperLower == +1,
            (* If raising the index, use the inverse metric (with two upper indices). *)
            useMetric = TensorData[metricID]["Components"][{{+1, +1}, coordinatesID}],
        True,
            (* If the index is not being raised or lowered, do nothing. *)
            Return[oldIndices]
    ];
    (* The new indices are the same as the old indices, except for the one index being raised or lowered. *)
    newIndices = ReplacePart[oldIndices, indexPos -> upperLower];
    (* If a representation with the desired index configuration already exists, do nothing. *)
    If[
        KeyExistsQ[TensorData[ID]["Components"], {newIndices, coordinatesID}],
    (* Then *)
        Return[newIndices]
    ];
    (* Define the variables to be used in the calculation. As an example, say we want to lower the last index of T^abc. Then we have T^ab_c = g_cd T^abd. In this case newVars = {a, b, c}, oldVars = {a, b, d}, raiseVar = c, and sumVar = d. *)
    newVars = Unique[Table["var", {Length[newIndices]}]];
    oldVars = ReplacePart[newVars, indexPos -> sumVar];
    raiseVar = newVars[[indexPos]];
    (* Create the new components using a Table[] with newVars as iterators. Each component will be calculated using a Sum[] with sumVar as the summation variable. *)
    newComponents = (TensorSimplify[Table[
            Sum[
                useMetric[[raiseVar, sumVar]] * components[[Sequence @@ oldVars]],
                {sumVar, 1, dim}
            ], ##
        ]] &) @@ ({#, 1, dim} &) /@ newVars;
    (* Store the new representation in the tensor object. *)
    allComponents = TensorData[ID]["Components"];
    allComponents[{newIndices, coordinatesID}] = newComponents;
    ChangeTensorKey[ID, "Components", allComponents];
    (* Return the new index configuration. *)
    Return[newIndices];
];


(* Remove the tensor object with the given ID. *)
RemoveTensorID[ID_String] := (
    Unprotect[TensorData];
    KeyDropFrom[TensorData, ID];
    Protect[TensorData];
);


(* Create a new tensor object with the given ID, or assign it new data if it already exists. *)
SetTensorID[ID_String, data_Association] := (
    Unprotect[TensorData];
    TensorData[ID] = data;
    Protect[TensorData];
);


(* Show or list a tensor's components. Called by TShow and TList. *)
ShowList[ID_String, indices_List, coordinatesID_String, showOrList_String] := Module[
    {
        allElements,
        components,
        coordSymbols,
        grid,
        i,
        row,
        uniqueValues,
        uniqueValuesSign,
        useCoords,
        useIndices
    },
    (* Check that the tensor object ID exists. *)
    CheckIfTensorExists[ID];
    (* If specific indices are not given, use the tensor's default indices. *)
    If[
        indices === {"UseDefault"},
    (* Then *)
        useIndices = TensorData[ID]["DefaultIndices"],
    (* Else *)
        (* Check that the list of indices is of the correct form and has the correct rank. *)
        CheckIndicesForm[indices];
        CheckIndicesRank[indices, ID];
        useIndices = indices;
    ];
    (* If a specific coordinate system is not given, use the tensor's default coordinate system. *)
    If[
        coordinatesID === "UseDefault",
    (* Then *)
        useCoords = TensorData[ID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    (* Get the components of the tensor in the given representation (indices and coordinate system), or calculate it if it does not already exist. *)
    components = AddRepresentation[ID, useIndices, useCoords];
    (* Execute the following if called by TShow. *)
    If[
        showOrList === "Show",
    (* Then *)
        (* Print the tensor's ID followed by a colon. *)
        row = {ID, ":   "};
        (* Print the tensor's symbol along with the desired upper and lower indices. The index letters will be taken from DefaultLetters. IndicesToLetters will return a row with only the upper indices or only the lower indices depending on the first argument. *)
        row = Append[row, Subsuperscript[
            TensorData[ID]["Symbol"],
            IndicesToLetters[-1, useIndices, DefaultLetters],
            IndicesToLetters[+1, useIndices, DefaultLetters]
        ]];
        (* Print the tensor's coordinates as function arguments, e.g. (t, x, y, z), but only if the tensor does not itself represent coordinates. *)
        If[
            TensorData[ID]["Role"] =!= "Coordinates",
        (* Then *)
            row = Join[row, {"(", Row[TensorData[useCoords]["Components"][{{1}, useCoords}], ","], ")"}]
        ];
        (* Print an equal sign. *)
        row = Append[row, " = "];
        (* Print the components of the tensor. Use MatrixForm, unless it is a scalar. *)
        If[
            Length[components] * ArrayDepth[components] != 1,
        (* Then *)
            row = Append[row, MatrixForm[components]],
        (* Else *)
            row = Join[row, components]
        ];
        (* Return the row, formatted using Nice. *)
        Return[Nice[Row[row]]];
    ];
    (* Execute the following if called by TList. *)
    If[
        showOrList === "List",
    (* Then *)
        If[
            AllTrue[Flatten[components], # === 0 &],
        (* Then *)
            grid = "No non-zero elements.",
        (* Else *)
            (* The coordinate symbols (e.g. t, x, y, z) will be used in place of the indices. *)
            coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
            (* Create an array of elements of the form {value, label}, where label is the label of the specific component (with the coordinates as indices, e.g. g_xy is the component with x for the first index and y for the second index) and value is its value. *)
            allElements = Flatten[
                MapIndexed[
                    {
                        #1,
                        Subsuperscript[
                            TensorData[ID]["Symbol"],
                            IndicesToLetters[-1, useIndices, StringJoin[ToString /@ coordSymbols[[#2]]]],
                            IndicesToLetters[+1, useIndices, StringJoin[ToString /@ coordSymbols[[#2]]]]
                        ]
                    } &,
                    components,
                    {ArrayDepth[components]}
                ],
                ArrayDepth[components] - 1
            ];
            (* List only the unique tensor elements out of the array we created above. *)
            uniqueValues = DeleteCases[DeleteDuplicates[allElements[[All, 1]]], 0];
            (* List only the tensor elements that are unique up to sign. *)
            uniqueValuesSign = {};
            Scan[
                If[
                    !MemberQ[uniqueValuesSign, -#1],
                (* Then *)
                    AppendTo[uniqueValuesSign, #1]
                ] &,
                uniqueValues
            ];
            (* Create a grid of the unique tensor elements. In each row we find the elements of allElements that have a specific unique value out of uniqueValuesSign, and print all of them followed by the value. *)
            grid = Grid[
                Table[
                    {
                        Row[
                            Join[
                                Select[allElements, #1[[1]] ==  uniqueValuesSign[[i]] &][[All, 2]],
                                -Select[allElements, #1[[1]] == -uniqueValuesSign[[i]] &][[All, 2]]
                            ],
                            "="
                        ],
                        "=",
                        uniqueValuesSign[[i]]
                    },
                    {i, 1, Length[uniqueValuesSign]}
                ],
                Alignment -> {Left, Baseline}
            ];
        ];
        (* Return the grid, preceded by the tensor's ID, formatted using Nice. *)
        Return[Nice[Column[
            {
                Row[{ID, ":"}],
                grid
            },
            Alignment -> {Center, Baseline}
        ]]];
    ];
];


(* Multiply a tensor object by a scalar. *)
TensorByScalar::ErrorCoords = "The tensor \"`1`\" cannot be multiplied by a scalar, as it represents a coordinate system.";
TensorByScalar[ID_String[indices_String], scalar_] := Module[
    {
        newComponents,
        newID,
        useCoords,
        useIndices
    },
    (* Check that the tensor exists. *)
    CheckIfTensorExists[ID];
    (* Check that the tensor does not represent a coordinate system. *)
    If[
        TensorData[ID]["Role"] === "Coordinates",
    (* Then *)
        Message[TensorByScalar::ErrorCoords, ID];
        Abort[];
    ];
    (* Check that the index string matches the rank of the tensor. *)
    CheckIndicesRank[indices, ID];
    (* The components that will be multiplied are the ones corresponding to the default representation of the tensor. *)
    useIndices = TensorData[ID]["DefaultIndices"];
    useCoords = TensorData[ID]["DefaultCoords"];
    newComponents = scalar * TensorData[ID]["Components"][{useIndices, useCoords}];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{useIndices, useCoords} -> TensorSimplify[newComponents]],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> useIndices,
        "Metric" -> TensorData[ID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> DefaultSymbol
    ]];
    Return[newID[indices]];
];


(* Simplify an expression with optional user-defined assumptions. *)
UserAssumptions = None;
TensorSimplify[expression_] := FullSimplify[expression, UserAssumptions];


(* Take the trace of a tensor. *)
TensorTrace::ErrorMoreThanTwo = "The index specification \"`1`\" is invalid, as it contains more than two instances of the index \"`2`\".";
TensorTrace[ID_String[indices_String]] := Module[
    {
        allowedLetters,
        chars = Characters[indices],
        count,
        out,
        pos,
        tally,
        toContract
    },
    (* Check that the tensor exists. *)
    CheckIfTensorExists[ID];
    (* Tally the indices, i.e. count the multiplicities of all distinct indices. *)
    tally = Tally[chars];
    (* If no index appears twice, we do not need to take the trace. Just return the tensor as is. *)
    If[
        Max[tally[[All, 2]]] < 2,
    (* Then *)
        Return[ID[indices]];
    ];
    (* We want to replace any repeated indices by contractions with the metric, so for each contraction, we will need an arbitrary index letter to sum upon. To ensure that there are no collisions with the indices given by the user, we use characters from an unused Unicode plane. *)
    allowedLetters = CharacterRange["\|040000", "\|040100"];
    (* The list toContract will be populated with the index letters we are contracting. *)
    toContract = {};
    count = 0;
    Scan[
        (
            (* Check that no indices appear more than twice. *)
            If[
                #[[2]] > 2,
            (* Then *)
                Message[TensorTrace::ErrorMoreThanTwo, indices, #[[1]]];
                Abort[];
            ];
            (* For each index that appears exactly twice, add it to the list of indices to contract, and replace it with a summation index. *)
            If[
                #[[2]] == 2,
            (* Then *)
                count++;
                pos = Position[chars, #[[1]]][[2, 1]];
                (* Each entry in toContract will be of the form {contracted letter, summation letter}. These will be the two indices used in the metric for each contracted index. *)
                AppendTo[toContract, {chars[[pos]], allowedLetters[[count]]}];
                chars[[pos]] = allowedLetters[[count]];
            ];
        ) &,
        tally
    ];
    (* Start with the tensor we are taking the trace of, with the contracted indices replaced. *)
    out = ID[StringJoin[chars]];
    (* Contract the metric with each traced index of this tensor. *)
    Scan[
        (out = ContractTensors[TensorData[ID]["Metric"][StringJoin @@ #], out]) &,
        toContract
    ];
    Return[out];
];


(* Transform the components of a tensor, with the specified index configuration, from one coordinate system to another. Returns the new components as output. *)
TransformCoordinates::ErrorNoRules = "Rules for transforming coordinates from \"`1`\" to \"`2`\" have not been defined."
TransformCoordinates[ID_String, indices_List, sourceID_String, targetID_String] := Module[{
        allComponents,
        dim,
        inverseJacobian,
        jacobian,
        k,
        newComponents,
        newCoordSymbols,
        newVars,
        oldComponents,
        oldCoordSymbols,
        oldVars,
        rank,
        sumVar1,
        sumVar2,
        transRules
    },
    (* Get the components of all existing representations. *)
    allComponents = TensorData[ID]["Components"];
    (* If a representation in the desired coordinate system already exists, do nothing. *)
    If[
        KeyExistsQ[allComponents, {indices, targetID}],
    (* Then *)
        Return[allComponents[{indices, targetID}]]
    ];
    (* Get the components in the source coordinate system. *)
    oldComponents = allComponents[{indices, sourceID}];
    (* Check that rules to transform from the source to the target coordinate system have been defined. *)
    If[
        KeyExistsQ[TensorData[sourceID], "CoordTransformations"] && KeyExistsQ[TensorData[sourceID]["CoordTransformations"], targetID],
    (* Then *)
        transRules = TensorData[sourceID]["CoordTransformations"][targetID],
    (* Else *)
        Message[TransformCoordinates::ErrorNoRules, sourceID, targetID];
        Abort[];
    ];
    If[
        indices == {},
    (* Then *)
        (* If the tensor is a scalar, simply transform its one component. *)
        newComponents = TensorSimplify[oldComponents /. transRules],
    (* Else *)
        (* If the tensor is not a scalar, transform its components using contractions with the Jacobian. *)
        (* Get the rank of the tensor, corresponding to the number of variables to use. *)
        rank = ArrayDepth[oldComponents];
        (* Get the symbols (e.g. x, y, z) of the old and new coordinates, in terms of which the transformation is defined. *)
        oldCoordSymbols = TensorData[sourceID]["Components"][{{1}, sourceID}];
        newCoordSymbols = TensorData[targetID]["Components"][{{1}, targetID}];
        (* Get the dimension of the coordinates. *)
        dim = Length[oldCoordSymbols];
        (* Define the variables in terms of which to calculate the Jacobian. *)
        oldVars = Unique[Table["old", {rank}]];
        newVars = Unique[Table["new", {rank}]];
        (* Calculate the Jacobian for the transformation. *)
        jacobian = TensorSimplify[Table[
            D[oldCoordSymbols[[sumVar1]] /. transRules, newCoordSymbols[[sumVar2]]],
            {sumVar1, 1, dim},
            {sumVar2, 1, dim}
        ]];
        (* If there are any upper indices, calculate the inverse Jacobian as well. *)
        If[Count[indices, 1] > 0, inverseJacobian = TensorSimplify[Inverse[jacobian]]];
        (* Calculate the new components by contracting each lower index with the Jacobian and each upper index with the inverse Jacobian. *)
        newComponents = TensorSimplify[(
            Table[(
                Sum[Product[
                        Switch[
                            indices[[k]],
                            -1, jacobian[[oldVars[[k]], newVars[[k]]]],
                            +1, inverseJacobian[[newVars[[k]], oldVars[[k]]]]
                        ],
                        {k, 1, rank}
                    ] * (oldComponents[[##]] &) @@ oldVars, ##] &
            ) @@ ({#, 1, dim} &) /@ oldVars, ##] &) @@ ({#, 1, dim} &) /@ newVars /. transRules
        ];
        (* If the tensor is a metric, store the inverse metric and identity matrix for future use. *)
        If[
            TensorData[ID]["Role"] === "Metric",
        (* Then *)
            If[
                indices == {-1, -1} && !KeyExistsQ[allComponents, {{+1, +1}, targetID}],
            (* Then *)
                allComponents[{{+1, +1}, targetID}] = TensorSimplify[Inverse[newComponents]]
            ];
            If[
                indices == {+1, +1} && !KeyExistsQ[allComponents, {{-1, -1}, targetID}],
            (* Then *)
                allComponents[{{-1, -1}, targetID}] = TensorSimplify[Inverse[newComponents]]
            ];
            allComponents[{{+1, -1}, targetID}] = IdentityMatrix[dim];
            allComponents[{{-1, +1}, targetID}] = IdentityMatrix[dim];
        ];
    ];
    (* Store the new components. *)
    allComponents[{indices, targetID}] = newComponents;
    ChangeTensorKey[ID, "Components", allComponents];
    Return[newComponents];
];


End[]; (* OGRe`Private` *)


(* Protect all OGRe symbols so they will not be accidentally overwritten elsewhere. *)
Protect["OGRe`*"];
Protect["OGRe`Private`*"];


EndPackage[]; (* OGRe *)
