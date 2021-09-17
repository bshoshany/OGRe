(* ::Package:: *)


(*
OGRe: An (O)bject-Oriented (G)eneral (Re)lativity Package for Mathematica
By Barak Shoshany (baraksh@gmail.com) (baraksh.com)
GitHub repository: https://github.com/bshoshany/OGRe
Copyright (c) 2021 Barak Shoshany. Licensed under the MIT license.

If you use this package in published research, please cite it as follows:

* Shoshany, B., (2021). OGRe: An Object-Oriented General Relativity Package for Mathematica. Journal of Open Source Software, 6(65), 3416, https://doi.org/10.21105/joss.03416

You can also use the following BibTeX entry:

@article{Shoshany2021_OGRe,
    author    = {Barak Shoshany},
    doi       = {10.21105/joss.03416},
    journal   = {Journal of Open Source Software},
    number    = {65},
    pages     = {3416},
    publisher = {The Open Journal},
    title     = {OGRe: An Object-Oriented General Relativity Package for Mathematica},
    url       = {https://doi.org/10.21105/joss.03416},
    volume    = {6},
    year      = {2021}
}
*)

BeginPackage["OGRe`"];


(* Check if the package has already been loaded, in case Get was used instead of Needs. *)
If[
    ValueQ[OGRe`Private`AlreadyLoaded],
(* Then *)
    (* Unprotect and clear all symbols, so they can be redefined. Useful for debugging, or to reload the package after updating. *)
    Unprotect["OGRe`*"];
    Unprotect["OGRe`Private`*"];
    (* Keep the tensor objects and settings created so far during the session, so they don't get deleted when the package is reloaded. *)
    OGReTemp`TensorData = OGRe`Private`TensorData;
    ClearAll["OGRe`*"];
    ClearAll["OGRe`Private`*"];
    OGRe`Private`TensorData = OGReTemp`TensorData;
    Remove["OGReTemp`*"];
    OGRe`Private`AlreadyLoaded = True,
(* Else *)
    OGRe`Private`AlreadyLoaded = True;
    (* Initialize the symbol TensorData, which is used to store the data for the tensor objects, as well as user settings. This is done only on first load. *)
    OGRe`Private`TensorData = Association[];
];


(* A dirty trick to make the package's public modules globally visible without defining their usage messages in advance. I prefer to define each usage message at the same time as the module itself, so it can also serve as documentation for the code. *)
Null[{
    TAddCoordTransformation,
    TCalc,
    TCalcChristoffel,
    TCalcEinsteinTensor,
    TCalcGeodesicFromChristoffel,
    TCalcGeodesicFromLagrangian,
    TCalcGeodesicWithTimeParameter,
    TCalcLagrangian,
    TCalcNormSquared,
    TCalcRicciScalar,
    TCalcRicciTensor,
    TCalcRiemannTensor,
    TChangeDefaultCoords,
    TChangeDefaultIndices,
    TChangeID,
    TChangeSymbol,
    TCheckForUpdates,
    TCite,
    TCovariantD,
    TDelete,
    TDocs,
    TExport,
    TExportAll,
    TGetComponents,
    TImport,
    TImportAll,
    TInfo,
    TLineElement,
    TList,
    TMessage,
    TNewCoordinates,
    TNewMetric,
    TNewTensor,
    TPartialD,
    TSetAllowOverwrite,
    TSetAssumptions,
    TSetAutoUpdates,
    TSetCurveParameter,
    TSetIndexLetters,
    TSetParallelization,
    TSetReservedSymbols,
    TShow,
    TSimplify,
    TVolumeElementSquared
}];


Begin["`Private`"]; (* OGRe`Private` *)


(* DO NOT change the format of the next line. It is used by TCheckForUpdates to detect the version of this file. Changing it will break the automatic update mechanism. Only change the version number and date. *)
OGReVersion = "v1.7.0 (2021-09-17)";


(* The raw URL of this file on GitHub. *)
OGReURL = "https://raw.githubusercontent.com/bshoshany/OGRe/master/OGRe.m";


(* If the global options LocalSymbol has been previously set, then it will have the Head Association. Otherwise it will have the Head LocalSymbol, and we create it now for later use. *)
If[
    Head[LocalSymbol["OGReGlobalOptions"]] =!= Association,
(* Then *)
    LocalSymbol["OGReGlobalOptions"] = Association[];
];
(* Load the global options from the persistent storage. *)
OGReGlobalOptions = LocalSymbol["OGReGlobalOptions"];
(* Set the "OGReVersion" key to the current version of the package. *)
OGReGlobalOptions["OGReVersion"] = OGReVersion;
(* If the option "AutoUpdates" has not been set, or is not a boolean value, we set it now to the default value of True. *)
If[
    !KeyExistsQ[OGReGlobalOptions, "AutoUpdates"] || !BooleanQ[OGReGlobalOptions["AutoUpdates"]],
(* Then *)
    OGReGlobalOptions["AutoUpdates"] = True;
];
(* If the option "AllowOverwrite" has not been set, or is not a boolean value, we set it now to the default value of False. *)
If[
    !KeyExistsQ[OGReGlobalOptions, "AllowOverwrite"] || !BooleanQ[OGReGlobalOptions["AllowOverwrite"]],
(* Then *)
    OGReGlobalOptions["AllowOverwrite"] = False;
];
(* Save the global options to the persistent storage. *)
LocalSymbol["OGReGlobalOptions"] = OGReGlobalOptions;


(* ===================================================
   Public modules (accessible to the user) start here.
   =================================================== *)


(* Create a nicely-formatted usage message. *)
CreateUsageMessage[f_, msg_String : {}] := Evaluate[f::usage] = ToString[TextCell[Row[Flatten[{List @@ StringReplace[msg, {"`" ~~ (x : Shortest[__]) ~~ "`" :> Style[x, Bold]}]}]]], StandardForm];


CreateUsageMessage[TAddCoordTransformation, "TAddCoordTransformation[`sourceID` \[Rule] `targetID`, `rules`] adds a transformation from the coordinate system `sourceID` to the coordinate system `targetID`.
`rules` must be a list of transformation rules. For example, {x \[Rule] r Sin[\[Theta]] Cos[\[Phi]], y \[Rule] r Sin[\[Theta]] Sin[\[Phi]], z \[Rule] r Cos[\[Theta]]} is a transformation from Cartesian to spherical coordinates."];
TAddCoordTransformation::ErrorRulesForm = "The transformation rules must be a list of rules of the form x \[Rule] y.";
TAddCoordTransformation::ErrorDifferentCoords = "The source and target coordinate systems must be different.";
TAddCoordTransformation::ErrorNotSameDim = "The source and target coordinate systems must be of the same dimension.";
TAddCoordTransformation[sourceID_String -> targetID_String, rules_List] := TAddCoordTransformation[sourceID, targetID, rules];
TAddCoordTransformation[sourceID_String, targetID_String, rules_List] := Module[
    {
        allJacobians,
        ChristoffelJacobian,
        dim,
        i,
        inverseJacobian,
        j,
        jacobian,
        k,
        newCoordSymbols,
        oldCoordSymbols
    },
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
    (* Calculate the Jacobian, inverse Jacobian, and the "Christoffel Jacobian", i.e. the extra second-derivative term in the coordinate transformation of the Christoffel symbols, and store them in the tensor object of the source coordinates, to be used later whenever a coordinate transformation is performed. *)
    oldCoordSymbols = TensorData[sourceID]["Components"][{{1}, sourceID}];
    newCoordSymbols = TensorData[targetID]["Components"][{{1}, targetID}];
    dim = Length[oldCoordSymbols];
    jacobian = TensorSimplify[Table[
        D[oldCoordSymbols[[i]] /. rules, newCoordSymbols[[j]]],
        {i, 1, dim},
        {j, 1, dim}
    ]];
    inverseJacobian = TensorSimplify[Inverse[jacobian]];
    ChristoffelJacobian = TensorSimplify[Table[
        D[
            oldCoordSymbols[[i]] /. rules,
            newCoordSymbols[[j]],
            newCoordSymbols[[k]]
        ],
        {i, 1, dim},
        {j, 1, dim},
        {k, 1, dim}
    ]];
    allJacobians = Association[
        "Jacobian" -> jacobian,
        "InverseJacobian" -> inverseJacobian,
        "ChristoffelJacobian" -> ChristoffelJacobian
    ];
    (* Add all three Jacobians to the Jacobians key of the source object, or create it if it doesn't already exist. *)
    If[
        KeyExistsQ[TensorData[sourceID], "Jacobians"] && AssociationQ[TensorData[sourceID]["Jacobians"]],
    (* Then *)
        ChangeTensorKey[sourceID, "Jacobians", Append[TensorData[sourceID]["Jacobians"], targetID -> allJacobians]],
    (* Else *)
        ChangeTensorKey[sourceID, "Jacobians", Association[targetID -> allJacobians]];
    ];
    Return[sourceID];
];


DefaultResultID = "Result";
DefaultSymbol = "\[DottedSquare]";
CreateUsageMessage[TCalc, "TCalc[`formula`] calculates a tensor `formula`, which may involve any number of tensors in the format `ID`[`indices`], where `ID` is a tensor object and `indices` is a string representing the order of indices, along with any combination of the following operations:
\[Bullet] Addition: For example, \"A\"[\"\[Mu]\[Nu]\"] + \"B\"[\"\[Mu]\[Nu]\"].
\[Bullet] Contraction: For example, \"A\"[\"\[Mu]\[Lambda]\"] . \"B\"[\"\[Lambda]\[Nu]\"].
\[Bullet] Multiplication by scalar: For example, 2 * \"A\"[\"\[Mu]\[Nu]\"].
TCalc[`targetID`[`targetIndices`], `formula`, `symbol`] calculates a tensor `formula` and stores the result in a new tensor object.
`targetID` specifies the ID of the tensor object in which to store the result. If omitted, the ID \"" <> DefaultResultID <> "\" will be used.
`targetIndices` specifies the order of indices of the resulting tensor. The indices must be a permutation of the free indices of `formula`. If omitted, the indices are assumed to be in the same order as they appear in `formula`.
`symbol` specifies the symbol to use for the resulting tensor. If omitted, the placeholder symbol " <> DefaultSymbol <> " will be used."];
TCalc::ErrorIndices = "The LHS index specification \"`1`\" and the RHS index specification \"`2`\" must be the same up to permutation.";
TCalc::ErrorResult = "Invalid tensor expression obtained: `1`. Please check that the tensor expression you entered contains only tensor references of the form \"ID\"[\"indices\"] combined using addition, contraction (dot product), or multiplication by scalar."
TCalc[RHSExpression_, symbol_String : DefaultSymbol] := TCalc[DefaultResultID[""], RHSExpression, symbol];
TCalc[LHSTensorID_String, RHSExpression_, symbol_String : DefaultSymbol] := TCalc[LHSTensorID[""], RHSExpression, symbol];
TCalc[LHSTensorID_String[LHSIndices_String], RHSExpression_, symbol_String : DefaultSymbol] := Module[
    {
        allVars,
        components,
        LHSVars,
        newComponents,
        newIndices,
        result,
        resultID,
        resultIndices,
        RHSVars,
        rules,
        useCoords,
        useIndices
    },
    (* Check that the tensor LHSTensorID doesn't already exist, but only if it's not the default ID. *)
    If[
        LHSTensorID =!= DefaultResultID,
    (* Then *)
        CheckIfOverwriting[LHSTensorID];
    ];
    (* Define the rules for computing tensor formulas. *)
    rules = {
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
    (* Repeatedly replace tensor operations with their results until we reach a fixed point. *)
    result = ReplaceRepeated[RHSExpression, rules];
    (* Check that the result is valid, i.e. of the form "tensorID"["indices"]. *)
    If[
        !MatchQ[result, _String[_String]],
    (* Then *)
        Message[TCalc::ErrorResult, result];
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
    (* Simplify the components. *)
    components = TensorSimplify[components];
    If[
        LHSIndices === "",
    (* Then *)
        (* Either a scalar, or no rearranging of indices is desired. Store the result directly in a new tensor object. *)
        SetTensorID[LHSTensorID, Association[
            "Components" -> Association[{useIndices, useCoords} -> components],
            "DefaultCoords" -> useCoords,
            "DefaultIndices" -> useIndices,
            "Metric" -> TensorData[resultID]["Metric"],
            "Role" -> "Calculated",
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
            "Role" -> "Calculated",
            "Symbol" -> symbol
        ]];
    ];
    (* Print the explicit formula we calculated, including the correct index placement. TODO: Uncomment this in a future version, once this feature works properly. *)
    (* OGRePrint["Calculated: ", TensorData[resultID]["Symbol"]]; *)
    (* Clear the temporary tensors that were created for the purpose of the calculation. *)
    ClearTemp[];
    Return[LHSTensorID];
];


CreateUsageMessage[TCalcChristoffel, "TCalcChristoffel[`metricID`] calculates the Christoffel symbols (the coefficients of the Levi-Civita connection) from the metric `metricID` and stores the result in a new tensor object with ID \"`metricID`Christoffel\". Note that the Christoffel symbols are not the components of a tensor, but this tensor object will know to transform according to the correct rules under change of coordinates."];
TCalcChristoffel::ErrorNotMetric = "The Christoffel symbols can only be calculated from a tensor object representing a metric.";
TCalcChristoffel[metricID_String] := Module[
    {
        christoffelID,
        inverseMetricID = NewTempID[]
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcChristoffel::ErrorNotMetric];
        Abort[];
    ];
    (* Create a temporary tensor for the inverse metric, with two upper indices as its default configuration, to force the Christoffel symbols to have the correct index configuration. We do this to increase performance, since if we don't, then we'll have to raise the first index later, which is a costly operation. *)
    SetTensorID[inverseMetricID, Association[
        "Components" -> TensorData[metricID]["Components"],
        "DefaultCoords" -> TensorData[metricID]["DefaultCoords"],
        "DefaultIndices" -> {1, 1},
        "Metric" -> TensorData[metricID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> Superscript[TensorData[metricID]["Symbol"], "\[Lambda]\[Sigma]"]
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
    (* Set the role of the tensor to Christoffel, so that OGRe will know to transform it as a connection and not as a tensor. *)
    ChangeTensorKey[christoffelID, "Role", "Christoffel"];
    Return[christoffelID];
];


CreateUsageMessage[TCalcEinsteinTensor, "TCalcEinsteinTensor[`metricID`] calculates the Einstein tensor from the metric `metricID` and stores the result in a new tensor object with ID \"`metricID`Einstein\". If a tensor with ID \"`metricID`RicciTensor\" exists, it will be assumed to be the Ricci tensor of the metric, and will be used in the calculation. Otherwise, \"`metricID`RicciTensor\" will be created using TCalcRicciTensor[]."];
TCalcEinsteinTensor::ErrorNotMetric = "The Einstein tensor can only be calculated from a tensor object representing a metric.";
TCalcEinsteinTensor[metricID_String] := Module[
    {
        EinsteinID
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcEinsteinTensor::ErrorNotMetric];
        Abort[];
    ];
    (* If the Ricci tensor was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "RicciTensor"],
    (* Then *)
        TCalcRicciTensor[metricID];
    ];
    (* Calculate the Einstein tensor, and give the tensor object the correct ID and symbol. *)
    EinsteinID = TCalc[
        (metricID <> "Einstein")["\[Mu]\[Nu]"],
        (metricID <> "RicciTensor")["\[Mu]\[Nu]"] - 1/2 metricID["\[Mu]\[Nu]"] . (metricID <> "RicciTensor")["\[Rho]\[Rho]"],
        "G"
    ];
    (* Set the role of the tensor to Einstein for future reference. *)
    ChangeTensorKey[EinsteinID, "Role", "Einstein"];
    Return[EinsteinID];
];


CreateUsageMessage[TCalcGeodesicFromChristoffel, "TCalcGeodesicFromChristoffel[`metricID`, `coordinatesID`] calculates the geodesic equations obtained for each of the coordinates in `coordinatesID` using the Christoffel symbols of the metric `metricID` and stores the result in a new rank-1 tensor object with ID \"`metricID`GeodesicFromChristoffel\". Equating the components to zero will yield the full system of geodesic equations.
The result will be given in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to the curve parameter. The curve parameter can be selected using TSetCurveParameter[].
If `coordinatesID` is not specified, the default coordinate system of the metric will be used."]
TCalcGeodesicFromChristoffel::ErrorNotMetric = "The geodesic equation vector can only be calculated from a tensor object representing a metric.";
TCalcGeodesicFromChristoffel[metricID_String, coordinatesID_String : "_UseDefault_"] := Module[
    {
        accelComponents,
        accelID,
        christComponents,
        christID,
        coordSymbols,
        newID = metricID <> "GeodesicFromChristoffel",
        parameter = Symbol[TensorData[Options]["CurveParameter"]],
        tangentComponents,
        tangentID,
        tempMetricComponents,
        tempMetricID,
        useCoords
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcGeodesicFromChristoffel::ErrorNotMetric];
        Abort[];
    ];
    (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[metricID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    (* If the Christoffel symbols were not already calculated, calculate them now. This should be done first, otherwise the temporary tensors will be deleted prematurely when TCalc finishes. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Christoffel"],
    (* Then *)
        TCalcChristoffel[metricID];
    ];
    (* Make sure we have the components of the Christoffel symbols in the correct coordinate system. *)
    AddRepresentation[metricID <> "Christoffel", {1, -1, -1}, useCoords];
    coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* Define a temporary metric with any instance of the coordinate symbols replaced with coordinate functions in terms of the curve parameter. For example, with coordinates {x, y, z}, the expression z * f[x, y] will be replaced with z[c] * f[x[c], y[c]] where c is the curve parameter. *)
    tempMetricID = NewTempID[];
    tempMetricComponents = TensorData[metricID]["Components"][{{-1, -1}, useCoords}] /. (# -> #[parameter] & /@ coordSymbols);
    SetTensorID[tempMetricID, Association[
        "Components" -> Association[{{-1, -1}, useCoords} -> tempMetricComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {-1, -1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> TensorData[metricID]["Symbol"]
    ]];
    (* Define a temporary vector representing the tangent vector to the curve. The vector consists of the derivatives of the coordinates with respect to the curve parameter. The vector will be associated to the metric tempMetricID. *)
    tangentID = NewTempID[];
    tangentComponents = coordSymbols /. ((# -> #'[parameter]) & /@ coordSymbols);
    SetTensorID[tangentID, Association[
        "Components" -> Association[{{1}, useCoords} -> tangentComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> OverDot[TensorData[useCoords]["Symbol"]]
    ]];
    (* Define a temporary vector representing the acceleration of the curve. The vector consists of the second derivatives of the coordinates with respect to the curve parameter. The vector will be associated to the metric tempMetricID. *)
    accelID = NewTempID[];
    accelComponents = coordSymbols /. ((# -> #''[parameter]) & /@ coordSymbols);
    SetTensorID[accelID, Association[
        "Components" -> Association[{{1}, useCoords} -> accelComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> Overscript[TensorData[useCoords]["Symbol"], "\[DoubleDot]"]
    ]];
    (* Copy the Christoffel symbols to a new temporary tensor associated with the new metric and replace the coordinate symbols with coordinate functions of the curve parameters as above. *)
    christID = NewTempID[];
    christComponents = TensorData[metricID <> "Christoffel"]["Components"][{{1, -1, -1}, useCoords}] /. (# -> #[parameter] & /@ coordSymbols);
    SetTensorID[christID, Association[
        "Components" -> Association[{{1, -1, -1}, useCoords} -> christComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1, -1, -1},
        "Metric" -> tempMetricID,
        "Role" -> "Christoffel",
        "Symbol" -> "\[CapitalGamma]"
    ]];
    (* Calculate the geodesic equation vector and give it the symbol 0 since it is equal to the zero vector. *)
    TCalc[newID, accelID["\[Sigma]"] + christID["\[Sigma]\[Mu]\[Nu]"] . tangentID["\[Mu]"] . tangentID["\[Nu]"], "0"];
    (* Delete the copy of the Christoffel symbols tensor. We did not mark it as temporary, to ensure that it is correctly treated as Christoffel symbols and not as a proper tensor, therefore TCalc will not remove it automatically. *)
    RemoveTensorID[christID];
    (* Change the geodesic equation vector's associated metric back to the original metric. *)
    ChangeTensorKey[newID, "Metric", metricID];
    (* Set the geodesic equation vector's role. *)
    ChangeTensorKey[newID, "Role", "GeodesicFromChristoffel"];
    Return[newID];
];


CreateUsageMessage[TCalcGeodesicWithTimeParameter, "TCalcGeodesicWithTimeParameter[`metricID`, `coordinatesID`] calculates the geodesic equations obtained for each of the coordinates in `coordinatesID` using the Christoffel symbols of the metric `metricID` and stores the result in a new rank-1 tensor object with ID \"`metricID`GeodesicFromChristoffel\". Equating the components to zero will yield the full system of geodesic equations.
The result will be given in terms of the spatial coordinate symbols as functions of the time coordinate and their derivatives with respect to time. The first coordinate of `coordinatesID` will be assumed to be the time coordinate, even if its symbol is not t.
If `coordinatesID` is not specified, the default coordinate system of the metric will be used."]
TCalcGeodesicWithTimeParameter::ErrorNotMetric = "The geodesic equation vector can only be calculated from a tensor object representing a metric.";
TCalcGeodesicWithTimeParameter[metricID_String, coordinatesID_String : "_UseDefault_"] := Module[
    {
        accelComponents,
        accelID,
        christComponents,
        christID,
        christWith0Components,
        christWith0ID,
        coordSymbols,
        coordSymbolsWithoutTime,
        newID = metricID <> "GeodesicWithTimeParameter",
        parameter,
        tangentComponents,
        tangentID,
        tempMetricComponents,
        tempMetricID,
        useCoords
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcGeodesicWithTimeParameter::ErrorNotMetric];
        Abort[];
    ];
    (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[metricID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    (* If the Christoffel symbols were not already calculated, calculate them now. This should be done first, otherwise the temporary tensors will be deleted prematurely when TCalc finishes. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Christoffel"],
    (* Then *)
        TCalcChristoffel[metricID];
    ];
    (* Make sure we have the components of the Christoffel symbols in the correct coordinate system. *)
    AddRepresentation[metricID <> "Christoffel", {1, -1, -1}, useCoords];
    coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* Use the first coordinate as the curve parameter. *)
    parameter = coordSymbols[[1]];
    (* For the replacements below we only want to replace the non-time coordinates. *)
    coordSymbolsWithoutTime = coordSymbols[[2 ;;]];
    (* Define a temporary metric with any instance of the coordinate symbols, except t, replaced with coordinate functions in terms of t. For example, with coordinates {t, x, y, z}, the expression z * f[x, y] will be replaced with z[t] * f[t, x[t], y[t]]. *)
    tempMetricID = NewTempID[];
    tempMetricComponents = TensorData[metricID]["Components"][{{-1, -1}, useCoords}] /. (# -> #[parameter] & /@ coordSymbolsWithoutTime);
    SetTensorID[tempMetricID, Association[
        "Components" -> Association[{{-1, -1}, useCoords} -> tempMetricComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {-1, -1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> TensorData[metricID]["Symbol"]
    ]];
    (* Define a temporary vector representing the tangent vector to the curve. The vector consists of the derivatives of the coordinates with respect to t, thus the first component will be equal to 1. The vector will be associated to the metric tempMetricID. *)
    tangentID = NewTempID[];
    tangentComponents = coordSymbols /. ((# -> #'[parameter]) & /@ coordSymbolsWithoutTime);
    tangentComponents[[1]] = 1;
    SetTensorID[tangentID, Association[
        "Components" -> Association[{{1}, useCoords} -> tangentComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> OverDot[TensorData[useCoords]["Symbol"]]
    ]];
    (* Define a temporary vector representing the acceleration of the curve. The vector consists of the second derivatives of the coordinates with respect to t, thus the first component will be equal to 0. The vector will be associated to the metric tempMetricID. *)
    accelID = NewTempID[];
    accelComponents = coordSymbols /. ((# -> #''[parameter]) & /@ coordSymbolsWithoutTime);
    accelComponents[[1]] = 0;
    SetTensorID[accelID, Association[
        "Components" -> Association[{{1}, useCoords} -> accelComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> Overscript[TensorData[useCoords]["Symbol"], "\[DoubleDot]"]
    ]];
    (* Copy the Christoffel symbols to a new temporary tensor associated with the new metric and replace the coordinate symbols, except t, with coordinate functions of t as above. *)
    christID = NewTempID[];
    christComponents = TensorData[metricID <> "Christoffel"]["Components"][{{1, -1, -1}, useCoords}] /. (# -> #[parameter] & /@ coordSymbolsWithoutTime);
    SetTensorID[christID, Association[
        "Components" -> Association[{{1, -1, -1}, useCoords} -> christComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1, -1, -1},
        "Metric" -> tempMetricID,
        "Role" -> "Christoffel",
        "Symbol" -> "\[CapitalGamma]"
    ]];
    (* For the geodesic equations in terms of t, we also need the Christoffel symbols with 0 in the first index, which is a rank-2 tensor. The following is just a temporary solution until I implement the option to do something like christID["0AB"] and get the components automatically. *)
    christWith0ID = NewTempID[];
    christWith0Components = christComponents[[1, All, All]];
    SetTensorID[christWith0ID, Association[
        "Components" -> Association[{{-1, -1}, useCoords} -> christWith0Components],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {-1, -1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> "\[CapitalGamma]"
    ]];
    (* Calculate the geodesic equation vector and give it the symbol 0 since it is equal to the zero vector. *)
    TCalc[newID, accelID["\[Sigma]"] + (christID["\[Sigma]\[Mu]\[Nu]"] - christWith0ID["\[Mu]\[Nu]"] . tangentID["\[Sigma]"]) . tangentID["\[Mu]"] . tangentID["\[Nu]"], "0"];
    (* Delete the copy of the Christoffel symbols tensor. We did not mark it as temporary, to ensure that it is correctly treated as Christoffel symbols and not as a proper tensor, therefore TCalc will not remove it automatically. *)
    RemoveTensorID[christID];
    (* Change the geodesic equation vector's associated metric back to the original metric. *)
    ChangeTensorKey[newID, "Metric", metricID];
    (* Set the geodesic equation vector's role. *)
    ChangeTensorKey[newID, "Role", "GeodesicWithTimeParameter"];
    Return[newID];
];


CreateUsageMessage[TCalcGeodesicFromLagrangian, "TCalcGeodesicFromLagrangian[`metricID`, `coordinatesID`] calculates the geodesic equations obtained for each of the coordinates in `coordinatesID` using the curve Lagrangian of the metric `metricID` and stores the result in a new rank-1 tensor object with ID \"`metricID`GeodesicFromLagrangian\". Equating the components to zero will yield the full system of geodesic equations.
Derivatives with respect to the curve parameter in the Euler-Lagrange equation will be left unevaluated using Inactive[], which can sometimes help solve the geodesic equations by inspection. Use Activate[] to evaluate the derivatives.
The result will be given in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to the curve parameter. The curve parameter can be selected using TSetCurveParameter[].
If `coordinatesID` is not specified, the default coordinate system of the metric will be used."]
TCalcGeodesicFromLagrangian::ErrorNotMetric = "The geodesic equation vector can only be calculated from a tensor object representing a metric.";
TCalcGeodesicFromLagrangian[metricID_String, coordinatesID_String : "_UseDefault_"] := Module[
    {
        coordSymbols,
        EulerLagrangeComponents,
        LagrangianComponents,
        newID = metricID <> "GeodesicFromLagrangian",
        parameter = Symbol[TensorData[Options]["CurveParameter"]],
        useCoords
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcGeodesicFromLagrangian::ErrorNotMetric];
        Abort[];
    ];
    (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[metricID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    (* If the Lagrangian was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Lagrangian"],
    (* Then *)
        TCalcLagrangian[metricID, useCoords];
    ];
    (* Make sure we have the components of the Lagrangian in the correct coordinate system. We divide them by 2 since geodesics calculated in this way will inevitably get additional factors of 2 from taking the derivatives of squares. *)
    LagrangianComponents = AddRepresentation[metricID <> "Lagrangian", {}, useCoords][[1]] / 2;
    coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* Calculate the geodesic equation vector from the Lagrangian by calculating the Euler-Lagrange equation for each variable. We leave the derivative with respect to the curve parameter implicit using Inactive. *)
    EulerLagrangeComponents = Table[D[LagrangianComponents, coordSymbols[[n]][parameter]] - Inactive[D][D[LagrangianComponents, coordSymbols[[n]]'[parameter]], parameter], {n, 1, Length[coordSymbols]}];
    (* Store the result in a new tensor object and give it the symbol 0 since it is equal to the zero vector. *)
    SetTensorID[newID, Association[
        "Components" -> Association[{{1}, useCoords} -> TensorSimplify[EulerLagrangeComponents]],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1},
        "Metric" -> metricID,
        "Role" -> "GeodesicFromLagrangian",
        "Symbol" -> "0"
    ]];
    Return[newID];
];


CreateUsageMessage[TCalcLagrangian, "TCalcLagrangian[`metricID`, `coordinatesID`] calculates the curve Lagrangian of the metric `metricID`, defined as the norm-squared of the tangent to the curve, and stores the result in a new tensor object with ID \"`metricID`Lagrangian\".
Taking the square root of (the absolute value of) the Lagrangian yields the integrand of the curve length functional. Varying the Lagrangian using the Euler-Lagrange equations yields the geodesic equations.
The result will be given in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to the curve parameter. The curve parameter can be selected using TSetCurveParameter[].
If `coordinatesID` is not specified, the default coordinate system of the metric will be used."]
TCalcLagrangian::ErrorNotMetric = "The curve Lagrangian can only be calculated from a tensor object representing a metric.";
TCalcLagrangian[metricID_String, coordinatesID_String : "_UseDefault_"] := Module[
    {
        coordSymbols,
        newID = metricID <> "Lagrangian",
        parameter = Symbol[TensorData[Options]["CurveParameter"]],
        tangentComponents,
        tangentID,
        tempMetricComponents,
        tempMetricID,
        useCoords
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcLagrangian::ErrorNotMetric];
        Abort[];
    ];
    (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[metricID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* Define a temporary metric with any instance of the coordinate symbols replaced with coordinate functions in terms of the curve parameter. For example, with coordinates {x, y, z}, the expression z * f[x, y] will be replaced with z[c] * f[x[c], y[c]] where c is the curve parameter. *)
    tempMetricID = NewTempID[];
    tempMetricComponents = TensorData[metricID]["Components"][{{-1, -1}, useCoords}] /. (# -> #[parameter] & /@ coordSymbols);
    SetTensorID[tempMetricID, Association[
        "Components" -> Association[{{-1, -1}, useCoords} -> tempMetricComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {-1, -1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> TensorData[metricID]["Symbol"]
    ]];
    (* Define a temporary vector representing the tangent vector to the curve. The vector consists of the derivatives of the coordinates with respect to the curve parameter. The vector will be associated to the metric tempMetricID. *)
    tangentID = NewTempID[];
    tangentComponents = coordSymbols /. ((# -> #'[parameter]) & /@ coordSymbols);
    SetTensorID[tangentID, Association[
        "Components" -> Association[{{1}, useCoords} -> tangentComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> {1},
        "Metric" -> tempMetricID,
        "Role" -> "Temporary",
        "Symbol" -> OverDot[TensorData[useCoords]["Symbol"]]
    ]];
    (* Calculate the Lagrangian and give it the symbol L. *)
    TCalc[newID, tangentID["\[Mu]"] . tangentID["\[Mu]"], "L"];
    (* Change the Lagrangian's associated metric back to the original metric. *)
    ChangeTensorKey[newID, "Metric", metricID];
    (* Set the Lagrangian's role. *)
    ChangeTensorKey[newID, "Role", "Lagrangian"];
    Return[newID];
];


CreateUsageMessage[TCalcNormSquared, "TCalcNormSquared[`tensorID` calculates the norm-squared of the tensor `tensorID` with respect to its metric, that is, the tensor contracted with itself in all indices. For example, for a vector v^a the norm-squared will be v^a v_a and for a rank-2 tensor T^ab the result will be T^ab T_ab. The result is stored in a new rank-0 tensor object with ID \"`tensorID`NormSquared\"."];
TCalcNormSquared[tensorID_String] := Module[
    {
        indices,
        NormSquaredID,
        rank
    },
    (* Check that tensorID exists. *)
    CheckIfTensorExists[tensorID];
    (* Get the tensor's rank so we know how many indices to contract, and prepare the index string. *)
    rank = Length[TensorData[tensorID]["DefaultIndices"]];
    indices = StringTake[DefaultIndexLetters, rank];
    (* Calculate the norm squared, and give the tensor object the correct ID. The symbol will be left as the default. *)
    NormSquaredID = TCalc[
        (tensorID <> "NormSquared")[""],
        tensorID[indices] . tensorID[indices]
    ];
    (* Set the role of the tensor to NormSquared for future reference. *)
    ChangeTensorKey[NormSquaredID, "Role", "NormSquared"];
    Return[NormSquaredID];
];


CreateUsageMessage[TCalcRicciScalar, "TCalcRicciScalar[`metricID` calculates the Ricci scalar from the metric `metricID` and stores the result in a new tensor object with ID \"`metricID`RicciScalar\". If a tensor with ID \"`metricID`RicciTensor\" exists, it will be assumed to be the Ricci tensor of the metric, and will be used in the calculation. Otherwise, \"`metricID`RicciTensor\" will be created using TCalcRicciTensor[]."];
TCalcRicciScalar::ErrorNotMetric = "The Ricci scalar can only be calculated from a tensor object representing a metric.";
TCalcRicciScalar[metricID_String] := Module[
    {
        RicciScalarID
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcRicciScalar::ErrorNotMetric];
        Abort[];
    ];
    (* If the Ricci tensor was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "RicciTensor"],
    (* Then *)
        TCalcRicciTensor[metricID];
    ];
    (* Calculate the Ricci scalar, and give the tensor object the correct ID and symbol. *)
    RicciScalarID = TCalc[
        (metricID <> "RicciScalar")[""],
        (metricID <> "RicciTensor")["\[Mu]\[Mu]"],
        "R"
    ];
    (* Set the role of the tensor to Ricci Scalar for future reference. *)
    ChangeTensorKey[RicciScalarID, "Role", "Ricci Scalar"];
    Return[RicciScalarID];
];


CreateUsageMessage[TCalcRicciTensor, "TCalcRicciTensor[`metricID`] calculates the Ricci tensor from the metric `metricID` and stores the result in a new tensor object with ID \"`metricID`RicciTensor\". If a tensor with ID \"`metricID`Riemann\" exists, it will be assumed to be the Riemann tensor of the metric, and will be used in the calculation. Otherwise, \"`metricID`Riemann\" will be created using TCalcRiemannTensor[]."];
TCalcRicciTensor::ErrorNotMetric = "The Ricci tensor can only be calculated from a tensor object representing a metric.";
TCalcRicciTensor[metricID_String] := Module[
    {
        RicciTensorID
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcRicciTensor::ErrorNotMetric];
        Abort[];
    ];
    (* If the Riemann tensor was not already calculated, calculate it now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Riemann"],
    (* Then *)
        TCalcRiemannTensor[metricID];
    ];
    (* Calculate the Ricci tensor, and give the tensor object the correct ID and symbol. *)
    RicciTensorID = TCalc[
        (metricID <> "RicciTensor")["\[Mu]\[Nu]"],
        (metricID <> "Riemann")["\[Lambda]\[Mu]\[Lambda]\[Nu]"],
        "R"
    ];
    (* Set the role of the tensor to Ricci Tensor for future reference. *)
    ChangeTensorKey[RicciTensorID, "Role", "Ricci Tensor"];
    Return[RicciTensorID];
];


CreateUsageMessage[TCalcRiemannTensor, "TCalcRiemannTensor[`metricID`] calculates the Riemann tensor from the metric `metricID` and stores the result in a new tensor object with ID \"`metricID`Riemann\". If a tensor with ID \"`metricID`Christoffel\" exists, it will be assumed to be the Christoffel symbols of the metric, and will be used in the calculation. Otherwise, \"`metricID`Christoffel\" will be created using TCalcChristoffel[]."];
TCalcRiemannTensor::ErrorNotMetric = "The Riemann tensor can only be calculated from a tensor object representing a metric.";
TCalcRiemannTensor[metricID_String] := Module[
    {
        RiemannID
    },
    (* Check that metricID exists. *)
    CheckIfTensorExists[metricID];
    (* Check that metricID is indeed a metric. *)
    If[
        TensorData[metricID]["Role"] =!= "Metric",
    (* Then *)
        Message[TCalcRiemannTensor::ErrorNotMetric];
        Abort[];
    ];
    (* If the Christoffel symbols were not already calculated, calculate them now. *)
    If[
        !KeyExistsQ[TensorData, metricID <> "Christoffel"],
    (* Then *)
        TCalcChristoffel[metricID];
    ];
    (* Calculate the Riemann tensor, and give the tensor object the correct ID, symbol, and default index configuration. *)
    RiemannID = TChangeDefaultIndices[
        TCalc[
            (metricID <> "Riemann")["\[Rho]\[Sigma]\[Mu]\[Nu]"],
            TPartialD["\[Mu]"] . (metricID <> "Christoffel")["\[Rho]\[Nu]\[Sigma]"] -
            TPartialD["\[Nu]"] . (metricID <> "Christoffel")["\[Rho]\[Mu]\[Sigma]"] +
            (metricID <> "Christoffel")["\[Rho]\[Mu]\[Lambda]"] . (metricID <> "Christoffel")["\[Lambda]\[Nu]\[Sigma]"] -
            (metricID <> "Christoffel")["\[Rho]\[Nu]\[Lambda]"] . (metricID <> "Christoffel")["\[Lambda]\[Mu]\[Sigma]"],
            "R"
        ],
        {1,-1,-1,-1}
    ];
    (* Set the role of the tensor to Riemann for future reference. *)
    ChangeTensorKey[RiemannID, "Role", "Riemann"];
    Return[RiemannID];
];


CreateUsageMessage[TChangeDefaultCoords, "TChangeDefaultCoords[`tensorID`, `coordinatesID`] changes the default coordinate system of the tensor object `tensorID` to `coordinatesID`."];
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


CreateUsageMessage[TChangeDefaultIndices, "TChangeDefaultIndices[`ID`, `indices`] changes the default index configuration of the tensor object `ID` to `indices`.
`indices` must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index."];
TChangeDefaultIndices::ErrorCoords = "Cannot change the default index configuration for a tensor object representing a coordinate system."
TChangeDefaultIndices::ErrorMetric = "Cannot change the default index configuration for a tensor object representing a metric."
TChangeDefaultIndices::ErrorChristoffel = "Cannot change the default index configuration for a tensor object representing a Levi-Civita connection (Christoffel symbols)."
TChangeDefaultIndices[ID_String, indices_List] := (
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
    (* Check that the tensor object does not represent a connection. *)
    If[
        TensorData[ID]["Role"] === "Christoffel",
    (* Then *)
        Message[TChangeDefaultIndices::ErrorChristoffel];
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


CreateUsageMessage[TChangeID, "TChangeID[`oldID` \[Rule] `newID`] changes the ID of the tensor object `oldID` to `newID`.
If the tensor is a metric or a coordinate system, all currently defined tensors will be scanned, and any references to `oldID` will be replaced with `newID`."];
TChangeID[oldID_String -> newID_String] := TChangeID[oldID, newID];
TChangeID[oldID_String, newID_String] := (
    (* Check that the tensor object oldID exists. *)
    CheckIfTensorExists[oldID];
    (* Check that the tensor object newID doesn't already exist. *)
    CheckIfOverwriting[newID];
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


CreateUsageMessage[TChangeSymbol, "TChangeSymbol[`ID`, `symbol`] changes the symbol of the tensor object `ID` to `symbol`."];
TChangeSymbol[ID_String, symbol_String] := (
    CheckIfTensorExists[ID];
    ChangeTensorKey[ID, "Symbol", symbol];
    Return[ID];
);


CreateUsageMessage[TCheckForUpdates, "TCheckForUpdates[] checks the GitHub repository for new versions of this package. If a new version is available, the user will be given the option to download or install it."];
TCheckForUpdates[] := Module[
    {
        errorMessage,
        newVersion,
        remoteFile,
        toPrint,
        versionLookup
    },
    errorMessage = Row[{"Error: Failed to check for updates. Please visit ", Hyperlink["https://github.com/bshoshany/OGRe"], " to check manually."}];
    OGRePrint["Checking GitHub repository for updates..."];
    remoteFile = Quiet[Import[OGReURL, "Text"]];
    If[
        remoteFile === $Failed,
    (* Then *)
        OGRePrint[errorMessage];
        Abort[];
    ];
    versionLookup = StringCases[remoteFile, Shortest["OGReVersion = \"" ~~ __ ~~ "\";"]];
    If[
        Length[versionLookup] == 1,
    (* Then *)
        newVersion = StringTake[versionLookup[[1]], {16, StringLength[versionLookup[[1]]] - 2}];
        If[
            newVersion === OGReVersion,
        (* Then *)
            OGRePrint["You have the latest version of the package."],
        (* Else *)
            toPrint = {Row[{"A new version of the package is available: ", Style[newVersion, Bold]}]};
            AppendTo[toPrint, Row[{"\[Bullet] ", CreateButton[
                "Visit GitHub repository.",
                SystemOpen["https://github.com/bshoshany/OGRe"]
            ]}]];
            AppendTo[toPrint, Row[{"\[Bullet] ", CreateButton[
                "Reload new version directly from GitHub without downloading it.",
                Get[OGReURL]
            ]}]];
            (* If the notebook is an Untitled notebook, meaning it is not an actual file in the file system, then NotebookDirectory[] will return $Failed and issue the error message NotebookDirectory::nosv. Otherwise, show an option to download the notebook to the current notebook directory. *)
            Off[NotebookDirectory::nosv];
            If[
                NotebookDirectory[] =!= $Failed,
            (* Then *)
                AppendTo[toPrint, Row[{"\[Bullet] ", CreateButton[
                    "Download new version to " <> FileNameJoin[{NotebookDirectory[], "OGRe.m"}] <> " and reload the package.",
                    URLDownload[OGReURL, FileNameJoin[{NotebookDirectory[], "OGRe.m"}]]; OGRePrint["Downloaded! Reloading..."]; Get[FileNameJoin[{NotebookDirectory[], "OGRe.m"}]]
                ]}]];
            ];
            On[NotebookDirectory::nosv];
            AppendTo[toPrint, Row[{"\[Bullet] ", CreateButton[
                "Install new version to " <> FileNameJoin[{$UserBaseDirectory, "Applications", "OGRe.m"}] <> " and reload the package.",
                URLDownload[OGReURL, FileNameJoin[{$UserBaseDirectory, "Applications", "OGRe.m"}]]; OGRePrint["Installed! Reloading..."]; Get[FileNameJoin[{$UserBaseDirectory, "Applications", "OGRe.m"}]]
            ]}]];
            OGRePrint[Column[toPrint]];
        ],
    (* Else *)
        OGRePrint[errorMessage];
    ];
];


CreateUsageMessage[TCite, "TCite[] displays information on how to cite this package in published research. Thank you for citing my work! :)"];
TCite[] := CellPrint[{
    Cell[
        "If you use this package in published research, please cite it as follows:",
        "Text"
    ],
    Cell[
        "Shoshany, B., (2021). OGRe: An Object-Oriented General Relativity Package for Mathematica. Journal of Open Source Software, 6(65), 3416, https://doi.org/10.21105/joss.03416",
        "Item"
    ],
    Cell[
        "You can also use the following BibTeX entry:",
        "Text"
    ],
    Cell[
        "@article{Shoshany2021_OGRe,
    author    = {Barak Shoshany},
    doi       = {10.21105/joss.03416},
    journal   = {Journal of Open Source Software},
    number    = {65},
    pages     = {3416},
    publisher = {The Open Journal},
    title     = {OGRe: An Object-Oriented General Relativity Package for Mathematica},
    url       = {https://doi.org/10.21105/joss.03416},
    volume    = {6},
    year      = {2021}
}",
        "Program"
    ],
    Cell[
        "Thank you for citing my work! :)",
        "Text"
    ]
}];


CreateUsageMessage[TDelete, "TDelete[`ID`] permanently deletes the tensor object `ID`. If the tensor is a metric or coordinate system, it cannot be deleted unless all tensors referring to it have been deleted first."];
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


CreateUsageMessage[TDocs, "TDocs[] opens the Mathematica notebook OGRe_Documentation.nb from the GitHub repository, which contains the full documentation for the package."];
TDocs[] := (
    If[
        NotebookOpen["https://raw.githubusercontent.com/bshoshany/OGRe/master/OGRe_Documentation.nb"] === $Failed,
    (* Then *)
        OGRePrint[Row[{"Error: Failed to load the documentation. Please visit ", Hyperlink["https://github.com/bshoshany/OGRe"], " to download it manually."}]],
    (* Else *)
        OGRePrint["Successfully loaded the documentation from GitHub."];
    ]
);


CreateUsageMessage[TExport, "TExport[`ID`] exports the raw tensor data for the tensor object `ID` as an Association."];
TExport[ID_String] := (
    CheckIfTensorExists[ID];
    Return[Association[
        ID -> Append[TensorData[ID], "OGReVersion" -> OGReVersion]
    ]];
);


CreateUsageMessage[TExportAll, "TExportAll[] exports the raw tensor data for all tensors defined in the current session as an Association.
TExportAll[`filename`] exports the data to `filename`. If a full path is not given, the file will be created in the current working directory, as given by Directory[]. This directory can be changed using SetDirectory[]. Note that the file will be overwritten if it already exists."];
TExportAll[] := TensorData;
TExportAll[filename_String] := Module[
    {
        stream
    },
    stream = OpenWrite[filename];
    Write[stream, TensorData];
    Close[stream];
    OGRePrint["Exported all tensor data to ", filename, "."];
];


CreateUsageMessage[TGetComponents, "TGetComponents[`ID`, `indices`, `coordinatesID`] extracts the components of the tensor object `ID` with the index configuration `indices` and in the coordinate system `coordinatesID` as a list.
`indices` must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
If `indices` and/or `coordinatesID` are omitted, the default values are used, and a message will let the user know which representation the components are given in, to avoid confusion.
TGetComponents[`ID`, `indices`, `coordinatesID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are returned. Typically this would be ReplaceAll[rules] to apply the rules to the elements, but any function can be used."];
TGetComponents::UsingDefault = "Using `1`.";
TGetComponents[ID_String, indices_List : {"_UseDefault_"}, coordinatesID_String : "_UseDefault_", function_ : Identity] /; (!ListQ[function] && !StringQ[function]) := Module[
    {
        components,
        useCoords,
        useIndices,
        usingMessage = ""
    },
    (* Check that the tensor object ID exists. *)
    CheckIfTensorExists[ID];
    (* If specific indices are not given, use the tensor's default indices. *)
    If[
        indices === {"_UseDefault_"},
    (* Then *)
        useIndices = TensorData[ID]["DefaultIndices"];
        (* Make sure the user knows which representation they got. *)
        usingMessage = "the default index configuration " <> ToString[useIndices],
    (* Else *)
        (* Check that the list of indices is of the correct form and has the correct rank. *)
        CheckIndicesForm[indices];
        CheckIndicesRank[indices, ID];
        useIndices = indices;
    ];
    (* If a specific coordinate system is not given, use the tensor's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[ID]["DefaultCoords"];
        (* Make sure the user knows which representation they got. *)
        If[
            usingMessage =!= "",
        (* Then *)
            usingMessage = usingMessage <> " and "
        ];
        usingMessage = usingMessage <> "the default coordinate system \"" <> useCoords <> "\"",
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    If[
        usingMessage =!= "",
    (* Then *)
        Message[TGetComponents::UsingDefault, usingMessage];
    ];
    (* Get the components of the tensor in the desired representation. They will be calculated if the representation is not yet stored in the tensor object. *)
    components = AddRepresentation[ID, useIndices, useCoords];
    (* Apply the optional function to the components. *)
    If[
        function =!= Identity,
    (* Then *)
        components = Map[function, components, {ArrayDepth[components]}];
        (* Simplify the components after applying the function. *)
        components = TensorSimplify[components];
    ];
    Return[components];
];


CreateUsageMessage[TImport, "TImport[`data`] imports a tensor that has been exported using TExport[]."];
TImport[data_Association] := Module[
    {
        newID
    },
    newID = Keys[data][[1]];
    If[
        !KeyExistsQ[data[[1]], "OGReVersion"] || data[[1]]["OGReVersion"] != OGReVersion,
    (* Then *)
        OGRePrint["Warning: The imported tensor was created in a different version of OGRe. Compatibility issues may occur."];
    ];
    (* Check that the imported tensor object doesn't already exist. *)
    CheckIfOverwriting[newID];
    SetTensorID[newID, KeyDrop[Values[data][[1]], "OGReVersion"]];
    Return[newID];
];


CreateUsageMessage[TImportAll, "TImportAll[`source`] imports tensor data that has been exported using TExportAll[]. If `source` is an Association, imports the data directly. If `source` is a file name, imports the data from that file. If a full path is not given, the file is assumed to be located in the current working directory, as given by Directory[]. This directory can be changed using SetDirectory[].
`WARNING: This will irreversibly delete ALL of the tensors already defined in the current session.`"];
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
    OGRePrint["Imported all tensor data from ", filename, "."];
];


CreateUsageMessage[TInfo, "TInfo[] lists all the tensors created so far in this session: coordinate systems, metrics, and the tensors associated with each metric.
TInfo[`ID`] displays information about the tensor object `ID`, including its symbol, role, associated metric, and default coordinates and indices, in human-readable form.
If `ID` represents a coordinate system, displays a list of all tensors using it as their default coordinate system.
If `ID` represents a metric, displays a list of all tensors using it as their associated metric."];
TInfo[ID_String] := Module[
    {
        info
    },
    (* Check that the tensor object ID exists. *)
    CheckIfTensorExists[ID];
    (* Collect information about the object. *)
    info = {Row[{Style["ID: ", Bold], ID}]};
    AppendTo[info, Row[{Style["Symbol: ", Bold], TensorData[ID]["Symbol"]}]];
    AppendTo[info, Row[{Style["Role: ", Bold], TensorData[ID]["Role"]}]];
    If[TensorData[ID]["Role"] =!= "Coordinates" && TensorData[ID]["Role"] =!= "Metric", AppendTo[info, Row[{Style["Metric: ", Bold], InfoButton[TensorData[ID]["Metric"]]}]]];
    If[TensorData[ID]["Role"] =!= "Coordinates", AppendTo[info, Row[{Style["Default Coordinates: ", Bold], InfoButton[TensorData[ID]["DefaultCoords"]]}]]];
    AppendTo[info, Row[{Style["Default Indices: ", Bold], TensorData[ID]["DefaultIndices"]}]];
    (* If ID represents a coordinate system, display a list of all tensors using it as their default coordinate system. *)
    If[TensorData[ID]["Role"] === "Coordinates", AppendTo[info, Row[{Style["Default Coordinates For: ", Bold], Row[InfoButton /@ Select[Keys[TensorData], TensorData[#]["DefaultCoords"] === ID && # =!= ID &], ", "]}]]];
    (* If ID represents a metric, display a list of all tensors using it as their associated metric. *)
    If[TensorData[ID]["Role"] === "Metric", AppendTo[info, Row[{Style["Tensors Using This Metric: ", Bold], Row[InfoButton /@ Select[Keys[TensorData], TensorData[#]["Metric"] === ID && # =!= ID &], ", "]}]]];
    (* Provide links to print out the components of the tensor using TList or TShow. *)
    AppendTo[info, Row[{
        Style["Components: ", Bold],
        CreateButton["TList", TList[ID]],
        " | ",
        CreateButton["TShow", TShow[ID]]
    }]];
    (* Print out the collected information. *)
    OGRePrint[Column[info]];
];
TInfo[] := Module[
    {
        tensorList, countCoord = 1, countMetric = 1
    },
    (* List all tensors except for Options (which is not actually a tensor) and DefaultResultID. *)
    tensorList = Sort[DeleteCases[Keys[TensorData], Options | DefaultResultID]];
    (* Print a list of the coordinate systems, metrics, and the tensors associated with each metric. *)
    OGRePrint[Column[{
        Row[{Style["Total tensors created: ", Bold], Length[tensorList]}],
        Style["Coordinate Systems:", Bold],
        Column[Table[Row[{countCoord++, ". ", ID}], {ID, InfoButton /@ Select[tensorList, TensorData[#]["Role"] == "Coordinates" &]}]],
        Style["Metrics:", Bold],
        Column[Table[Row[{countMetric++, ". ", Style[InfoButton[ID], Bold], " \[RightArrow] ", Row[InfoButton /@ Select[Keys[TensorData], TensorData[#]["Metric"] === ID && # =!= ID && # =!= DefaultResultID &], "|"]}], {ID, Select[tensorList, TensorData[#]["Role"] == "Metric" &]}]]
    }]];
];
(* Create a link that will execute TInfo for a specific tensor when clicked. *)
InfoButton[ID_String] := CreateButton[ID, TInfo[ID]];


CreateUsageMessage[TLineElement, "TLineElement[`metricID`] displays the line element of the metric `metricID` in its default coordinate system.
TLineElement[`metricID`, `coordinatesID`] displays the line element in the coordinate system `coordinatesID`."];
TLineElement::ErrorNotMetric = "Only a metric can be displayed as a line element.";
TLineElement[ID_String, coordinatesID_String : "_UseDefault_"] := Module[
    {
        components,
        coordSymbols,
        dim,
        useCoords
    },
    (* Check that ID is indeed a metric. *)
    If[
        TensorData[ID]["Role"] =!= "Metric",
    (* Then *)
        Message[TLineElement::ErrorNotMetric];
        Abort[];
    ];
    (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[ID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    (* Get the coordinate symbols to be used in the line element. *)
    coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* The dimension is the number of coordinates. *)
    dim = Length[coordSymbols];
    (* Get the metric's components in the desired representation, adding it if it does not already exist. *)
    components = AddRepresentation[ID, {-1, -1}, useCoords];
    (* Return the line element. *)
    Return[TensorSimplify[Sum[components[[m, n]] * Symbol["\[DoubleStruckD]"<>ToString[coordSymbols[[m]]]] * Symbol["\[DoubleStruckD]"<>ToString[coordSymbols[[n]]]], {m, 1, dim}, {n, 1, dim}]]];
];


CreateUsageMessage[TList, "TList[`ID`] lists the unique, non-zero components of the tensor object `ID` in its default index configuration and coordinate system.
TList[`ID`, `indices`] lists the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TList[`ID`, `coordinatesID`] lists the components in the coordinate system `coordinatesID`.
TList[`ID`, `indices`, `coordinatesID`] lists the components in the index configuration `indices` and the coordinate system `coordinatesID`.
TList[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are displayed. Typically this would be ReplaceAll[rules] to apply the rules to the elements, but any function can be used.
TList[`ID`, `indices`, `coordinatesID`, `function`] does all of the above; either `indices` or `coordinatesID` can be omitted."];
TList[ID_String, indices_List : {"_UseDefault_"}, coordinatesID_String : "_UseDefault_", function_ : Identity] /; (!ListQ[function] && !StringQ[function]) := ShowList[ID, indices, coordinatesID, "List", function];


CreateUsageMessage[TMessage, "TMessage is a placeholder symbol to which messages not associated with any specific OGRe module are attached."];


CreateUsageMessage[TNewCoordinates, "TNewCoordinates[`coordinatesID`, `symbols`], creates a new tensor object representing a coordinate system.
`coordinatesID` is a string that will be used to identify the new object, and must be unique.
`symbols` are the coordinate symbols, e.g. {t, x, y, z}. They will automatically be cleared and protected against future changes using TSetReservedSymbols[]."];
TNewCoordinates::ErrorEmptyList = "The coordinate symbols cannot be an empty list. At least one coordinate symbol must be specified.";
TNewCoordinates[coordinatesID_String, coordinates_List?VectorQ] := (
    (* Check that the target tensor object doesn't already exist. *)
    CheckIfOverwriting[coordinatesID];
    (* Check that the coordinates are not an empty list. *)
    If[coordinates == {}, Message[TNewCoordinates::ErrorEmptyList]; Abort[]];
    (* Clear any definitions previously used for the coordinate symbols and protect them against future changes. *)
    TSetReservedSymbols[Unevaluated[coordinates]];
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


CreateUsageMessage[TNewMetric, "TNewMetric[`metricID`, `coordinatesID`, `components`, `symbol`} creates a new tensor object representing a metric.
`metricID` is a string that will be used to identify the new object, and must be unique.
`coordinatesID` is the unique ID of a tensor object representing a coordinate system, created using TNewCoordinates[].
`components` is a square, symmetric, and invertible matrix representing the metric with two lower indices in that coordinate system.
`symbol` will be used to represent the metric in formulas. If not given, \"g\" will be used."];
TNewMetric::ErrorIncorrectDim = "The metric components must have the same dimension as the coordinates.";
TNewMetric::ErrorNotInvertible = "The metric must be invertible.";
TNewMetric::ErrorNotSymmetric = "The metric must be symmetric.";
TNewMetric::ErrorNotSquare = "The components of the metric must be a square matrix.";
TNewMetric::WarningOverwrite = "All curvature tensors previously calculated from the metric being overwritten will be deleted.";
TNewMetric[metricID_String, coordinatesID_String, components_List, symbol_String : "g"] := Module[
    {
        dim = Length[components],
        inverse,
        overwriting = False,
        roles,
        simplified
    },
    (* Check that the target tensor object doesn't already exist. *)
    CheckIfOverwriting[metricID];
    If[KeyExistsQ[TensorData, metricID], overwriting = True];
    (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
    CheckIfTensorExists[coordinatesID];
    CheckIfCoordinates[coordinatesID];
    (* Simplify the components. *)
    simplified = TensorSimplify[components];
    (* Check that the matrix is square. *)
    If[
        !SquareMatrixQ[simplified],
    (* Then *)
        Message[TNewMetric::ErrorNotSquare];
        Abort[];
    ];
    (* Check that the metric components have the same dimension as the coordinates. *)
    If[
        dim != Length[TensorData[coordinatesID]["Components"][{{1}, coordinatesID}]],
    (* Then *)
        Message[TNewMetric::ErrorIncorrectDim];
        Abort[];
    ];
    (* Check that the matrix is symmetric. *)
    If[
        !SymmetricMatrixQ[simplified],
    (* Then *)
        Message[TNewMetric::ErrorNotSymmetric];
        Abort[];
    ];
    (* Invert the components, and return an error if the matrix is singular or cannot be inverted for any reason. *)
    inverse = Quiet[Check[Inverse[simplified], "Error"]];
    If[
        inverse === "Error",
    (* Then *)
        Message[TNewMetric::ErrorNotInvertible];
        Abort[];
    ];
    inverse = TensorSimplify[inverse];
    (* Create a new tensor object for the metric with the desired ID. The components of the matrix in every possible index configuration will be calculated in advance in the default coordinate system, to improve performance. *)
    SetTensorID[metricID, Association[
        "Components" -> Association[
            {{-1, -1}, coordinatesID} -> simplified,
            {{+1, +1}, coordinatesID} -> inverse,
            {{+1, -1}, coordinatesID} -> IdentityMatrix[dim],
            {{-1, +1}, coordinatesID} -> IdentityMatrix[dim]
        ],
        "DefaultCoords" -> coordinatesID,
        "DefaultIndices" -> {-1, -1},
        "Metric" -> metricID,
        "Role" -> "Metric",
        "Symbol" -> symbol
    ]];
    (* If we are overwriting an existing metric, make sure to delete any curvature tensors that were calculated from the old metric, since they will not have the correct components. *)
    If[
        overwriting,
    (* Then *)
        Message[TNewMetric::WarningOverwrite];
        roles = {"Christoffel", "Einstein", "GeodesicFromChristoffel", "GeodesicFromLagrangian", "Lagrangian", "Ricci Scalar", "Ricci Tensor", "Riemann"};
        Scan[
            If[
                TensorData[#]["Metric"] === metricID && MemberQ[roles, TensorData[#]["Role"]],
            (* Then *)
                RemoveTensorID[#];
            ] &,
            Keys[TensorData]
        ];
    ];
    Return[metricID];
];


CreateUsageMessage[TNewTensor, "TNewTensor[`tensorID`, `metricID`, `coordinatesID`, `indices`, `components`, `symbol`] creates a new tensor object.
`tensorID` is a string that will be used to identify the new object, and must be unique.
`metricID` is the unique ID of a tensor object representing a metric, created using TNewMetric[]. The metric will be used to raise and lower indices for the new tensor.
`coordinatesID` is the unique ID of a tensor object representing a coordinate system, created using TNewCoordinates[]. This coordinate system will be used to specify the components of the new tensor. If omitted, the default coordinate system of the metric `metricID` will be used.
`indices` must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
`components` is a list specifying the representation of the tensor with the index configuration `indices` and in the coordinate system `coordinatesID`.
`symbol` will be used to represent the tensor in formulas. If not given, the placeholder " <> DefaultSymbol <> " will be used."];
TNewTensor::ErrorDimension = "The components must have the same dimension as the coordinate system.";
TNewTensor::ErrorRank = "The number of indices must match the rank of the components.";
TNewTensor[tensorID_String, metricID_String, coordinatesID_String : "_UseDefault_", indices_List, components_List, symbol_String : DefaultSymbol] := Module[
    {
        useCoords
    },
    (* Check that the target tensor object doesn't already exist. *)
    CheckIfOverwriting[tensorID];
    (* Check that the tensor object metricID exists and represents a metric. *)
    CheckIfTensorExists[metricID];
    CheckIfMetric[metricID];
    (* Check that the list of indices is of the correct form. *)
    CheckIndicesForm[indices];
    (* Determine which coordinate system the components for the new tensor are given in. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[metricID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID
    ];
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
        "Role" -> "Tensor",
        "Symbol" -> symbol
    ]];
    Return[tensorID];
];


CreateUsageMessage[TSetAllowOverwrite, "TSetAllowOverwrite[`True`] allows overwriting tensors. If the user creates a new tensor with the same ID as an existing tensor, the latter will be overwritten. Note that this can result in loss of data. TSetAllowOverwrite[`False`] disallows overwriting, which is the default setting. TSetAllowOverwrite[] returns the current setting. Note that this setting is persistent between sessions."];
TSetAllowOverwrite::Notify = "Overwriting tensors turned `1`.";
TSetAllowOverwrite[bool_?BooleanQ] := (
    Unprotect[OGReGlobalOptions];
    OGReGlobalOptions["AllowOverwrite"] = bool;
    Message[TSetAllowOverwrite::Notify, If[bool, "on", "off"]];
    LocalSymbol["OGReGlobalOptions"] = OGReGlobalOptions;
    Protect[OGReGlobalOptions];
);
TSetAllowOverwrite[] := OGReGlobalOptions["AllowOverwrite"];


CreateUsageMessage[TSetAssumptions, "TSetAssumptions[] shows the assumptions to be used when simplifying expressions.
TSetAssumptions[`assumptions`] appends new assumptions to the previously added assumptions.
TSetAssumptions[None] clears all previously added assumptions.
TSetAssumptions[!Reals] disables the default assumption that all variable are real, which secretly adds the assumption Element[_, Reals] to the list of assumptions. TSetAssumptions[Reals] re-enables this assumption.
The output of this module is always an Association indicating whether variables are assumed to be real and listing the user-defined assumptions."];
TSetAssumptions[] := TensorData[Options]["SimplifyAssumptions"];
TSetAssumptions[assumptions_] := (
    Unprotect[TensorData];
    Switch[
        assumptions,
        None, TensorData[Options]["SimplifyAssumptions"]["User"] = None,
        Reals, TensorData[Options]["SimplifyAssumptions"]["AssumeReal"] = True,
        !Reals, TensorData[Options]["SimplifyAssumptions"]["AssumeReal"] = False,
        _, If[
            TensorData[Options]["SimplifyAssumptions"]["User"] === None,
        (* Then *)
            TensorData[Options]["SimplifyAssumptions"]["User"] = {assumptions},
        (* Else *)
            If[
                !MemberQ[TensorData[Options]["SimplifyAssumptions"]["User"], assumptions],
            (* Then *)
                AppendTo[TensorData[Options]["SimplifyAssumptions"]["User"], assumptions];
            ];
        ];
    ];
    Protect[TensorData];
    Return[TensorData[Options]["SimplifyAssumptions"]];
);
Attributes[TSetAssumptions] = HoldAll;


CreateUsageMessage[TSetAutoUpdates, "TSetAutoUpdates[`False`] turns off automatic checks for updates at startup. TSetAutoUpdates[`True`] turns them back on, which is the default setting. TSetAutoUpdates[] returns the current setting. Note that this setting is persistent between sessions."];
TSetAutoUpdates[bool_?BooleanQ] := (
    Unprotect[OGReGlobalOptions];
    OGReGlobalOptions["AutoUpdates"] = bool;
    OGRePrint["Auto updates turned ", If[bool, "on", "off"], "."];
    LocalSymbol["OGReGlobalOptions"] = OGReGlobalOptions;
    Protect[OGReGlobalOptions];
);
TSetAutoUpdates[] := OGReGlobalOptions["AutoUpdates"];


DefaultCurveParameter = "Global`\[Lambda]";
ReplaceCurveParameter[ID_String, oldParameter_Symbol, newParameter_Symbol] := (
    Scan[
        (TensorData[ID]["Components"][#] = ReplaceAll[TensorData[ID]["Components"][#], f_[oldParameter] -> f[newParameter]]) &,
        Keys[TensorData[ID]["Components"]]
    ]
);
CreateUsageMessage[TSetCurveParameter, "TSetCurveParameter[] shows the curve parameter used for calculating Lagrangians and geodesics.
TSetCurveParameter[`parameter`] changes the curve parameter. The new parameter will be cleared and protected, and the old parameter will be unprotected. Any tensors currently using the old parameter will be modified to use the new parameter. `parameter` can be given either as a symbol name or a string representing a symbol name.
TSetCurveParameter[Automatic] resets the curve parameter to the default: " <> StringReplace[DefaultCurveParameter, "Global`" -> ""] <> "."];
TSetCurveParameter[] := StringReplace[TensorData[Options]["CurveParameter"], "Global`" -> ""];
TSetCurveParameter[newParameter_String] := Module[
    {
        oldParameter = TensorData[Options]["CurveParameter"]
    },
    (* Unprotect the old parameter. *)
    Unprotect[Evaluate[oldParameter]];
    (* Clear and protect the new parameter. *)
    Unprotect[Evaluate[newParameter]];
    ClearAll[Evaluate[newParameter]];
    Protect[Evaluate[newParameter]];
    (* Store the new parameter in the Options key. We store it as a string because strings are simpler to handle if the desired parameter already has a value. We store the Global` context explicitly because otherwise it would be evaluated in the OGRe`Private` context at startup. *)
    Unprotect[TensorData];
    TensorData[Options]["CurveParameter"] = "Global`" <> StringReplace[newParameter, "Global`" -> ""];
    (* Ensure that the old parameter is replaced with the new one in any previously made calculations. *)
    Scan[
        ReplaceCurveParameter[#, Symbol[oldParameter], Symbol[newParameter]] &,
        Keys[TensorData]
    ];
    Protect[TensorData];
    Return[newParameter];
];
TSetCurveParameter[parameter_Symbol] := TSetCurveParameter[Evaluate[ToString[Unevaluated[parameter]]]];
TSetCurveParameter[Automatic] := TSetCurveParameter[Evaluate[DefaultCurveParameter]];
Attributes[TSetCurveParameter] = HoldAll;


DefaultIndexLetters = "\[Mu]\[Nu]\[Rho]\[Sigma]\[Kappa]\[Lambda]\[Alpha]\[Beta]\[Gamma]\[Delta]\[CurlyEpsilon]\[Zeta]\[Epsilon]\[Theta]\[Iota]\[Xi]\[Pi]\[Tau]\[Phi]\[Chi]\[Psi]\[Omega]";
CreateUsageMessage[TSetIndexLetters, "TSetIndexLetters[] shows the index letters used when displaying indices.
TSetIndexLetters[`letters`] changes the index letters.
TSetIndexLetters[Automatic] resets the index letters to the default: \"" <> DefaultIndexLetters <> "\"."];
TSetIndexLetters[] := TensorData[Options]["IndexLetters"];
TSetIndexLetters[letters_String] := (
    Unprotect[TensorData];
    TensorData[Options]["IndexLetters"] = letters;
    Protect[TensorData];
    Return[letters];
);
TSetIndexLetters[Automatic] := TSetIndexLetters[DefaultIndexLetters];


CreateUsageMessage[TSetParallelization, "TSetParallelization[`True`] enables the parallelization of tensor simplifications, and TSetParallelization[`False`] disables it. The default value is `False`. TSetParallelization[] returns the current value. If simplifications take less than a few seconds, then you should leave parallelization off, as it has a small overhead and may actually impede performance. However, if simplifications are taking more than a few seconds, then it is highly recommended to enable parallelization for a significant performance boost."];
TSetParallelization[par_?BooleanQ] := (
    Unprotect[TensorData];
    TensorData[Options]["Parallelize"] = par;
    Protect[TensorData];
    If[
        par,
    (* Then *)
        OGRePrint["Parallelization enabled."];
        (* Launch all of the kernels for parallelization if they have not been launched yet, or if fewer than the maximum available number of kernels have been launched. Better do it now than cause a delay later. *)
        If[
            $KernelCount < $MaxLicenseSubprocesses,
        (* Then *)
            LaunchKernels[$MaxLicenseSubprocesses - $KernelCount];
            OGRePrint[$KernelCount, " parallel kernels launched. CPU has ", $ProcessorCount, " cores."];
        ],
    (* Else *)
        OGRePrint["Parallelization disabled."];
        OGRePrint["Closing ", $KernelCount, " kernels."];
        CloseKernels[];
    ];
);
TSetParallelization[] := TensorData[Options]["Parallelize"];


CreateUsageMessage[TSetReservedSymbols, "TSetReservedSymbols[`symbol`] clears any definitions previously used for `symbol` and protects it against future changes. `symbol` can be either a symbol name or a string representing a symbol name. After completion, the module will return a list of the currently reserved symbols.
Useful for making sure coordinate variables, parameters, and abstract functions used in tensors remain abstract symbols and do not accidentally obtain values and break the code.
If the reserved symbol is a function, this function will be displayed without arguments when using TList[] and TShow[], for improved readability. The reserved symbols will be exported when using TExportAll[] so they can later be imported using TImportAll[].
TSetReservedSymbols[{`symbol1`, `symbol2`, ...}] reserves all of the given symbols. Each of the symbols can be either a symbol name or a string representing a symbol name.
TSetReservedSymbols[] returns the currently reserved symbols."];
TSetReservedSymbols::ErrorNotGlobal = "Cannot reserve \"`1`\", as it is in the context `2`. Only symbols in the context Global`.` can be reserved.";
TSetReservedSymbols::ErrorNotSymbol = "Cannot reserve \"`1`\", as it is not a symbol.";
TSetReservedSymbols[symbol_Symbol] := (
    If[
        Context[symbol] =!= "Global`",
    (* Then *)
        Message[TSetReservedSymbols::ErrorNotGlobal, Unevaluated[symbol], Context[Unevaluated[symbol]]],
    (* Else *)
        Unprotect[Unevaluated[symbol]];
        ClearAll[Unevaluated[symbol]];
        Protect[Unevaluated[symbol]];
        Unprotect[TensorData];
        AppendTo[TensorData[Options]["ReservedSymbols"], ToString[symbol]];
        TensorData[Options]["ReservedSymbols"] = DeleteDuplicates[TensorData[Options]["ReservedSymbols"]];
        Protect[TensorData];
        Return[TensorData[Options]["ReservedSymbols"]];
    ];
);
TSetReservedSymbols[symbol_String] := (
    Unprotect[symbol];
    ClearAll[symbol];
    Quiet[Check[
        Protect @@ ToExpression[symbol, StandardForm, Hold],
        Message[TSetReservedSymbols::ErrorNotSymbol, symbol];
        Abort[],
        Protect::pssl
    ], Protect::pssl];
    Unprotect[TensorData];
    AppendTo[TensorData[Options]["ReservedSymbols"], symbol];
    TensorData[Options]["ReservedSymbols"] = DeleteDuplicates[TensorData[Options]["ReservedSymbols"]];
    Protect[TensorData];
    Return[TensorData[Options]["ReservedSymbols"]];
);
TSetReservedSymbols[symbols_List] := (
    Scan[TSetReservedSymbols, Unevaluated[symbols]];
    Return[TensorData[Options]["ReservedSymbols"]];
)
TSetReservedSymbols[] := TensorData[Options]["ReservedSymbols"];
Attributes[TSetReservedSymbols] = HoldAll;


CreateUsageMessage[TShow, "TShow[`ID`] shows the components of the tensor object `ID` in its default index configuration and coordinate system.
TShow[`ID`, `indices`] shows the components in the index configuration `indices`, which should be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.
TShow[`ID`, `coordinatesID`] shows the components in the coordinate system `coordinatesID`.
TShow[`ID`, `indices`, `coordinatesID`] shows the components in the index configuration `indices` and the coordinate system `coordinatesID`.
TShow[`ID`, `function`] maps `function` to each of the tensor's elements, and then automatically simplifies them, before they are displayed. Typically this would be ReplaceAll[rules] to apply the rules to the elements, but any function can be used.
TShow[`ID`, `indices`, `coordinatesID`, `function`] does all of the above; either `indices` or `coordinatesID` can be omitted."];
TShow[ID_String, indices_List : {"_UseDefault_"}, coordinatesID_String : "_UseDefault_", function_ : Identity] /; (!ListQ[function] && !StringQ[function]) := ShowList[ID, indices, coordinatesID, "Show", function];


CreateUsageMessage[TSimplify, "TSimplify[`ID`] simplifies all previously-calculated representations of the tensor object `ID` based on the user-defined simplification assumptions set using TSetAssumptions[]. To be used if the assumptions have changed after the components have already been calculated.
TSimplify[`expression`] simplifies `expression` based on the user-defined simplification assumptions. If `expression` is a list, the components will be simplified in parallel.
"];
TSimplify[ID_String] := (
    CheckIfTensorExists[ID];
    ChangeTensorKey[ID, "Components", TensorSimplify /@ TensorData[ID]["Components"]];
    Return[ID];
);
TSimplify[expression_] := TensorSimplify[expression];


CreateUsageMessage[TVolumeElementSquared, "TVolumeElement[`metricID`, `coordinatesID`] returns the determinant of the metric `metricID` in the coordinate system `coordinatesID`. If `coordinatesID` is not specified, the default coordinate system of the metric will be used. The square root of the determinant (or its negative, for a pseudo-Riemannian metric) is the volume element."];
TVolumeElementSquared::ErrorNotMetric = "The volume element squared can only be calculated from a metric.";
TVolumeElementSquared[ID_String, coordinatesID_String : "_UseDefault_"] := Module[
    {
        useCoords
    },
    (* Check that ID is indeed a metric. *)
    If[
        TensorData[ID]["Role"] =!= "Metric",
    (* Then *)
        Message[TLineElement::ErrorNotMetric];
        Abort[];
    ];
    (* If a specific coordinate system is not given, use the metric's default coordinate system. *)
    If[
        coordinatesID === "_UseDefault_",
    (* Then *)
        useCoords = TensorData[ID]["DefaultCoords"],
    (* Else *)
        (* Check that the tensor object coordinatesID exists and represents a coordinate system. *)
        CheckIfTensorExists[coordinatesID];
        CheckIfCoordinates[coordinatesID];
        useCoords = coordinatesID;
    ];
    (* Return the volume element squared *)
    Return[TensorSimplify[Det[TensorData[ID]["Components"][{{-1, -1}, useCoords}]]]];
];


(* =================================================== *)
(* Private modules (for internal use only) start here. *)
(* =================================================== *)


(* Add a new representation with a specific index configuration and coordinate system to a tensor object, if it does not already exist. Returns the components of the representation in any case. *)
AddRepresentation::ErrorCoordinatesCoord = "Cannot transform coordinates for a tensor object representing a coordinate system.";
AddRepresentation::ErrorCoordinatesIndex = "Cannot lower index for a tensor object representing a coordinate system.";
AddRepresentation[ID_String, indices_List, coordinatesID_String] := Module[
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
    (* Transform the tensor to different coordinates if required. We do this first because the Christoffel symbols have a special transformation rule, so we first transform the coordinates using a fixed rule that works only for the default {1, -1, -1} index configuration, and then raise or lower indices if necessary. *)
    TransformCoordinates[ID, defIndices, defCoords, coordinatesID];
    (* Raise or lower indices if required, one by one. We only need to do this if the tensor is not a metric, since metric tensors already have their components pre-calculated with all possible index configurations. *)
    If[
        TensorData[ID]["Role"] =!= "Metric",
    (* Then *)
        oldIndices = defIndices;
        Do[
            oldIndices = RaiseLower[ID, coordinatesID, oldIndices, i, indices[[i]]],
            {i, 1, Length[defIndices]}
        ];
    ];
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
        newSymbol,
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
    (* Calculate the explicit symbolic representation for this operation, including the correct index placement. *)
    newSymbol = Row[{
        If[
            TensorData[firstID]["Role"] === "Temporary",
        (* Then *)
            TensorData[firstID]["Symbol"],
        (* Else *)
            Subsuperscript[
                TensorData[firstID]["Symbol"],
                IndicesToLetters[-1, useIndices, firstIndices],
                IndicesToLetters[+1, useIndices, firstIndices]
            ]
        ],
        " + ",
        If[
            TensorData[secondID]["Role"] === "Temporary",
        (* Then *)
            TensorData[secondID]["Symbol"],
        (* Else *)
            Subsuperscript[
                TensorData[secondID]["Symbol"],
                IndicesToLetters[-1, useSecondIndices, secondIndices],
                IndicesToLetters[+1, useSecondIndices, secondIndices]
            ]
        ]
    }];
    (* Let the user know the explicit operation we are performing, including the correct index placement. TODO: Uncomment this in a future version, once this feature works properly. *)
    (* PrintTemporary["Adding ", newSymbol, "..."]; *)
    (* Add the two tensors by summing over their components using the appropriate variables. *)
    sumComponents = Table[
        firstComponents[[Sequence @@ firstVars]] + secondComponents[[Sequence @@ secondVars]],
        Evaluate[Sequence @@ ({#, 1, Length[firstComponents]} & /@ firstVars)]
    ];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{useIndices, useCoords} -> sumComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> useIndices,
        "Metric" -> TensorData[firstID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> Row[{"(", newSymbol, ")"}]
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


(* Check if a tensor with the given ID exists. If overwriting is allowed, just print a warning. If it's not allowed, print an error message and abort. *)
TMessage::ErrorOverwrite = "A tensor with the ID \"`1`\" already exists. Please rename it using TChangeID[] or delete it using TDelete[] first. Type TSetAllowOverwrite[True] to allow overwriting tensors.";
TMessage::WarningOverwrite = "Overwriting the tensor \"`1`\"."
CheckIfOverwriting[ID_String] :=
If[
    KeyExistsQ[TensorData, ID],
(* Then *)
    If[
        !OGReGlobalOptions["AllowOverwrite"],
    (* Then *)
        Message[TMessage::ErrorOverwrite, ID];
        Abort[],
    (* Else *)
        Message[TMessage::WarningOverwrite, ID];
    ];
];

(* Check if a tensor with the given ID exists, and abort if it doesn't. *)
TMessage::ErrorDoesNotExist = "The tensor \"`1`\" does not exist.";
CheckIfTensorExists[ID_String] := If[
    !KeyExistsQ[TensorData, ID],
(* Then *)
    Message[TMessage::ErrorDoesNotExist, ID];
    Abort[];
];


(* Check that an index list has been entered correctly: a one-dimensional list with all its components either plus or minus 1. *)
TMessage::ErrorIndexForm = "The indices must be a list of the form {\[PlusMinus]1, \[PlusMinus]1, ...}, where +1 corresponds to an upper index and -1 corresponds to a lower index.";
CheckIndicesForm[indices_List] := If[
    !VectorQ[indices] || !AllTrue[indices, (#^2 == 1) &],
(* Then *)
    Message[TMessage::ErrorIndexForm];
    Abort[];
];


(* Check that an index list matches the tensor's rank. *)
TMessage::ErrorIndexRank = "The index configuration `1` does not match the rank of the tensor \"`2`\". The number of indices should be `3`.";
CheckIndicesRank[indices_List, ID_String] := If[
    Length[indices] != Length[TensorData[ID]["DefaultIndices"]],
(* Then *)
    Message[TMessage::ErrorIndexRank, indices, ID, Length[TensorData[ID]["DefaultIndices"]]];
    Abort[];
];
CheckIndicesRank[indices_String, ID_String] := If[
    StringLength[indices] != Length[TensorData[ID]["DefaultIndices"]],
(* Then *)
    Message[TMessage::ErrorIndexRank, "\"" <> indices <> "\"", ID, Length[TensorData[ID]["DefaultIndices"]]];
    Abort[];
];


(* Clear all temporary tensors created by TCalc, and reset the counter. *)
ClearTemp[] := (
    ImportTensorData[Select[TensorData, #["Role"] =!= "Temporary" &]];
    Unprotect[TempID];
    TempID = 0;
    Protect[TempID];
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
        newSymbol,
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
        Return[TensorByScalar[secondID[secondIndices], TensorData[firstID]["Components"][{useFirstIndices, useCoords}][[1]], TensorData[firstID]["Symbol"]]]
    ];
    If[
        Length[useSecondIndices] == 0,
        (* Then *)
        Return[TensorByScalar[firstID[firstIndices], TensorData[secondID]["Components"][{useSecondIndices, useCoords}][[1]], TensorData[secondID]["Symbol"]]]
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
    (* Calculate the explicit symbolic representation for this operation, including the correct index placement. *)
    newSymbol = Row[{
        If[
            TensorData[firstID]["Role"] === "Temporary",
        (* Then *)
            TensorData[firstID]["Symbol"],
        (* Else *)
            Subsuperscript[
                TensorData[firstID]["Symbol"],
                IndicesToLetters[-1, useFirstIndices, firstIndices],
                IndicesToLetters[+1, useFirstIndices, firstIndices]
            ]
        ],
        If[
            TensorData[secondID]["Role"] === "Temporary",
        (* Then *)
            TensorData[secondID]["Symbol"],
        (* Else *)
            Subsuperscript[
                TensorData[secondID]["Symbol"],
                IndicesToLetters[-1, useSecondIndices, secondIndices],
                IndicesToLetters[+1, useSecondIndices, secondIndices]
            ]
        ]
    }];
    (* Let the user know the explicit operation we are performing, including the correct index placement. TODO: Uncomment this in a future version, once this feature works properly. *)
    (* PrintTemporary["Contracting ", newSymbol, "..."]; *)
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
        "Components" -> Association[{newIndices, useCoords} -> newComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> newIndices,
        "Metric" -> TensorData[firstID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> newSymbol
    ]];
    Return[newID[outIndices]];
];


(* Take the covariant derivative of a tensor. *)
CreateUsageMessage[TCovariantD, "TCovariantD[`index`] represents the covariant derivative when used in a tensor expression given to TCalc[]. If a tensor with ID \"`metricID`Christoffel\" exists, where `metricID` is the metric associated with the tensor the derivative is acting upon, then it will be assumed to be the Levi-Civita connection of the metric, and will be used in the calculation. Otherwise, \"`metricID`Christoffel\" will be created using TCalcChristoffel[]."];
CovariantDivOrGrad[derivativeIndex_String, tensorID_String[tensorIndices_String]] := Module[
    {
        myChristoffel,
        newComponents,
        newID,
        out,
        useCoords,
        useIndices
    },
    (* If the tensor is a scalar, then the covariant derivative reduces to the partial derivative. *)
    If[
        TensorData[tensorID]["DefaultIndices"] === {},
    (* Then *)
        Return[DivOrGrad[derivativeIndex, tensorID[tensorIndices]]];
    ];
    (* If the Christoffel symbols were not already calculated, calculate them now. *)
    myChristoffel = TensorData[tensorID]["Metric"] <> "Christoffel";
    If[
        !KeyExistsQ[TensorData, myChristoffel],
    (* Then *)
        TCalcChristoffel[TensorData[tensorID]["Metric"]];
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
        "Role" -> "CovariantDivOrGrad",
        "Symbol" -> TensorData[tensorID]["Symbol"]
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
    (* Delete the temporary tensor we created above. *)
    RemoveTensorID[newID];
    Return[out];
];


(* Create a clickable button that looks and behaves like a hyperlink. *)
CreateButton[label_, action_] := Button[
    MouseAppearance[Mouseover[
        Style[label, "Hyperlink"],
        Style[label, "HyperlinkActive"]
    ], "LinkHand"],
    action,
    Appearance -> "Frameless",
    BaseStyle -> "Hyperlink"
];
Attributes[CreateButton] = HoldRest;


(* Take the divergence or gradient of a tensor. *)
CreateUsageMessage[TPartialD, "TPartialD[`index`] represents the partial derivative when used in a tensor expression given to TCalc[]."];
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
        newSymbol,
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
    (* Check that the derivative has exactly one index. *)
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
    (* Calculate the explicit symbolic representation for this operation, including the correct index placement. *)
    newSymbol = Row[{
        Subscript["\[PartialD]", derivativeIndex],
        If[
            TensorData[tensorID]["Role"] === "Temporary",
        (* Then *)
            TensorData[tensorID]["Symbol"],
        (* Else *)
            Subsuperscript[
                TensorData[tensorID]["Symbol"],
                IndicesToLetters[-1, useIndices, tensorIndices],
                IndicesToLetters[+1, useIndices, tensorIndices]
            ]
        ]
    }];
    (* Let the user know the explicit operation we are performing, including the correct index placement. TODO: Uncomment this in a future version, once this feature works properly. *)
    (* PrintTemporary["Taking the partial derivative ", newSymbol, "..."]; *)
    (* Calculate the components of the new tensor by summing over the contracted variables and taking the derivatives of the tensor's components. *)
    (* If the tensor is a scalar, then the components will be a list with 1 item. This will then lead to the resulting vector being a list of lists. Replacing the components with the single component itself solves that issue. *)
    If[
        TensorData[tensorID]["DefaultIndices"] === {},
    (* Then *)
        components = components[[1]];
    ];
    newComponents = Table[
        If[
            Length[sumVars] > 0,
        (* Then *)
            Sum[
                D[components[[Sequence @@ secondVars]], coordinateSymbols[[firstVars[[1]]]]],
                Evaluate[Sequence @@ ({#, 1, Length[coordinateSymbols]} & /@ sumVars)]
            ],
        (* Else *)
            D[components[[Sequence @@ secondVars]], coordinateSymbols[[firstVars[[1]]]]]
        ],
        Evaluate[
            If[
                Length[outVars] > 0,
            (* Then *)
                Sequence @@ ({#, 1, Length[coordinateSymbols]} & /@ outVars),
            (* Else *)
                1
            ]
        ]
    ];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{newIndices, useCoords} -> newComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> newIndices,
        "Metric" -> TensorData[tensorID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> newSymbol
    ]];
    Return[newID[outIndices]];
];


(* Replace TensorData with the given Association. All previously defined tensor objects will be erased. The Options key will be created if it does not already exist. *)
ImportTensorData[data_Association] := (
    If[
        !KeyExistsQ[data, Options] || !KeyExistsQ[data[Options], "OGReVersion"] || data[Options]["OGReVersion"] != OGReVersion,
    (* Then *)
        OGRePrint["Warning: The imported tensors were created in a different version of OGRe. Compatibility issues may occur."];
    ];
    Unprotect[TensorData];
    TensorData = data;
    PopulateOptions[];
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


(* Print an expression in an uneditable cell with the label OGRe. *)
OGRePrint[expression_] := CellPrint[ExpressionCell[expression, "Output", Editable -> False, CellLabel -> "OGRe:", CellLabelStyle -> Directive["CellLabel", Smaller, Blue]]];
OGRePrint[expressions__] := OGRePrint[Row[{expressions}]];


(* A special key in TensorData, Options, is used to store information about the current session, for the purpose of exporting and importing between sessions using TExportAll and TImportAll. Since this key is not a string, it cannot be accidentally overwritten by a tensor definition. *)
PopulateOptions[] := Module[
    {
        useCurveParameter,
        useIndexLetters,
        useParallelize,
        useReservedSymbols,
        useSimplifyAssumptions
    },
    If[
        !KeyExistsQ[TensorData, Options],
    (* Then *)
        (* If the Options key doesn't exist, which can happen when the package first loads or when importing from an old version of OGRe, create it with the default values. *)
        TensorData[Options] = Association[
            "CurveParameter" -> DefaultCurveParameter,
            "IndexLetters" -> DefaultIndexLetters,
            "OGReVersion" -> OGReVersion,
            "Parallelize" -> False,
            "ReservedSymbols" -> {},
            "SimplifyAssumptions" -> Association["AssumeReal" -> True, "User" -> None]
        ];
        useCurveParameter = DefaultCurveParameter;
        useReservedSymbols = {},
    (* Else *)
        (* If the Options key does exist, populate it with the imported values, but substitute the default values if any keys are missing, which can happen when importing from a different version of OGRe. *)
        useCurveParameter = Lookup[TensorData[Options], "CurveParameter", DefaultCurveParameter];
        useIndexLetters = Lookup[TensorData[Options], "IndexLetters", DefaultIndexLetters];
        useParallelize = Lookup[TensorData[Options], "Parallelize", False];
        If[
            KeyExistsQ[TensorData[Options], "SimplifyAssumptions"],
        (* Then *)
            useSimplifyAssumptions = Association[
                "AssumeReal" -> Lookup[TensorData[Options]["SimplifyAssumptions"], "AssumeReal", True],
                "User" -> Lookup[TensorData[Options]["SimplifyAssumptions"], "User", None]
            ],
        (* Else *)
            useSimplifyAssumptions = Association[
                "AssumeReal" -> True,
                "User" -> None
            ]
        ];
        useReservedSymbols = Lookup[TensorData[Options], "ReservedSymbols", {}];
        TensorData[Options] = Association[
            "CurveParameter" -> useCurveParameter,
            "IndexLetters" -> useIndexLetters,
            "OGReVersion" -> OGReVersion,
            "Parallelize" -> useParallelize,
            "ReservedSymbols" -> {},
            "SimplifyAssumptions" -> useSimplifyAssumptions
        ];
    ];
    (* Reserve the symbols that should be reserved. This will also populate the key "ReservedSymbols" automatically, if the symbols are valid. *)
    TSetReservedSymbols[Evaluate[useReservedSymbols]];
    (* Clear and protect the curve parameter. *)
    Unprotect[Evaluate[useCurveParameter]];
    ClearAll[Evaluate[useCurveParameter]];
    (* Protect won't protect a symbol that hasn't been used yet if the argument is a string pattern, so we convert the string to a symbol first. *)
    useCurveParameter = Symbol[useCurveParameter];
    Protect[Evaluate[useCurveParameter]];
];
PopulateOptions[];


(* Change one of a tensor's indices to the desired position (upper = +1 or lower = -1), starting from a specific index configuration (assumed to be already calculated). The new components are then saved as a separate representation within the tensor object. Returns the new index configuration. *)
RaiseLower[ID_String, coordinatesID_String, oldIndices_List, indexPos_Integer, upperLower_Integer] := Module[
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
    (* Create the new components using a Table with newVars as iterators. Each component will be calculated using a Sum with sumVar as the summation variable. *)
    newComponents = (Table[
            Sum[
                useMetric[[raiseVar, sumVar]] * components[[Sequence @@ oldVars]],
                {sumVar, 1, dim}
            ], ##
        ] &) @@ ({#, 1, dim} &) /@ newVars;
    (* Store the new representation in the tensor object. *)
    allComponents = TensorData[ID]["Components"];
    allComponents[{newIndices, coordinatesID}] = TensorSimplify[newComponents];
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
ShowList[ID_String, indices_List, coordinatesID_String, showOrList_String, function_ : Identity] := Module[
    {
        allElements,
        components,
        coordSymbols,
        curveParameter = Symbol[TensorData[Options]["CurveParameter"]],
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
        indices === {"_UseDefault_"},
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
        coordinatesID === "_UseDefault_",
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
    (* Apply the optional function to the components. *)
    If[
        function =!= Identity,
    (* Then *)
        components = Map[function, components, {ArrayDepth[components]}];
        (* Simplify the components after applying the function. *)
        components = TensorSimplify[components];
    ];
    coordSymbols = TensorData[useCoords]["Components"][{{1}, useCoords}];
    (* If the tensor is a curve Lagrangian or a geodesic vector, nicely format the functions and derivatives to produce a clean expression without explicitly displaying the curve parameter or the coordinates. *)
    If[
        TensorData[ID]["Role"] === "Lagrangian" || TensorData[ID]["Role"] === "GeodesicFromChristoffel" || TensorData[ID]["Role"] === "GeodesicFromLagrangian",
    (* Then *)
        components = components /. Flatten[{
            #[curveParameter] -> #,
            #'[curveParameter] -> OverDot[#],
            #''[curveParameter] -> OverDot[#, 2] (* Note: This shows a syntax error if entered in the front end, but works nevertheless... *)
            } & /@ coordSymbols];
    ];
    (* If the tensor is a geodesic vector with time as a parameter, do the same as above, with the first coordinate instead of the curve parameter. *)
    If[
        TensorData[ID]["Role"] === "GeodesicWithTimeParameter",
    (* Then *)
        components = components /. Flatten[{
            #[coordSymbols[[1]]] -> #,
            #'[coordSymbols[[1]]] -> OverDot[#],
            #''[coordSymbols[[1]]] -> OverDot[#, 2]
            } & /@ coordSymbols[[2 ;;]]];
    ];
    (* Display partial derivatives in a nicer way. This will apply to partial derivatives of any function with respect to any arguments, not just functions of the coordinates. *)
    components = components /. Derivative[orders__][f_][args__] :> Inactive[D][f[args], Sequence @@ Map[ReplaceAll[{arg_, order_} :> arg^order], DeleteCases[Transpose[{{args}, {orders}}], {_, 0}]]];
    (* If a function is a reserved symbol and has only the coordinates as its argument(s), remove the arguments. *)
    components = components /. (f_?(MemberQ[Symbol /@ TensorData[Options]["ReservedSymbols"], #] &))[args__?(SubsetQ[coordSymbols, List[#]] &)] :> f;
    (* Execute the following if called by TShow. *)
    If[
        showOrList === "Show",
    (* Then *)
        (* Print the tensor's ID followed by a colon. *)
        row = {ID, ":   "};
        (* Print the tensor's symbol along with the desired upper and lower indices. The index letters will be taken from TensorData[Options]["IndexLetters"]. IndicesToLetters will return a row with only the upper indices or only the lower indices depending on the first argument. *)
        row = Append[row, Subsuperscript[
            TensorData[ID]["Symbol"],
            IndicesToLetters[-1, useIndices, TensorData[Options]["IndexLetters"]],
            IndicesToLetters[+1, useIndices, TensorData[Options]["IndexLetters"]]
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
        (* Print the row, formatted using Nice. *)
        OGRePrint[Nice[Row[row]]];
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
            (* Create an array of elements of the form {value, label}, where label is the label of the specific component (with the coordinates as indices, e.g. g_xy is the component with x for the first index and y for the second index) and value is its value. *)
            allElements = Flatten[
                MapIndexed[
                    {
                        #1,
                        Subsuperscript[
                            TensorData[ID]["Symbol"],
                            (* The coordinate symbols (e.g. t, x, y, z) will be used in place of the indices. *)
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
                                 Select[allElements, #1[[1]] ===  uniqueValuesSign[[i]] &][[All, 2]],
                                (* The reason for the extra condition in the next line is that if the value is non-zero but equal to minus itself, for example if it is ComplexInfinity, we should avoid listing the same element twice. *)
                                -Select[allElements, #1[[1]] === -uniqueValuesSign[[i]] && #1[[1]] =!= uniqueValuesSign[[i]] &][[All, 2]]
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
        (* Print the grid, preceded by the tensor's ID, formatted using Nice. *)
        OGRePrint[Nice[Column[
            {
                Row[{ID, ":"}],
                grid
            },
            Alignment -> {Center, Baseline}
        ]]];
    ];
];


StartupCheckForUpdates[] := Module[
    {
        newVersion,
        remoteFile,
        versionLookup
    },
    remoteFile = Quiet[Import[OGReURL, "Text"]];
    Unprotect[UpdateMessage];
    If[
        remoteFile === $Failed,
    (* Then *)
        UpdateMessage = Row[{"Could not check for updates automatically. Please visit ", Hyperlink["https://github.com/bshoshany/OGRe"], " to check manually."}],
    (* Else *)
        versionLookup = StringCases[remoteFile, Shortest["OGReVersion = \"" ~~ __ ~~ "\";"]];
        If[
            Length[versionLookup] == 1,
        (* Then *)
            newVersion = StringTake[versionLookup[[1]], {16, StringLength[versionLookup[[1]]] - 2}];
            If[
                newVersion === OGReVersion,
            (* Then *)
                UpdateMessage = "You have the latest version of the package.",
            (* Else *)
                UpdateMessage = Row[{"A new version of the package is available: ", Style[newVersion, Bold], ". For more information, type ", CreateButton["TCheckForUpdates[]", TCheckForUpdates[]], "."}];
            ];
        ];
    ];
    Protect[UpdateMessage];
];


(* Multiply a tensor object by a scalar. *)
TensorByScalar::ErrorCoords = "The tensor \"`1`\" cannot be multiplied by a scalar, as it represents a coordinate system.";
TensorByScalar[ID_String[indices_String], scalar_, useSymbol_: False] := Module[
    {
        newComponents,
        newID,
        newSymbol,
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
    (* Calculate the explicit symbolic representation for this operation, including the correct index placement. *)
    newSymbol = Row[{
        (* If the scalar is -1, we just prefix the tensor with a negative sign. If it's any other number, we use that number as the prefix. If it is a rank 0 tensor, we use its symbol. Otherwise, we add parentheses in case it is an expression with several terms. *)
        Which[
            NumberQ[scalar] && scalar == -1, "-",
            NumberQ[scalar], scalar,
            useSymbol =!= False, useSymbol,
            True, Row[{"(", scalar, ")"}]
        ],
        If[
            TensorData[ID]["Role"] === "Temporary",
        (* Then *)
            TensorData[ID]["Symbol"],
        (* Else *)
            Subsuperscript[
                TensorData[ID]["Symbol"],
                IndicesToLetters[-1, useIndices, indices],
                IndicesToLetters[+1, useIndices, indices]
            ]
        ]
    }];
    (* Let the user know the explicit operation we are performing, including the correct index placement. TODO: Uncomment this in a future version, once this feature works properly. *)
    (* PrintTemporary["Multiplying by scalar ", newSymbol, "..."]; *)
    (* Calculate the product of the scalar with the tensor. *)
    newComponents = scalar * TensorData[ID]["Components"][{useIndices, useCoords}];
    (* Store the result in a new temporary tensor, which will be deleted once the recursive calculation in TCalc is complete. *)
    newID = NewTempID[];
    SetTensorID[newID, Association[
        "Components" -> Association[{useIndices, useCoords} -> newComponents],
        "DefaultCoords" -> useCoords,
        "DefaultIndices" -> useIndices,
        "Metric" -> TensorData[ID]["Metric"],
        "Role" -> "Temporary",
        "Symbol" -> newSymbol
    ]];
    Return[newID[indices]];
];

(* Simplify an expression with optional user-defined assumptions. *)
TensorSimplify[expression_] := Module[
    {
        assumptions,
        progress,
        result,
        tasks
    },
    assumptions = Join[
        {
            If[
                TensorData[Options]["SimplifyAssumptions"]["AssumeReal"],
            (* Then *)
                Element[_, Reals],
            (* Else *)
                None
            ]
        },
        Flatten[{TensorData[Options]["SimplifyAssumptions"]["User"]}]
    ];
    (* If the expression is not a list, or it's a list with just one component, then we don't need parallelization. *)
    If[
        Head[expression] =!= List || Length[expression] * ArrayDepth[expression] == 1,
    (* Then *)
        result = FullSimplify[expression, assumptions],
    (* Else *)
        (* Print a dynamic progress indicator. The progress parameter will be shared between all kernels so they can update it. *)
        progress = 0;
        (* Share the progress parameter and simplification assumptions with all of the kernels. *)
        SetSharedVariable[progress, assumptions];
        PrintTemporary["Simplification progress: ", ProgressIndicator[Dynamic[progress], {0, Times @@ Dimensions[expression]}]];
        If[
            TensorData[Options]["Parallelize"],
        (* Then *)
            (* Submit the simplification of each element in the tensor as an individual task. Whenever a kernel becomes available, it will pick up the next available task. This results in better performance than ParallelMap. *)
            tasks = Map[
                (* Using Block instead of Module here for maximum performance. *)
                Function[
                    element,
                    ParallelSubmit[
                        Block[
                            {simplified = FullSimplify[element, assumptions]},
                            progress++;
                            simplified
                        ]
                    ]
                ],
                expression,
                {ArrayDepth[expression]}
            ];
            (* Wait for all tasks to be completed. *)
            result = WaitAll[tasks],
        (* Else *)
            (* Do the same without parallelization. *)
            result = Map[
                Function[element, Block[{simplified = FullSimplify[element, assumptions]}, progress++; simplified]],
                expression,
                {ArrayDepth[expression]}
            ];
        ];
        UnsetShared[progress];
    ];
    Return[result];
];


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
        ChristoffelJacobian,
        dim,
        inverseJacobian,
        jacobian,
        k,
        newComponents,
        newCoordSymbols,
        newVars,
        old,
        oldComponents,
        oldCoordSymbols,
        oldVars,
        rank,
        sumVar1,
        sumVar2,
        sumVar3,
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
        (* If the Jacobians have not already been calculated for some reason, calculate them now. *)
        If[
            !KeyExistsQ[TensorData[sourceID], "Jacobians"],
        (* Then *)
            TAddCoordTransformation[sourceID, targetID, transRules];
        ];
        (* Collect the Jacobians from the object's data. *)
        jacobian = TensorData[sourceID]["Jacobians"][targetID]["Jacobian"];
        inverseJacobian = TensorData[sourceID]["Jacobians"][targetID]["InverseJacobian"];
        ChristoffelJacobian = TensorData[sourceID]["Jacobians"][targetID]["ChristoffelJacobian"];
        (* Calculate the new components by contracting each lower index with the Jacobian and each upper index with the inverse Jacobian. *)
        newComponents = (
            Table[(
                Sum[Product[
                        Switch[
                            indices[[k]],
                            -1, jacobian[[oldVars[[k]], newVars[[k]]]],
                            +1, inverseJacobian[[newVars[[k]], oldVars[[k]]]]
                        ],
                        {k, 1, rank}
                    ] * (oldComponents[[##]] &) @@ oldVars, ##] &
            ) @@ ({#, 1, dim} &) /@ oldVars, ##] &) @@ ({#, 1, dim} &) /@ newVars /. transRules;
        (* If the tensor object represents a Levi-Civita connection, then it does not transform like a tensor; we need to add an extra term to the transformation. Note that here we are using the class invariant which guarantees that the Christoffel symbols always have {1, -1, -1} as their default index configuration. *)
        If[
            TensorData[ID]["Role"] === "Christoffel",
        (* Then *)
            newComponents += Table[
                Sum[
                    inverseJacobian[[sumVar1, old]] * ChristoffelJacobian[[old, sumVar2, sumVar3]],
                    {old, 1, dim}
                ],
                {sumVar1, 1, dim},
                {sumVar2, 1, dim},
                {sumVar3, 1, dim}
            ] /. transRules;
        ];
        (* Simplify the result. *)
        newComponents = TensorSimplify[newComponents];
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


(* Check for updates at startup, if the AutoUpdates setting is turned on. *)
If[
    OGReGlobalOptions["AutoUpdates"],
(* Then *)
    UpdateMessage = "Checking for updates...";
    UpdateCheck = Row[{Dynamic[UpdateMessage], " To disable automatic checks for updates at startup, type ", CreateButton["TSetAutoUpdates[False]", TSetAutoUpdates[False]], "."}];
    SessionSubmit[StartupCheckForUpdates[]],
(* Else *)
    UpdateCheck = Row[{"To check for updates, type ", CreateButton["TCheckForUpdates[]", TCheckForUpdates[]], ". ", "To enable automatic checks for updates at startup, type ", CreateButton["TSetAutoUpdates[True]", TSetAutoUpdates[True]], "."}];
]


(* Print a welcome message at startup. *)
OGRePrint[Column[{
    Style[Row[{"OGRe: An ", Style["O", Underlined], "bject-Oriented ", Style["G", Underlined], "eneral ", Style["Re", Underlined], "lativity Package for Mathematica"}], Bold, Larger],
    Style[Row[{"By Barak Shoshany (", Hyperlink["baraksh@gmail.com", "mailto:baraksh@gmail.com"], ") (", Hyperlink["baraksh.com", "https://baraksh.com/"], ")"}], Bold],
    Style[Row[{OGReVersion}], Bold],
    Style[Row[{"GitHub repository: ", Hyperlink["https://github.com/bshoshany/OGRe"]}], Bold],
    Row[{"\[Bullet] To view the full documentation for the package, type ", CreateButton["TDocs[]", TDocs[]], "."}],
    Row[{"\[Bullet] To list all available modules, type ", CreateButton["?OGRe`*", OGRePrint[Information["OGRe`*"]]], "."}],
    Row[{"\[Bullet] To get help on a particular module, type ", Style["?", "Input"], " followed by the module name."}],
    Row[{"\[Bullet] To enable parallelization, type ", CreateButton["TSetParallelization[True]", TSetParallelization[True]], "."}],
    Row[{"\[Bullet] ", UpdateCheck}]
}]];


End[]; (* OGRe`Private` *)


(* Protect all OGRe symbols so they will not be accidentally overwritten elsewhere. *)
Protect["OGRe`*"];
Protect["OGRe`Private`*"];


EndPackage[]; (* OGRe *)
