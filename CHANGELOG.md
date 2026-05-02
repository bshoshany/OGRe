[![Author: Barak Shoshany](https://img.shields.io/badge/author-Barak_Shoshany-009933)](https://baraksh.com/)
[![DOI: 10.21105/joss.03416](https://joss.theoj.org/papers/10.21105/joss.03416/status.svg)](https://doi.org/10.21105/joss.03416)
[![arXiv:2109.04193](https://img.shields.io/badge/arXiv-2109.04193-b31b1b.svg)](https://arxiv.org/abs/2109.04193)
[![License: MIT](https://img.shields.io/github/license/bshoshany/OGRe)](https://github.com/bshoshany/OGRe/blob/master/LICENSE.txt)
![Language: Mathematica 14](https://img.shields.io/badge/Language-Mathematica%2014-yellow)
[![GitHub stars](https://img.shields.io/github/stars/bshoshany/OGRe?style=flat&color=009999)](https://github.com/bshoshany/OGRe/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/bshoshany/OGRe?style=flat&color=009999)](https://github.com/bshoshany/OGRe/forks)
[![GitHub release](https://img.shields.io/github/v/release/bshoshany/OGRe?color=660099)](https://github.com/bshoshany/OGRe/releases)
[![Open in Visual Studio Code](https://img.shields.io/badge/Open_in_Visual_Studio_Code-007acc)](https://vscode.dev/github/bshoshany/OGRe)

# OGRe: An Object-Oriented General Relativity Package for Mathematica

**Barak Shoshany**\
Department of Physics, Brock University,\
1812 Sir Isaac Brock Way, St. Catharines, Ontario, L2S 3A1, Canada\
[bshoshany@brocku.ca](mailto:bshoshany@brocku.ca) | [https://baraksh.com/](https://baraksh.com/)\
DOI: [10.21105/joss.03416](https://doi.org/10.21105/joss.03416)

* [Version history](#version-history)
    * [v2.0.0 (2026-05-02)](#v200-2026-05-02)
    * [v1.7.0 (2021-09-17)](#v170-2021-09-17)
    * [v1.6.1 (2021-09-01)](#v161-2021-09-01)
    * [v1.6 (2021-08-07)](#v16-2021-08-07)
    * [v1.5 (2021-06-07)](#v15-2021-06-07)
    * [v1.4 (2021-05-09)](#v14-2021-05-09)
    * [v1.3 (2021-05-06)](#v13-2021-05-06)
    * [v1.2 (2021-04-28)](#v12-2021-04-28)
    * [v1.1 (2021-04-15)](#v11-2021-04-15)
    * [v1.0 (2021-02-10)](#v10-2021-02-10)

## Version history

### v2.0.0 (2026-05-02)

* After a long time with no updates, a major OGRe update is finally here! This long-anticipated update includes numerous new features, improvements, and bug fixes. Many of these are due to feature requests, bug reports, and other feedback from the scientific community - submitted on GitHub, sent by email, or even discussed in person at conferences. More updates are coming soon!
* **Breaking changes (may break pre-v2.0.0 code):**
    * Replacing all `TCalc*` modules with cached references:
        * The modules for built-in tensors derived from a metric: `TCalcChristoffel`, `TCalcEinsteinTensor`, `TCalcGeodesicFromChristoffel`, `TCalcGeodesicFromLagrangian`, `TCalcGeodesicWithTimeParameter`, `TCalcLagrangian`, `TCalcRicciScalar`, `TCalcRicciTensor`, and `TCalcRiemannTensor`, have been removed.
        * They are replaced by new modules: `TChristoffel`, `TEinstein`, `TGeodesicFromChristoffel`, `TGeodesicFromLagrangian`, `TGeodesicWithTimeParameter`, `TLagrangian`, `TRicciScalar`, `TRicci`, and `TRiemann`.
        * Unlike the `TCalc*` modules, the new modules do not perform a calculation every time they are called, and do not generate new user-accessible tensors with IDs of the form `"<metric><role>"` (such as `"SchwarzschildRiemann"`). Instead, the new modules perform the calculation only the first time they are called, and then cache the results internally and reuse them on subsequent calls. To access the calculated tensor, you don't use a separate tensor ID (like `"SchwarzschildRiemann"`); instead, you call the module directly every time (e.g. `TRiemann["Schwarzschild"]`).
        * This new syntax can be used with most modules that accept tensors, for example `TList[TRiemann["Schwarzschild"]]`, and also in arbitrary tensor calculations with indices using `TCalc[]`, for example `TCalc[TRiemann["Schwarzschild"]["\[Lambda]\[Mu]\[Lambda]\[Nu]"],"R"]`. The new design makes the package much more user-friendly and easy to use, and it avoids recalculating the same tensor multiple times, cluttering the namespace with many derived tensor IDs, and having to use long and confusing IDs like `"SchwarzschildRiemann"`.
        * Internally, the tensors are cached inside the metric object itself, so they will be exported when using `TExport[]` on the metric. The new modules essentially just return special reference IDs of the form `"<metric>-><role>"`, with `->` separating the metric of origin from the tensor's role, for example `"Schwarzschild->Riemann"`. These references are then routed to the cached tensors automatically, but the user doesn't need to use the references directly; the new modules should be used instead.
        * You cannot change the ID, symbol, or default indices/coordinates of the cached tensors.
        * Tensor IDs created by the user can no longer contain `->`, as this is now reserved for references to cached tensors.
        * **Migration guide for pre-v2.0.0 code:** Wherever you used a `TCalc*` module or a derived tensor ID, use the corresponding new module instead, noting that some module names have been shortened (the word `Tensor` was removed). For example, if before you used `TCalcEinsteinTensor["MyMetric"]` to calculate the Einstein tensor, and then used the tensor ID `"MyMetricEinsteinTensor"` to refer to the calculated tensor, now you should simply use `TEinstein["MyMetric"]` instead, in both cases.
    * The module `TCalcNormSquared[]` has also been removed, and replaced by `TNormSquared[]`. The new module returns a scalar expression directly instead of creating a rank-0 tensor object, and accepts an optional coordinate system as the second argument. It does not cache the result, and it can be used for tensors of any type.
        * **Migration guide for pre-v2.0.0 code:** Wherever you used `TCalcNormSquared[]` before, use `TNormSquared[]` now, and note that the result is given directly. For example, if before you used `TCalcNormSquared["MyTensor"]` and subsequently accessed the scalar component using `TGetComponents["MyTensorNormSquared"]`, now you should simply use `TNormSquared["MyTensor"]` instead and get the scalar directly.
    * `TAddCoordTransformation[]` and `TChangeID[]` now use `,` instead of `->` to separate the first two arguments, so that tensor ID completion can work properly. The old `->` syntax has been removed, so old code using `->` will need to be updated to use the new syntax.
        * **Migration guide for pre-v2.0.0 code:** Replace `TAddCoordTransformation["MyCoords1"->"MyCoords2",rules]` with `TAddCoordTransformation["MyCoords1","MyCoords2",rules]`. Replace `TChangeID["MyTensor1"->"MyTensor2"]` with `TChangeID["MyTensor1","MyTensor2"]`.
    * `TVolumeElementSquared[]` has been renamed to `TMetricDeterminant[]`, as this is a more accurate name for what the module actually calculates, the determinant of the metric, which may be negative for pseudo-Riemannian metrics. A new module, `TVolumeElement[]`, calculates the actual volume element of a given metric in the form $\mathrm{d}V = \sqrt{|\det(g)|} \, {\mathrm{d}x}^{0} \wedge \cdots \wedge {\mathrm{d}x}^{n-1}$, where $n$ is the number of dimensions.
        * **Migration guide for pre-v2.0.0 code:** Replace `TVolumeElementSquared["MyMetric"]` with `TMetricDeterminant["MyMetric"]`.
* UI improvements:
    * **Tensor ID completion:** A widely requested feature is now implemented, making OGRe much more user-friendly and easy to use! All OGRe modules that take existing tensor IDs as arguments will now show a dropdown menu with a list of relevant IDs - coordinate systems, metrics, and/or tensors - as soon as you type the opening double quotes for a string in a relevant argument position. Some other strings are also auto-completed, such as common symbols. For example:
        * `TList["`, `TShow["`, and `TGetComponents["` will show a list of all tensors.
        * `TRiemann["`, `TLagrangian["`, and `TGeodesicFromChristoffel["` will show a list of all metrics.
        * `TAddCoordTransformation["`, `TNewMetric["MyMetric", "`, and `TNewTensor["MyTensor", "MyMetric", "` will show a list of all coordinate systems (for the first, second, and third argument respectively).
        * `TNewMetric[..., "` will show a list of commonly used metric symbols (for the last argument).
        * `TSetCurveParameter["` will show a list of commonly used curve parameter symbols.
        * And so on.
    * Added `SyntaxInformation` for all public modules. This allows Mathematica to indicate when an argument is missing, or if there are too many arguments.
* New modules:
    * `TKretschmann[]` calculates and caches the Kretschmann scalar of a metric.
    * `TWeyl[]` calculates and caches the Weyl tensor of a metric.
    * `TDim[]` returns the dimension of a tensor's manifold.
    * `TRank[]` returns the rank of a tensor.
    * `TSetSimplifyFunc[]` allows setting a custom simplification function.
    * `TTeXShow[]` returns the output of `TShow[]` as a TeX string.
    * `TTeXList[]` returns the output of `TList[]` as a TeX string.
    * `TSetExactSignChecks[]` can be used to enable or disable exact sign checks. When exact sign checks are enabled (the default), if `TList` cannot detect equality of two components up to sign by direct comparison, it checks whether the sum of the components can be proven to equal zero, which may require simplification. For tensors with complicated components, this may take a long time, in which case this option can be disabled using `TSetExactSignChecks[False]`; however, `TList` will then only be able to detect when two components are the same up to sign in simple cases.
* Changes to existing modules:
    * `TInfo[]` now displays the dimension and rank.
    * `TList[]`, `TShow[]`, and `TGetComponents[]`:
        * The modules `TList[]`, `TShow[]`, and `TGetComponents[]` now accept a rule or a list of rules as an optional argument. This applies `ReplaceAll[rules]` to each of the tensor's elements, and then automatically simplifies them, before they are displayed or returned.
        * The full syntax of these modules is now:
            * `TList[ID, indices, coordinatesID, rules, function]`
            * `TShow[ID, indices, coordinatesID, rules, function]`
            * `TGetComponents[ID, indices, coordinatesID, rules, function]`
        * Any of the arguments can be omitted. If both `rules` and `function` are given, the rules will be applied before the function.
    * `TList[]` and `TShow[]` now use spacers instead of invisible letters to align the indices, which will make copied components less awkward (each index will only appear once).
    * `TExportAll[]` and `TImportAll[]` now handle file errors better, add the `.m` extension if an extension is not provided (unless the file name explicitly ends with a dot), and print out the full path of the file as a hyperlink that can be clicked to open it directly in Mathematica.
    * `TCheckForUpdates[]` now uses GitHub releases to check for updates, instead of looking directly into the source code. If a new version is available, it gives a link to the release notes, and also allows viewing the release notes directly in the notebook.
    * `TDocs[]` now loads the documentation for the version of the package that is currently installed, instead of always loading the documentation for the latest version.
    * `TImport[]` now checks if the metric and coordinates used by the imported tensor exist and have the correct dimension before importing.
    * `TCovariantD[]` now preserves the default index configuration of the tensor, matching the behavior of `TPartialD[]` (previously, `TCovariantD[]` always acted on the representation with all upper indices).
* Derivative shorthand notation:
    * `TList[]` and `TShow[]` now use a clearer shorthand notation for derivatives. Previously the shorthand for something like `Derivative[1, 1, 2][f][x, y, z]` was $\partial_{x,y,z^2}f$, which was confusing, especially because it seems like the last derivative is with respect to $z^2$, rather than a second derivative with respect to $z$. Now the shorthand for the same is $\partial_x \partial_y \partial_z^2 f$, which more closely resembles familiar tensor notation.
    * Additionally, the shorthand notation has been made more clear in cases where the derivative is taken to a power. Previously the shorthand for something like `a'[t]^2` was $\partial_t a^2$, which makes it look like the function itself is squared, instead of the derivative. Now the shorthand for the same is $(\partial_t a)^2$, which is much more clear.
* Lagrangians, geodesic equations, and curve parameters:
    * The `Lagrangian`, `GeodesicFromChristoffel`, `GeodesicFromLagrangian`, and `GeodesicWithTimeParameter` roles now transform correctly under coordinate transformations.
    * All geodesic equation types now transform correctly under index transformations. The `GeodesicFromLagrangian` and `GeodesicFromChristoffel` roles now use the metric components evaluated along the curve, instead of the unmodified metric components. The `GeodesicWithTimeParameter` role now rejects index transformations, since these equations do not transform as a tensor.
    * Internally, Lagrangians and affine-parameter geodesic equations are now stored using a placeholder for the curve parameter (`$CurveParam`). The placeholder is replaced with the user-selected curve parameter only when they are displayed or the components are retrieved. When exporting tensors using `TExport[]` or `TExportAll[]`, the tensors will be exported with the placeholder, so that when they are imported later, they will be imported correctly regardless of which curve parameter is currently selected. This also eliminates the need to change the curve parameter retroactively for all existing tensors when using `TSetCurveParameter[]`, which is how this worked before. However, `TExportAll[]` also exports the choice of user-facing curve parameter separately as part of the `Options` key, and it is imported when using `TImportAll[]`.
    * The `GeodesicFromLagrangian` role now follows the Euler-Lagrange equations more rigorously: the defining representation now has a lower index, and the sign has been corrected.
* Performance optimizations:
    * Christoffel symbols, Riemann tensors, and Weyl tensors are recalculated directly when requested in a new coordinate system, instead of transforming the existing components. This improves performance, both because coordinate transformations on rank 3 or 4 tensors are slow, and because the directly calculated components are easier to simplify.
    * Internally, all built-in tensors derived from a metric are now calculated directly from the component formulas instead of using `TCalc[]`, avoiding the overhead.
* Bug fixes:
    * Fixed the default index letters (previously, $\epsilon$ appeared twice and $\eta$ was unused).
    * Fixed a bug where coordinate transformations with an incomplete `"Jacobians"` entry could fail to calculate the missing Jacobians for a specific target coordinate system.
    * `TChangeDefaultIndices[]` now verifies that the desired indices have the correct rank.
    * `TCalc[]` now shows an appropriate error message when trying to use a coordinate system as a tensor in any expression.
    * `TAddCoordTransformation[]` now correctly aborts when provided with a non-invertible coordinate transformation.
    * `TShow[]` and `TList[]` now correctly handle multi-letter coordinate symbols.
* Documentation:
    * The `README.md` file in the GitHub repository now includes the complete documentation for easier access, but **without cell outputs**. The Markdown file is compiled to create the `OGRe_Documentation.nb` notebook, which is then evaluated to generate the outputs. This also makes maintaining the documentation much easier.
    * The documentation and usage messages have been corrected and clarified in many places.
    * All error messages from internal modules are now displayed as `TMessage::<message_name>` instead of the awkward ``OGRe`Private`<module_name>::<message_name>``.
    * Formulas now look nicer in the notebook.
    * `OGRe_Documentation.nb` can now be opened with the free [Wolfram Player](https://www.wolfram.com/player/).
    * The welcome message now provides links to the current release notes.
* The package now has a comprehensive internal test suite which thoroughly tests all OGRe modules and verifies that it accurately reproduces a wide variety of known results from differential geometry and general relativity, ensuring users can fully trust OGRe as a reliable tool for research and pedagogy. This is currently used only internally, but will eventually be made available on the GitHub repository, once it matures a bit more and is cleaned up and properly documented.
* This package now has a Python port, [OGRePy: An Object-Oriented General Relativity Package for Python](https://github.com/bshoshany/OGRePy). If you are interested in doing OGRe-style tensor calculations in Python, please check it out!

### v1.7.0 (2021-09-17)

* New modules:
    * `TCalcGeodesicWithTimeParameter[]` calculates the geodesic equations with respect to the time parameter (which is assumed to be the first coordinate) instead of an affine curve parameter.
    * `TCalcNormSquared[]` calculates the norm-squared of a tensor with respect to its metric, that is, the tensor contracted with itself in all indices. For example, for a vector v<sup>&mu;</sup> the norm-squared will be v<sup>&mu;</sup>v<sub>&mu;</sub> and for a rank-2 tensor T<sup>&mu;&nu;</sup> the result will be T<sup>&mu;&nu;</sup> T<sub>&mu;&nu;</sub>.
    * `TCite[]` displays information on how to cite this package in published research. Thank you for citing my work! :)
* Changes to existing modules:
    * `TCalc*` modules no longer display a progress bar for the calculation. The progress bar wasn't too useful, since almost all tensor calculations are very fast even on an average laptop. Simplifications are the only operations which take any considerable amount of time, and thus progress bars are now displayed only for simplifications.
    * `TCovariantD` and `TPartialD`:
        * If `TCovariantD` is used on a scalar, it is now replaced automatically with `TPartialD` to improve performance, since the covariant derivative of a scalar is just a partial derivative anyway.
        * Fixed bug where acting on scalars incorrectly returned a list of lists instead of a vector.
    * `TGetComponents` now applies a function given in the last argument to the components, and then simplifies them (in parallel, if parallelization is enabled), before returning the components. Note that `TShow` and `TList` have already had this option for some time, but now `TGetComponents` has it too.
    * `TLineElement` now simplifies the line element before returning it.
    * `TSimplify` now allows the user to simplify any expression, not just tensor objects. `TSimplify[expression]` simplifies `expression` based on the user-defined simplification assumptions set using `TSetAssumptions`. If `expression` is a `List`, the components will be simplified in parallel. The user can thus make use of OGRe's optimized simplification process to simplify any Mathematica expression.
* Other changes:
    * Fixed a bug where simplification assumptions were not applied correctly if parallelization was enabled.
    * Added arXiv badge to `README.md` and `CHANGELOG.md`.
    * Added a `CITATION.cff` file (in YAML format) to the GitHub repository. This should add [an option to get citations in different formats](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github/about-citation-files) directly from the GitHub repository by clicking on "cite this repository" on the sidebar to the right.
    * Added a `CITATION.bib` file (in BibTeX format) to the GitHub repository. You can use it to easily cite this package in your papers.

### v1.6.1 (2021-09-01)

* This package is now [published in the Journal of Open Source Software](https://joss.theoj.org/papers/10.21105/joss.03416)! I added links to the paper and updated the citation information. The old Zenodo citation should not be used anymore.
* If you use this package in published research, please cite the JOSS paper as detailed in the documentation. A BibTeX entry is also provided.
* The source code itself has not been changed.

### v1.6 (2021-08-07)

* New modules:
    * `TCalcGeodesicFromChristoffel`:
        * Creates a new rank-1 tensor object containing the geodesic equations obtained for each of the coordinates using the Christoffel symbols of the given metric: <i>&#7821;</i><sup>&sigma;</sup> + &Gamma;<sup>&sigma;</sup><sub>&mu;&nu;</sub><i>&#7819;</i><sup>&mu;</sup><i>&#7819;</i><sup>&nu;</sup> = 0.
        * The Christoffel symbols will be calculated automatically using `TCalcChristoffel` if they have not already been calculated.
    * `TCalcGeodesicFromLagrangian`:
        * Creates a new rank-1 tensor object containing the geodesic equations obtained for each of the coordinates by applying the Euler-Lagrange equations to the curve Lagrangian.
        * The Lagrangian will be calculated automatically using `TCalcLagrangian` (see below) if it has not already been calculated.
        * This module leaves the derivatives with respect to the curve parameter in the Euler-Lagrange equation unevaluated (using `Inactive`), which can sometimes help solve the geodesic equations by inspection. Use `Activate` to evaluate the derivatives. (Recall that `TList` and `TShow` can apply a function to the tensor's components before displaying them, so you can write e.g. `TList["ID", Activate]`.)
        * Often the equations obtained in this way will be different from the ones obtained using `TCalcGeodesicFromChristoffel`, but they will always have the same solutions. Usually, one of `TCalcGeodesicFromChristoffel` or `TCalcGeodesicFromLagrangian` will generate simpler equations for a given metric and/or coordinate system.
    * `TCalcLagrangian`: Calculates the curve Lagrangian of a metric, defined as the norm-squared of the tangent to the curve: <i>g</i><sub>&mu;&nu;</sub><i>&#7819;</i><sup>&mu;</sup><i>&#7819;</i><sup>&nu;</sup>. Taking the square root of (the absolute value of) the Lagrangian yields the integrand of the curve length functional. Varying the Lagrangian using the Euler-Lagrange equations yields the geodesic equations (see `TCalcGeodesicFromLagrangian` above).
    * `TMessage`:
        * Not really a module, just a placeholder symbol to which messages not associated with any specific OGRe module are attached.
        * In particular, when a private module (called only internally within the package) invokes `Message`, the message will now be displayed as `TMessage::<message_name>` instead of the awkward ``OGRe`Private`<module_name>::<message_name>``.
        * Not all modules use `TMessage` yet; the transition will be performed gradually in the upcoming releases.
    * `TSetAllowOverwrite`:
        * Allows or disallows overwriting tensors. The default value is `False`, which means you cannot create a new tensor with the same ID as an existing tensor. Calling `TSetAllowOverwrite[True]` will allow overwriting tensors, which is more convenient, but can result in loss of data.
        * You will be warned whenever a tensor is being overwritten, but this warning can be turned off (like any other `Message`) using `Off[TMessage::WarningOverwrite]`.
        * This setting is persistent between sessions.
    * `TSetCurveParameter`:
        * Sets the curve parameter used by `TCalcGeodesicFromChristoffel`, `TCalcGeodesicFromLagrangian`, and `TCalcLagrangian`. These modules will produce results in terms of the coordinate symbols as functions of the curve parameter and their derivatives with respect to this parameter. The default value is &lambda;.
        * If the Lagrangian or geodesic equation vector is displayed using `TList` or `TShow`, the arguments of the coordinate functions are omitted (e.g. <i>x</i> instead of <i>x</i>[&lambda;]) and derivatives with respect to the curve parameter are displayed in Newton (dot) notation (e.g. <i>&#7819;</i> instead of <i>x</i>'[&lambda;]) for improved readability. However, extracting the components using `TGetComponents` will produce the full expressions (e.g. to be used with `DSolve`).
        * When the curve parameter is changed, the parameter of the coordinate functions in all of the tensors calculated so far will be changed accordingly.
    * `TSetReservedSymbols`:
        * Works similar to `TInitializeSymbols`, which has now been removed. However, `TSetReservedSymbols` also saves the reserved symbols so they can be exported and then imported in a later session.
        * If the reserved symbol is a function of the coordinates, `TList` and `TShow` will not show the arguments of the function when displaying the components of a tensor, for improved readability.
    * `TVolumeElementSquared`: Calculates the determinant of a given metric. The square root of the determinant (or its negative, for a pseudo-Riemannian metric) is the volume element.
* Changes to existing modules:
    * All `TCalc*` modules now check if the metric exists first.
    * `TGetComponents`: This module now gets the components of the tensor in the default index configuration and/or coordinate system if either or both are not specified. However, if the default value is used, a message will let you know which representation the components are given in, to avoid confusion.
    * `TInitializeSymbols` has been removed and replaced with `TSetReservedSymbols` (see above).
    * `TList` and `TShow`:
        * Partial derivatives are now displayed in compact notation for improved readability.
        * `TList` will no longer list the same element twice if it is non-zero but equal to minus itself (e.g. `ComplexInfinity`).
        * See `TSetCurveParameter` and `TSetReservedSymbols` above for other changes.
    * `TNewMetric`: If the new metric overrides a previous metric with the same ID, all of the curvature tensors calculated from the metric being overwritten will be automatically deleted, for consistency.
    * `TSetParallelization`:
        * Now uses `$MaxLicenseSubprocesses` instead of the deprecated (as of Mathematica 12.3) `$ConfiguredKernels` to determine how many kernels to launch when enabling parallelization.
        * Disabling parallelization now also closes the kernels.
        * Tensor simplifications will no longer invoke parallelization if the tensor only has one component, to avoid unnecessary overhead.
* Other changes:
    * A button to open the GitHub repository directly in Visual Studio Code has been added to the badges in `README.md`.
* This release is dedicated to my grandfather Yona Shoshany, who taught me BASIC, my first programming language, in my early childhood. He passed away a day before this release was published.

### v1.5 (2021-06-07)

* New modules:
    * `TLineElement`: Displays the line element of a given metric in a coordinate system of your choice.
    * `TSetAutoUpdates`: Enables or disables automatic checks for updates at startup. Note that this setting is persistent between sessions.
* Changes to existing modules:
    * `TInfo`:
        * Whenever this module prints out the name of a related tensor (e.g. the metric or the default coordinates), clicking on the tensor's name will execute `TInfo` for that tensor.
        * This module now provides clickable links for printing out the components of the tensor using `TList` or `TShow`.
        * Calling `TInfo[]` with no parameters lists all the tensors created so far in this session: coordinate systems, metrics, and the tensors associated with each metric.
    * `TSetParallelization`: Calling `TSetParallelization[]` with no parameters now returns the current setting, `True` or `False`.
* Other changes:
    * The startup message is now more compact. Instead of saying "To do X, type Y or click here", it now just says "To do X, type Y", where Y is a clickable link which executes the relevant command.
    * The previous release of the package introduced an incompatibility with Mathematica 12.0 due to the use of the `Splice` function, which only works in v12.1 and later. This has now been fixed, and compatibility with v12.0 has been restored. Although I always develop the package using the latest version of Mathematica, I have now installed Mathematica 12.0 on my computer alongside the latest version, and will use it to ensure compatibility with v12.0 in all future releases of the package.
    * Dynamic content was removed from `OGRe_Documentation.nb` to prevent the message *"This file contains potentially unsafe dynamic content"* from appearing when opening it.
    * The version history has become too long to be included in `README.md`, so I moved it to a separate file, `CHANGELOG.md`.

### v1.4 (2021-05-09)

* Changes to existing modules:
    * `TAddCoordTransformation` now has the clearer syntax `TAddCoordTransformation[sourceID -> targetID, rules]`. The old syntax (with `,` instead of `->`) can still be used.
    * `TChangeID` now has the clearer syntax `TChangeID[oldID -> newID]`. The old syntax (with `,` instead of `->`) can still be used.
    * `TChristoffel`, `TEinsteinTensor`, `TRicciScalar`, `TRicciTensor`, and `TRiemannTensor` have been renamed to `TCalcChristoffel`, `TCalcEinsteinTensor`, `TCalcRicciScalar`, `TCalcRicciTensor`, and `TCalcRiemannTensor` respectively, to group them all together and clarify that they all calculate specific tensors using `TCalc`.
    * `TIndexLetters`, `TParallelize`, and `TSimplifyAssumptions` have been renamed to `TSetIndexLetters`, `TSetParallelization`, and `TSetAssumptions` respectively, to group them all together and clarify that they all change various settings.
    * `TCalc`, `TCalcChristoffel`, `TCalcEinsteinTensor`, `TCalcRicciScalar`, `TCalcRicciTensor`, `TCalcRiemannTensor`, `TChangeID`, `TImport`, `TNewCoordinates`, `TNewMetric`, and `TNewTensor` no longer overwrite existing tensors, to prevent data loss. The user will be instructed to rename the existing tensor using `TChangeID` or delete it using `TDelete` first.
* Other changes:
    * Usage messages:
        * Many usage messages have been simplified, improved, or clarified.
        * Fixed a bug where loading the package directly from GitHub led to usage messages not being displayed properly due to doubling of line breaks.
        * The way that usage messages are defined internally has also been simplified.
    * Automatic checks for updates:
        * The package now checks for new versions automatically at startup. No information (personal or otherwise) is sent or collected; the package simply checks the GitHub repository for updates. This check is done asynchronously, to prevent delaying the loading of the package.
        * An upcoming release will introduce persistent storage of user settings, including a setting which will allow disabling the checks for updates at startup. Until then, users interested in turning off these automatic checks can change the line `SessionSubmit[StartupCheckForUpdates[]];` in `OGRe.m` to `UpdateMessage = "Checking for updates is disabled.";`.
    * Fixed several lines in the code where a semicolon was missing at the end of the line, causing the code to not run correctly since Mathematica interprets newlines as multiplication by default.
    * This package now has a DOI for citation purposes. Information on how to cite it in publications has been added to the source code and to `README.md`.
    * Added GitHub badges to `README.md`.

### v1.3 (2021-05-06)

* Changes to existing modules:
    * `TExport`, `TExportAll`, `TImport`, and `TImportAll`:
        * Added a clarification in the usage messages for `TExportAll` and `TImportAll` that the directory where the file will be saved or read from can be changed using `SetDirectory`.
        * Importing tensors from a different OGRe version (whether older or newer) will now issue a warning that compatibility issues may occur.
        * `Options` will now be populated correctly with the specific keys relevant to this version, even if you import from other versions which may use different `Options` keys.
* Other changes:
    * The previous update of the package made use of the new Mathematica syntax for anonymous functions, `x |-> body`. However, since this is a new feature introduced in Mathematica 12.2, this accidentally made the package incompatible with earlier Mathematica versions. I changed it to the old syntax `Function[x, body]` in order to maintain compatibility.

### v1.2 (2021-04-28)

* New modules:
    * `TDocs`: Opens the Mathematica notebook `OGRe_Documentation.nb` directly from the GitHub repository. This allows users to instantly access the latest documentation from any Mathematica session, whether the package itself was loaded locally or from GitHub.
    * `TParallelize`:
        * Enables or disables the parallelization of tensor simplifications (see below). It is disabled by default.
        * As a rule of thumb, if simplifications are taking less than a few seconds, then you should leave parallelization off, as it has a small overhead and may actually impede performance in that case. However, if simplifications are taking more than a few seconds, then it is highly recommended to enable parallelization for a significant performance boost.
        * This setting will be exported when using `TExportAll`, so it will be imported automatically with `TImportAll` in future sessions.
        * Aspects of the package other than simplification are not parallelized, since they are typically short calculations with few elements, and would not benefit from parallelization.
* Changes to existing modules:
    * `TCalc` now displays a progress bar (see below).
    * `TCheckForUpdates`:
        * Fixed a bug where this module falsely reported that a new version is available.
        * Fixed a bug where this module issued an error when giving the option to download `OGRe.m` to the current notebook directory if the notebook is an Untitled notebook, meaning it is not an actual file in the file system and thus does not have a notebook directory.
        * This module now gives the user the option to reload the new version directly from GitHub without saving it locally.
    * `TList` and `TShow`:
        * The output of these modules is now uneditable. I made this change after I noticed that some users were editing the output, thinking that they are editing the tensor itself. (An option to actually edit the elements of a tensor in `MatrixForm` may be added in a future version.)
        * These modules now take a fourth optional argument: a function to map to each of the tensor's elements before they are displayed. Typically this would be `ReplaceAll[rules]` to apply the `rules` to the elements, but any function can be used. Note that applying `ReplaceAll` to the output of `TShow` or `TList` directly won't work, since the output is formatted.
    * `TNewMetric` now verifies that the metric components are an invertible, symmetric, and square matrix.
* Other changes:
    * The performance of simplifications has been improved considerably!
        * Simplifications are now performed automatically less often, to prevent simplifying the same expression multiple times.
        * In addition, simplifications can now be parallelized. This is enabled using the new module `TParallelize` (see above). The parallelization is implemented using a thread pool model (with `ParallelSubmit` and `WaitAll`), which in my benchmarks was found to provide better performance than the simpler `Parallelize` or `ParallelMap`.
        * Parallelization can potentially provide a speedup proportional to the number of parallel kernels. Note that this number is determined by your Mathematica license, and it may be less than the number of cores in your CPU.
    * Calculations with `TCalc` and simplifications now both display progress bars, so you can keep track of long calculations. This was a popular feature request, especially for simplifications, since they are usually the most time-consuming operations.
    * Improved the appearance of the welcome message.
    * Messages from OGRe are now displayed with a special cell label.
    * Information on how to load the package is now displayed in the `README.md` file.

### v1.1 (2021-04-15)

* New modules:
    * `TCheckForUpdates`: Automatically checks the GitHub repository for updates. If a new version of the package is available, the module will offer an option to download or install the new version and reload the package.
    * `TGetComponents`: Extracts the components of a tensor object in a specific representation as a `List`.
    * `TInfo`: Displays information about a tensor object, and any other objects associated with it, in human-readable form.
* Changes to existing modules:
    * `TAddCoordTransformation` now also calculates the Jacobian of the transformation when it is executed, and stores the result for future use within the tensor object of the source coordinates. This improves performance by using the pre-calculated Jacobian whenever a coordinate transformation is performed, instead of calculating it from scratch every time, as was the case in v1.0.
    * `TChristoffel` now explicitly marks the resulting tensor object as having special transformation properties. The Levi-Civita connection, whose components are the Christoffel symbols, does not transform like a tensor under a coordinate transformation, and OGRe now automatically knows to use the correct transformation rule. **Please note that tensors created with `TChristoffel` in v1.0 will not transform correctly, so they should be recalculated after updating.**
    * `TExport` now adds a key named `"OGReVersion"` which records the version of the package used to create the tensor being exported. This will be used in future versions to ensure backwards compatibility.
    * `TExportAll` now exports, in addition to all the tensors defined so far, a special key, `Options`, containing information about the current session. Currently, this key stores the version of the package, the index letters to use, and the simplification assumptions set by the user. When importing the data in another session using `TImportAll`, the version number will be used to ensure backwards compatibility, and the other options will be used to restore any user-defined index letters and simplification assumptions made during the session.
    * `TNewTensor` now allows defining the components of the new tensor in any coordinate system. In v1.0, the components had to be defined in the default coordinate system of the associated metric. This is still the default behavior if a coordinate system is not specified, for compatibility with v1.0, but it is recommended to always specify the coordinate system explicitly, to avoid accidentally defining the tensor with the wrong components.
    * `TSimplifyAssumptions` now appends new simplification assumptions to the list of previously added  assumptions, instead of replacing it. Also, OGRe now automatically assumes that all variables are real, which helps simplify certain expressions. If you are using more exotic variables, use `TSimplifyAssumptions[!Reals]` to disable this assumption.
* Other changes:
    * The `"Role"` key of each tensor object now indicates how that object was created. In v1.0, tensors created with `TNewCoordinates` had the role `"Coordinates"` and tensors created with `TNewMetric` had the role `"Metric"`, but all other tensors had the role `"General"`. Now tensors will have the roles `"Tensor"`, `"Calculated"`, `"Christoffel"`, `"Riemann"`, `"Ricci Tensor"`, `"Ricci Scalar"`, or `"Einstein"` if they were created using `TNewTensor`, `TCalc`, `TChristoffel`, `TRiemannTensor`, `TRicciTensor`, `TRicciScalar`, or `TEinstein` respectively. This is currently just for bookkeeping, but may have other uses in future versions. Note that tensors imported from v1.0 will still have the role `"General"`.
    * Improved the formatting of the usage messages for all OGRe modules. They no longer break in the middle of words.
    * Debug mode has been removed. If the package is loaded more than once in a single session, it will redefine all symbols but keep any previously defined tensors intact. This is useful both for debugging and for reloading the package after an update.
* Bug fixes:
    * Fixed a bug where the partial and covariant derivatives of a scalar were not calculated correctly.

### v1.0 (2021-02-10)

* Initial release.
