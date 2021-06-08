# OGRe: An Object-Oriented General Relativity Package for Mathematica

## Version history

* v1.5 (2021-06-07)
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
* v1.4 (2021-05-09)
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
        * Automatic checks for update:
            * The package now checks for new versions automatically at startup. No information (personal or otherwise) is sent or collected; the package simply checks the GitHub repository for updates. This check is done asynchronously, to prevent delaying the loading of the package.
            * An upcoming release will introduce persistent storage of user settings, including a setting which will allow disabling the checks for update at startup. Until then, users interested in turning off these automatic checks can change the line `SessionSubmit[StartupCheckForUpdates[]];` in `OGRe.m` to `UpdateMessage = "Checking for updates is disabled.";`.
        * Fixed several lines in the code where a semicolon was missing at the end of the line, causing the code to not run correctly since Mathematica interprets newlines as multiplication by default.
        * This package now has a DOI for citation purposes. Information on how to cite it in publications has been added to the source code and to `README.md`.
        * Added GitHub badges to `README.md`.
* v1.3 (2021-05-06)
    * Changes to existing modules:
        * `TExport`, `TExportAll`, `TImport`, and `TImportAll`:
            * Added a clarification in the usage messages for `TExportAll` and `TImportAll` that the directory where the file will be saved or read from can be changed using `SetDirectory`.
            * Importing tensors from a different OGRe version (whether older or newer) will now issue a warning that compatibility issues may occur.
            * `Options` will now be populated correctly with the specific keys relevant to this version, even if you import from other versions which may use different `Options` keys.
    * Other changes:
        * The previous update of the package made use of the new Mathematica syntax for anonymous functions, `x |-> body`. However, since this is a new feature introduced in Mathematica 12.2, this accidentally made the package incompatible with earlier Mathematica versions. I changed it to the old syntax `Function[x, body]` in order to maintain compatibility.
* v1.2 (2021-04-28)
    * New modules:
        * `TDocs`: Opens the Mathematica notebook `OGRe_Documentation.nb` directly from the GitHub repository. This allows users to instantly access the latest documentation from any Mathematica session, whether the package itself was loaded locally or from GitHub.
        * `TParallelize`:
            * Enables or disable the parallelization of tensor simplifications (see below). It is disabled by default.
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
* v1.1 (2021-04-15)
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
* v1.0 (2021-02-10)
    * Initial release.
