# OGRe<sup>TM</sup>: An (O)bject-oriented (G)eneral (Re)lativity (T)oolkit for (M)athematica

<!-- TOC depthFrom:2 -->

- [Summary](#summary)
- [Features](#features)
- [Documentation](#documentation)
- [Citing](#citing)
- [Copyright and license](#copyright-and-license)

<!-- /TOC -->

<a id="markdown-summary" name="summary"></a>
## Summary

OGRe is a modern Mathematica package for tensor calculus, designed to be both powerful and user-friendly. It can be used in a variety of contexts where tensor calculations are needed, in both mathematics and physics, but it is especially suitable for general relativity.

Tensors are abstract objects, which can be represented as multi-dimensional arrays once a choice of index configuration and coordinate system is made. OGRe stays true to this definition, but takes away the complexities that come with combining tensors in different representations. This is done using an object-oriented programming approach, as detailed in the documentation.

The user initially defines each tensor in OGRe using its explicit components in any single representation. Operations on this tensor are then done abstractly, without needing to specify which representation to use. Possible operations include addition of tensors, multiplication of tensor by scalar, trace, contraction, and partial and covariant derivatives.

OGRe will automatically choose which representation to use for each tensor based on how the tensors are combined. For example, if two tensors are added, then OGRe will automatically use the same index configuration for both. Similarly, if two tensors are contracted, then OGRe will automatically ensure that the contracted indices are one upper (contravariant) and one lower (covariant). OGRe will also automatically transform all tensors being operated on to the same coordinate system.

Transformations between representations are done behind the scenes; all the user has to do is specify which metric to use for raising and lowering indices, and how to transform between the coordinate systems being used. This also means that there is no room for user error. The user cannot mistakenly perform "illegal" operations such as 2A<sup>&mu;&nu;</sup>+B<sub>&mu;&lambda;</sub>C<sub>&lambda;&nu;</sub>. Instead, the user simply inputs the names of the tensors, the order (but **not** the configuration) of indices for each, and the operations to perform - and the correct combination 2A<sup>&mu;&nu;</sup>+B<sup>&mu;</sup><sub>&lambda;</sub>C<sup>&lambda;&nu;</sup> will be automatically deduced.

I initially created OGRe for use in my own research, so I made it as flexible and powerful as possible. I also wanted my students to be able to use it easily and efficiently, even if they only have minimal experience with Mathematica and/or general relativity, so I made it simple to learn and easy to use. As a result, this package is equally suitable for both experienced and novice researchers.

<a id="markdown-features" name="features"></a>
## Features

* Define coordinate systems and the transformation rules between them. Tensor components are then transformed automatically between coordinates behind the scenes as needed.
* Each tensor is associated with a specific metric. Tensor components are then transformed automatically between different index configurations, raising and lowering indices behind the scenes as needed.
* Display any tensor in any index configuration and coordinate system, either in vector/matrix form or as a list of all unique non-zero elements.
* Automatically simplify tensor components, optionally with user-defined simplification assumptions.
* Export tensors to a Mathematica notebook or to a file, so they can later be imported into another Mathematica session without having to redefine them from scratch.
* Easily calculate arbitrary tensor formulas using any combination of addition, multiplication by scalar, trace, contraction, partial derivative, and covariant derivative.
* Built-in modules for calculating the Christoffel symbols (Levi-Civita connection), Riemann tensor, Ricci tensor and scalar, and Einstein tensor. More will be added in future versions.
* Fully portable. Can be imported directly from the web into any Mathematica notebook, without downloading or installing anything.
* Clear and detailed documentation, with many examples, in both [Mathematica notebook](OGRe_Documentation.nb) and [PDF](OGRe_Documentation.pdf) format. Detailed usage messages are also provided.
* Open source. The code is extensively documented; please feel free to fork and modify it as you see fit.
* Under active development. Please see the "future plans" section of the documentation for more information. Bug reports and feature requests are welcome, and should be made via GitHub issues.

<a id="markdown-documentation" name="documentation"></a>
## Documentation

The full and detailed documentation for this package may be found in the following repository files:

* `OGRe_Documentation.nb`: An interactive Mathematica notebook. Requires Mathematica to open.
* `OGRe_Documentation.pdf`: A PDF version of the notebook. Can be viewed with any PDF reader.

<a id="markdown-citing" name="citing"></a>
## Citing

If you use this package in your research, please cite it as follows:

* Barak Shoshany, OGRe: An Object-Oriented General Relativity Toolkit for Mathematica, [https://github.com/bshoshany/OGRe](https://github.com/bshoshany/OGRe) (2021).

(This citation will be replaced with a journal reference once a paper is published.)

<a id="markdown-copyright-and-license" name="copyright-and-license"></a>
## Copyright and license

Copyright (c) 2021 [Barak Shoshany](http://baraksh.com) (baraksh@gmail.com). Licensed under the [MIT license](LICENSE.txt).
