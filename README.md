
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgpeanuts <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://leonawicz.github.io/blog/)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<image class="orcid" src="https://members.orcid.org/sites/default/files/vector_iD_icon.svg" height="16"></a>
[![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/leonawicz/pkgpeanuts)
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![Travis-CI Build
Status](https://travis-ci.org/leonawicz/pkgpeanuts.svg?branch=master)](https://travis-ci.org/leonawicz/pkgpeanuts)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/pkgpeanuts?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/pkgpeanuts)
[![Coverage
Status](https://img.shields.io/codecov/c/github/leonawicz/pkgpeanuts/master.svg)](https://codecov.io/github/leonawicz/pkgpeanuts?branch=master)

The intent of `pkgpeanuts` is to automate building out robust R package
scaffolding for a newly created package. Create a new R package project
in RStudio. Make an initial commit and link up the local repository with
your empty remote repository. Then let `pkgpeanuts` take care of the
rest.

This package is in early development. It is not intended for operating
on existing, developed packages; only new packages as newly created
RStudio projects. It also does not work yet for creating new packages
itself (difficulties with git and Windows), hence the instructions above
about creating a new project in RStudio. *Use at your own risk and/or
read the source code.*

## Installation

You can install the development version of `pkgpeanuts` from GitHub
with:

``` r
# install.packages("remotes")
remotes::install_github("leonawicz/pkgpeanuts")
```

## Package setup

After creating a new package in RStudio, make sure the new, empty
repository exists on GitHub (or BitBucket) as well. Make an initial
commit, e.g. commit the `.Rproj` file, so that you can add and push to
the remote repository. If this has not been done, `pkgpeanuts` will
likely fail. Its use will become less problematic later, but currently
there are issues yet to be worked out.

Once this minimal setup is complete, call `pkgpeanuts::pour` from the
package root directory with your desired arguments. I suggest not doing
this, however. Instead, use the RStudio addin.

`pkgpeanuts` has many arguments. See the help documentation for details.
And again, this is not intended for existing packages.

## Motivation

The primary motivation is for the convenient RStudio addin.

At each of my last two workplaces I have created workplace
context-specific renditions of this package. This has enabled me to spin
up new packages at work quickly and still have them conforming to
professional standards and looking polished, despite limited time. By
avoiding manually fussing with extraneous details that can be automated,
I can focus on the fun part: building out the core package content.
`pkgpeanuts` represents a generalization of my previous packages,
stripping away code related to specific contexts. By design it must try
to achieve less than my other packages did, but if generalized
appropriately, could be useful to the R community.

This is largely a wrapper around a plethora of `usethis` functions, plus
additional functions from `pkgpeanuts`. `usethis` has also received a
number of significant developments recently, which has led to some new
simplifications of things I no longer need to do myself when making
packages.
