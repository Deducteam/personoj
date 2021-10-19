Source: lambdapi-pvs
Section: misc
Priority: optional
Homepage: https://github.com/Deducteam/personoj
Standards-Version: 3.9.2

Package: lambdapi-pvs
Version: 1.0
Maintainer: Gabriel Hondet <gabriel (dot) hondet (at) inria (dot) fr>
# Pre-Depends: <comma-separated list of packages>
Depends: zlib1g-dev,libx11-dev,libgmp-dev,bubblewrap,m4,gcc,autoconf,make,unzip,pkg-config,git,rsync,bmake,ksh,c-compiler,sbcl
Recommends: emacs-nox
# Suggests: <comma-separated list of packages>
# Provides: <comma-separated list of packages>
# Replaces: <comma-separated list of packages>
# Architecture: all
# Multi-Arch: <one of: foreign|same|allowed>
# Copyright: BSD3
# Changelog: <changelog file; defaults to a generic changelog>
# Readme: <README.Debian file; defaults to a generic one>
# Extra-Files: <comma-separated list of additional files for the doc directory>
# Links: <pair of space-separated paths; First is path symlink points at, second is filename of link>
# Files: <pair of space-separated paths; First is file to include, second is destination>
#  <more pairs, if there's more than one file to include. Notice the starting space>
Description: Meta package to install dependencies for lambdapi and PVS
 This package gathers the dependencies required to export theories from PVS to
 Dedukti.
