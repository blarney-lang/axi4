# AXI4 library

This is a library for working with the AXI4 protocol in
[Blarney](https://github.com/blarney-lang/blarney).  It supports
development of AXI4 components (managers, subordinates, and stream
processors), interfacing with existing AXI4 components written in
other languages, and connecting AXI4 components together.  It is in
the early stages of development.

## Usage

To use the library, add `blarney` and `blarney-axi4` as package
dependenices to your project's `.cabal` file.  Also tell cabal where
to find these packages in your project's `cabal.project` file. See the
[AXI4 memory-mapped register
component](https://github.com/blarney-lang/mmreg) for a complete
example of how to do this.
