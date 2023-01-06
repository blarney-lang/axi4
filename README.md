# An AXI4 library for Blarney

This is a library for working with the AXI4 protocol in
[Blarney](https://github.com/blarney-lang/blarney).  It will help you
develop your own AXI4 components (managers, subordinates, and stream
processors), interface with existing AXI4 components written in other
languages, and provide interconnect to build systems comprising
multiple components.  It is in the early stages of development.

## Usage

To use the library in your own project you need to add `blarney` and
`blarney-axi4` as package dependenices to your project's `.cabal`
file.  You also need to tell cabal where to find these packages in
your project's `cabal.project` file. See the [PIO
component](https://github.com/blarney-lang/pio) for complete example
of how to do this.
