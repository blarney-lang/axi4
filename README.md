# AXI4 in Blarney

This is a library for working with the AXI4 protocol in
[Blarney](https://github.com/blarney-lang/blarney).  It will help you
develop your own AXI4 components (managers, subordinates, and stream
processors), along with the interconnect to needed to join them
together to form large systems.  It is in the early stages of
development.

## Usage

To use the library in your own project you need to add `blarney` and
`blarney-axi4` as package dependenices to your project's `.cabal`
file.  You also need to tell cabal where to find these packages in
your project's `cabal.project` file. See the [PIO
component](https://github.com/blarney-lang/pio) for complete example
of how to do this.
