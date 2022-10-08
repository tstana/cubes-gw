# Notice of Deprecation

This folder was intended to hold the files for gateware (logic running on the
FPGA) for the CUBES payload on the MIST satellite.

Since the start of the CUBES project, both the ASIC and the FPGA used on CUBES
have changed.

As a result, this repository is now _deprecated_.

The most up-to-date gateware code for CUBES can be found on Dropbox:

- https://www.dropbox.com/sh/cmfis964frhkbvq/AAD4gc_wlUnyZN-tQpHhtea9a?dl=0

## What this repository does contain

The contents of this repository demonstrate how to interface to a SIPHRA ASIC
from Integrated Detector Electronics AS (IDEAS).

- [SIPHRA ASIC on IDEAS website](https://ideas.no/products/ide3380/)

VHDL code that was capable of configuring the ASIC and retrieving data for pulse
height spectometry from it can be found in this repository. Communication to the
ASIC is performed via SPI.
