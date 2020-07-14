Calypso - MHD dyanmo model in a rotating spherical shell
========================================================

About
-----
Calypso is a set of programs for MHD dynamo simulation in a rotating spherical
shell using spherical harmonics expansion methods.

The Calypso distribution consists of the following files and directories.

% ls
CMakeLists.txt  LICENSE         README          configure.in    examples/
INSTALL         Makefile.in     configure       doxygen/        src/

INSTALL: Brief installation procedure
LICENSE: License
README:  This file

configure.in:     Input file for autoconf
Makefile.in:      Template for Makefile used by configure
configure:        Configuration script
CMakeLists.txt:   Configuration file for CMake

bin:      Directory for programs (created by make)
doxygen:  Directory for source documentation 
examples: Directory for examples
src:      Directory for source files
work:     Work directory (created by make)


Installation
------------
Quick installation procedure is described in INSTALL file.
For detailed instllation see Install section in the document.

Running
-------

Calypso needs to run on MPI environment. 
Consequently, 'mpirun' command is necessary to invoke the simulation program.
The simplest way to start the program is the following;

% mpirun -np [# of MPI processes] [BINDIR]/sph_mhd

where, [# of MPI processes] is the number of MPI processes to run,
and [BINDIR] is the directory wher the program is built.




