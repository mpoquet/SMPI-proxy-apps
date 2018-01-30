
# ==================================================================================================
# This file is part of the CodeVault project. The project is licensed under Apache Version 2.0.
# CodeVault is part of the EU-project PRACE-4IP (WP7.3.C).
#
# Author(s):
#   Evghenii Gaburov <evghenii.gaburov@surfsara.nl>
#
# ==================================================================================================

# CMake project
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)
project("8_IO" NONE)

# ==================================================================================================

# Dwarf *: IO methods
message("--------------------")
message("Dwarf 8: I/O methods:")
message("--------------------")
set(DWARF_PREFIX 8_io) # The prefix of the name of the binaries produced

# Add the examples
add_subdirectory(basicMPIIO)
add_subdirectory(read2shmem)
add_subdirectory(markovchain)

# ==================================================================================================




