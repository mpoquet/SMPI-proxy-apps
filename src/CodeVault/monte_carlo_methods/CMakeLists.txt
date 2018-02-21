
# ==================================================================================================
# This file is part of the CodeVault project. The project is licensed under Apache Version 2.0.
# CodeVault is part of the EU-project PRACE-4IP (WP7.3.C).
#
# Author(s):
#   Samet Demir (@itu)
#
# ==================================================================================================

# CMake project
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)
project("7_montecarlo")
include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/common.cmake)

# ==================================================================================================

# Dwarf 7: Monte Carlo
message("--------------------")
message("Dwarf 7: Monte Carlo:")
message("--------------------")
set(DWARF_PREFIX 7_montecarlo) # The prefix of the name of the binaries produced

# Add the examples
# add_subdirectory(pi)
# add_subdirectory(prng)
add_subdirectory(integral_basic)
add_subdirectory(prime)

# ==================================================================================================




