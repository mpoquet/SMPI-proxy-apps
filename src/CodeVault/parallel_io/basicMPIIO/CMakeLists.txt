
# ==================================================================================================
# This file is part of the CodeVault project. The project is licensed under Apache Version 2.0.
# CodeVault is part of the EU-project PRACE-4IP (WP7.3.C).
#
# Author(s):
#   Evghenii Gaburov <evghenii.gaburov@surfsara.nl>
#
# ==================================================================================================

# CMake project
cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)
project("Hermite4" NONE)


# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
  set(DWARF_PREFIX 8_io)
endif()
set(NAME ${DWARF_PREFIX}_basic_mpi_io)

enable_language(CXX)

include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
set(CXX11 ${COMPILER_SUPPORTS_CXX11})
set(CXX11_FLAGS -std=c++11)

set(CMAKE_INCLUDE_PATH ~/simgrid/src/smpi/)

if (NOT CXX11)
  message("## Skipping '${NAME}': no C++11 support")
  install(CODE "MESSAGE(\"${NAME} can only be built with C++11.\")")
  return()
endif()

if (SMPI_NOT_FOUND)
  message("## Skipping '${NAME}': no SMPI found")
  return()
endif()

include_directories(${MPI_INCLUDE_PATH})

set(cxx_flags "-Wall -Werror ${CXX11_FLAGS} -Wno-literal-suffix")


SET( CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )
SET( CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_COVERAGE_LINK_FLAGS}" )    

add_executable(${NAME} basic_mpi_io.cpp myMPI.cpp)
#target_link_libraries(${NAME} ${MPI_LIBRARIES})
set_target_properties(${NAME} PROPERTIES COMPILE_FLAGS ${cxx_flags})
install(TARGETS ${NAME} DESTINATION bin)

if (MPI_COMPILE_FLAGS)
  # PGI doesn't respect SMPI_COMPILE_FLAG. Need a guard for PGI, but for now comment
  #  set_property(TARGET ${NAME}  APPEND_STRING PROPERTY COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()

if (MPI_LINK_FLAGS)
  set_target_properties(${NAME} PROPERTIES LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()
  

# ==================================================================================================
