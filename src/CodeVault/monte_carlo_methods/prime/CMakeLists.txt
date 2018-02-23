# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

include(${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/common.cmake)
# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
set(DWARF_PREFIX 7_montecarlo)
endif()

set(NAME ${DWARF_PREFIX}_prime)

# C compiler settings

# find_package(Common)

option(enable_smpi "Enable SMPI")

if(enable_smpi)
    set(CMAKE_C_COMPILER "$ENV{SIMGRID_PATH}/bin/smpicc")
    include_directories(BEFORE "$ENV{SIMGRID_PATH}/include")
    cmake_policy(SET CMP0003 OLD)
    include_directories(${MPI_INCLUDE_PATH})
    add_executable(${NAME} prime.c)
    target_link_libraries(${NAME} ${MPI_LIBRARIES} stdc++ gmp)
    install(TARGETS ${NAME} DESTINATION bin)
    message("** Enabling '${NAME}': with SMPI")
 else()
     message("## Skipping '${NAME}': no SMPI support found")
#    dummy_install(${NAME} "MPI")
 endif()

unset(NAME)
# ==================================================================================================
