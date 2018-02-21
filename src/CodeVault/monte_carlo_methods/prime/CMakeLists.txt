# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

# find_package(MPI)      # Built-in in CMake
# find_package(SMPI)
set (CMAKE_MODULE_PATH ~/simgrid/src/smpi/)


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
    # include_directories(BEFORE "${CMAKE_HOME_DIRECTORY}/../../simgrid/include/smpi")
    set(CMAKE_C_COMPILER "/usr/local/bin/smpicc")
    include_directories(BEFORE "~/simgrid/include/smpi")
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

# set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_FLAGS}")

unset(NAME)
# ==================================================================================================
