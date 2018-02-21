# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

# find_package(OpenMP)   # Built-in in CMake
# find_package(MPI)      # Built-in in CMake

set (CMAKE_MODULE_PATH ~/simgrid/src/smpi/)

include(${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/common.cmake)

# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
  set(DWARF_PREFIX 7_montecarlo)
endif()
# set(NAME_OMP ${DWARF_PREFIX}_integral1d_omp)
set(NAME_SERIAL ${DWARF_PREFIX}_integral1d_serial)
set(NAME_MPI ${DWARF_PREFIX}_integral1d_mpi)


# C compiler settings

# find_package(Common)
option(enable_smpi "Enable SMPI")

# if (OPENMP_FOUND)
#     set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
#     add_executable(${NAME_OMP} integral1d_OMP.c)
#     target_link_libraries(${NAME_OMP} m)
#     install(TARGETS ${NAME_OMP} DESTINATION bin)
#     message("** Enabling '${NAME_OMP}': with OpenMP")
# else()
#      message("## Skipping 'integral1d_OMP': no OpenMP support found")
# #     dummy_install(${NAME} "OpenMP")
# endif()

if (enable_smpi)
    set(CMAKE_C_COMPILER "/usr/local/bin/smpicc")
    include_directories(BEFORE "~/simgrid/include/smpi")
    cmake_policy(SET CMP0003 OLD)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    # include_directories(${MPI_INCLUDE_PATH})
    add_executable(${NAME_MPI} integral1d_mpi.c)
    target_link_libraries(${NAME_MPI} m ${MPI_LIBRARIES} stdc++)
    install(TARGETS ${NAME_MPI} DESTINATION bin)
    message("** Enabling '${NAME_MPI}': with SMPI")
else()
    message("## Skipping 'integral1d_smpi': no SMPI support found")
#    dummy_install(${NAME} "SMPI")
endif()

add_executable(${NAME_SERIAL} integral1d.c)
target_link_libraries(${NAME_SERIAL} m)
install(TARGETS ${NAME_SERIAL} DESTINATION bin)
message("** Enabling '${NAME_SERIAL}': serial version")


set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_FLAGS}")


# ==================================================================================================
