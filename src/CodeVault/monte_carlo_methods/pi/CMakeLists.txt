# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

set (CMAKE_MODULE_PATH ~/simgrid/src/smpi/)

include(${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/common.cmake)

# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
  set(DWARF_PREFIX 7_montecarlo)
endif()
set(NAME_SERIAL ${DWARF_PREFIX}_pi_serial)
set(NAME_SMPI ${DWARF_PREFIX}_pi_mpi)


# C compiler settings

# find_package(Common)

# if (OPENMP_FOUND)
#     set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
#     add_executable(${NAME_OMP} pi_omp.c pi_shared.c)
#     install(TARGETS ${NAME_OMP} DESTINATION bin)
#     message("** Enabling '${NAME_OMP}': with OpenMP")
# else()
#      message("## Skipping 'pi_omp': no OpenMP support found")
# #     dummy_install(${NAME} "OpenMP")
# endif()

if(enable_smpi)
    set(CMAKE_C_COMPILER "~/simgrid/build/bin/smpicc")
    include_directories(BEFORE "~/simgrid/include/smpi")
    cmake_policy(SET CMP0003 OLD)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    add_executable(${NAME_SMPI} pi_mpi.c pi_shared.c)
    target_link_libraries(${NAME_SMPI} ${MPI_LIBRARIES} stdc++)
    install(TARGETS ${NAME_SMPI} DESTINATION bin)
    message("** Enabling '${NAME_SMPI}': with SMPI")
else()
     message("## Skipping '${NAME_SMPI}': no SMPI support found")
#     dummy_install(${NAME} "OpenMP")
endif()

add_executable(${NAME_SERIAL} pi_serial.c pi_shared.c)
message("** Enabling '${NAME_SERIAL}': serial version")

install(TARGETS ${NAME_SERIAL} DESTINATION bin)

unset(NAME)
# ==================================================================================================
