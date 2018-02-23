# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

# find_package(OpenMP)   # Built-in in CMake

include(${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/common.cmake)

# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
  set(DWARF_PREFIX 7_montecarlo)
endif()
# set(NAME_OMP ${DWARF_PREFIX}_prng_omp)
set(NAME_SERIAL ${DWARF_PREFIX}_prng_serial)
set(NAME_SMPI ${DWARF_PREFIX}_prng_mpi)


# C compiler settings

# find_package(Common)

# if (OPENMP_FOUND)
#     set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
#     add_executable(${NAME_OMP} prng_omp.c prng_shared.c)
#     install(TARGETS ${NAME_OMP} DESTINATION bin)
#     message("** Enabling '${NAME_OMP}': with OpenMP")
# else()
#      message("## Skipping 'prng_omp': no OpenMP support found")
# #     dummy_install(${NAME} "OpenMP")
# endif()

if(enable_smpi)
    set(CMAKE_C_COMPILER "$ENV{SIMGRID_PATH}/bin/smpicc")
    include_directories(BEFORE "$ENV{SIMGRID_PATH}/include")
    cmake_policy(SET CMP0003 OLD)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    # include_directories(${MPI_INCLUDE_PATH})
    add_executable(${NAME_SMPI} prng_mpi.c prng_shared.c)
    target_link_libraries(${NAME_SMPI} ${MPI_LIBRARIES} stdc++)
    install(TARGETS ${NAME_SMPI} DESTINATION bin)
    message("** Enabling '${NAME_SMPI}': with SMPI")
else()
     message("## Skipping '${NAME_SMPI}': no SMPI support found")
#     dummy_install(${NAME} "OpenMP")
endif()

add_executable(${NAME_SERIAL} prng_serial.c prng_shared.c)
message("** Enabling '${NAME_SERIAL}': serial version")

install(TARGETS ${NAME_SERIAL} DESTINATION bin)

unset(NAME)
# ==================================================================================================
