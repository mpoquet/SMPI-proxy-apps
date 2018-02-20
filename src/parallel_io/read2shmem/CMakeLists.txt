# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

find_package(MPI)      # Built-in in CMake

# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
  set(DWARF_PREFIX 8_io)
endif()

set(NAME ${DWARF_PREFIX}_read2shmem)

# C compiler settings

set(CMAKE_INCLUDE_PATH ~/simgrid/src/smpi/)

if (SMPI_FOUND)
    cmake_policy(SET CMP0003 OLD)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    include_directories(${SMPI_INCLUDE_PATH})
    add_executable(${NAME} read2shmem.c)
    target_link_libraries(${NAME} ${MPI_LIBRARIES} stdc++)
    install(TARGETS ${NAME} DESTINATION bin)
    message("** Enabling '${NAME}': with MPI")
else()
    message("## Skipping '${NAME}': no MPI support found")
#    dummy_install(${NAME} "MPI")
endif()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_FLAGS}")

unset(NAME)
# ==================================================================================================
