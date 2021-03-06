# Packages are optional: if they are not present, certain code samples are not compiled
cmake_minimum_required(VERSION 2.8.10 FATAL_ERROR)

set (CMAKE_MODULE_PATH ~/simgrid/src/smpi/)

include(${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/common.cmake)

# ==================================================================================================

if ("${DWARF_PREFIX}" STREQUAL "")
  set(DWARF_PREFIX 8_unstructured)
endif()

set(NAME ${DWARF_PREFIX}_haloex)

# C compiler settings

# find_package(Common)

set(CMAKE_C_COMPILER "$ENV{SIMGRID_PATH}/bin/smpicc")
include_directories(BEFORE "~/simgrid/include/smpi")
cmake_policy(SET CMP0003 OLD)
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
add_executable(${NAME} main.c configuration.c mpicomm.c mpitypes.c box.c field.c mesh.c)
target_link_libraries(${NAME} ${MPI_LIBRARIES} -lm stdc++)
install(TARGETS ${NAME} DESTINATION bin)
message("** Enabling '${NAME}': with SMPI")

unset(NAME)
# ==================================================================================================
