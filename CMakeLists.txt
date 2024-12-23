cmake_minimum_required(VERSION 3.14)
project(swipl-pack-swiplite)

# Include swipl.cmake from the running SWI-Prolog's home
list(INSERT CMAKE_MODULE_PATH 0 $ENV{SWIPL_HOME_DIR}/cmake)
include(swipl)

# Create the library as a CMake module
add_library(swiplite MODULE c/swiplite.c)

# Link the library to SWI-Prolog.  This also removes the `lib` prefix
# from the target on systems that define a common library file prefix
target_link_swipl(swiplite)

find_package(SQLite3 REQUIRED)
target_link_libraries(swiplite PRIVATE ${SQLite3_LIBRARIES})
target_include_directories(swiplite PRIVATE ${SQLite3_INCLUDE_DIRS})

# Install the foreign target. `${swipl_module_dir}` contains the
# directory for installing modules for this architecture.

install(TARGETS swiplite
	DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/${swipl_module_dir})

# Run  tests.  This  is  executed   before    the   pack  is  installed.
# swipl_test(name) runs Prolog with the command line below.
#
#    swipl -p foreign=${CMAKE_CURRENT_SOURCE_DIR}/${swipl_module_dir} \
#          -p library=${CMAKE_CURRENT_SOURCE_DIR}/prolog \
#          --on-error=status \
#          -g test_${name} \
#	   -t halt \
#	   ${CMAKE_CURRENT_SOURCE_DIR}/test/test_${name}.pl
#
# This  implies  that  a  test  `name`  must    be  defined  in  a  file
# `test/test_${name}.pl`, which exports a  predicate `test_${name}`. The
# test succeeds if this predicate  succeeds   and  no error messages are
# printed.

enable_testing()
swipl_add_test(sqlite)
