# This is the CMake build file for the chgres_cube utility in the
# UFS_UTILS package.
#
# George Gayno, Mark Potts, Kyle Gerheiser

set(lib_src
    weights4rhs_mod.F90)

set(exe_src genweights.F90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -fdefault-real-8")
endif()

set(exe_name oiprep)

add_library(wts4rh_lib STATIC ${lib_src})
add_executable(${exe_name} ${exe_src})

set(mod_dir "${CMAKE_CURRENT_BINARY_DIR}/mod")
set_target_properties(wts4rh_lib PROPERTIES Fortran_MODULE_DIRECTORY ${mod_dir})
target_include_directories(wts4rh_lib INTERFACE ${mod_dir})

target_link_libraries(
  wts4rh_lib
  PUBLIC
  esmf
  NetCDF::NetCDF_Fortran)

target_link_libraries(${exe_name} PRIVATE wts4rh_lib)

install(TARGETS ${exe_name})

# If doxygen documentation we enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)
endif()
