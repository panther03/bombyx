cmake_minimum_required(VERSION 3.15)

# Project name
project(bombyx-cc)

# Platform-specific default install path
set(DEFAULT_OPENCILK_HOME "/opt/OpenCilk") # Linux/macOS default

set(REL_OPENCILK_HOME "${DEFAULT_OPENCILK_HOME}" CACHE PATH "OpenCilk installation root directory")

# Convert to absolute path and verify
get_filename_component(OPENCILK_HOME "${REL_OPENCILK_HOME}" ABSOLUTE)

# Check if the directory exists (or can be created)
if(NOT EXISTS "${OPENCILK_HOME}")
    message(FATAL_ERROR 
        "OPENCILK_HOME directory '${OPENCILK_HOME}' does not exist.\n"
        "Please specify a valid path with -DOPENCILK_HOME=/path/to/opencilk."
    )
else()
    message("-- Using OpenCilk installation at ${OPENCILK_HOME}")
endif()

# Set the CMake prefix path to include custom LLVM and Clang installation paths
set(CMAKE_PREFIX_PATH "${OPENCILK_HOME}/lib/cmake")

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -g -fsanitize=address")

# Specify the custom LLVM and Clang installation path
set(LLVM_DIR "${OPENCILK_HOME}/lib/cmake/llvm")
set(CLANG_DIR "${OPENCILK_HOME}/lib/cmake/clang")

# Find LLVM and Clang
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

# Add LLVM headers
include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})
include_directories("${OPENCILK_HOME}/include")
# Link directories
link_directories("${OPENCILK_HOME}/lib")

# Add LLVM and Clang libraries
add_definitions(${LLVM_DEFINITIONS})

# Export compile commands for Clang tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories("include")
# Source file
set(SOURCE_FILES
    src/main.cpp
    src/IR.cpp
    src/OpenCilk2IR.cpp
    src/MakeExplicit.cpp
    src/Cilk1EmuTarget.cpp
)

# Add executable
add_executable(bombyx-cc ${SOURCE_FILES})

set_target_properties(bombyx-cc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Link against LLVM and Clang libraries
target_link_libraries(bombyx-cc PRIVATE
    LLVMCore
    clangTooling
    clangFrontend
    clangSerialization
    clangDriver
)


get_filename_component(CILK_INSTALL_INCLUDE_DIR "${OPENCILK_HOME}/include" ABSOLUTE)
get_filename_component(CILK_INSTALL_BIN_DIR "${OPENCILK_HOME}/bin" ABSOLUTE)

install(
    TARGETS bombyx-cc
    RUNTIME   
)
install(FILES support/cilk_explicit.hh DESTINATION include/)