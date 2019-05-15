cmake_minimum_required(VERSION 3.10)

project(day15)

add_executable(day15 day15.cpp)
target_compile_options(day15 PUBLIC /permissive- /std:c++latest -std=c++17 /profile)
target_link_options(day15 PUBLIC /profile)