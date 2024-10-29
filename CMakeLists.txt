# -----------------------------------------------------
# (*) brew install re2c ninja cmake

cmake_minimum_required(VERSION 3.19)
project(sbp VERSION 0.1.0 LANGUAGES C)

file(GLOB_RECURSE INCS "*.h")
file(GLOB_RECURSE SOURCES "*.re")

execute_process(
  COMMAND
    re2c "-o" "sbp.c" "sbp.re"
  )

execute_process(
  COMMAND
    lemon "sbp_grammar.y"
  )

execute_process(
  COMMAND
    re2c "-o" "rule.c" "rule.re"
  )

execute_process(
  COMMAND
    lemon "rule_grammar.y"
  )
  
find_package(PkgConfig)
pkg_search_module(GLIB REQUIRED glib-2.0)
pkg_search_module(JSON_GLIB REQUIRED json-glib-1.0)

  
add_executable(${CMAKE_PROJECT_NAME} sbp.c sbp_grammar.c rule.c rule_grammar.c ${INCS} ${SOURCES})

target_include_directories(${CMAKE_PROJECT_NAME}
   PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
   PUBLIC ${GLIB_INCLUDE_DIRS}
   PUBLIC ${JSON_GLIB_INCLUDE_DIRS}
   )
   
target_link_directories(${CMAKE_PROJECT_NAME}
   PUBLIC ${GLIB_LIBRARY_DIRS}
   PUBLIC ${JSON_GLIB_LIBRARY_DIRS}
   )
   
target_link_libraries(${CMAKE_PROJECT_NAME}  
	${JSON_GLIB_LIBRARIES})

IF (CMAKE_BUILD_TYPE STREQUAL Debug)
    ADD_DEFINITIONS(-DDEBUG)
ENDIF()


