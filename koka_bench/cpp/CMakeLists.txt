set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

foreach (source IN ITEMS rbtree.cpp nqueens.cpp cfold.cpp deriv.cpp fib.cpp)
  get_filename_component(name "${source}" NAME_WE)
  set(name "cpp-${name}")

  add_executable(${name} ${source})
  if(source MATCHES "binarytrees.cpp") 
    target_link_libraries(${name} pthread)
  endif()

  add_test(NAME ${name} COMMAND ${name})
  set_tests_properties(${name} PROPERTIES LABELS cpp)
endforeach ()
