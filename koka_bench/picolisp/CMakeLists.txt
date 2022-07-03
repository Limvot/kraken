
set(copy_wrapper "../../copy_wrapper.sh")

set(sources picolisp-fib.l picolisp-fib-let.l)
foreach (source IN LISTS sources)

  get_filename_component(name "${source}" NAME_WE)

  set(out_dir     "${CMAKE_CURRENT_BINARY_DIR}/out/bench")
  set(out_path    "${out_dir}/${name}")

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${copy_wrapper} "${CMAKE_CURRENT_SOURCE_DIR}/${source}" ${out_dir} ${name}
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS "${out_path}")
  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")
endforeach ()

