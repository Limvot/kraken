set(sources rbtree.kk nqueens.kk nqueens-int.kk cfold.kk deriv.kk fib.kk)

set(koka koka)


foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name    "kk-${basename}")

  set(out_dir     "${CMAKE_CURRENT_BINARY_DIR}/out/bench")
  set(out_path    "${out_dir}/${name}")

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${koka} --target=c --stack=128M --outputdir=${out_dir} --buildname=${name} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS "${out_path}")
  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")
endforeach ()
