set(sources rbtree.kp rbtree-opt.kp nqueens.kp cfold.kp deriv.kp fib.kp fib-let.kp)

set(kraken "../../kraken_wrapper.sh")


foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name    "kraken-${basename}")

  set(out_dir     "${CMAKE_CURRENT_BINARY_DIR}/out/bench")
  set(out_path    "${out_dir}/${name}")

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${kraken} "${CMAKE_CURRENT_SOURCE_DIR}/${source}" ${out_dir} ${name}
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS "${out_path}")
  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")
endforeach ()
