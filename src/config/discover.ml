module C = Configurator.V1

let () =
  C.main ~name:"theora-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = ["-ltheoraenc"; "-ltheoradec"]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
          | None -> default
          | Some pc -> (
              match C.Pkg_config.query pc ~package:"theoradec theoraenc" with
                | None -> default
                | Some deps -> deps)
      in
      C.Flags.write_sexp "c_flags.sexp" ("-fPIC" :: conf.cflags);
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
