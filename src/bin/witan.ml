
let () =
  (* Parse command line options *)
  let opts = match Cmdliner.Term.eval (Options.all, Options.info) with
    | `Version | `Help -> exit 0
    | `Error `Parse
    | `Error `Term
    | `Error `Exn -> exit 1
    | `Ok opts -> opts
  in
  (* Parse input *)
  let statements = Input.read
      ?language:Options.(opts.input.language)
      ~dir:Options.(opts.input.dir)
      Options.(opts.input.file)
  in
  Gen.iter (fun s ->
      Format.printf "%a@." Dolmen.Statement.print s
    ) statements;
  (* TODO: Solve, ^^ *)

