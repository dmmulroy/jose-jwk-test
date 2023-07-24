open Jose

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test () =
  let priv_pem = read_file "test_private_key.pem" in
  let jwk = Jwk.of_priv_pem priv_pem |> Result.get_ok in
  let header = Header.make_header jwk in
  let jwt = Jwt.sign ~header ~payload:Jwt.empty_payload jwk |> Result.get_ok in
  match Jwt.validate_signature ~jwk jwt with
  | Ok _jwt -> Format.eprintf "success@."
  | Error `Invalid_signature -> Format.eprintf "ERR(0): Invalid Signature@."
  | Error (`Msg msg) -> Format.eprintf "ERR(1): %s@." msg

let () = test ()
