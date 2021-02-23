open SCaml

let [@entry] main _param _storage =
  (* These zeros and ones are obtained from
     tests_python/tests_alpha/test_contract_bls12_381.py
  *)
(*
  let g1_zero = G1Bytes "0x400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" in
  let g2_zero = G2Bytes "0x400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" in
  let fr_zero = FrBytes "0x00" in
*)
  let g1_one = G1Bytes "0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1" in
  let g2_one = G2Bytes "0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb80606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801" in
(*
  let fr_one = FrBytes "0x01" in
*)

(*
   sk : Fr = random()   # secret key
   pk : G2 = sk * Q      # public key
   hash : G1 = random()  # "hash"
   sg : G1 = hash * sk
   args = [(hash, pk), (-sg, Q)]

   hash = h * P
   e(hash,pk) * e(-sg,Q) = e(h * P,pk) * e(-sg,Q)
                         = e(h * P,sk * Q) * e(-h * sk * P, Q)
                         = e(P,Q) ^ h*sk * e(P,Q)  ^ -(h*sk)
                         = e(P,Q) ^ (h*sk - h*sk)
                         = 1
*)

  let sk = FrBytes "0x012345" in
  let pk = BLS12_381.G2.( * ) g2_one sk in
  let hash = BLS12_381.G1.( * ) g1_one (FrBytes "0x789012") in
  let sg = BLS12_381.G1.( * ) hash sk in
  let a1 = (hash, pk) in
  let a2 = (BLS12_381.G1.(~-) sg, g2_one) in
  assert (BLS12_381.pairing_check [a1; a2]);
  [], ()
