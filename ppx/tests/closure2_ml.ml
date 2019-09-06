let a = 1

let f c = c + a

let f' (a,c) = c + a
let f = fun c -> f' (a,c)


