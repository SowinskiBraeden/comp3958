type color = R | B

type 'a rbtree = L | N of color * 'a rbtree * 'a * 'a rbtree

let balance = function
  | B, N (R, N (R, a, x, b), y, c), z, d
  | B, N (R, a, x, N (R, b, y, c)), z, d
  | B, a, x, N (R, N (R, b, y, c), z, d)
  | B, a, x, N (R, b, y, N (R, c, z, d)) ->
    N (R, N (B, a, x, b), y, N (B, c, z, d))
  | c, l, x, r ->
    N (c, l, x, r)

let insert x t =
  let rec ins = function
    | L -> N (R, L, x, L)
    | N (c, l, y, r) when x < y ->
      balance (c, ins l, x, r)
    | N (c, l, y, r) when x > y ->
      balance (c, l, x, ins r)
    | _ -> t
  in
  ins t
