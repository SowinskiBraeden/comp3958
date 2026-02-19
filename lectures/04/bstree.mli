type bstree_size : 'a bstree -> int
val bstree_height : 'a bstree -> int
val bstree_empty : 'a bstree
val bstree_is_empty : 'a bstree -> bool
val bstree_insert : cmp:('a -> 'a -> int) -> 'a -> 'a bstree -> 'a bstree
val bstree_of_list : cmp:('a -> 'a -> int) -> 'a list -> 'a bstree
val bstree_mem : cmp:('a -> 'b -> int) -> 'a -> 'b bstree -> bool
val bstree_largest : 'a bstree -> 'a
val bstree_smallest : 'a bstree -> 'a
val bstree_delete : cmp:('a -> 'a -> int) -> 'a -> 'a bstree -> 'a bstree
