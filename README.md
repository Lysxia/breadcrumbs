Persistent Bounded Buffers
==========================

Various persistent implementations of finite sequences/circular buffers, where
inserting an element pushes the oldest one out.

With a buffer size of 3:

                    ""
    push 's'
                    "s"
    push 'n'
                    "ns"
    push 'o'
                    "ons"
    push 'm'
                    "mon"
    push 'e'
                    "emo"
    push 'l'
                    "lem"

More formally, a `Buffer` is a *persistent* data structure supporting the
following operations:

```haskell
-- Create an empty buffer with size n.
empty :: Int -> Buffer a

-- Insert an element.
push :: a -> Buffer a -> Buffer a

-- Access the last n inserted elements,
-- or less if not that many have been inserted.
bufferToList :: Buffer a -> [a]
```

Implementations
---------------

- `List`: A naive implementation with a simple list which consumes arbitrarily
  more memory than necessary.

- `DropList`: A less naive implementation which truncates the list just enough,
  at a cost.

- `Seq`: Based on `Data.Sequence` (finite sequences, implemented with finger
  trees).

- `Deque`: The simple purely functional deque. Amortized constant time insertion.

- `Flips`: A purely functional deque-like structure exploiting the boundedness
  of the buffer to achieve worst-case constant time insertion.

- `Breadcrumbs`: A variant of the
  ["tree buffers"](https://arxiv.org/abs/1504.04757) algorithm that exploits
  automatically managed memories to clean up a list-like structure.

- `IO`: An interface of `Breadcrumbs` that does not lie about its impurity.
  One may sometimes access more elements than they asked, if they haven't been
  garbage-collected.

- `Internal`: Internals of `Breadcrumbs`.

