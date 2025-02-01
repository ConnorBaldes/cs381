-- CS381 HW2 
-- Connor Baldes
-- baldesc@oregonstate.edu
-- 01/30/2023

module  HW2sol where
import HW2types
sname = "Connor Baldes"

ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x, 1)] -- If the input multiset is empty, add the new element with a count of 1
ins x (y:ys)
  | x == fst y = (x, snd y + 1) : ys -- If the element x is already in the multiset, increment its count
  | otherwise = y : ins x ys -- If x is not in the multiset, add it to the end of the list

del :: Eq a => a -> Bag a -> Bag a
del _ [] = [] -- If the multiset is empty, return an empty list
del x (y:ys)
  | x == fst y = if snd y == 1 then ys -- If the count of the element x is 1, remove it from the multiset
                 else (x, snd y - 1) : ys -- If the count of the element x is greater than 1, decrement its count
  | otherwise = y : del x ys -- If x is not in the multiset, leave the list unchanged

bag :: Eq a => [a] -> Bag a -- Create a bag data type from a list
bag [] = [] -- If the list is empty, return an empty bag
bag (x:xs) = ins x (bag xs) -- For a non-empty list, insert the first element into the bag created from the rest of the elements

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] ys = True -- Empty bags are always a subbag of any bag, including empty bags!
subbag ((x, nx):xs) ys = -- If we find the same element in the second bag, make sure it has enough of them and continue checking for the rest
  case lookup x ys of
    Just ny -> nx <= ny && subbag xs ys
    Nothing -> False -- If the current element isn't in the second bag at all, this bag can't be a subbag of the other one!


isSet :: Eq a => Bag a -> Bool
isSet [] = True -- If the bag is empty, it is a set
isSet ((x,n):xs)
  | n /= 1 = False -- If an element occurs more than once, it is not a set
  | otherwise = isSet xs -- If the current element occurs only once, check the rest of the bag

-- This line defines a function "size" that takes a "Bag a" as input and returns an "Int".
-- It calculates the sum of all "n" (the number of occurences of elements in the bag) by filtering through a list comprehension.
size :: Bag a -> Int
size bag = sum [n | (_, n) <- bag]

-- This line defines a function "nodes" that takes a "Graph" as input and returns a list of "Node".
-- It calculates the unique nodes in the graph by concatenating the list of first elements and second elements of the edges in the graph, and normalizing the result.
nodes :: Graph -> [Node]
nodes g = norm (map fst g ++ map snd g)

-- This line defines a function "suc" that takes a "Node" and a "Graph" as input and returns a list of "Node".
-- It calculates the list of successors for the given node by filtering through the edges in the graph and keeping the second element of each edge where the first element matches the given node.
suc :: Node -> Graph -> [Node]
suc node graph = [b | (a, b) <- graph, a == node]

-- This line defines a function "detach" that takes a "Node" and a "Graph" as input and returns a "Graph".
-- It calculates the graph after removing a given node and its incident edges by filtering through the edges in the graph and keeping only those where neither the first nor the second element matches the given node.
detach :: Node -> Graph -> Graph
detach node graph = [(x, y) | (x, y) <- graph, x /= node, y /= node]

-- This line defines a function "cyc" that takes an "Int" as input and returns a "Graph".
-- It creates a cycle of the given size by creating a list of edges where each node is connected to the next node in the list, with the last node connected to the first node.
cyc :: Int -> Graph
cyc n = [(i, (i `mod` n) + 1) | i <- [1..n]]

-- Returns the width of a shape, with 0 for a point, 2 * radius for a circle, and w 
width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = 2 * r
width (Rect _ w h) = w

-- Returns the bounding box of a shape as a pair of points.
bbox :: Shape -> BBox
bbox (Pt p) = (p, p) -- The bounding box of a circle is a square centered at the center of the circle with side length of 2 * radius.
bbox (Circle (x, y) r) = ((x-r, y-r), (x+r, y+r)) -- The bounding box of a rectangle is a rectangle centered at the bottom left corner of the original rectangle.
bbox (Rect (x, y) w h) = ((x, y), (x+w, y+h))

-- Returns the minimum X value of a shape.
minX :: Shape -> Number
minX (Pt (x, _)) = x
minX (Circle (x, _) _) = x
minX (Rect (x, _) _ _) = x

-- Returns a shape that is moved by a given distance.
move :: Shape -> Point -> Shape
move (Pt p) (dx, dy) = Pt (addPt p (dx, dy)) -- A point can be moved by adding the displacement to its X and Y values.
move (Circle p r) (dx, dy) = Circle (addPt p (dx, dy)) r -- A circle can be moved by adding the displacement to its center X and Y values.
move (Rect p w h) (dx, dy) = Rect (addPt p (dx, dy)) w h -- A rectangle can be moved by adding the displacement to its bottom left X and Y values.

-- Adds two points together to get a new point.
addPt :: Point -> Point -> Point
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
