import Data.List

data Tree = Node String [Tree]
  deriving (Eq,Show)

getName :: Tree -> String
getName (Node name _) = name

getLinks :: Tree -> [Tree]
getLinks (Node _ links) = links

split :: Char -> String -> [String]
-- my usual split string function
split _ "" = []
split c s = [thisPart] ++ split c restOfString where
  (thisPart,tooLong) = break (==c) s
  restOfString = case tooLong of
    [] -> ""
    _ -> tail tooLong

prettyList :: (Show a) => [a] -> String
prettyList [] = ""
prettyList (x:xs) = (show x) ++ "\n" ++ prettyList xs

processLine :: String -> Tree
processLine s = Node thisName links where
  [thisName,restOfString] = split ':' s
  links = map (\x -> Node x []) (words restOfString)

buildTree :: Tree -> [Tree] -> Tree
buildTree oldTree [] = oldTree
buildTree startNode (t:ts) = buildTree (graft startNode t) ts

graft :: Tree -> Tree -> Tree
graft (Node startName startKids) (Node newName newKids)
  | startName == newName = Node startName (startKids ++ newKids)
  | otherwise = Node startName (map (\x -> graft x (Node newName newKids)) startKids)

showTree :: Int -> Tree -> String
showTree n (Node name kids) =
  (take n $ repeat ' ') ++ name ++ "\n" ++ (concatMap (showTree (n+1)) kids)

getYouTrees :: Tree -> [Tree]
getYouTrees (Node name kids)
  | name == "you" = [Node name kids]
  | otherwise = concatMap getYouTrees kids

countOuts :: Tree -> Int
countOuts (Node name kids)
  | name == "out" = 1
  | kids == [] = 0
  | otherwise = foldl1 (+) $ map countOuts kids

main = do
  myFile <- readFile "data11.txt"
  let myData = map processLine $ lines myFile

  let myTree = buildTree (head myData) (tail myData)
  
  --let youTree = head $ getYouTrees myTree
  
  putStrLn $ showTree 0 $ myTree
  --print $ countOuts youTree
