import System.Random
import Data.List (intersperse)

heads = [
    "accelerate",
    "optimize",
    "establish",
    "implement",
    "build",
    "develop"]

searches = [
    "proprietary",
    "opensource",
    "predictive",
    "validation",
    "influence",
    "decision",
    "user interface (UI)",
    "user experience (UX)",
    "backend",
    "frontend",
    "extensible",
    "scalable",
    "responsive",
    "cluster",
    "cloud",
    "geospatial",
    "mapping",
    "database",
    "text-mining",
    "machine learning",
    "data-mining",
    "integrity",
    "recommendation",
    "information graphic",
    "data visualization",
    "social network",
    "social media",

    -- Bullshit tools that I don't know
    "graph",
    "NoSQL",
    "MySQL",
    "SQL Server",
    "Java",
    "MapReduce",
    "Hadoop",
    "Rails",
    "distributed systems",
    "multi-tier architectures",
    "SAS",
    "MATLAB",
    "Oracle",
    "object-oriented",

    "integration",
    "interactive",
    "realtime",
    "qualitative",
    "quantitative",
    "structured",
    "unstructured",
    "statistical",
    "supervised",
    "unsupervised",
    "semantic"]

sample :: RandomGen g => [String] -> g -> (String, [String], g)
sample buzzwords seed = (buzzword, leftovers, g)
  where
    (number, g) = next seed
    index = number `mod` (length buzzwords)
    buzzword = buzzwords !! index
    (a,b) = splitAt index buzzwords
    leftovers = a ++ (tail b)

sortRandom :: RandomGen g => [String] -> g -> [String]
sortRandom [] seed = []
sortRandom buzzwords seed = current : (sortRandom next_buzzwords next_seed)
  where
    (current, next_buzzwords, next_seed) = sample buzzwords seed

main = do
  seedA <- newStdGen 
  seedB <- newStdGen 
  putStrLn "Heads"
  putStrLn "--------"
  putStrLn $ concat $ intersperse "\n" $ sortRandom heads seedA
  putStrLn ""
  putStrLn "Searches"
  putStrLn "--------"
  putStrLn $ concat $ intersperse "\n" $ sortRandom searches seedB
