import System.Random
import Data.List (intersperse)

heads = [
    "acceleration",
    "accelerate",
    "grow",
    "optimize",
    "establish",
    "implement",
    "build",
    "develop"]

searches = [
    "proprietary",
    "opensource",
    "reveal",
    "business",
    "marketing",
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
    "geo",
    "map",
    "database",
    "geofence",
    "text-mining",
    "machine learning",
    "data-mining",
    "integrity",
    "natural language processing",
    "recommendation",
    "information graphic (infographic)",
    "data visualization (dataviz)",
    "social network",
    "social media",

    -- Bullshit tools
    "graph",
    "NoSQL",
    "RDBMS",
    "MySQL",
    "Java",
    "MapReduce",
    "Hadoop",
    "Mongo",
    "C++",
    "Rails",
    "Python",
    "Javascript",
    "CSS",
    "HTML",
    "XML",
    "Node.js",
    "Backbone",
    "Scala",
    "Clojure",
    "jQuery",
    "MVVC",
    "MVC",
    "AJAX",
    "Linux",
    "PHP",
    "Design",
    "UI",
    "Python",
    "C#",
    "F#",
    "distributed systems",
    "multi-tier architectures",
    "SAS",
    "SSPS",
    "Stata",
    "MATLAB",
    "Hbase",
    "Hive",
    "Pig",
    "HQL",
    "Oracle",
    "SQL Server",
    "Access",
    "LDAP",
    "SOAP interface",
    "ReSTful interface",
    

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
