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

meat = [
    -- Businessy
    "insight",
    "action",
    "return on investment (ROI)",
    "app",
    "solution",
    "community",
    "customer",
    "discovery",
    "proprietary",
    "opensource",
    "lead generation",
    "synergy",
    "reveal",
    "business",
    "marketing",
    "predictive",
    "validation",
    "influence",

    -- Computery
    "key performance indicator (KPI)",
    "decision",
    "hypothesis",
    "user interface (UI)",
    "user experience (UX)",
    "relational state transfer (ReST)",
    "backend",
    "frontend",
    "extensible",
    "scalable",
    "responsive",
    "cluster",
    "cloud",
    "geo",
    "map",
    "Bayes",
    "A/B test",
    "database",
    "geofence",
    "text-mining",
    "machine-leaning",
    "data-mining",
    "integrity",
    "natural language processing",
    "recommendation",
    "information graphic (infographic)",
    "data visualization (dataviz)",
    "social network",
    "social media",
    "graph",
    "NoSQL",
    "RDBMS",
    "business intelligence (BI)",
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
  putStrLn "Meat"
  putStrLn "--------"
  putStrLn $ concat $ intersperse "\n" $ sortRandom meat seedB
