import System.Random
import Data.List (intersperse)

fascia = [
    "insight",
    "action",
    "return on investment (ROI)",
    "key performance indicator (KPI)",
    "decision",
    "app",
    "solution",
    "community",
    "customer",
    "discovery",
    "proprietary",
    "opensource",
    "lead generation",
    "acceleration",
    "kickstart",
    "accelerate",
    "grow",
    "hypothesis",
    "optimize",
    "establish",
    "synergy",
    "reveal",
    "business",
    "marketing",
    "validation",
    "influence"]

meat = [
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
    "artificial intelligence",
    "machine-leaning",
    "data-mining",
    "integrity",
    "natural language processing",
    "recommendation",
    "infographic",
    "information graphic",
    "dataviz",
    "data visualization",
    "discovery",
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
    "predictive",
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
  putStrLn "Fascia"
  putStrLn "--------"
  putStrLn $ concat $ intersperse "\n" $ sortRandom fascia seedA
  putStrLn ""
  putStrLn "Meat"
  putStrLn "--------"
  putStrLn $ concat $ intersperse "\n" $ sortRandom meat seedB
