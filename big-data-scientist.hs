import System.Random

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
    "business intelligence (BI)",
    "lead generation",
    "acceleration",
    "kickstart",
    "accelerate",
    "grow",
    "hypothesis",
    "influence"]

meat = [
    "user interface (UI)",
    "user experience (UX)",
    "relational state transfer (ReST)",
    "SOAP",
    "backend",
    "frontend",
    "extensible",
    "scalable",
    "responsive",
    "d3",
    "cluster",
    "cloud",
    "geo",
    "map",
    "GIS",
    "Bayes",
    "A/B test",
    "validation",
    "database",
    "geofence",
    "text-mining",
    "artificial intelligence",
    "machine-leaning",
    "data-mining",
    "integrity",
    "validation",
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
  putStrLn $ show $ sortRandom fascia seedA
  putStrLn $ show $ sortRandom meat seedA
