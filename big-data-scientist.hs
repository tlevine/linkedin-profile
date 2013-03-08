import System.Random

management_nouns = [
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
    "acceleration"]

management_verbs = [
    "kickstart",
    "accelerate",
    "grow"]

management_adjectives =[]

programming_nouns = [
    "user interface (UI)",
    "user experience (UX)",
    "relational state transfer (ReST)",
    "SOAP",
    "backend",
    "frontend"]

programming_adjectives = [
    "extensible",
    "scalable",
    "responsive"]

data_nouns = [
    "d3",
    "hypothesis",
    "influence",
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
    "interactive"]

data_adjectives = [
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

buzzwords = concat [management_verbs, management_nouns, management_adjectives, programming_adjectives, programming_nouns, data_adjectives, data_nouns]

random_buzzword :: RandomGen g => [String] -> g -> (String, [String], g)
random_buzzword buzzwords seed = (buzzword, leftovers, g)
  where
    (number, g) = next seed
    index = number `mod` (length buzzwords)
    buzzword = buzzwords !! index
    (a,b) = splitAt index buzzwords
    leftovers = a ++ (tail b)

profile :: RandomGen g => [String] -> g -> [String]
profile [] seed = []
profile buzzwords seed = current : (profile next_buzzwords next_seed)
  where
    (current, next_buzzwords, next_seed) = random_buzzword buzzwords seed

main = do
  seed <- newStdGen 
  putStrLn $ show $ profile buzzwords seed
