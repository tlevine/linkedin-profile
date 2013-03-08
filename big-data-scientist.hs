import qualified Data.Set as S

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

buzzwords = S.fromList $ concat [management_verbs, management_nouns, management_adjectives, programming_adjectives, programming_nouns, data_adjectives, data_nouns]

main = do
  putStrLn $ show $ buzzwords