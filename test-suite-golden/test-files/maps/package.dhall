-- Covers flags and dependencies as maps, a map nested within a map and an
-- empty map.
{ name = "maps"
, flags = toMap
    { example = { description = "An example", manual = True, default = False } }
, dependencies = toMap { base = ">=4 && <5" }
, executables = toMap
    { example = { main = "Main.hs", dependencies = toMap { text = ">=2" } } }
, tests = [] : List { mapKey : Text, mapValue : { main : Text } }
}
