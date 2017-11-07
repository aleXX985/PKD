type P = (String, String, String, Int, String)

updatePersonName :: P -> Int -> String -> P
updatePersonName (a, _, c, d, e) 1 ny = (a, ny, c, d, e)
updatePersonName (_, b, c, d, e) 2 ny = (ny, b, c, d, e)
updatePersonName p _ _  = p


