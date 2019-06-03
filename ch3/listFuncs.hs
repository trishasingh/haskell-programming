module ListFuncs where

    x = "Trisha"

    
    x_cons = 't' : x  -- add char to beginning of string
    x_head = head x
    x_tail = tail x
    x_take = take 3 x -- substr starting at 0
    x_drop = drop 3 x -- removes first 3 chars
    x_concat = x ++ " " ++ "Singh"
    x_index = x !! 3 
