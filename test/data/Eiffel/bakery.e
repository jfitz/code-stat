class
    BAKERY

feature
    
    number_of_cakes : INTEGER
        -- A variable containing an integer

    name_of_my_favourite_cake : STRING
        -- A variable containing a string

    price_of_one_cake : REAL
        -- A variable containing a floating point number

    buy_cake (price : REAL; flavour : STRING)   -- A function accepting arguments
        do
            -- Some code here...
        end

    is_cake_available : BOOLEAN   -- A function returning true or false 
        do
            Result := number_of_cakes > 0
        end

end