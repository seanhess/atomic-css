
OK, so....

hover needs to work with multiple classes:
    => hover (bg Green <> color Red)

but overriding selectors needs to work in a sane way
    -- use a monadic bind?
    -- this sure looks like one!
    setSelector $ \this $ a |> b >> this

-- they don't have to be directly serializable
-- they could be functions!

bg Green => {bg-green}
hover (bg Green) => \sel -> sel ': "hover"


-- I like the new stuff. Now you can't do setSelector (placeholder "woot")


Ok ok ok ... so... selector...


