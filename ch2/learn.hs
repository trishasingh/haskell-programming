-- module intro
module Learn where
    -- First, we decide the name of our module so 
    -- it can be imported by name in a project
    -- Demonstrate that you can declare values in
    -- any order
    myResult = x * 5
    x = 10 * 5 + y
    y = 10

    foo x =
        let y = x * 2
            z = x ^ 2
        in 2 * y * z

    -- equivalent to
    foo2 x = 2 * x * 2 * x ^ 2

    -- exercises
    area x = 3.14 * x * x

    double x = x * 2

    a = 7
    b = 10
    c = a + b

    -- sectioning
    d = 5
    e = (1 -)
    f = e d
    g = (/1) 5
    h = (1/) 5

    -- laws for quotients and remainders
    i = (quot (-11) 2) * 2 + (rem (-11) 2) == (-11)
    j = (div (-11) 2) * 2 + (mod (-11) 2) == (-11)
