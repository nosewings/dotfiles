def schwarzian(f, x):
    return (
        f.derivative(x, 3)/f.derivative(x) -
        (3/2)*(f.derivative(x, 2)/f.derivative(x))**2
    )

%colors linux
