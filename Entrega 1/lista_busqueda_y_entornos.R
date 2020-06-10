

# Segun la documentación oficial en https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/search
# la funcion search() devuelve una lista de paquetes y "Objetos de R".

search()

# searchpaths() devuelve una lista con las rutas de dichos paquetes

searchpaths()

# Por ejemplo, en mi salida el 4 elemento de search() es "package:graphics"
# Y en correspondiente en searchpaths() es "/usr/lib/R/library/graphics/" la ruta del paquete en mi instalación en Linux

# Con ls() podemos ver los datos y funciones que he definido,
ls()

# Por ejemplo mi salida actual es
# > ls()
# [1] "dong1"         "dong1.alt"     "dong1.alt.t"   "dong1.alt.t.m"
# [5] "f_to_c"        "f_to_c_2"      "Inicio"
