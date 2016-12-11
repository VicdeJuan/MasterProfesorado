# Apuntes Ingeniería Informática - Matemáticas

Esta es una colección de apuntes y trabajos del Máster de Profesorado - especialidad Matemáticas de la URJC. Están disponibles libremente ([licencia CC-BY-NC-SA](#licencia)). Si encuentras algún fallo o quieres contribuir, puedes mandarnos un pull request o abrir un [_issue_](https://github.com/VicDeJuan/MasterProfesorado/issues) en Github.

La documentación de los paquetes está en el [manual en PDF](https://github.com/VicdeJuan/Apuntes/raw/master/Cosas%20guays%20LaTeX/Manual/_Manual.pdf), junto con una introducción a LaTeX básico y algunas pinceladas de cosas un poco más avanzadas.



## Autores
* [Gustavo Martínez](http://github.com/VicDeJuan)


### Contribuidores

La plantilla básica, la clase de LaTeX, el manual y todo lo que no es el texto en sí ha sido desarrollado por  [Guillermo Julián Moreno](http://github.com/gjulianm) para los [Apuntes Mates-Informática](http://github.com/vicdejuan/Apuntes)

## Detalles técnicos de LaTeX

### Compilación

Los apuntes necesitan los archivos en la carpeta `Cosas Guays LaTeX` para compilar (hay clases de documentos y algunos paquetes). Se pueden o bien instalar directamente en el sistem usando el script `install` del directorio o bien copiarlos al directorio del archivo `.tex` para que el compilador los reconozca.

Es recomendable también usar `latexmk` para compilar de forma continua los apuntes mientras se editan. El comando completo sería

```
latexmk -shell-escape -synctex=1 -pdf -silent -interaction=nonstopmode -pvc <archivo .tex>
```

Sobre los argumentos: `-shell-escape` permite crear subprocesos de LaTeX para compilar archivos auxilares, `-synctex=1` genera un archivo _synctex.gz_ que permite a ciertos lectores de PDF (Skim en OS X, Okular en Linux) relacionar una posición en el PDF con la parte correspondiente del _tex_, `-silent` reduce la salida del compilador, `-interaction=nonstopmode` evita que el compilador se pare cuando se encuentra un error, y por último `-pvc` habilita la compilación continua cuando se guarda el archivo _tex_.

### Paquete extendido de comandos

En el directorio `Cosas Guays LaTeX` se encuentran varios paquetes y clases para facilitar la escritura de apuntes y mantener un formato coherente. Los comentarios explican qué hace cada comando, y también hay un [manual en PDF](https://github.com/VicdeJuan/Apuntes/raw/master/Cosas%20guays%20LaTeX/Manual/_Manual.pdf).

### Sugerencias de comandos (TeXstudio - LatexCwl en Sublime)

Para facilitar las cosas, hay un script `cwlmaker.py` que genera un archivo `.cwl` a partir de los comandos declarados en un fichero de LaTeX. Este archivo permite a [TeXstudio](http://texstudio.sourceforge.net/) dar sugerencias de los comandos que hemos creado.

El archivo `.cwl` también funciona en Sublime Text si se tienen instalados los plugins Latexing y LaTeX-cwl. El _script_ de instalación de los paquetes extras (`install`) copia automáticamente el `.cwl` al directorio de LaTeX-cwl en OS X / Sublime Text 3. Si usas otro sistema, añade la ruta correspondiente en el [_script_](https://github.com/VicdeJuan/Apuntes/blob/master/Cosas%20guays%20LaTeX/install).

### Inclusión de PDFs

Para que git meta en el control de código archivos PDF, hay que renombrarlos con una barra baja al principio, para denotar que son archivos auxiliares y no generados por LaTeX.

### Puñetas de LaTeX

* Hay que instalar prácticamente todos los paquetes de LaTeX. En Ubuntu, suele ser _texlive-latex-full_, _texlive-lang-spanish_, _texlive-fonts-extra_, y con cuidado de que instale los paquetes _imakeidx_ y alguno más, que a veces desaparece sin dejar rastro.
* Tal y como está hecho el comando `\makefirstuc`, que se usa en el entorno de definiciones, peta por todo lo alto cuando el primer carácter es una tilde. Por ejemplo, `\begin{defn}[Índice]` no funciona, con un error raro de UTF8. La solución es meter la primera letra entre llaves, así: `\begin{defn}[{Í}ndice]`. Ya, es raro, pero no hay otra.
* Cada vez que se cambian los paquetes hay que volver a instalarlos. Así que si por lo que sea dejan de compilar los apuntes con errores de comandos sin definir, vuelve a ejecutar el script de instalación (`Cosas guays LaTeX/install`) por si acaso.
* Para que la compilación sea más rápida cuando hay dibujos Tikz, hay una instrucción que los "precompila" y los guarda para no recrearlos mientras no cambien (`\precompileTikz` dentro del paquete `tikztools`, por si tenéis curiosidad). Esa "caché" se guarda en el directorio `tikzgen` de cada carpeta de apuntes. Por desgracia, LaTeX es muy puñetero y si no encuentra ese directorio se va a quejar con un error raro, así que si no os compilan apuntes y tienen dibujos, probad a crear ese directorio. Otra opción si no funciona eso es asegurarse de que el compilador de LaTeX es libre como el viento, pasándole la opción `-shell-escape`.

### Instalación de LaTeX



#### OS X

Recomendada instalación a través de MacPorts. En la página [están todos los paquetes y lo que incluye cada uno](https://trac.macports.org/wiki/TeXLivePackages), aunque por resumir: _texlive-basic, texlive-bin-extra_ (para _texdoc_), _texlive-fonts-extra_, _texlive-lang-spanish_, _texlive-latex_, _texlive-latex-extra_, _texlive-math-extra_.

#### Ubuntu

Hay que instalar el paquete base y también los extras ya que se usan varios paquetes presentes ahí: _texlive-latex-base_, _texlive-latex-extra_, _texlive-latex-recommended_. También es interesante instalarse la documentación de cada de uno de esos paquetes.

#### Windows

Búscate la vida como puedas...

## Sacar el máximo provecho a LaTeX

La idea original de este proyecto era tomar apuntes en clase directamente a LaTeX. Para ello, evidentemente, es necesario tener una buena velocidad de escritura a máquina, un dominio bastante extenso de los comandos desarrollados en este proyecto y una customización del teclado a gusto del usuario.

En un principio, cada vez que queramos escribir una letra griega (algo extremadamente frecuente en matemáticas), deberemos escribir un comando de la forma \sigma o \Sigma para representar σ y Σ respectivamente.

Esto no resulta nada eficiente y resta mucho dinamismo a la hora de transcribir a LaTeX una clase en directo. Para ello, lo más cómo es modificar la configuración del teclado para tener la siguiente combinación de teclas, normalmente sin usar:

* AltGr + letra = letra griega minúscula.
* AltGr + shift + letra = letra griega mayúscula.

En ubuntu, la forma más cómoda de cambiar esta configuración es a través del siguiente [README](https://github.com/VicdeJuan/Apuntes/tree/master/Cosas%20guays%20LaTeX/ConfiguracionTeclado)

# Licencia

Estos apuntes se comparten bajo la licencia [Creative Commons - No Commercial - Share Alike](http://creativecommons.org/licenses/by-nc-sa/4.0/legalcode): se pueden usar libremente siempre y cuando se de crédito a los autores originales, no se usen con propósitos comerciales y los trabajos derivados se compartan bajo esta misma licencia.
