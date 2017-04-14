# Generating the slides

The Markdown files in this repository can be used to generate [reveal.js](https://github.com/hakimel/reveal.js/)
slides, using [pandoc](http://pandoc.org). To avoid the hassle of having to install reveal.js, a
docker container is provided at [this git repository](https://github.com/capitanbatata/revealjs-server). 

To generate the slides out of a Markdown file use:

    pandoc -t revealjs -s --variable="revealjs-url":"" file.md -o index.html

The following example sets additional variables:

    pandoc -t revealjs -s --variable="revealjs-url":"" getting-started-with-haskell.md -V theme=solarized -V history=true -o index.html

Then, assuming the slides are at the current working directory start the
docker container with:

    docker run -p 49160:8000  -v `pwd`:/revealjs/presentation/ --rm dnadales/revealjs-server:latest

The navigate to:

    http://localhost:49160/presentation/

