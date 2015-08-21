Context-free grammar checking
-----------------------------

### How to install / compile

* Install sbt
* Go inside the folder `CFG-checking`
* Run `sbt compile`

#### Create a jar file
* Run `sbt one-jar`. The jar file will be somewhere in the `target` directory.
* Use `java -jar [Name of the jar file]` to run the jar.

#### Run the main class directly
* Run `sbt run` 

#### Launch the tests
* Run `sbt test`