## Shop Matcher

Shop matcher is written in Scala, for JSON parsing uses [circe.io](https://github.com/circe/circe)

#### Features:

1. Matches the users with the most applicable shops 
based on the distance as well as on the frequency they pass by

2. Classifies users according to the age
 and finds the shops that each age group prefers
 
#### How to use:

- go to the root of the folder
- open sbt shell
  -  to test feature #1: print `run match` than print the id of the user and the limit of shops you want to see. 
For example `run match 5 3` will output 3 most applicable shops for user with id 1
  - to test feature #2: print `run classify` 

#### Notes: 
The data that the application is using is stored in `src/main/resources/testdata`

The rules of what shops are considered close are set 
and can be changed in `src/main/scala/shopmatcher/Rules`:
- by default shops that are within 400 meters of a user are considered close.
- by default user is considered inside the shop if they are within 10 meters to it.
