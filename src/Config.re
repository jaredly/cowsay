// Config.re

// Any value that was Blue becomes Green
[@migrate.Blue (Blue) => Green]
type color = Red | Green;

[@rename.Spanish "español"]
[@rename.English "english"]
[@rename.German "deutsch"]
type language = Spanish | English | German;

// Previously the only direction the cow could face was left, so that makes sense as the migration default.
[@migrate.cowDirection (_) => `Left]
type config = {
  languages: array((language, color)),
  defaultGreeting: option(string),
  cowDirection: [`Left | `Right],
}


let empty = {
  languages: [|(English, Red)|],
  defaultGreeting: None,
  cowDirection: `Left,
}