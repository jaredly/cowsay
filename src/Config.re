// Config.re
type color = Red | Blue | Green;

[@rename.Spanish "español"]
[@rename.English "english"]
[@rename.German "deutsch"]
type language = Spanish | English | German;

type config = {
  languages: array((language, color)),
  defaultGreeting: option(string),
}

let empty = {
  languages: [|(English, Red)|],
  defaultGreeting: None,
}