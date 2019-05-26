// Config.re
type color = Red | Blue | Green;

[@rename.Spanish "español"]
[@rename.English "english"]
[@rename.German "deutsch"]
type language = Spanish | English | German;

type config = {
  color,
  languages: array(language),
  defaultGreeting: option(string),
}

let empty = {
  color: Red,
  languages: [|English|],
  defaultGreeting: None,
}