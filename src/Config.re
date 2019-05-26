// Config.re
type color = Red | Blue | Green;

[@rename.Spanish "español"]
[@rename.English "english"]
[@rename.German "deutsch"]
type language = Spanish | English | German;

[@migrate.languages oldConfig => oldConfig.languages |> Array.map(lang => (lang, oldConfig.color)) ]
type config = {
  languages: array((language, color)),
  defaultGreeting: option(string),
}

let empty = {
  languages: [|(English, Red)|],
  defaultGreeting: None,
}