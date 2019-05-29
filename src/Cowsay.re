
let greetingForLanguage = language => switch (language) {
  | Config.English => "The cow says:"
  | German => "Die Kuh sagt:"
  | Spanish => "La vaca dice:"
};

let cowSuffix =  {|
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
|}

let cowSay = (greeting, text) => {
  let count = String.length(text);
  let blank = Array.make(count + 1, "") |> Array.to_list;
  let top =  blank |> String.concat( "_");
  let bottom =  blank |> String.concat( "-");
  [greeting, top, text, bottom, cowSuffix] |> String.concat("\n")
}

let main = () => {
  let data = Stdio.In_channel.read_all("./config.json") |> Ezjsonm.from_string;
  let config = TypeSerde.deserializeConfig(data);
  switch config {
    | Ok(config) =>
      // We now have the loaded config
      let convert = color => switch color {
        | Config.Red => ANSITerminal.Red
        // | Blue => ANSITerminal.Blue
        | Green => ANSITerminal.Green
      };
      let show = (color, text) => ANSITerminal.print_string([ANSITerminal.Foreground(color)], text ++ "\n");

      let message = switch (Sys.argv |> Array.to_list |> List.tl) {
        | [] => switch (config.defaultGreeting) {
          | None => "Usage: cowsay some text"
          | Some(greeting) => greeting
        }
        | words => String.concat(" ", words)
      };

      if (config.languages == [||]) {
        show(ANSITerminal.Green, "\n" ++ cowSay( greetingForLanguage(English), message))
      } else {
        config.languages |> Array.iter(((language, color)) => {
          show(convert(color), "\n" ++ cowSay( greetingForLanguage(language), message))
        });
      }
      // print_endline("Hello in " ++ color)
    | Error(error) =>
      print_endline(String.concat(" >> ", error));
      exit(1)
  }
}

main();
