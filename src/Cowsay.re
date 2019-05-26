
let main = () => {
  let data = Stdio.In_channel.read_all("./config.json") |> Ezjsonm.from_string;
  let config = TypeSerde.deserializeConfig(data);
  switch config {
    | Error(error) =>
      print_endline(String.concat(" >> ", error));
      exit(1)
    | Ok(config) =>
      let color = switch config.color {
        | Red => "red"
        | Blue => "blue"
        | Green => "green"
      };
      print_endline("Hello in " ++ color)
  }
}

main();
