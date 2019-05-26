[@ocaml.warning "-34"];
module Types1 = {
  type _Config__color = Config.color = | Red | Blue | Green
  and _Config__config =
    Config.config = {
      color: _Config__color,
      languages: array(_Config__language),
      defaultGreeting: option(string),
    }
  and _Config__language = Config.language = | Spanish | English | German;
};
let currentVersion = 1;
type target = [
  | `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `A(list(target))
  | `O(list((string, target)))
];
module Version1 = {
  open Types1;
  let rec deserialize_Config____color:
    target => result(_Config__color, list(string)) =
    constructor =>
      switch (constructor) {
      | `A([`String(tag)])
      | `String(tag) when "Red" == tag => Ok(Red: _Config__color)
      | `A([`String(tag)])
      | `String(tag) when "Blue" == tag => Ok(Blue: _Config__color)
      | `A([`String(tag)])
      | `String(tag) when "Green" == tag => Ok(Green: _Config__color)
      | `A([`String(tag), ..._]) => Error(["Invalid constructor: " ++ tag])
      | _ => Error(["Expected an array"])
      }
  and deserialize_Config____config:
    target => result(_Config__config, list(string)) =
    record =>
      switch (record) {
      | `O(items) =>
        let inner = attr_defaultGreeting => {
          let inner = attr_languages => {
            let inner = attr_color =>
              Ok(
                {
                  color: attr_color,
                  languages: attr_languages,
                  defaultGreeting: attr_defaultGreeting,
                }: _Config__config,
              );
            switch (items |> List.assoc("color")) {
            | exception Not_found => Error(["No attribute 'color'"])
            | json =>
              switch (deserialize_Config____color(json)) {
              | Error(error) => Error(["attribute 'color'", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (items |> List.assoc("languages")) {
          | exception Not_found => Error(["No attribute 'languages'"])
          | json =>
            switch (
              (
                (
                  (transformer, array) =>
                    switch (array) {
                    | `A(items) =>
                      let rec loop = (collected, items) =>
                        switch (items) {
                        | [] => Ok(List.rev(collected))
                        | [one, ...rest] =>
                          switch (transformer(one)) {
                          | Error(error) =>
                            Error(["array element", ...error])
                          | Ok(value) => loop([value, ...collected], rest)
                          }
                        };
                      switch (loop([], items)) {
                      | Error(error) => Error(error)
                      | Ok(value) => Ok(Array.of_list(value))
                      };
                    | _ => Error(["expected an array"])
                    }
                )(
                  deserialize_Config____language,
                )
              )(
                json,
              )
            ) {
            | Error(error) => Error(["attribute 'languages'", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (items |> List.assoc("defaultGreeting")) {
        | exception Not_found => inner(None)
        | json =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | `Null => Ok(None)
                  | _ =>
                    switch (transformer(option)) {
                    | Error(error) => Error(["optional value", ...error])
                    | Ok(value) => Ok(Some(value))
                    }
                  }
              )(
                string =>
                switch (string) {
                | `String(string) => Ok(string)
                | _ => Error(["epected a string"])
                }
              )
            )(
              json,
            )
          ) {
          | Error(error) => Error(["attribute 'defaultGreeting'", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Error(["Expected an object"])
      }
  and deserialize_Config____language:
    target => result(_Config__language, list(string)) =
    constructor =>
      switch (constructor) {
      | `A([`String(tag)])
      | `String(tag) when "español" == tag => Ok(Spanish: _Config__language)
      | `A([`String(tag)])
      | `String(tag) when "english" == tag => Ok(English: _Config__language)
      | `A([`String(tag)])
      | `String(tag) when "deutsch" == tag => Ok(German: _Config__language)
      | `A([`String(tag), ..._]) => Error(["Invalid constructor: " ++ tag])
      | _ => Error(["Expected an array"])
      }
  and serialize_Config____color: _Config__color => target =
    constructor =>
      switch (constructor) {
      | Red => `A([`String("Red")])
      | Blue => `A([`String("Blue")])
      | Green => `A([`String("Green")])
      }
  and serialize_Config____config: _Config__config => target =
    record =>
      `O([
        ("color", serialize_Config____color(record.color)),
        (
          "languages",
          (
            (
              (transformer, array) =>
                `A(Array.to_list(Array.map(transformer, array)))
            )(
              serialize_Config____language,
            )
          )(
            record.languages,
          ),
        ),
        (
          "defaultGreeting",
          (
            (
              transformer =>
                fun
                | None => `Null
                | Some(v) => transformer(v)
            )(
              s =>
              `String(s)
            )
          )(
            record.defaultGreeting,
          ),
        ),
      ])
  and serialize_Config____language: _Config__language => target =
    constructor =>
      switch (constructor) {
      | Spanish => `A([`String("español")])
      | English => `A([`String("english")])
      | German => `A([`String("deutsch")])
      };
};
module Current = Version1;
let parseVersion = json =>
  switch (json) {
  | `O(items) =>
    switch (items |> List.assoc("$schemaVersion")) {
    | exception Not_found => Error("No $schemaVersion")
    | `Float(schemaVersion) =>
      [@implicit_arity] Ok(int_of_float(schemaVersion), json)
    | _ => Error("Invalid schema version - expected number")
    }
  | `A([`Float(version), payload]) =>
    [@implicit_arity] Ok(int_of_float(version), payload)
  | _ => Error("Not wrapped in a version")
  };
let wrapWithVersion = (version, payload) =>
  switch (payload) {
  | `O(items) =>
    `O([("$schemaVersion", `Float(float_of_int(version))), ...items])
  | _ => `A([`Float(float_of_int(version)), payload])
  };
let serializeConfig = data =>
  wrapWithVersion(currentVersion, Version1.serialize_Config____config(data))
and deserializeConfig = data =>
  switch (parseVersion(data)) {
  | Error(err) => Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 1 =>
      switch (Version1.deserialize_Config____config(data)) {
      | Error(error) => Error(error)
      | Ok(data) => Ok(data)
      }
    | _ => Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
module Modules = {
  module Config = {
    type t = Types1._Config__config;
    let serialize = serializeConfig;
    let deserialize = deserializeConfig;
  };
};
