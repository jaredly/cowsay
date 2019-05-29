[@ocaml.warning "-34-39"];
module Types1 = {
  type _Config__color =
    | Red
    | Blue
    | Green
  and _Config__config = {
    color: _Config__color,
    languages: array(_Config__language),
    defaultGreeting: option(string),
  }
  and _Config__language = Config.language = | Spanish | English | German;
};
module Types2 = {
  type _Config__color = Types1._Config__color = | Red | Blue | Green
  and _Config__config = {
    languages: array((_Config__language, _Config__color)),
    defaultGreeting: option(string),
  }
  and _Config__language = Config.language = | Spanish | English | German;
  let rec migrate_Config____color: Types1._Config__color => _Config__color =
    _input_data => _input_data
  and migrate_Config____config: Types1._Config__config => _Config__config =
    _input_data => {
      let _converted_languages =
        (
          oldConfig =>
            oldConfig.languages |> Array.map(lang => (lang, oldConfig.color)):
            Types1._Config__config =>
            array((_Config__language, _Config__color))
        )(
          _input_data,
        );
      let _converted_defaultGreeting =
        switch (_input_data.defaultGreeting) {
        | None => None
        | Some(_item) => Some(_item)
        };
      {
        defaultGreeting: _converted_defaultGreeting,
        languages: _converted_languages,
      };
    }
  and migrate_Config____language: Types1._Config__language => _Config__language =
    _input_data => _input_data;
};
module Types3 = {
  type _Config__color = Config.color = | Red | Green
  and _Config__config =
    Config.config = {
      languages: array((_Config__language, _Config__color)),
      defaultGreeting: option(string),
      cowDirection: [ | `Right | `Left],
    }
  and _Config__language = Config.language = | Spanish | English | German;
  let rec migrate_Config____color: Types2._Config__color => _Config__color =
    _input_data =>
      switch (_input_data) {
      | Red => Red
      | Blue => Green
      | Green => Green
      }
  and migrate_Config____config: Types2._Config__config => _Config__config =
    _input_data => {
      let _converted_languages =
        _input_data.languages
        |> Array.map(_item => {
             let (arg0, arg1) = _item;
             (
               migrate_Config____language(arg0),
               migrate_Config____color(arg1),
             );
           });
      let _converted_defaultGreeting =
        switch (_input_data.defaultGreeting) {
        | None => None
        | Some(_item) => Some(_item)
        };
      let _converted_cowDirection =
        (_ => `Left: Types2._Config__config => [ | `Right | `Left])(
          _input_data,
        );
      {
        cowDirection: _converted_cowDirection,
        defaultGreeting: _converted_defaultGreeting,
        languages: _converted_languages,
      };
    }
  and migrate_Config____language: Types2._Config__language => _Config__language =
    _input_data => _input_data;
};
let currentVersion = 3;
type target = [
  | `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `A(list(target))
  | `O(list((string, target)))
];
let schemaPropertyName = "$schemaVersion";
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
      };
};
module Version2 = {
  open Types2;
  let rec deserialize_Config____color:
    target => result(_Config__color, list(string)) = Version1.deserialize_Config____color
  and deserialize_Config____config:
    target => result(_Config__config, list(string)) =
    record =>
      switch (record) {
      | `O(items) =>
        let inner = attr_defaultGreeting => {
          let inner = attr_languages =>
            Ok(
              {
                languages: attr_languages,
                defaultGreeting: attr_defaultGreeting,
              }: _Config__config,
            );
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
                  json =>
                  switch (json) {
                  | `A([arg0, arg1]) =>
                    switch (deserialize_Config____color(arg1)) {
                    | Ok(arg1) =>
                      switch (deserialize_Config____language(arg0)) {
                      | Ok(arg0) => Ok((arg0, arg1))
                      | Error(error) => Error(["tuple element 0", ...error])
                      }
                    | Error(error) => Error(["tuple element 1", ...error])
                    }
                  | _ => Error(["Expected array"])
                  }
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
    target => result(_Config__language, list(string)) = Version1.deserialize_Config____language;
};
module Version3 = {
  open Types3;
  let rec deserialize_Config____color:
    target => result(_Config__color, list(string)) =
    constructor =>
      switch (constructor) {
      | `A([`String(tag)])
      | `String(tag) when "Red" == tag => Ok(Red: _Config__color)
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
        let inner = attr_cowDirection => {
          let inner = attr_defaultGreeting => {
            let inner = attr_languages =>
              Ok(
                {
                  languages: attr_languages,
                  defaultGreeting: attr_defaultGreeting,
                  cowDirection: attr_cowDirection,
                }: _Config__config,
              );
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
                    json =>
                    switch (json) {
                    | `A([arg0, arg1]) =>
                      switch (deserialize_Config____color(arg1)) {
                      | Ok(arg1) =>
                        switch (deserialize_Config____language(arg0)) {
                        | Ok(arg0) => Ok((arg0, arg1))
                        | Error(error) =>
                          Error(["tuple element 0", ...error])
                        }
                      | Error(error) => Error(["tuple element 1", ...error])
                      }
                    | _ => Error(["Expected array"])
                    }
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
            | Error(error) =>
              Error(["attribute 'defaultGreeting'", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (items |> List.assoc("cowDirection")) {
        | exception Not_found => Error(["No attribute 'cowDirection'"])
        | json =>
          switch (
            (
              constructor =>
                switch (constructor) {
                | `A([`String(tag)])
                | `String(tag) when "Right" == tag => Ok(`Right)
                | `A([`String(tag)])
                | `String(tag) when "Left" == tag => Ok(`Left)
                | `A([`String(tag), ..._]) =>
                  Error(["Invalid constructor: " ++ tag])
                | _ => Error(["Expected an array"])
                }
            )(
              json,
            )
          ) {
          | Error(error) => Error(["attribute 'cowDirection'", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Error(["Expected an object"])
      }
  and deserialize_Config____language:
    target => result(_Config__language, list(string)) = Version2.deserialize_Config____language
  and serialize_Config____color: _Config__color => target =
    constructor =>
      switch (constructor) {
      | Red => `A([`String("Red")])
      | Green => `A([`String("Green")])
      }
  and serialize_Config____config: _Config__config => target =
    record =>
      `O([
        (
          "languages",
          (
            (
              (transformer, array) =>
                `A(Array.to_list(Array.map(transformer, array)))
            )(
              ((arg0, arg1)) =>
              `A([
                serialize_Config____language(arg0),
                serialize_Config____color(arg1),
              ])
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
        (
          "cowDirection",
          switch (record.cowDirection) {
          | `Right => `A([`String("Right")])
          | `Left => `A([`String("Left")])
          },
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
module Current = Version3;
let parseVersion = json =>
  switch (json) {
  | `O(items) =>
    switch (items |> List.assoc(schemaPropertyName)) {
    | exception Not_found => Error("No " ++ schemaPropertyName)
    | `Float(schemaVersion) =>
      [@implicit_arity] Ok(int_of_float(schemaVersion), json)
    | _ => Error("Invalid " ++ schemaPropertyName ++ " - expected number")
    }
  | `A([`Float(version), payload]) =>
    [@implicit_arity] Ok(int_of_float(version), payload)
  | _ => Error("Not wrapped in a version")
  };
let wrapWithVersion = (version, payload) =>
  switch (payload) {
  | `O(items) =>
    `O(items @ [(schemaPropertyName, `Float(float_of_int(version)))])
  | _ => `A([`Float(float_of_int(version)), payload])
  };
let serializeConfig = data =>
  wrapWithVersion(currentVersion, Version3.serialize_Config____config(data))
and deserializeConfig = data =>
  switch (parseVersion(data)) {
  | Error(err) => Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 3 =>
      switch (Version3.deserialize_Config____config(data)) {
      | Error(error) => Error(error)
      | Ok(data) => Ok(data)
      }
    | 2 =>
      switch (Version2.deserialize_Config____config(data)) {
      | Error(error) => Error(error)
      | Ok(data) =>
        let data = Types3.migrate_Config____config(data);
        Ok(data);
      }
    | 1 =>
      switch (Version1.deserialize_Config____config(data)) {
      | Error(error) => Error(error)
      | Ok(data) =>
        let data = Types2.migrate_Config____config(data);
        let data = Types3.migrate_Config____config(data);
        Ok(data);
      }
    | _ => Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
module Modules = {
  module Config = {
    type t = Types3._Config__config;
    let serialize = serializeConfig;
    let deserialize = deserializeConfig;
  };
};
