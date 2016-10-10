(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

let prologue = "
let urlencode b s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ' ' -> Buffer.add_string b \"%20\"
    | '!' -> Buffer.add_string b \"%21\"
    | '\"' -> Buffer.add_string b \"%22\"
    | '.' -> Buffer.add_string b \"%2E\"
    | '/' -> Buffer.add_string b \"%2F\"
    | ':' -> Buffer.add_string b \"%3A\"
    | c -> Buffer.add_char b c
  done
"

module Parser = struct
  open Yojson.Basic.Util

  type type_descr =
    | Object of (string * schema) list
    | String of string option
    | Integer of string option
    | Number
    | Array of schema
    | Boolean
    | Ref of string
    | Enum of (string * string) list

  and schema =
    {
      id: string option;
      type_descr: type_descr;
      default: string option;
      description: string option;
      required: bool;
      repeated: bool;
      location: string option;
      annotations: string list; (* methods which require this requst/schema *)
    }

  let rec schema_of_json json =
    let id = json |> member "id" |> to_string_option in
    let type_descr =
      let type_ = json |> member "type" |> to_string_option in
      let ref_ = json |> member "$ref" |> to_string_option in
      let format = json |> member "format" |> to_string_option in
      match type_, ref_ with
      | Some "boolean", _ ->
          Boolean
      | Some "integer", _ ->
          Integer format
      | Some "number", _ ->
          Number
      | Some "string", _ ->
          let enum = [json] |> filter_member "enum" |> flatten |> filter_string in
          let enum_descriptions =
            [json] |> filter_member "enumDescriptions" |> flatten |> filter_string
          in
          begin match enum with
          | [] -> String format
          | _ :: _ -> Enum (List.combine enum enum_descriptions)
          end
      | Some "array", _ ->
          let items = json |> member "items" |> to_option schema_of_json in
          begin match items with
          | Some items -> Array items
          | None -> failwith "schema_of_json: no array item schema"
          end
      | Some "object", _ ->
          let properties =
            [json] |> filter_member "properties" |> List.map to_assoc |> List.flatten |>
            List.map (fun (key, json) -> key, schema_of_json json)
          in
          Object properties
      | Some s, _ ->
          Printf.ksprintf failwith "schema_of_json: unrecognized type (%s)" s
      | None, Some s ->
          Ref s
      | None, None ->
          failwith "schema_of_json: no schema type"
    in
    let description = json |> member "description" |> to_string_option in
    let default = json |> member "default" |> to_string_option in
    let repeated = json |> member "repeated" |> to_bool_option in
    let repeated = match repeated with Some b -> b | None -> false in
    let location = json |> member "location" |> to_string_option in
    let required = json |> member "required" |> to_bool_option in
    let required = match required with Some b -> b | None -> false in
    let annotations =
      [json] |> filter_member "annotations" |> filter_member "required" |> flatten |>
      filter_string
    in
    {id; type_descr; description; default; required; repeated; location; annotations}

  type scope =
    {
      id: string;
      description: string;
    }

  let scope_of_json (id, json) =
    let description = json |> member "description" |> to_string in
    {id; description}

  type method_ =
    {
      id: string;
      path: string;
      http_method: string;
      description: string option;
      parameters: (string * schema) list;
      parameter_order: string list;
      request: string option;
      response: string option;
      scopes: string list;
    }

  let method_of_json json =
    let id = json |> member "id" |> to_string in
    let path = json |> member "path" |> to_string in
    let http_method = json |> member "httpMethod" |> to_string in
    let description = json |> member "description" |> to_string_option in
    let parameters =
      [json] |> filter_member "parameters" |> List.map to_assoc |> List.flatten |>
      List.map (fun (key, json) -> key, schema_of_json json)
    in
    let parameter_order = [json] |> filter_member "parameterOrder" |> flatten |> filter_string in
    let request =
      let l = [json] |> filter_member "request" |> filter_member "$ref" |> filter_string in
      match l with x :: _ -> Some x | [] -> None
    in
    let response =
      let l = [json] |> filter_member "response" |> filter_member "$ref" |> filter_string in
      match l with x :: _ -> Some x | [] -> None
    in
    let scopes = [json] |> filter_member "scopes" |> flatten |> filter_string in
    {id; path; http_method; description; parameters; parameter_order;
     request; response; scopes}

  type resource =
    {
      id: string;
      methods: (string * method_) list;
      resources: resource list;
    }

  let rec resource_of_json (id, json) =
    let methods =
      [json] |> filter_member "methods" |> List.map to_assoc |> List.flatten |>
      List.map (fun (key, json) -> key, method_of_json json)
    in
    let resources =
      [json] |> filter_member "resources" |> List.map to_assoc |> List.flatten |>
      List.map resource_of_json
    in
    {id; methods; resources}

  type api =
    {
      name: string;
      version: string;
      base_url: string;
      scopes: scope list;
      schemas: (string * schema) list;
      resources: resource list;
    }

  let api_of_json json =
    let name = json |> member "name" |> to_string in
    let version = json |> member "version" |> to_string in
    let base_url = json |> member "baseUrl" |> to_string in
    let scopes =
      json |> member "auth" |> member "oauth2" |> member "scopes" |> to_assoc |>
      List.map scope_of_json
    in
    let schemas =
      json |> member "schemas" |> to_assoc |>
      List.map (fun (key, json) -> key, schema_of_json json)
    in
    let resources = json |> member "resources" |> to_assoc |> List.map resource_of_json in
    {name; version; base_url; scopes; schemas; resources}
end

module Emit = struct
  open Printf
  open Parser

  let unreserve = function
    | "type" -> "type_"
    | "to" -> "to_"
    | "include" -> "include_"
    | s -> s

  let pretty s =
    let b = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | 'A' .. 'Z' as c ->
          if i > 0 then Buffer.add_char b '_';
          Buffer.add_char b (Char.lowercase_ascii c)
      | c ->
          Buffer.add_char b c
    done;
    unreserve (Buffer.contents b)

  let emit_separated sep f oc l =
    match l with
    | [] -> ()
    | x :: xs -> f oc x; List.iter (fun x -> fprintf oc "%s%a" sep f x) xs

  let emit_value parameter oc s =
    match parameter.type_descr with
    | String _ -> fprintf oc "%S" s
    | Enum _ -> fprintf oc "`%s" (unreserve s)
    | Boolean -> fprintf oc "%s" s
    | Integer _ -> fprintf oc "%d" (int_of_string s)
    | _ -> ksprintf failwith "emit_value: not supported (%S)" s

  let emit_parameter oc (key, parameter) =
    match parameter with
    | {required = false; default = None; repeated = true; _} ->
        fprintf oc "?(%s = [])" (unreserve key)
    | {default = Some _; repeated = true; _} ->
        failwith "emit_parameter: repeated parameter with supported characteristics"
    | {required = false; default = None; repeated = false; _} ->
        fprintf oc "?%s" (unreserve key)
    | {required = true; default = None; _} ->
        fprintf oc "~%s" (unreserve key)
    | {default = Some s; repeated = false; _} ->
        fprintf oc "?(%s = %a)" (unreserve key) (emit_value parameter) s

  let emit_parameters schemas oc (parameters, request) =
    emit_separated " " emit_parameter oc parameters;
    match request with
    | None -> ()
    | Some ref_ ->
        let parameters =
          match List.assoc ref_ schemas with
          | {type_descr = Object params; _} -> params
          | _ -> failwith "emit_parameters: not an object"
        in
        fprintf oc " ";
        emit_separated " " emit_parameter oc parameters

  let rec emit_query_parameter first url id key oc parameter =
    match parameter.repeated, parameter.required, parameter.default with
    | true, _, _ ->
        fprintf oc "if %s <> [] then begin\n" id;
        emit_query_parameter first url (sprintf "(List.hd %s)" id) key oc
          {parameter with repeated = false};
        fprintf oc "List.iter (fun x ->\n";
        emit_query_parameter '&' url "x" key oc {parameter with repeated = false};
        fprintf oc ") (List.tl %s);\nend;\n" id
    | false, true, _
    | false, false, Some _ ->
        begin match parameter.type_descr with
        | String _ ->
            fprintf oc "Printf.bprintf %s \"%c%s=%%a\" urlencode %s;\n"
              url first key id
        | Integer _ ->
            fprintf oc "Printf.bprintf %s \"%c%s=%%d\" %s;\n" url first key id
        | Boolean ->
            fprintf oc "Printf.bprintf %s \"%c%s=%%b\" %s;\n" url first key id
        | Enum enums ->
            fprintf oc "Printf.bprintf %s \"%c%s=%%s\"\n" url first key;
            fprintf oc "(match %s with\n" id;
            List.iter (fun (s, _) ->
                fprintf oc "| `%s -> %S\n" (unreserve s) s
              ) enums;
            fprintf oc ");\n"
        | _ ->
            failwith "emit_query_parameter: unsupported type"
        end
    | false, false, None ->
        fprintf oc "(match %s with None -> () | Some x ->\n%a);\n"
          id (emit_query_parameter first url "x" key) {parameter with required = true}

  let emit_query_parameters url oc parameters =
    fprintf oc "Printf.bprintf %s \"?access_token=%%s\" token;\n" url;
    match parameters with
    | [] -> ()
    | (key, schema) :: xs ->
        emit_query_parameter '&' url (unreserve key) key oc schema;
        List.iter
          (fun (key, schema) ->
             emit_query_parameter '&' url (unreserve key) key oc schema
          ) xs

  let emit_path_parameters path url oc parameters =
    let aux (key, parameter) =
      if parameter.repeated then
        ksprintf failwith "emit_path_parameters: repeated not supported (%s)" key;
      if not parameter.required then
        ksprintf failwith "emit_path_parameters: not required not supported (%s)" key;
      match parameter.type_descr with
      | String _ ->
          fprintf oc "Printf.bprintf %s \"%%s\" %s;\n" url (unreserve key)
      | Integer _ ->
          fprintf oc "Printf.bprintf %s \"%%d\" %s;\n" url (unreserve key)
      | Boolean ->
          fprintf oc "Printf.bprintf %s \"%%b\" %s;\n" url (unreserve key)
      | _ ->
          failwith "emit_query_parameter: unsupported type"
    in
    let rec loop i =
      match String.index_from path i '{' with
      | j ->
          if i > 0 then fprintf oc "Printf.bprintf %s %S;\n" url (String.sub path i (j-i));
          let k = String.index_from path j '}' in
          let id = String.sub path (j+1) (k-j-1) in
          let parameter = List.find (fun (key, _) -> key = id) parameters in
          aux parameter;
          loop (k+1)
      | exception Not_found ->
          if i < String.length path then
            fprintf oc "Printf.bprintf %s %S;\n" url (String.sub path i (String.length path - i))
    in
    loop 0

  let http_method method_ =
    match method_.http_method with
    | "GET" -> "get"
    | "PUT" -> "put"
    | "POST" -> "post"
    | "DELETE" -> "delete"
    | "PATCH" -> "patch"
    | s -> ksprintf failwith "http_method: method not supported (%s)" s

  let emit_method base_url schemas oc (key, method_) =
    fprintf oc "method %s ?(token = \"\") %a () =\n"
      (pretty key) (emit_parameters schemas) (method_.parameters, method_.request);
    fprintf oc "let url = Buffer.create 0 in\n";
    fprintf oc "Printf.bprintf url \"%s\";\n" base_url;
    let query_parameters, path_parameters =
      List.partition (fun (_, schema) ->
          match schema.location with
          | Some "query" -> true
          | Some "path" -> false
          | _ -> failwith "emit_method: unsupported location"
        ) method_.parameters
    in
    emit_path_parameters method_.path "url" oc path_parameters;
    emit_query_parameters "url" oc query_parameters;
    fprintf oc "let url = Buffer.contents url in\n";
    fprintf oc "let open Lwt.Infix in\n";
    fprintf oc "Http.%s (Uri.of_string url) >>= fun (_, body) ->\n" (http_method method_);
    match method_.response with
    | None ->
        fprintf oc "Lwt.return_unit\n"
    | Some x ->
        fprintf oc "Cohttp_lwt_body.to_string body >>= fun body ->\n";
        fprintf oc "let json = Yojson.Basic.from_string body in\n";
        fprintf oc "let response = %s.of_json json in\n" x;
        fprintf oc "Lwt.return response\n"

  let emit_methods base_url schemas oc methods =
    List.iter (emit_method base_url schemas oc) methods

  let rec emit_resource base_url schemas oc resource =
    fprintf oc "method %s =\nobject\n" (pretty resource.id);
    emit_methods base_url schemas oc resource.methods;
    emit_resources base_url schemas oc resource.resources;
    fprintf oc "end\n"

  and emit_resources base_url schemas oc resources =
    List.iter (emit_resource base_url schemas oc) resources

  let rec emit_schema_property oc (key, schema) =
    fprintf oc "%s: %a;\n" (pretty key) emit_schema_type schema

  and emit_schema_properties oc properties =
    List.iter (emit_schema_property oc) properties

  and emit_schema_type oc schema =
    match schema.type_descr with
    | Integer _ ->
        fprintf oc "int"
    | Number ->
        fprintf oc "float"
    | String _ ->
        fprintf oc "string"
    | Enum enum ->
        fprintf oc "[\n";
        List.iteri (fun i (s, _) ->
            if i > 0 then fprintf oc "|";
            fprintf oc "`%s" (unreserve s)
          ) enum;
        fprintf oc "]"
    | Boolean ->
        fprintf oc "bool"
    | Array items ->
        fprintf oc "%a list" emit_schema_type items
    | Object [] ->
        fprintf oc "unit"
    | Object properties ->
        fprintf oc "<\n%a>\n" emit_schema_properties properties
    | Ref ref_ ->
        fprintf oc "%s" (pretty ref_)

  let rec emit_schema_to_json f oc schema =
    match schema.type_descr with
    | Integer _ ->
        fprintf oc "`Int %t" f
    | Number ->
        fprintf oc "`Float %t" f
    | String _ ->
        fprintf oc "`String %t" f
    | Enum enum ->
        fprintf oc "match %t with\n" f;
        List.iter (fun (s, _) ->
            fprintf oc "| `%s -> `String %S\n" (unreserve s) s
          ) enum
    | Boolean ->
        fprintf oc "`Bool %t" f
    | Array items ->
        fprintf oc "`List (List.map (fun x -> %a) %t)"
          (emit_schema_to_json (fun oc -> fprintf oc "x"))
          items f
    | Ref ref_ ->
        fprintf oc "%s_to_json %t" (pretty ref_) f
    | Object properties ->
        let aux f oc schema =
          fprintf oc "match %t with\n" f;
          fprintf oc "| exception _ -> `Null\n";
          fprintf oc "| x -> %a"
            (emit_schema_to_json (fun oc -> fprintf oc "x")) schema
        in
        fprintf oc "`Assoc\n";
        fprintf oc "[\n";
        List.iter (fun (key, schema) ->
            fprintf oc "(%S, %a);\n" key
              (aux (fun oc -> fprintf oc "%t#%s" f (pretty key)))
              schema
          ) properties;
        fprintf oc "]"

  let rec emit_schema_of_json oc schema =
    match schema.type_descr with
    | Integer _ ->
        fprintf oc "to_int"
    | Number ->
        fprintf oc "to_number"
    | String _ ->
        fprintf oc "to_string"
    | Enum enum ->
        fprintf oc "to_string |> function\n";
        List.iter (fun (s, _) ->
            fprintf oc "| %S -> `%s\n" s (unreserve s)
          ) enum;
        fprintf oc "| s -> invalid_arg (%S ^ s)" "unrecognized enum: "
    | Boolean ->
        fprintf oc "to_bool"
    | Array items ->
        fprintf oc "to_list |> List.map (%a)" emit_schema_of_json items
    | Ref ref_ ->
        fprintf oc "%s_of_json" (pretty ref_)
    | Object properties ->
        fprintf oc "fun json -> object\n";
        List.iter (fun (key, schema) ->
            fprintf oc "method %s = json |> member %S |> %a\n"
              (pretty key) key emit_schema_of_json schema
          ) properties;
        fprintf oc "end"

  let emit_schema first oc (key, schema) =
    match schema.type_descr with
    | Object properties ->
        fprintf oc "%s %s_of_json =\n" first (pretty key);
        fprintf oc "%a\n" emit_schema_of_json schema;
        fprintf oc "and %s_to_json x =\n" (pretty key);
        fprintf oc "%a\n" (emit_schema_to_json (fun oc -> fprintf oc "x")) schema
        (* fprintf oc "end\n" *)
    | _ ->
        assert false

  let emit_schemas oc schemas =
    let aux = function
      | [] -> ()
      | x :: xs ->
          fprintf oc "open Yojson.Basic.Util\n";
          emit_schema "let rec" oc x;
          List.iter (fun x -> emit_schema "and" oc x) xs
    in
    aux schemas

  let emit oc api =
    fprintf oc "%s\n" prologue;
    fprintf oc "module %s (Http : Cohttp_lwt.Client) = struct\n"
      (String.capitalize_ascii (pretty api.version));
    emit_schemas oc api.schemas;
    fprintf oc "let service =\nobject\n";
    emit_resources api.base_url api.schemas oc api.resources;
    fprintf oc "end\n";
    fprintf oc "end\n";
    flush oc
end

let command cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 0 in
  try
    while true do Buffer.add_channel buf ic 4096 done;
    assert false
  with End_of_file ->
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 ->
        Buffer.contents buf
    | Unix.WEXITED n ->
        Printf.ksprintf failwith "Command %S failed with exit code %d" cmd n
    | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        Printf.ksprintf failwith "Command %S stopped by signal %d" cmd n

let get_api_json ~api ~version =
  Printf.ksprintf command "curl https://www.googleapis.com/discovery/v1/apis/%s/%s/rest" api version

let list_apis () =
  let json = Printf.ksprintf command "curl https://www.googleapis.com/discovery/v1/apis" in
  print_endline json

let main () =
  let api = ref None in
  let version = ref None in
  let list = ref false in
  let show = ref false in
  let gen = ref false in
  let spec =
    Arg.align
      [
        "-api", Arg.String (fun s -> api := Some s), " Which Google API to download";
        "-version", Arg.String (fun s -> version := Some s), " Which API version to download";
        "-list", Arg.Set list, " List available APIs";
        "-show", Arg.Set show, " Dump raw API JSON";
        "-gen", Arg.Set gen, " Generate OCaml code";
      ]
  in
  let usage_msg = "Automatic Google APIs for OCaml" in
  Arg.parse spec (fun _ -> ()) usage_msg;
  if !list then list_apis ();
  let get_api () =
    match !api with
    | Some api -> api
    | None -> failwith "Missing -api parameter"
  in
  let get_version () =
    match !version with
    | Some version -> version
    | None -> failwith "Missing -version parameter"
  in
  if !show then begin
    let json = get_api_json ~api:(get_api ()) ~version:(get_version ()) in
    print_endline json
  end;

  if !gen then begin
    let json = get_api_json ~api:(get_api ()) ~version:(get_version ()) in
    Parser.api_of_json (Yojson.Basic.from_string json) |> Emit.emit stdout
  end

let () =
  try
    main ()
  with e ->
    flush_all ();
    let s = match e with Failure s -> s | e -> Printexc.to_string e in
    Printf.eprintf "\nERROR: %s.\n**Backtrace:**\n%t%!" s Printexc.print_backtrace
