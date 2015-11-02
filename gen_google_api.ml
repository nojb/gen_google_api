
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
    | Array of schema
    | Boolean
    | Ref of string
    | Enum of (string * string) list

  and schema =
    {
      id : string option;
      type_descr : type_descr;
      default : string option;
      description : string option;
      required : bool;
      repeated : bool;
      location : string option;
      annotations : string list;
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
          | None -> failwith "array"
          end
      | Some "object", _ ->
          let properties =
            [json] |> filter_member "properties" |> List.map to_assoc |> List.flatten |>
            List.map (fun (key, json) -> key, schema_of_json json)
          in
          Object properties
      | Some _, _ ->
          failwith "unrecognized type"
      | None, Some s ->
          Ref s
      | None, None ->
          failwith "no schema type"
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
      id : string;
      description : string;
    }

  let scope_of_json (id, json) =
    let description = json |> member "description" |> to_string in
    {id; description}

  type method_ =
    {
      id : string;
      path : string;
      http_method : string;
      description : string option;
      parameters : (string * schema) list;
      parameter_order : string list;
      request : string option;
      response : string option;
      scopes : string list;
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
      id : string;
      methods : (string * method_) list;
      resources : resource list;
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
      name : string;
      version : string;
      base_url : string;
      scopes : scope list;
      schemas : (string * schema) list;
      resources : resource list;
    }

  let parse path =
    let json = Yojson.Basic.from_file path in
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

  let pretty s =
    let b = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | 'A' .. 'Z' as c ->
          if i > 0 then Buffer.add_char b '_';
          Buffer.add_char b (Char.lowercase c)
      | c ->
          Buffer.add_char b c
    done;
    match Buffer.contents b with
    | "type" -> "type_"
    | s -> s

  let emit_separated sep f oc l =
    match l with
    | [] -> ()
    | x :: xs -> f oc x; List.iter (fun x -> fprintf oc "%s%a" sep f x) xs

  let emit_value parameter oc s =
    match parameter.type_descr with
    | String _ -> fprintf oc "%S" s
    | Enum _ -> fprintf oc "`%s" (String.capitalize (pretty s))
    | Boolean -> fprintf oc "%s" s
    | Integer _ -> fprintf oc "%d" (int_of_string s)
    | _ -> ksprintf failwith "emit_value: not supported (%S)" s

  let emit_parameter oc (key, parameter) =
    match parameter with
    | {required = false; default = None; repeated = true; _} ->
        fprintf oc "?(%s = [])" (pretty key)
    | {default = Some _; repeated = true; _} ->
        failwith "emit_parameter: repeated parameter with supported characteristics"
    | {required = false; default = None; repeated = false; _} ->
        fprintf oc "?%s" (pretty key)
    | {required = true; default = None; _} ->
        fprintf oc "~%s" (pretty key)
    | {default = Some s; repeated = false; _} ->
        fprintf oc "?(%s = %a)" (pretty key) (emit_value parameter) s

  let emit_parameters oc parameters =
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
                fprintf oc "| `%s -> %S\n" (String.capitalize (pretty s)) s
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
        emit_query_parameter '&' url (pretty key) key oc schema;
        List.iter
          (fun (key, schema) ->
             emit_query_parameter '&' url (pretty key) key oc schema
          ) xs

  let emit_path_parameters path url oc parameters =
    let aux (key, parameter) =
      if parameter.repeated then
        ksprintf failwith "emit_path_parameters: repeated not supported (%s)" key;
      if not parameter.required then
        ksprintf failwith "emit_path_parameters: not required not supported (%s)" key;
      match parameter.type_descr with
      | String _ ->
          fprintf oc "Printf.bprintf %s \"%%a\" urlencode %s;\n" url (pretty key)
      | Integer _ ->
          fprintf oc "Printf.bprintf %s \"%%d\" %s;\n" url (pretty key)
      | Boolean ->
          fprintf oc "Printf.bprintf %s \"%%b\" %s;\n" url (pretty key)
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

  let emit_method base_url oc (key, method_) =
    fprintf oc "let %s ?(token = \"\") %a () =\n" (pretty key) emit_parameters method_.parameters;
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

  let emit_methods base_url oc methods =
    List.iter (emit_method base_url oc) methods

  let rec emit_resource base_url oc resource =
    fprintf oc "module %s = struct\n" (String.capitalize (pretty resource.id));
    emit_methods base_url oc resource.methods;
    emit_resources base_url oc resource.resources;
    fprintf oc "end\n"

  and emit_resources base_url oc resources =
    List.iter (emit_resource base_url oc) resources

  let simple_type = function
    | Integer _ -> "int"
    | Boolean -> "bool"
    | String _ -> "string"
    | _ -> failwith "simple_type: not a simple type"

  let rec emit_schema_property oc (id, schema) =
    fprintf oc "%s : %a option;\n" (pretty id) emit_schema_type schema

  and emit_schema_properties oc properties =
    List.iter (emit_schema_property oc) properties

  and emit_schema_type oc schema =
    match schema.type_descr with
    | Integer _ ->
        fprintf oc "int"
    | String _ ->
        fprintf oc "string"
    | Enum enum ->
        fprintf oc "[\n";
        List.iter (fun (s, _) ->
            fprintf oc "| `%s\n" (String.capitalize (pretty s))
          ) enum;
        fprintf oc "]"
    | Boolean ->
        fprintf oc "bool"
    | Array items ->
        fprintf oc "%a list" emit_schema_type items
    | Object properties ->
        fprintf oc "{\n%a}\n" emit_schema_properties properties
    | Ref ref_ ->
        fprintf oc "%s.t" ref_

  let emit_schema_getter schema_id oc (key, _) =
    fprintf oc
      "let get_%s (x : t) =\nmatch x.%s with Some x -> x | None -> invalid_arg %S\n"
      (pretty key) (pretty key) (sprintf "%s.%s" schema_id key)

  let emit_schema_getter_sig oc (key, schema) =
    fprintf oc "val get_%s : t -> %a\n" (pretty key) emit_schema_type schema

  let emit_schema_constructor_sig oc schema =
    match schema.type_descr with
    | Object properties ->
        let aux oc (key, schema) =
          fprintf oc " ?%s:%a ->\n" (pretty key) emit_schema_type schema
        in
        fprintf oc "val create :\n";
        fprintf oc "%a unit -> t\n" (fun oc l -> List.iter (aux oc) l) properties
    | _ ->
        ()

  let emit_schema_constructor oc schema =
    match schema.type_descr with
    | Object properties ->
        let aux oc (id, _) = fprintf oc " ?%s" (pretty id) in
        fprintf oc "let create %a () =\n" (fun oc l -> List.iter (aux oc) l) properties;
        fprintf oc "{\n";
        List.iter (fun (id, _) -> fprintf oc "%s;\n" (pretty id)) properties;
        fprintf oc "}\n"
    | _ ->
        ()

  let rec emit_schema_of_json oc schema =
    match schema.type_descr with
    | Integer _ ->
        fprintf oc "to_int"
    | String _ ->
        fprintf oc "to_string"
    | Enum enum ->
        fprintf oc "to_string |> function\n";
        List.iter (fun (s, _) ->
            fprintf oc "| %S -> `%s\n" s (String.capitalize (pretty s))
          ) enum;
        fprintf oc "| s -> invalid_arg (%S ^ s)\n" "unrecognized enum: "
    | Boolean ->
        fprintf oc "to_bool"
    | Array items ->
        fprintf oc "to_list |> List.map (%a)" emit_schema_of_json items
    | Ref ref_ ->
        fprintf oc "%s.of_json" ref_
    | Object properties ->
        fprintf oc "fun json ->\n";
        List.iter (fun (key, schema) ->
            fprintf oc "let %s =\n" (pretty key);
            fprintf oc "json |> member %S |> to_option (fun x -> x |> %a)\n"
              key emit_schema_of_json schema;
            fprintf oc "in\n"
          ) properties;
        fprintf oc "{\n";
        List.iter (fun (key, _) -> fprintf oc "%s;\n" (pretty key)) properties;
        fprintf oc "}"

  let emit_schema_module first oc (key, schema) =
    match schema.type_descr with
    | Object properties ->
        fprintf oc "%s %s : sig\n" first key;
        fprintf oc "type t\n";
        emit_schema_constructor_sig oc schema;
        List.iter (emit_schema_getter_sig oc) properties;
        fprintf oc "val of_json: Yojson.Basic.json -> t\n";
        fprintf oc "end = struct\n";
        fprintf oc "type t =\n";
        emit_schema_type oc schema;
        emit_schema_constructor oc schema;
        List.iter (emit_schema_getter key oc) properties;
        fprintf oc "open Yojson.Basic.Util\n";
        fprintf oc "let of_json json =\n";
        fprintf oc "json |> %a\n" emit_schema_of_json schema;
        fprintf oc "end\n"
    | _ ->
        assert false

  let emit_schemas oc schemas =
    let aux = function
      | [] -> ()
      | x :: xs ->
          emit_schema_module "module rec" oc x;
          List.iter (fun x -> emit_schema_module "and" oc x) xs
    in
    aux schemas

  let emit oc api =
    fprintf oc "%s\n" prologue;
    fprintf oc "module %s (Http : Cohttp_lwt.Client) = struct\n"
      (String.capitalize (pretty api.version));
    emit_schemas oc api.schemas;
    emit_resources api.base_url oc api.resources;
    fprintf oc "end\n";
    flush oc
end

let () =
  Parser.parse "gmail.json" |> Emit.emit stdout
