
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

  let option_map f = function
    | None -> None
    | Some x -> Some (f x)

  type type_ =
    | Object
    | String
    | Integer
    | Array
    | Boolean

  let type_of_string id =
    match id with
    | "object" -> Object
    | "string" -> String
    | "integer" -> Integer
    | "array" -> Array
    | "boolean" -> Boolean
    | _ -> Printf.ksprintf failwith "type_of_string: %s" id

  type annotation =
    | Required of string list

  let annotation_of_json (id, json) =
    match id with
    | "required" ->
        let selectors = json |> to_list |> List.map to_string in
        Required selectors
    | _ ->
        Printf.ksprintf failwith "annotations_of_json: %s" id

  type items =
    {
      ref_ : string option;
      type_ : type_ option;
    }

  let item_of_json json =
    let ref_ = json |> member "$ref" |> to_string_option in
    let type_ = json |> member "type" |> to_string_option |> option_map type_of_string in
    {ref_; type_}

  type schema =
    {
      id : string option;
      type_ : type_ option;
      ref_ : string option;
      required : string list;
      description : string option;
      enum : string list;
      enum_descriptions : string list;
      format : string option;
      items : items option;
      properties : (string * schema) list;
    }

  let rec schema_of_json (schema_id, json) =
    let id = json |> member "id" |> to_string_option in
    let type_ = json |> member "type" |> to_string_option |> option_map type_of_string in
    let ref_ = json |> member "$ref" |> to_string_option in
    let required =
      [json] |> filter_member "annotations" |> filter_member "required" |> flatten |>
      filter_string
    in
    let description = json |> member "description" |> to_string_option in
    let enum = [json] |> filter_member "enum" |> flatten |> filter_string in
    let enum_descriptions =
      [json] |> filter_member "enumDescriptions" |> flatten |> filter_string
    in
    let format = json |> member "format" |> to_string_option in
    let items = json |> member "items" |> to_option item_of_json in
    let properties =
      [json] |> filter_member "properties" |> List.map to_assoc |> List.flatten |>
      List.map schema_of_json
    in
    schema_id,
    {id; type_; description; required; ref_; enum; enum_descriptions; format; items; properties}

  type scope =
    {
      id : string;
      description : string;
    }

  let scope_of_json (id, json) =
    let description = json |> member "description" |> to_string in
    {id; description}

  type parameter =
    {
      id : string;
      type_ : type_;
      description : string;
      default : string option;
      required : bool;
      repeated : bool;
      location : string;
    }

  let parameter_of_json (id, json) =
    let type_ = json |> member "type" |> to_string |> type_of_string in
    let description = json |> member "description" |> to_string in
    let default = json |> member "default" |> to_string_option in
    let required = json |> member "required" |> to_bool_option in
    let required = match required with Some b -> b | None -> false in
    let repeated = json |> member "repeated" |> to_bool_option in
    let repeated = match repeated with Some b -> b | None -> false in
    let location = json |> member "location" |> to_string in
    {id; type_; description; default; required; repeated; location}

  type method_ =
    {
      name : string;
      id : string;
      path : string;
      http_method : string;
      description : string option;
      parameters : parameter list;
      parameter_order : string list;
      request : string option;
      response : string option;
      scopes : string list;
    }

  let method_of_json (name, json) =
    let id = json |> member "id" |> to_string in
    let path = json |> member "path" |> to_string in
    let http_method = json |> member "httpMethod" |> to_string in
    let description = json |> member "description" |> to_string_option in
    let parameters =
      [json] |> filter_member "parameters" |> List.map to_assoc |> List.flatten |>
      List.map parameter_of_json
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
    {id; name; path; http_method; description; parameters; parameter_order;
     request; response; scopes}

  type resource =
    {
      id : string;
      methods : method_ list;
      resources : resource list;
    }

  let rec resource_of_json (id, json) =
    let methods =
      [json] |> filter_member "methods" |> List.map to_assoc |> List.flatten |>
      List.map method_of_json
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
    let schemas = json |> member "schemas" |> to_assoc |> List.map schema_of_json in
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

  let emit_value type_ oc s =
    match type_, s with
    | String, _ -> fprintf oc "%S" s
    | Boolean, ("true" | "false") -> fprintf oc "%s" s
    | Integer, _ -> fprintf oc "%d" (int_of_string s)
    | _ -> ksprintf failwith "emit_value: not supported (%S)" s

  let emit_parameter oc parameter =
    match parameter with
    | {required = false; default = None; repeated = true; _} ->
        fprintf oc "?(%s = [])" (pretty parameter.id)
    | {default = Some _; repeated = true; _} ->
        failwith "emit_parameter: repeated parameter with supported characteristics"
    | {required = false; default = None; repeated = false; _} ->
        fprintf oc "?%s" (pretty parameter.id)
    | {required = true; default = None; _} ->
        fprintf oc "~%s" (pretty parameter.id)
    | {default = Some s; repeated = false; _} ->
        fprintf oc "?(%s = %a)" (pretty parameter.id) (emit_value parameter.type_) s

  let emit_parameters oc parameters =
    emit_separated " " emit_parameter oc parameters

  let rec emit_query_parameter first url id oc parameter =
    match parameter.repeated with
    | true ->
        fprintf oc "if %s <> [] then begin\n" id;
        emit_query_parameter first url (sprintf "(List.hd %s)" id) oc
          {parameter with repeated = false};
        fprintf oc "List.iter (fun x ->\n";
        emit_query_parameter '&' url "x" oc {parameter with repeated = false};
        fprintf oc ") (List.tl %s);\nend;\n" id
    | false ->
        begin match parameter.required, parameter.default, parameter.type_ with
          | true, _, String | false, Some _, String ->
              fprintf oc "Printf.bprintf %s \"%c%s=%%a\" urlencode %s;\n"
                url first parameter.id id
          | true, _, Integer | false, Some _, Integer ->
              fprintf oc "Printf.bprintf %s \"%c%s=%%d\" %s;\n" url first parameter.id id
          | true, _, Boolean | false, Some _, Boolean ->
              fprintf oc "Printf.bprintf %s \"%c%s=%%b\" %s;\n" url first parameter.id id
          | false, None, _ ->
              fprintf oc "(match %s with None -> () | Some x ->\n%a);\n"
                id (emit_query_parameter first url "x") {parameter with required = true}
          | _ ->
              failwith "emit_query_parameter: unsupported type"
        end

  let emit_query_parameters url oc parameters =
    fprintf oc "Printf.bprintf %s \"?access_token=%%s\" token;\n" url;
    match parameters with
    | [] -> ()
    | (x : parameter) :: xs ->
        emit_query_parameter '&' url (pretty x.id) oc x;
        List.iter
          (fun (x : parameter) ->
             emit_query_parameter '&' url (pretty x.id) oc x)
          xs

  let emit_path_parameters path url oc parameters =
    let aux parameter =
      if parameter.repeated then
        ksprintf failwith "emit_path_parameters: repeated not supported (%s)"
          parameter.id;
      if not parameter.required then
        ksprintf failwith "emit_path_parameters: not required not supported (%s)"
          parameter.id;
      match parameter.type_ with
      | String ->
          fprintf oc "Printf.bprintf %s \"%%a\" urlencode %s;\n"
            url (pretty parameter.id)
      | Integer ->
          fprintf oc "Printf.bprintf %s \"%%d\" %s;\n" url (pretty parameter.id)
      | Boolean ->
          fprintf oc "Printf.bprintf %s \"%%b\" %s;\n" url (pretty parameter.id)
      | _ ->
          failwith "emit_query_parameter: unsupported type"
    in
    let rec loop i =
      match String.index_from path i '{' with
      | j ->
          if i > 0 then fprintf oc "Printf.bprintf %s %S;\n" url (String.sub path i (j-i));
          let k = String.index_from path j '}' in
          let id = String.sub path (j+1) (k-j-1) in
          let parameter =
            List.find (fun (parameter : parameter) -> parameter.id = id) parameters
          in
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

  let emit_method base_url oc (method_ : method_) =
    fprintf oc "let %s ?(token = \"\") %a () =\n" (pretty method_.name) emit_parameters method_.parameters;
    fprintf oc "let url = Buffer.create 0 in\n";
    fprintf oc "Printf.bprintf url \"%s\";\n" base_url;
    let query_parameters, path_parameters =
      List.partition (function
          | {location = "query"; _} -> true
          | {location = "path"; _} -> false
          | {location = s; _} ->
              ksprintf failwith "emit_method: unsupported location (%s)" s
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
    | Integer -> "int"
    | Boolean -> "bool"
    | String -> "string"
    | _ -> failwith "simple_type: not a simple type"

  let rec emit_schema_property oc (id, schema) =
    fprintf oc "%s : %a option;\n" (pretty id) emit_schema_type schema

  and emit_schema_properties oc properties =
    List.iter (emit_schema_property oc) properties

  and emit_schema_type oc (schema : schema) =
    let id = match schema.id with None -> "" | Some id -> id in
    match schema.type_ with
    | Some Integer ->
        fprintf oc "int"
    | Some String ->
        begin match schema.enum with
          | [] ->
              fprintf oc "string"
          | _ :: _ ->
              fprintf oc "[\n";
              List.iter (fun s ->
                  fprintf oc "| `%s\n" (String.capitalize (pretty s))
                ) schema.enum;
              fprintf oc "]"
        end
    | Some Boolean ->
        fprintf oc "bool"
    | Some Array ->
        let items =
          match schema.items with
          | None ->
              ksprintf failwith "emit_schema: 'array' schema items not given (%S)" id
          | Some items ->
              items
        in
        let s =
          match items.ref_, items.type_ with
          | Some x, None -> sprintf "%s.t" x
          | None, Some x -> simple_type x
          | _ -> ksprintf failwith "emit_schema: malformed items (%S)" id
        in
        fprintf oc "%s list" s
    | Some Object ->
        fprintf oc "{\n%a}\n" emit_schema_properties schema.properties
    | None ->
        let x =
          match schema.ref_ with
          | Some x -> x
          | None ->
              ksprintf failwith "emit_schema: type_ = None && ref_ = None (%S)" id
        in
        fprintf oc "%s.t" x

  let emit_schema_getter schema_id oc (id, _) =
    fprintf oc
      "let get_%s (x : t) =\nmatch x.%s with Some x -> x | None -> invalid_arg %S\n"
      (pretty id) (pretty id) (sprintf "%s.%s" schema_id id)

  let emit_schema_getter_sig oc (id, schema) =
    fprintf oc "val get_%s : t -> %a\n" (pretty id) emit_schema_type schema

  let emit_schema_constructor_sig oc (schema : schema) =
    let aux oc (id, schema) = fprintf oc " ?%s:%a ->\n" (pretty id) emit_schema_type schema in
    fprintf oc "val create :\n";
    fprintf oc "%a unit -> t\n" (fun oc l -> List.iter (aux oc) l) schema.properties

  let emit_schema_constructor oc (schema : schema) =
    let aux oc (id, _) = fprintf oc " ?%s" (pretty id) in
    fprintf oc "let create %a () =\n" (fun oc l -> List.iter (aux oc) l) schema.properties;
    fprintf oc "{\n";
    List.iter (fun (id, _) -> fprintf oc "%s;\n" (pretty id)) schema.properties;
    fprintf oc "}\n"

  let emit_schema_of_json json oc (schema : schema) =
    let id = match schema.id with None -> "" | Some id -> id in
    match schema.type_ with
    | Some Integer ->
        fprintf oc "%s |> to_int_option\n" json
    | Some String ->
        begin match schema.enum with
          | [] ->
              fprintf oc "%s |> to_string_option\n" json
          | _ :: _ ->
              fprintf oc "match %s |> to_string_option with\n" json;
              List.iter (fun s ->
                  fprintf oc "| Some %S -> Some `%s\n" s (String.capitalize (pretty s))
                ) schema.enum;
              fprintf oc "| None -> None\n";
              fprintf oc "| Some s -> invalid_arg (%S ^ s)\n" "unrecognized enum: "
        end
    | Some Boolean ->
        fprintf oc "%s |> to_bool_option\n" json
    | Some Array ->
        let items =
          match schema.items with
          | None ->
              ksprintf failwith "emit_schema: 'array' schema items not given (%S)" id
          | Some items ->
              items
        in
        let s =
          match items.ref_, items.type_ with
          | Some x, None -> sprintf "%s.of_json" x
          | None, Some x -> sprintf "to_%s" (simple_type x)
          | _ -> ksprintf failwith "emit_schema: malformed items (%S)" id
        in
        fprintf oc "let json = %s |> to_option to_list in\n" json;
        fprintf oc "match json with None -> None | Some l -> Some (List.map %s l)\n" s
    | Some Object ->
        failwith "emit_schema_of_json: 'object' unsupported"
    | None ->
        let x =
          match schema.ref_ with
          | Some x -> x
          | None ->
              ksprintf failwith "emit_schema: type_ = None && ref_ = None (%S)" id
        in
        fprintf oc "%s |> to_option %s.of_json\n" json x

  let emit_schema_module first oc (id, (schema : schema)) =
    match schema.type_ with
    | Some Object ->
        fprintf oc "%s %s : sig\n" first id;
        fprintf oc "type t\n";
        emit_schema_constructor_sig oc schema;
        List.iter (emit_schema_getter_sig oc) schema.properties;
        fprintf oc "val of_json: Yojson.Basic.json -> t\n";
        fprintf oc "end = struct\n";
        fprintf oc "type t =\n";
        emit_schema_type oc schema;
        emit_schema_constructor oc schema;
        List.iter (emit_schema_getter id oc) schema.properties;
        fprintf oc "open Yojson.Basic.Util\n";
        fprintf oc "let of_json json =\n";
        List.iter (fun (id, schema) ->
            fprintf oc "let %s =\nlet json = json |> member %S in\n%ain\n"
              (pretty id) id (emit_schema_of_json "json") schema
          ) schema.properties;
        fprintf oc "{\n";
        List.iter (fun (id, _) -> fprintf oc "%s;\n" (pretty id)) schema.properties;
        fprintf oc "}\n";
        fprintf oc "end\n"
    | Some _ | None ->
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
