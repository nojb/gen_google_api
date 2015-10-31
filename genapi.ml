let base_url = "https://www.googleapis.com/gmail/v1/users"

module type HTTP_CLIENT = sig
  type 'a io

  val get : string -> string io
end

module type IO = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module V1 (A : sig val token : string end) (IO : IO) (Http : HTTP_CLIENT with type 'a io := 'a IO.t) : sig
  type 'a io = 'a IO.t

  class type label =
    object
      method id : string option
      method label_list_visibility : [`Label_hide | `Label_show | `Label_show_if_unread] option
      method message_list_visibility : [`Hide | `Show] option
      method messages_total : int option
      method messages_unread : int option
      method name : string option
    end

  class type list_labels_response =
    object
      method labels : label list
    end

  class users :
    object
      method labels :
        <
          list : ?user_id:string -> unit -> list_labels_response io;
        >
    end
end = struct
  type 'a io = 'a IO.t

  class type label =
    object
      method id : string option
      method label_list_visibility : [`Label_hide | `Label_show | `Label_show_if_unread] option
      method message_list_visibility : [`Hide | `Show] option
      method messages_total : int option
      method messages_unread : int option
      method name : string option
    end

  class type list_labels_response =
    object
      method labels : label list
    end

  open Yojson.Basic.Util

  let label_of_json json =
    object
      method id = json |> member "id" |> to_string_option
      method label_list_visibility : [`Label_hide|`Label_show|`Label_show_if_unread] option =
        match json |> member "labelListVisibility" |> to_string_option with
        | Some "labelHide" -> Some `Label_hide
        | Some "labelShow" -> Some `Label_show
        | Some "labelShowIfUnread" -> Some `Label_show_if_unread
        | _ -> None
      method message_list_visibility : [`Hide | `Show] option =
        match json |> member "messageListVisibility" |> to_string_option with
        | Some "hide" -> Some `Hide
        | Some "show" -> Some `Show
        | _ -> None
      method messages_total =
        json |> member "messagesTotal" |> to_int_option
      method messages_unread =
        json |> member "messagesUnread" |> to_int_option
      method name =
        json |> member "name" |> to_string_option
    end

  class users =
    object
      method labels =
        object
          method list ?(user_id = "me") () =
            let url = Printf.sprintf "%s/%s/labels?access_token=%s" base_url user_id A.token in
            IO.bind (Http.get url) (fun body ->
                let json = Yojson.Safe.to_basic (Yojson.Safe.from_string body) in
                let labels = json |> member "labels" |> to_list |> List.map label_of_json in
                IO.return (object
                  method labels = labels
                end)
              )
        end
    end
end

module Auth (C : sig val client_id : string val client_secret : string end) :
sig
  val request_token : string list -> string Lwt.t
end = struct
  let urlencode s =
    let b = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | ' ' -> Buffer.add_string b "%20"
      | '!' -> Buffer.add_string b "%21"
      | '"' -> Buffer.add_string b "%22"
      | '.' -> Buffer.add_string b "%2E"
      | '/' -> Buffer.add_string b "%2F"
      | ':' -> Buffer.add_string b "%3A"
      | c -> Buffer.add_char b c
    done;
    Buffer.contents b

  let redirect_uri = "urn:ietf:wg:oauth:2.0:oob"

  let request_token scopes =
    let url =
      Printf.sprintf "https://accounts.google.com/o/oauth2/auth?response_type=code&client_id=%s&redirect_uri=%s&scope=%s"
        C.client_id redirect_uri (urlencode (String.concat " " scopes))
    in
    ignore (Printf.ksprintf Sys.command "open %S" url);
    Printf.printf "\nEnter access code: ";
    let code = read_line () in
    let url = "https://www.googleapis.com/oauth2/v3/token" in
    let body =
      Printf.sprintf
        "code=%s&\
         client_id=%s&\
         client_secret=%s&\
         redirect_uri=%s&\
         grant_type=authorization_code"
        code C.client_id C.client_secret redirect_uri
    in
    let headers = Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded" in
    let open Lwt.Infix in
    Cohttp_lwt_unix.Client.post (Uri.of_string url) ~headers ~body:(Cohttp_lwt_body.of_string body) >>= fun (_, body) ->
    Cohttp_lwt_body.to_string body >>= fun json ->
    let open Yojson.Basic.Util in
    let access_token = json |> Yojson.Basic.from_string |> member "access_token" |> to_string in
    Lwt.return access_token
end

(* open Lwt *)
(* open Cohttp *)
(* open Cohttp_lwt_unix *)

(* let server = *)
(*   let callback _conn req body = *)
(*     body |> Cohttp_lwt_body.to_string >>= fun body -> Server.respond_string ~status:`OK ~body:"" () *)
(*   in *)
(*   Server.create ~mode:(`TCP (`Port 80)) (Server.make ~callback ()) *)

let main () =
  let module A = Auth (struct
let client_id = "921324454177-4gp2pll42hlasklqq7ug0qh2g55up267.apps.googleusercontent.com"
let client_secret = "!!SECRET!!"
end)
  in
  let open Lwt.Infix in
  let t =
    A.request_token ["https://mail.google.com"] >>= fun token ->
    Printf.printf "TOKEN: %s\n%!" token;
    let module Client =
    struct
      module C = Cohttp_lwt_unix.Client
      let get url =
        C.get (Uri.of_string url) >>= fun (_, body) -> Cohttp_lwt_body.to_string body
    end
    in
    (* let t = Client.get auth >>= fun _ -> Lwt.return_unit in *)
    let module V1 = V1 (struct let token = token end) (Lwt) (Client) in
    let users = new V1.users in
    users # labels # list () >>= fun labels ->
    List.iter (fun label -> match label # name with Some label -> Printf.eprintf "LABEL: %s\n%!" label | None -> ()) (labels # labels);
    Lwt.return_unit
  in
  Lwt_unix.run t


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

  type property =
    {
      id : string;
      type_ : type_ option;
      description : string option;
      required : string list;
      ref_ : string option;
      enum : string list;
      enum_descriptions : string list;
      format : string option;
      items : items option;
    }

  let property_of_json (id, json) =
    let type_ = json |> member "type" |> to_string_option |> option_map type_of_string in
    let description = json |> member "description" |> to_string_option in
    let required =
      [json] |> filter_member "annotations" |> filter_member "required" |> flatten |>
      filter_string
    in
    let ref_ = json |> member "$ref" |> to_string_option in
    let enum = [json] |> filter_member "enum" |> flatten |> filter_string in
    let enum_descriptions =
      [json] |> filter_member "enumDescriptions" |> flatten |> filter_string
    in
    let format = json |> member "format" |> to_string_option in
    let items = json |> member "items" |> to_option item_of_json in
    {id; type_; description; required; ref_; enum; enum_descriptions; format; items}

  type schema =
    {
      id : string;
      type_ : type_;
      description : string option;
      properties : property list;
    }

  let schema_of_json (_, json) =
    let id = json |> member "id" |> to_string in
    let type_ = json |> member "type" |> to_string |> type_of_string in
    let description = json |> member "description" |> to_string_option in
    let properties = json |> member "properties" |> to_assoc |> List.map property_of_json in
    {id; type_; description; properties}

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
      location : string;
    }

  let parameter_of_json (id, json) =
    let type_ = json |> member "type" |> to_string |> type_of_string in
    let description = json |> member "description" |> to_string in
    let default = json |> member "default" |> to_string_option in
    let required = json |> member "required" |> to_bool_option in
    let required = match required with Some b -> b | None -> false in
    let location = json |> member "location" |> to_string in
    {id; type_; description; default; required; location}

  type method_ =
    {
      name : string;
      id : string;
      path : string;
      http_method : string;
      description : string option;
      parameters : parameter list;
      parameter_order : string list;
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
    let response = None in
    let scopes = [json] |> filter_member "scopes" |> flatten |> filter_string in
    {id; name; path; http_method; description; parameters; parameter_order;
     response; scopes}

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
      schemas : schema list;
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
    let schemas = if false then json |> member "schemas" |> to_assoc |> List.map schema_of_json else [] in
    let resources = json |> member "resources" |> to_assoc |> List.map resource_of_json in
    {name; version; base_url; scopes; schemas; resources}

end
