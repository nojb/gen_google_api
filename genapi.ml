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

let () =
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
