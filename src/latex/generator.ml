open Odoc_document.Types

module Doctree = Odoc_document.Doctree

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list_concat_map ?sep ~f xs in
    match sep with
    | None -> hd @ tl
    | Some sep -> hd @ sep :: tl


type elt =
  | Txt of string
  | Section of {level:int; label:string option; content:t }
  | Escaped of string
  | Verbatim of string
  | Ref of string * t option
  | Label of string
  | Raw of string
  | Tag of string * t
  | Style of [`Emphasis|`Bold|`Superscript|`Subscript|`Italic] * t
  | BlockCode of t
  | InlineCode of t
  | Break
  | List of { typ : Block.list_type; items: t list }
  | Description of (t * t) list
  | Table of t list list

and t = elt list




let str = Format.pp_print_string


let option ppf pp = Format.fprintf ppf "[%t]" pp
let macro name ?(options=[]) pp ppf content =
  Format.fprintf ppf {|\%s%a{%a}|} name
    (Format.pp_print_list option) options
    pp content



let escape_ref ppf s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '_' -> Format.fprintf ppf "--underscore--"
    | c -> Format.fprintf ppf "%c" c
  done

let mlabel ppf = macro "label" escape_ref ppf
let verbatim = macro "verbatim" str
let mbegin ?options = macro "begin" ?options str
let mend = macro "end" str


let label = function
  | None -> []
  | Some  {Odoc_document.Url.Anchor.anchor ; _ } -> [Label anchor]



let mstyle = function
  | `Emphasis | `Italic -> macro "emph"
  | `Bold -> macro "bold"
  | `Subscript -> macro "textsubscript"
  | `Superscript -> macro "textsuperscript"



let break ppf () = Format.fprintf ppf {|@,|}
let tbreak ppf () = Format.fprintf ppf "@,"


let env name pp ?(args=[]) ppf content =
  mbegin ppf name;
  Format.pp_print_list (fun ppf pp -> Format.fprintf ppf "{%t}" pp) ppf args;
  pp ppf content;
  mend ppf name;
  tbreak ppf ()


let texttt = macro "texttt"
let alltt = env "alltt"


let level_macro = function
  | 0 -> macro "section"
  | 1 -> macro "section"
  | 2 -> macro "subsection"
  | 3 | _ -> macro "subsubsection"


let space ppf () = Format.fprintf ppf " "

let list kind pp ppf x =
  let list =
    match kind with
    | Block.Ordered -> env "enumerate"
    | Unordered -> env "itemize" in
  let elt ppf = macro "item" pp ppf in
  list
    (Format.pp_print_list ~pp_sep:space elt)
    ppf
    x


let bind pp x ppf = pp ppf x

let escaped ppf = function
  | "#45" -> str ppf "-"
  | "gt" -> str ppf ">"
  | "#8288" -> ()
  | s -> str ppf s


let rec pp_elt ppf = function
  | Txt x -> if String.trim x <> "" then Format.fprintf ppf "%s" x
  | Section {level; label; content } ->
    let with_label ppf (label,content) =
      pp ppf content;
      match label with
      | None -> ()
      | Some label -> mlabel ppf label in
    level_macro level with_label ppf (label,content);
    tbreak ppf ()
  | Escaped s -> escaped ppf s
  | Break -> break ppf ()
  | Raw s -> str ppf s
  | Tag (x,t) -> env x pp ppf t
  | Verbatim s -> verbatim ppf s
  | Ref (l,x) -> href ppf (l,x)
  | Style (s,x) -> mstyle s pp ppf x
  | BlockCode x -> alltt pp ppf x
  | InlineCode x -> texttt pp ppf x
  | List {typ; items} -> list typ pp ppf items
  | Description items ->
    let pair ppf (d,x) = macro "item" ~options:[bind pp d] pp ppf x in
    env "description"
      (Format.pp_print_list ~pp_sep:space pair)
      ppf items
  | Table [] -> ()
  | Table (a::_ as rows) ->
    let columns = List.length a in
    let row ppf x =
      let ampersand ppf () = Format.fprintf ppf "& " in
      Format.pp_print_list ~pp_sep:ampersand pp ppf x;
      break ppf () in
    let rec repeat n ppf c = if n = 0 then () else
        Format.fprintf ppf "%s%a" c (repeat @@ n - 1) c in
    env "tabular"
      ~args:[bind (repeat columns) "c" ]
      (Format.pp_print_list row)  ppf
       rows
  | Label x -> mlabel ppf x
  | _ -> .

and pp ppf x = Format.fprintf  ppf "%a" (Format.pp_print_list ~pp_sep:space pp_elt) x

and href ppf (l,t) =
  match t with
  | None ->
    macro "ref" str ppf l
  | Some txt ->
    Format.fprintf ppf {|\hyperref[%a]{%a\ref*{%a}}|} escape_ref l pp txt escape_ref l


type package = { name:string; options:string list }
type header = {
  packages: package list;
  kind: string;
  options: string list
}
type doc = { header: header option; content: t }


let raw_markup (t:Raw_markup.t) =
  match t with
  | `Latex, c -> [Raw c]
  | _ -> []


module Link = struct

  let rec _flatten_path ppf (x: Odoc_document.Url.Path.t) = match x.parent with
    | Some p -> Format.fprintf ppf "%a-%s-%s" _flatten_path p x.kind x.name
    | None -> Format.fprintf ppf "%s-%s" x.kind x.name

 let href ~resolve:_ (x:Odoc_document.Url.t) =
   Format.asprintf "%s"
(*     flatten_path x.page 
     x.kind *)
     x.anchor
end

let source k (t : Source.t) =
  let rec token (x : Source.token) = match x with
    | Elt i -> k i
    | Tag (None, l) -> tokens l
    | Tag (Some s, l) -> [Tag(s, tokens l)]
  and tokens t = list_concat_map t ~f:token
  in
  tokens t

let rec internalref
    ~resolve
    (t : InternalLink.t) =
  match t with
  | Resolved (uri, content) ->
    let href = Link.href ~resolve uri in
    Ref(href, Some (inline ~resolve content))
  | Unresolved content ->
    (* let title =
     *   Html.a_title (Printf.sprintf "unresolved reference to %S"
     *       (ref_to_string ref)
     * in *)
    Ref("xref-unresolved", Some(inline ~resolve content))

and inline ~resolve (l : Inline.t) =
  let one (t : Inline.one) =
    match t.desc with
    | Text s -> [Txt s]
    | Entity s -> [Escaped s]
    | Linebreak -> []
    | Styled (style, c) ->
      [Style(style, inline ~resolve c)]
    | Link (href, c) ->
      let content = inline ~resolve  c in
      [Ref(href, Some content)]
    | InternalLink c ->
      [internalref ~resolve c]
    | Source c ->
      [InlineCode (source (inline ~resolve) c)]
    | Raw_markup r ->
      raw_markup r in
  list_concat_map ~f:one l

let heading ~resolve (h : Heading.t) =
  let content = inline ~resolve h.title in
  Section { label=h.label; level=h.level; content }

let rec block ~resolve (l: Block.t)  =
  let one (t : Block.one) =
    match t.desc with
    | Inline i ->
      inline ~resolve i
    | Paragraph i ->
      inline ~resolve i
    | List (typ, l) ->
      [List { typ; items = List.map (block ~resolve) l }]
    | Description l ->
      [Description (List.map (fun (i,b) ->
          let i = inline ~resolve i in
          (i, block ~resolve b)
        ) l ) ]
    | Raw_markup r ->
      raw_markup r
    | Verbatim s -> [Verbatim s]
    | Source c ->
      [BlockCode (source (inline ~resolve) c) ]
  in
  list_concat_map ~sep:Break l ~f:one


let documentedSrc ~resolve (t : DocumentedSrc.t) =
  let open DocumentedSrc in
  let take_code l =
    Doctree.Take.until l ~classify:(function
      | Code code -> Accum code
      | Subpage p -> Accum p.summary
      | _ -> Stop_and_keep
    )
  in
  let take_descr l =
    Doctree.Take.until l ~classify:(function
      | Documented { attrs; anchor; code; doc }  ->
        Accum [{DocumentedSrc. attrs ; anchor ; code = `D code; doc }]
      | Nested { attrs; anchor; code; doc } ->
        Accum [{DocumentedSrc. attrs ; anchor ; code = `N code; doc }]
      | _ -> Stop_and_keep
    )
  in
  let rec to_latex t = match t with
    | [] -> []
    | (Code _ | Subpage _) :: _ ->
      let code, _, rest = take_code t in
      BlockCode (source (inline ~resolve) code)
      :: to_latex rest
    | (Documented _ | Nested _) :: _ ->
      let l, _, rest = take_descr t in
      let one dsrc =
        let content = match dsrc.code with
          | `D code -> inline ~resolve code
          | `N n -> to_latex n
        in
        let doc = block ~resolve dsrc.doc in
        (label dsrc.anchor @ content) @ doc
      in
      Break :: Table [List.map one l] :: to_latex rest
  in
  to_latex t

let rec is_only_text l =
  let is_text : Item.t -> _ = function
    | Heading _ | Text _ -> true
    | Declaration _
      -> false
    | Subpage { content = { content = Items items; _ }; _ }
      -> is_only_text items
    | Subpage { content = { content = Page p; _ }; _ }
      -> is_only_text p.items
  in
  List.for_all is_text l

let items ~resolve l =
  let[@tailrec] rec walk_items
      ~only_text acc (t : Item.t list) =
    let continue_with rest elts =
      walk_items ~only_text (List.rev_append elts acc) rest
    in
    match t with
    | [] -> List.rev acc
    | Text _ :: _ as t ->
      let text, _, rest = Doctree.Take.until t ~classify:(function
        | Item.Text text -> Accum text
        | _ -> Stop_and_keep)
      in
      let content = block ~resolve text in
      let elts = content in
      elts
      |> continue_with rest
    | Heading h :: rest ->
      [heading ~resolve h]
      |> continue_with rest
    | Subpage
        { kind=_; anchor; doc ; content = { summary; status=_; content } }
      :: rest ->
      let included = match content with
        | Items i -> items i
        | Page p -> items p.items
      in
      let docs = block ~resolve doc in
      let _summary = source (inline ~resolve) summary in
      let content = included in
      (label anchor @ docs @ content)
      |> continue_with rest

    | Declaration {Item. kind=_; anchor ; content ; doc} :: rest ->
      let content =  label anchor @ documentedSrc ~resolve content in
      let elts = match doc with
        | [] -> content
        | docs ->
          [Description [content, block ~resolve docs]]
      in
      continue_with rest elts

  and items l = walk_items ~only_text:(is_only_text l) [] l in
  items l


module Doc = struct

  let hyperref = {name="hyperref"; options=[]}
  let _default_header = { packages = [hyperref] ; kind="article"; options = [] }

  let pp ppf x =
    Format.fprintf ppf "@[<v>%a@]@." pp x.content

  let page_creator ?head content =
    { header=head; content = content }

let make ?head filename content children =
  let doc = page_creator ?head content in
  let content ppf = pp ppf doc in
  {Odoc_document.Renderer. filename; content; children }
end

module Page = struct

  let on_sub (subp : Subpage.t) = match subp.status with
    | `Closed | `Open | `Default -> None
    | `Inline -> Some 0

  let rec subpage ?theme_uri {Subpage. content ; _} =
    match content with
    | Page p -> [page ?theme_uri p]
    | Items _ -> []

  and subpages  ?theme_uri i =
    list_concat_map ~f:(subpage ?theme_uri) @@ Doctree.Subpages.compute i

  and page ?theme_uri ({Page. title; header; items = i; url=_ } as p) =
    let resolve = () in
    let i = Doctree.Shift.compute ~on_sub i in
    let subpages = subpages ?theme_uri p in
    let header = items ~resolve header in
    let content = items ~resolve i in
    let page =
      Doc.make title (header@content) subpages
    in
    page

end

let render ?theme_uri page = Page.page ?theme_uri page
