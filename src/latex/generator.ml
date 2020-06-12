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


type text_kind = Verbatim | Textual

type elt =
  | Txt of { kind: text_kind; words: string list }
  | Section of {level:int; label:string option; content:t }
  | Verbatim of string
  | Internal_ref of string * t option
  | External_ref of string * t option
  | Label of string
  | Raw of string
  | Tag of string * t
  | Style of [`Emphasis|`Bold|`Superscript|`Subscript|`Italic] * t
  | BlockCode of t
  | InlineCode of t
  | Break
  | List of { typ : Block.list_type; items: t list }
(*  | Description of (t * t) list *)
  | Table of t list list

and t = elt list




let str = Format.pp_print_string


let option ppf pp = Format.fprintf ppf "[%t]" pp
let macro name ?(options=[]) pp ppf content =
  Format.fprintf ppf {|\%s%a{%a}|} name
    (Format.pp_print_list option) options
    pp content




let escape_text =
  let b = Buffer.create 17 in
  fun ppf s ->
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '{' -> Buffer.add_string b "\\{"
      | '}' ->  Buffer.add_string b "\\}"
      | '\\' ->  Buffer.add_string b "\\textbackslash{}"
      | '%' ->  Buffer.add_string b "\\%"
      | '~' ->  Buffer.add_string b "\texttilde{}"
      | '^' -> Buffer.add_string b "\textasciicircum{}"
      | '_' ->  Buffer.add_string b "\\_"
      | '&' ->  Buffer.add_string b "\\&"
      | '#' ->  Buffer.add_string b "\\#"
      | '$' ->  Buffer.add_string b "\\$"


      | c ->  Buffer.add_char b c
    done;
    let s = Buffer.contents b in
    Buffer.reset b;
    Fmt.string ppf s



let escape_ref ppf s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '~' -> Format.fprintf ppf "---"
    | '_' -> Format.fprintf ppf "+"
    | '+' -> Format.fprintf ppf "++"
    | c -> Format.fprintf ppf "%c" c
  done

module Link = struct

  let rec flatten_path ppf (x: Odoc_document.Url.Path.t) = match x.parent with
    | Some p -> Format.fprintf ppf "%a-%s-%s" flatten_path p x.kind x.name
    | None -> Format.fprintf ppf "%s-%s" x.kind x.name

 let label (x:Odoc_document.Url.t) =
   Format.asprintf "%a-%s"
     flatten_path x.page
     x.anchor

end



let bind pp x ppf = pp ppf x
let mlabel ppf = macro "label" escape_ref ppf
let verbatim = macro "verbatim" str
let mbegin ?options = macro "begin" ?options str
let mend = macro "end" str
let mhyperref ref x =
  (* {|\hyperref[%a]{%a\ref*{%a}}|} *)
  match ref with
  | "" ->  x
  | s -> macro "hyperref" ~options:[bind escape_ref s] x


let texttt ppf s =
    macro "texttt" escape_text ppf s

let label = function
  | None -> []
  | Some  x (* {Odoc_document.Url.Anchor.anchor ; page;  _ }*) -> [Label (Link.label  x)]



let mstyle = function
  | `Emphasis | `Italic -> macro "emph"
  | `Bold -> macro "textbf"
  | `Subscript -> macro "textsubscript"
  | `Superscript -> macro "textsuperscript"



let break ppf () = Format.fprintf ppf {|\\@,|}
let pbreak ppf () = Format.fprintf ppf {|@,@,|}

let _vbreak ppf space =
  pbreak ppf (); macro "vspace" Format.pp_print_string ppf space

let tbreak ppf () = Format.fprintf ppf "@,"


let env name pp ?(opts=[]) ?(args=[]) ppf content =
  mbegin ppf name;
  Format.pp_print_list (fun ppf pp -> Format.fprintf ppf "[%t]" pp) ppf opts;
  Format.pp_print_list (fun ppf pp -> Format.fprintf ppf "{%t}" pp) ppf args;
  pp ppf content;
  mend ppf name;
  tbreak ppf ()


let inline_code = macro "inlinecode"
let block_code = macro "blockcode"


let level_macro = function
  | 0 -> (fun x -> macro "section" x)
  | 1 -> (fun x -> macro "subsection" x)
  | 2 -> (fun x -> macro "subsubsection" x)
  | 3 | _ -> fun pp ppf x ->
    macro "paragraph" ~options:[bind pp x] (fun _ () -> ()) ppf ()


let _space ppf () = Format.fprintf ppf " "
let _dotted_space ppf ()=Format.pp_print_string ppf "."
let none _ppf () = ()


let list kind pp ppf x =
  let list =
    match kind with
    | Block.Ordered -> env "enumerate"
    | Unordered -> env "itemize" in
  let elt ppf = macro "item" pp ppf in
  list
    (Format.pp_print_list ~pp_sep:tbreak elt)
    ppf
    x


let escape_entity  = function
  | "#45" -> "-"
  | "gt" -> ">"
  | "#8288" -> ""
  | s -> s

let rec pp_elt ppf = function
  | Txt { kind; words } ->
    let word_printer = match kind with
      | Textual -> escape_text
      | Verbatim -> texttt in
    Format.pp_print_list word_printer ~pp_sep:none ppf words
  | Section {level; label; content } ->
    let with_label ppf (label,content) =
      pp ppf content;
      match label with
      | None -> ()
      | Some label -> mlabel ppf label in
    level_macro level with_label ppf (label,content);
    tbreak ppf ()
  | Break -> pbreak ppf ()
  | Raw s -> str ppf s
  | Tag (x,t) -> env x pp ppf t
  | Verbatim s -> verbatim ppf s
  | Internal_ref (l,x) -> hyperref ppf (l,x)
  | External_ref (l,x) -> href ppf (l,x)
  | Style (s,x) -> mstyle s pp ppf x
  | BlockCode [] -> ()
  | BlockCode x -> block_code pp ppf x
  | InlineCode x -> inline_code pp ppf x
  | List {typ; items} -> list typ pp ppf items
(*  | Description items ->
    (* let pair ppf (d,x) = macro "item" ~options:[bind pp d] pp ppf x in
    env "description"
      (Format.pp_print_list ~pp_sep:space pair)
      ppf items *)
    let pair ppf (d,x) =
      Format.fprintf ppf "%a%a%a" pp d break () pp x in
    Format.fprintf ppf "%a%a%a"
      vbreak "0.2cm"
      (Format.pp_print_list pair) items
      vbreak "0.2cm"
*)
  | Table [] -> ()
  | Table (a  :: _ as l) ->
    let columns = List.length a in
    let row ppf x =
      let ampersand ppf () = Format.fprintf ppf "& " in
      Fmt.list ~sep:ampersand pp ppf x;
      break ppf () in
    let matrix ppf m = List.iter (row ppf) m in
    let rec repeat n s ppf = if n = 0 then () else
        Format.fprintf ppf "%t%t" s (repeat (n - 1) s) in
    break ppf ();
    let frac ppf = Format.fprintf ppf " p{%.2f\\linewidth} " (1. /. float columns) in
(*    Format.fprintf ppf "{";*)
    env "tabular"
      ~args:[(*Format.dprintf {|\linewidth|};*) repeat columns frac  ]
      matrix ppf l;
(*    Format.fprintf ppf "}";*)
    break ppf ()
  | Label x -> mlabel ppf x
  | _ -> .

and pp ppf = function
  | [] -> ()
  | Break :: (Break :: _ as q) ->
    pp ppf q
  | a :: q ->
    pp_elt ppf a; pp ppf q

and hyperref ppf (l,t) =
  match t with
  | None ->
    macro "ref" str ppf l
  | Some txt -> mhyperref l pp ppf txt

and href ppf (l,txt) =
  match txt with
  | Some txt ->
    Format.fprintf ppf {|\href{%s}{%a}|} l pp txt
  | None -> Format.fprintf ppf {|\url{%s}|} l

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



let source k (t : Source.t) =
  let rec token (x : Source.token) = match x with
    | Elt i -> k i
    | Tag (None, l) -> tokens l
    | Tag (Some s, l) -> [Tag(s, tokens l)]
  and tokens t = list_concat_map t ~f:token
  in
  tokens t

let if_not_empty f x =
  if x = "" then [] else [f x]

let block_code_text t =
  let fragment = String.split_on_char '\n' t in
  list_concat_map ~sep:Break ~f:(if_not_empty (fun x -> Txt { kind=Verbatim; words=[x]})) fragment

let rec internalref
    ~resolve
    (t : InternalLink.t) =
  match t with
  | Resolved (uri, content) ->
    let href = Link.label uri in
    Internal_ref(href, Some (inline ~in_source:false ~resolve content))
  | Unresolved content ->
    (* let title =
     *   Html.a_title (Printf.sprintf "unresolved reference to %S"
     *       (ref_to_string ref)
     * in *)
    Internal_ref("xref-unresolved", Some(inline ~in_source:false ~resolve content))

and inline ~in_source ~resolve (l : Inline.t) =
  let one (t : Inline.one) =
    match t.desc with
    | Text _s -> assert false
    | Linebreak -> [Break]
    | Styled (style, c) ->
      [Style(style, inline ~in_source ~resolve c)]
    | Link (ext, c) ->
      let content = inline ~resolve ~in_source:false  c in
      [External_ref(ext, Some content)]
    | InternalLink c ->
      [internalref ~resolve c]
    | Source c ->
      [InlineCode (source (inline ~resolve ~in_source:true) c)]
    | Raw_markup r -> raw_markup r
    | Entity s -> [Txt { kind=if in_source then Verbatim else Textual; words = [escape_entity s]}] in

  let take_text (l: Inline.t) =
    Doctree.Take.until l ~classify:(function
      | { Inline.desc = Text code; _ } -> Accum [code]
      | { desc = Entity e; _ } -> Accum [escape_entity e]
      | _ -> Stop_and_keep
    )
  in
(* if in_source then block_code_txt s else if_not_empty (fun x -> Txt x) s *)
  let rec prettify = function
    | { Inline.desc = Inline.Text _; _ } :: _ as l ->
      let words, _, rest = take_text l in
      let text =
        if in_source then
          List.concat_map  block_code_text words
        else
          let words = List.filter (( <> ) "" ) words in
          [Txt { kind = if in_source then Verbatim else Textual; words }]
      in
      text @ prettify rest
    | o :: q -> one o @ prettify q
    | [] -> [] in

(*as t :: { desc = Inline.Entity e; _ } :: q ->
      prettify ({ t with desc = Inline.Text (x ^ escape_entity e) } :: q)
    | { desc = Inline.Entity  e; _ } as a :: q ->
      prettify ({ a with desc = Inline.Text (escape_entity e) } :: q) *)
  prettify l

let heading ~resolve (h : Heading.t) =
  let content = inline ~in_source:false ~resolve h.title in
  Section { label=h.label; level=h.level; content }

let non_empty_block_code ~resolve c =
  let s = source (inline ~in_source:true ~resolve) c in
  match s with
  | [] -> []
  | _ :: _ as l -> [BlockCode l]


let rec block ~in_source ~resolve (l: Block.t)  =
  let one (t : Block.one) =
    match t.desc with
    | Inline i ->
      inline ~in_source:false ~resolve i
    | Paragraph i ->
      inline ~in_source:false ~resolve i @ [Break]
    | List (typ, l) ->
      [List { typ; items = List.map (block ~in_source:false ~resolve) l }]
    | Description l ->
      List.concat_map (fun (i,b) ->
          let i = inline ~in_source:true ~resolve i in
          i @ Break :: block ~resolve ~in_source:true b
        ) l
    | Raw_markup r ->
      raw_markup r
    | Verbatim s -> [Verbatim s]
    | Source c -> non_empty_block_code ~resolve c @ if in_source then [] else [Break]
  in
  list_concat_map l ~f:one


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
      non_empty_block_code ~resolve code
      @ to_latex rest
    | (Documented _ | Nested _) :: _ ->
      let l, _, rest = take_descr t in
      let one dsrc =
        let content = match dsrc.code with
          | `D code -> inline ~in_source:true ~resolve code
          | `N n -> to_latex n
        in
        let doc = [block ~resolve ~in_source:true dsrc.doc] in
        (content @ label dsrc.anchor ) :: doc
      in
      Table (List.map one l) :: to_latex rest
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
      let content = block ~resolve ~in_source:false text in
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
      let docs = block ~resolve ~in_source:true  doc in
      let _summary = source (inline ~in_source:true ~resolve) summary in
      let content = included in
      (label anchor @ docs @ content)
      |> continue_with rest

    | Declaration {Item. kind=_; anchor ; content ; doc} :: rest ->
      let content =  label anchor @ documentedSrc ~resolve content in
      let elts = match doc with
        | [] -> content
        | docs -> content @ Break :: block ~resolve ~in_source:true docs
          (*[Description [content, block ~resolve docs]] *)
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
