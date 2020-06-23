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

type break_hierarchy =
  | Aesthetic
  | Simple
  | Line
  | Paragraph
  | Separation


type elt =
  | Txt of { kind: text_kind; words: string list }
  | Section of {level:int; label:string option; content:t }
  | Verbatim of string
  | Internal_ref of reference
  | External_ref of string * t option
  | Label of string
  | Raw of string
  | Tag of string * t
  | Style of [`Emphasis|`Bold|`Superscript|`Subscript|`Italic] * t
  | BlockCode of t
  | InlineCode of t
  | Break of break_hierarchy
  | List of { typ : Block.list_type; items: t list }
  | Description of (t * t) list
  | Table of t list list

and t = elt list
and reference = { short:bool; target:string; content: t option }




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
      | '~' ->  Buffer.add_string b "\\textasciitilde{}"
      | '^' -> Buffer.add_string b "\\textasciicircum{}"
      | '_' ->  Buffer.add_string b "\\_"
      | '&' ->  Buffer.add_string b "\\&"
      | '#' ->  Buffer.add_string b "\\#"
      | '$' ->  Buffer.add_string b "\\$"


      | c ->  Buffer.add_char b c
    done;
    let s = Buffer.contents b in
    Buffer.reset b;
    Fmt.string ppf s

let escape_verbatim_text =
  let b = Buffer.create 17 in
  fun ppf s ->
    let rec normal i =
      if i = String.length s then () else
      match s.[i] with
      | '{' ->  Buffer.add_string b "\\{"                 ; normal (i+1)
      | '}' ->  Buffer.add_string b "\\}"                 ; normal (i+1)
      | '\\' -> Buffer.add_string b "\\textbackslash{}"   ; normal (i+1)
      | '%' ->  Buffer.add_string b "\\%"                 ; normal (i+1)
      | '~' ->  Buffer.add_string b "\\textasciitilde{}"  ; normal (i+1)
      | '^' ->  Buffer.add_string b "\\textasciicircum{}" ; normal (i+1)
      | '_' ->  Buffer.add_string b "\\_"                 ; normal (i+1)
      | '&' ->  Buffer.add_string b "\\&"                 ; normal (i+1)
      | '#' ->  Buffer.add_string b "\\#"                 ; normal (i+1)
      | '$' ->  Buffer.add_string b "\\$"                 ; normal (i+1)

      | '\n' -> Buffer.add_string b "\\\\\n"              ; after_newline (i+1)

      | c ->  Buffer.add_char b c                         ; normal (i+1)
    and after_newline i =
      if i = String.length s then () else
        match s.[i] with
        | ' ' -> Buffer.add_string b {|\-|}; indent (i+1)
        | _ -> normal i
    and indent i =
      if i = String.length s then () else
        match s.[i] with
        | ' ' -> Buffer.add_string  b {|\ |}; indent (i+1)
        | _ -> normal i in
    let () = normal 0 in
    let s = Buffer.contents b in
    Buffer.reset b;
    Fmt.string ppf s


let escape_ref ppf s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '~' -> Format.fprintf ppf "+t+"
    | '_' -> Format.fprintf ppf "+u+"
    | '+' -> Format.fprintf ppf "+++"
    | c -> Format.fprintf ppf "%c" c
  done

module Link = struct

  let rec flatten_path ppf (x: Odoc_document.Url.Path.t) = match x.parent with
    | Some p -> Format.fprintf ppf "%a-%s-%s" flatten_path p x.kind x.name
    | None -> Format.fprintf ppf "%s-%s" x.kind x.name

 let page p =
   Format.asprintf "%a-"
     flatten_path p


 let label (x:Odoc_document.Url.t) =
   Format.asprintf "%a-%s"
     flatten_path x.page
     x.anchor

  let rec is_class_or_module_path (url : Odoc_document.Url.Path.t) = match url.kind with
    | "module" | "package" | "class" ->
      begin match url.parent with
      | None -> true
      | Some url -> is_class_or_module_path url
      end
    | _ -> false

  let should_inline x = not @@ is_class_or_module_path x


end



let bind pp x ppf = pp ppf x
let mlabel ppf = macro "label" escape_ref ppf
let verbatim = macro "verbatim" str
let mbegin ?options = macro "begin" ?options str
let mend = macro "end" str
let mhyperref pp r ppf =
  match r.target, r.content with
  | "", None -> ()
  | "", Some content ->  pp ppf content
  | s, None ->
    macro "ref" escape_ref ppf s
  | s, Some content ->
      let pp =
        if r.short then pp else
          fun ppf x -> Fmt.pf ppf "%a[%a]" pp x (macro "ref*" escape_ref) s in
      macro "hyperref" ~options:[bind escape_ref s] pp ppf content


let texttt ppf s = escape_verbatim_text ppf s

let label = function
  | None -> []
  | Some  x (* {Odoc_document.Url.Anchor.anchor ; page;  _ }*) -> [Label (Link.label  x)]



let mstyle = function
  | `Emphasis | `Italic -> macro "emph"
  | `Bold -> macro "textbf"
  | `Subscript -> macro "textsubscript"
  | `Superscript -> macro "textsuperscript"



let break level ppf motivation =
  let pre: _ format6 = match level with
    | Line -> {|\\|}
    | Separation -> {|\medbreak|}
    | _ -> "" in
  let post: _ format6 = match level with
    | Line | Separation | Aesthetic | Simple -> "@,"
    | Paragraph -> "@,@," in
  let fmt : _ format6 = match level with
    | Simple | Paragraph -> pre ^^ post ^^ "%%" ^^ motivation ^^ "@," (* "\n%Comment\n" *)
    | Aesthetic | Line | Separation -> pre ^^ "%%" ^^ motivation ^^ post in
  Fmt.pf ppf fmt

let env name pp ?(with_break=false) ?(opts=[]) ?(args=[]) ppf content =
  mbegin ppf name;
  Format.pp_print_list (fun ppf pp -> Format.fprintf ppf "[%t]" pp) ppf opts;
  Format.pp_print_list (fun ppf pp -> Format.fprintf ppf "{%t}" pp) ppf args;
  pp ppf content;
  mend ppf name;
  break (if with_break then Simple else Aesthetic) ppf "after env %s" name

let inline_code = macro "inlinecode"
let block_code = macro "blockcode"


let level_macro = function
  | 0 ->  macro "section"
  | 1 -> macro "subsection"
  | 2 -> macro "subsubsection"
  | 3 | _ -> macro "paragraph"


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
    (Fmt.list ~sep:(fun ppf () -> break Aesthetic ppf "list") elt)
    ppf
    x

let description pp ppf x =
  let elt ppf (d,elt) = macro "item" ~options:[bind pp d] pp ppf elt in
  let all ppf x =
    Format.fprintf ppf
      {|\kern-\topsep
\makeatletter\advance\%@topsepadd-\topsep\makeatother%% topsep is hardcoded
|};
    Fmt.list ~sep:(fun ppf () -> break Aesthetic ppf "description") elt ppf x in
  env "description" all ppf x


let escape_entity  = function
  | "#45" -> "-"
  | "gt" -> ">"
  | "#8288" -> ""
  | s -> s

let elide_table = function
  | [] -> []
  | a :: _ as m ->
    let start = List.map (fun _ -> false) a in
    let is_content = function [] -> false | _ :: _ -> true in
    let row mask l = List.map2 (fun x y -> x || is_content y) mask l in
    let mask = List.fold_left row start m in
    let filter_row row = List.filter_map (fun x -> x) @@ List.map2 (fun b x -> if b then Some x else None) mask row in
    List.map filter_row m

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
    level_macro level with_label ppf (label,content)
  | Break lvl -> break lvl ppf "elt"
  | Raw s -> str ppf s
  | Tag (x,t) -> env ~with_break:true x pp ppf t
  | Verbatim s -> verbatim ppf s
  | Internal_ref r -> hyperref ppf r
  | External_ref (l,x) -> href ppf (l,x)
  | Style (s,x) -> mstyle s pp ppf x
  | BlockCode [] -> ()
  | BlockCode x -> block_code pp ppf x
  | InlineCode x -> inline_code pp ppf x
  | List {typ; items} -> list typ pp ppf items
  | Description items -> description pp ppf items
  | Table [] -> ()
  | Table (a  :: _ as l) ->
    let columns = List.length a in
    let row ppf x =
      let ampersand ppf () = Format.fprintf ppf "& " in
      Fmt.list ~sep:ampersand pp ppf x;
      break Line ppf "row" in
    let matrix ppf m = List.iter (row ppf) m in
    let rec repeat n s ppf = if n = 0 then () else
        Format.fprintf ppf "%t%t" s (repeat (n - 1) s) in
    (* We are using pbox to be able to nest lists inside the tables *)
    let frac ppf = Format.fprintf ppf " p{%.2f\\linewidth} " (1. /. float columns) in
    break Line ppf "before tabular";
    env "tabular"
      ~args:[ repeat columns frac  ]
      matrix ppf l;
    break Line ppf "after tabular"
  | Label x -> mlabel ppf x
  | _ -> .

and pp ppf = function
  | [] -> ()
  | Break a :: (Break b :: q) ->
    pp ppf ( Break (max a b) :: q)
  | a :: q ->
    pp_elt ppf a; pp ppf q

and hyperref ppf r = mhyperref pp r ppf

and href ppf (l,txt) =
  match txt with
  | Some txt ->
    Format.fprintf ppf {|\href{%s}{%a}|} l pp txt
  | None -> Format.fprintf ppf {|\url{%s}|} l

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


let rec internalref ~in_source (t : InternalLink.t) =
  match t with
  | Resolved (uri, content) ->
    let target = Link.label uri in
    let content = Some (inline ~in_source content) in
    let short = in_source in
    Internal_ref { short; content; target }
  | Unresolved content ->
    let target = "xref-unresolved" in
    let content = Some (inline ~in_source content) in
    let short = in_source in
    Internal_ref { short; target; content }

and inline ~in_source (l : Inline.t) =
  let one (t : Inline.one) =
    match t.desc with
    | Text _s -> assert false
    | Linebreak -> [Break Line]
    | Styled (style, c) ->
      [Style(style, inline ~in_source c)]
    | Link (ext, c) ->
      let content = inline ~in_source:false  c in
      [External_ref(ext, Some content)]
    | InternalLink c ->
      [internalref ~in_source c]
    | Source c ->
      [InlineCode (source (inline ~in_source:true) c)]
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
          let words = List.filter (( <> ) "" ) words in
          [Txt { kind = if in_source then Verbatim else Textual; words }]
      in
      text @ prettify rest
    | o :: q -> one o @ prettify q
    | [] -> [] in
  prettify l

let heading (h : Heading.t) =
  let content = inline ~in_source:false h.title in
  [Section { label=h.label; level=h.level; content }; Break Aesthetic]

let non_empty_block_code ~in_source c =
  let s = source (inline ~in_source:true) c in
  match s with
  | [] -> []
  | _ :: _ as l ->
    if in_source then [BlockCode l] else [Break Separation; BlockCode l; Break Separation]



let rec block ~in_source (l: Block.t)  =
  let one (t : Block.one) =
    match t.desc with
    | Inline i ->
      inline ~in_source:false i
    | Paragraph i ->
      inline ~in_source:false i @ if in_source then [] else [Break Paragraph]
    | List (typ, l) ->
      [List { typ; items = List.map (block ~in_source:false) l }]
    | Description l ->
      [Description (List.map (fun (i,b) ->
          inline ~in_source i,
          block ~in_source b
      ) l)]
   | Raw_markup r ->
      raw_markup r
    | Verbatim s -> [Verbatim s]
    | Source c -> non_empty_block_code ~in_source c
  in
  list_concat_map l ~f:one

let rec is_only_text l =
  let is_text : Item.t -> _ = function
    | Heading _ | Text _ -> true
    | Declaration _
      -> false
    |  Include { content = items; _ }
      -> is_only_text items.content
  in
  List.for_all is_text l

let rec documentedSrc (t : DocumentedSrc.t) =
  let open DocumentedSrc in
  let rec to_latex t = match t with
    | [] -> []
    | Code _ :: _ ->
      let take_code l =
        Doctree.Take.until l ~classify:(function
          | Code code -> Accum code
          | _ -> Stop_and_keep
        )
      in
      let code, _, rest = take_code t in
      non_empty_block_code code ~in_source:true
      @ to_latex rest
    | Alternative (Expansion e) :: rest ->
      begin if Link.should_inline e.url then
        to_latex e.expansion
      else
        non_empty_block_code e.summary ~in_source:true
    end
        @ to_latex rest
    | Subpage subp :: rest ->
        items subp.content.items
      @ to_latex rest
    | (Documented _ | Nested _) :: _ ->
      let take_descr l =
        Doctree.Take.until l ~classify:(function
          | Documented { attrs; anchor; code; doc }  ->
            Accum [{DocumentedSrc. attrs ; anchor ; code = `D code; doc }]
          | Nested { attrs; anchor; code; doc } ->
            Accum [{DocumentedSrc. attrs ; anchor ; code = `N code; doc }]
          | _ -> Stop_and_keep
        )
      in
      let l, _, rest = take_descr t in
      let one dsrc =
        let content = match dsrc.code with
          | `D code -> inline ~in_source:true code
          | `N n -> to_latex n
        in
        let doc = [block ~in_source:true dsrc.doc] in
        (content @ label dsrc.anchor ) :: doc
      in
      Table (elide_table @@ List.map one l) :: Break Line :: to_latex rest
  in
  to_latex t


and items l =
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
      let content = block ~in_source:false text in
      let elts = content in
      elts
      |> continue_with rest
    | Heading h :: rest ->
      heading h
      |> continue_with rest
    | Include
        { kind=_; anchor; doc ; content = { summary; status=_; content } }
      :: rest ->
      let included = items content  in
      let docs = block ~in_source:true  doc in
      let summary = source (inline ~in_source:true) summary in
      let content = included in
      (label anchor @ docs @ summary @ content)
      |> continue_with rest

    | Declaration {Item. kind=_; anchor ; content ; doc} :: rest ->
      let content =  label anchor @ documentedSrc content in
      let elts = match doc with
        | [] -> content @ [Break Line]
        | docs -> content @ Break Line :: block ~in_source:true docs @ [Break Separation]
      in
      continue_with rest elts

  and items l = walk_items ~only_text:(is_only_text l) [] l in
  items l


module Doc = struct

let make  url filename content children =
  let label = Label (Link.page url) in
  let content = match content with
    | [] -> [label]
    | Section _ as s  :: q -> s :: label :: q
    | q -> label :: q in
  let content ppf = Format.fprintf ppf "@[<v>%a@]@." pp content in
  {Odoc_document.Renderer. filename; content; children }
end

module Page = struct

  let on_sub = function
    | `Inline _ | `Subpage |  _ -> Some 1

  let rec subpage (p:Subpage.t) = [ page p.content ]

  and subpages i =
    List.flatten @@ List.map subpage @@ Doctree.Subpages.compute i

  and page ({Page. title; header; items = i; url } as p) =
    let i = Doctree.Shift.compute ~on_sub i in
    let subpages = subpages p in
    let header = items header in
    let content = items i in
    let page =
      Doc.make url title (header@content) subpages
    in
    page

end

let render page = Page.page page
