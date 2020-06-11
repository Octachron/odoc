(** Intro

    This is an introductory text for this module. Don't try to make this sense of this
    lorem ipsum -like nonsense. There is little to be found, no rhythm nor entropy. Just
    the noise of a text lost in its self-contemplation

*)



(** {1 A title} *)

(** More text with little content 
    - does list work?
    - or not?
    - maybe?
    - [ 1+1 = 2 ], isn'it?

*)

(** {1 Basic record }*)

type basic = { x : int }
(** Just a named int *)

(** {1 Variants } *)

(** How many titles can be defined ? *)

(** Mutually recursive for even lists.
    The type odd is necessarily followed by an even list *)
type odd =
   Cons of even

and even =
 | Nil
 | Cons of odd (** *)
(** Only the even type can ever stop *)


(** A very simple file to be tested.
    Some inline code:
    {[
      let l = [1]
      let error = []
      let multi_line = true
    ]}

    Also [[]@[]=[]]
 *)

(** A very fancy non empty list type *)
  type 'a nonempty_list =
    (::) of 'a * 'a list


  (** A map function. Look at {!nonempty_list} to see why the function is total *)
  val map: ('a -> 'b) -> 'a nonempty_list -> 'b nonempty_list


(** {1 Long types} *)

(** {2 Variant } *)


type namarie = Ai | Laurie | Lantar | Lassi | Surinem | Yeni |  Unotime | Ve | Ramar | Aldaron |
  Andune | Pella | Varda | Tellumar


(** {2 Record } *)


(** A nice record *)
type record = { a: int; elbereth:int; gilthonien:int; silivren:int; penna: int; miriel: int }


(** {1 Field/constructor comments} *)

(** Constructor comment, do they work? *)

type greek =
  | Alpha (** From first *)
  | Beta (** second *)
  | Gamma (** third *)
  | Delta (** fourth *)
  | Omega (** to last *)
(** Here it goes *)


type inlined = 
  | Alpha of { aleph:int (** more comment*) ; beth:int (** even more *) } (** will it stop *)
  | Beta of { gamma:int (** more gamma *) ; delta:int (** don't forget delta *) } (** an encore *)
(** at last it stops *)
