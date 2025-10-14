type (!'a : value) t : immutable_data with 'a @@ contended portable

external make
  : ('a : value).
  'a @ contended once portable unique -> ('a t[@local_opt]) @ unique
  @@ portable
  = "%makemutable"

external get
  : ('a : value).
  'a t @ local unique -> 'a @ contended once portable unique
  @@ portable
  = "%atomic_load"

external set
  : ('a : value).
  'a t @ local -> 'a @ contended once portable unique -> unit
  @@ portable
  = "%atomic_set"

external exchange
  : ('a : value).
  'a t @ local
  -> 'a @ contended once portable unique
  -> 'a @ contended once portable unique
  @@ portable
  = "%atomic_exchange"

external compare_and_set
  : ('a : value).
  'a t @ local
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended once portable unique
  -> Basement.Compare_failed_or_set_here.t
  @@ portable
  = "%atomic_cas"
