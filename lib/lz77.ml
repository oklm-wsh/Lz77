module type ATOM =
sig
  type t = char (* It's mandatory *)

  val to_int : t -> int
end

module type SCALAR =
sig
  type elt
  type t

  val create  : int -> t
  val length  : t -> int
  val get     : t -> int -> elt
  val set     : t -> int -> elt -> unit
  val blit    : t -> int -> t -> int -> int -> unit
  val sub     : t -> int -> int -> t
end

module type S =
sig
  type atom
  type buffer

  type elt =
    | Buffer of buffer
    | Insert of int * int

  type t = elt list

  val pp_elt : Format.formatter -> elt -> unit
  val pp : Format.formatter -> t -> unit

  val to_freqs :
    get_length:(int -> (int * int * int)) ->
    get_distance:(int -> (int * int * int)) ->
    t -> (int array * int array)

  type state

  val make : ?window_bits:int -> ?level:int -> unit -> state

  val atomic_compress : state -> buffer -> int -> int -> unit
  val finish : state -> buffer -> int -> int -> t

  val compress   : ?window_bits:int -> ?level:int -> ?chunk:int -> buffer -> int -> int -> t
  val decompress : t -> buffer
end

module Common (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) =
struct
  type atom = Atom.t
  type buffer = Scalar.t

  type elt =
    | Buffer of Scalar.t
    | Insert of int * int

  type t = elt list

  let pp_scalar fmt scalar =
    let l = Scalar.length scalar in
    for i = 0 to l - 1
    do Format.fprintf fmt "%c" (Scalar.get scalar i) done

  let pp_elt fmt = function
    | Buffer scalar     -> Format.fprintf fmt "Buffer %a" pp_scalar scalar
    | Insert (off, len) -> Format.fprintf fmt "Insert (%d, %d)" off len

  let rec pp fmt l =
    Format.fprintf fmt "[@[<hov 2> ";
    List.iter (Format.fprintf fmt "%a;@ " pp_elt) l;
    Format.fprintf fmt "@]]@;"

  let to_freqs ~get_length ~get_distance lz77 =
    let freqs_lit_length = Array.make 286 0 in
    let freqs_distance = Array.make 30 0 in
    let rec aux = function
      | Buffer buff :: rest ->
        for i = 0 to Scalar.length buff
        do
          let code = Scalar.get buff i |> Atom.to_int in
          freqs_lit_length.(code) <- freqs_lit_length.(code) + 1
        done;
        aux rest
      | Insert (dist, length) :: rest ->
        let code, _, _ = get_length length in
        let dist, _, _ = get_distance dist in
        freqs_lit_length.(code) <- freqs_lit_length.(code) + 1;
        freqs_distance.(dist) <- freqs_distance.(dist) + 1;
        aux rest
      | [] -> ()
    in
    let () = freqs_lit_length.(256) <- 1 in
    let () = aux lz77 in
    freqs_lit_length, freqs_distance

  let size_of_elt = function
    | Buffer buff -> Scalar.length buff
    | Insert (_, l) -> l
end

module Make (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) : S
  with type atom = Atom.t
   and type buffer = Scalar.t =
struct
  module Int =
  struct
    type t = int
  end

  module IntArray =
  struct
    type elt = int
    type t = int array

    let create : int -> int array = fun size -> Array.make size 0
    let blit   : int array -> int -> int array -> int -> int -> unit = Array.blit
  end

  module Buffer =
  struct
    type t =
      { mutable buffer   : Scalar.t
      ; mutable position : int
      ; mutable length   : int
      ; initial          : Scalar.t }

    let create n =
      let n = if n < 1 then 1 else n in
      let n = if n > Sys.max_string_length then Sys.max_string_length else n in
      let s = Scalar.create n in
      { buffer = s; position = 0; length = n; initial = s }

    let contents { buffer; position; _ } = Scalar.sub buffer 0 position

    let clear buffer = buffer.position <- 0

    let resize buffer more =
      let len  = buffer.length in
      let len' = ref len in

      while buffer.position + more > !len'
      do len' := 2 * !len' done;

      if !len' > Sys.max_string_length
      then begin
        if buffer.position + more <= Sys.max_string_length
        then len' := Sys.max_string_length
        else failwith "Buffer.add: cannot grow buffer"
      end;

      let buffer' = Scalar.create !len' in
      Scalar.blit buffer.buffer 0 buffer' 0 buffer.position;
      buffer.buffer <- buffer';
      buffer.length <- !len'

    let add_atom buffer atom =
      let pos = buffer.position in
      if pos >= buffer.length then resize buffer 1;
      Scalar.set buffer.buffer pos atom;
      buffer.position <- pos + 1

    let empty { position; _ } = position = 0
  end

  module RingBuffer = RingBuffer.Make(Atom)(Scalar)

  include Common(Atom)(Scalar)

  exception Match
  exception Literal
  exception Break

  (* XXX: magic *)
  external get_u16 : Scalar.t -> int -> int   = "%caml_string_get16u"
  external get_u64 : Scalar.t -> int -> int64 = "%caml_string_get64u"

  (* TODO: optimize this! *)
  let repeat atom =
    let atom = Char.code atom |> Int64.of_int in
    let ( lor ) = Int64.logor in
    let ( lsl ) = Int64.shift_left in
    atom
    lor (atom lsl 8)
    lor (atom lsl 16)
    lor (atom lsl 24)
    lor (atom lsl 32)
    lor (atom lsl 40)
    lor (atom lsl 48)
    lor (atom lsl 56)

  let decompress t =
    let rec length acc = function
      | []     -> acc
      | h :: t -> length (size_of_elt h + acc) t
    in
    let buf = Scalar.create (length 0 t) in
    let rec fill off = function
      | []   -> ()
      | h :: t ->
        let () = match h with
          | Buffer buffer      -> Scalar.blit buffer 0 buf off (Scalar.length buffer)
          | Insert (diff, len) ->
            for i = 0 to len - 1 do Scalar.set buf (off + i) (Scalar.get buf (off - diff + i)) done
        in
        fill (off + size_of_elt h) t
    in
    fill 0 t;
    buf

  let hlog = [| 1; 11; 11; 11; 12; 13; 13; 13; 13; 13 |]

  type state =
    { window_bits : int
    ; htab        : int array
    ; hlog        : int
    ; level       : int
    ; buffer      : Buffer.t
    ; ringbuffer  : RingBuffer.t
    ; mutable res : t
    ; mutable idx : int }

  let make ?(window_bits = 15) ?(level = 0) () =
    let hlog = try Array.get hlog level with exn -> 1 in
    { window_bits
    ; htab       = Array.make (1 lsl hlog) 0
    ; hlog
    ; level
    ; buffer     = Buffer.create 64
    ; ringbuffer = RingBuffer.create (1 lsl window_bits)
    ; res        = []
    ; idx        = 0 }

  let really_compress state =
    let len   = RingBuffer.available_to_read state.ringbuffer in
    let buff  = Scalar.create len in
    let bound = len - 2 in
    let ip    = ref 0 in

    (* TODO may be is useful to use directly and only ringbuffer (we already
     * save the data in [atomic_compress]). in this case, we need to change
     * [ip_*] functions.
     *
     * The good point is to avoid the allocation of [buff].
     * The bad point is to sanitize at every moment [ip] (like [op]).
     *)

    let ip_chr, ip_u16, ip_u64 = Scalar.get buff, get_u16 buff, get_u64 buff in
    let rb_chr, rb_u16, rb_u64 =
      let buff = RingBuffer.to_buffer state.ringbuffer in
      Scalar.get buff, get_u16 buff, get_u64 buff
    in

    let hash idx =
      let v = ip_u16 idx in
      let v = (ip_u16 (idx + 1) lxor (v lsr (16 - state.hlog))) lxor v in
      v land ((1 lsl state.hlog) - 1)
    in

    let flushing () =
      if Buffer.empty state.buffer = false
      then begin
        state.res <- Buffer (Buffer.contents state.buffer) :: state.res;
        Buffer.clear state.buffer;
      end
    in

    (* we save the [rpos] (position of begin of [buff] in [ringbuffer])
     * and blit data in buff. *)
    let rpos = RingBuffer.rpos state.ringbuffer in
    RingBuffer.peek state.ringbuffer buff 0 len;

    (* we can't predicte something at this time, so we complete the [buffer]. *)
    Buffer.add_atom state.buffer (ip_chr !ip);
    incr ip;
    Buffer.add_atom state.buffer (ip_chr !ip);
    incr ip;

    while !ip < len - 12 (* we suppress 12 bytes because we read per 8-bytes at
                            a moment (when we found a pattern) and we limit the
                            the first boundary (because we search a pattern with
                            3 bytes at least) and the second boundary (after we
                            read n * 8 bytes).

                            The algorithm needs 12 bytes to predicte something. *)
    do
      let anchor   = !ip in   (* comparison starting-point *)
      let op       = ref 0 in (* ptr to old pattern in ring buffer *)
      let len      = ref 3 in (* len of pattern, minimum match length *)
      let distance = ref 0 in (* distance of pattern *)

      (* convenience function *)
      let cmp_and_incr () =
        let r = rb_chr !op = ip_chr !ip in
        op := RingBuffer.sanitize state.ringbuffer (!op + 1);
        incr ip;
        r
      in

      try
        (* if we have ['a'; 'a'; 'a'; 'a'], so we have a distone match. *)
        if ip_chr !ip = ip_chr (!ip - 1)
           && ip_u16 (!ip - 1) = ip_u16 (!ip + 1)
        then begin
          distance := 1;
          ip       := !ip + 3;
          op       := RingBuffer.sanitize
                        state.ringbuffer
                        (rpos + anchor - 1 + 3);

          raise Match
        end;

        let hval = hash !ip in

        op       := Array.get state.htab hval;
        distance := (rpos + anchor) - !op;

        if (!distance land 1) = 0 (* TODO *)
        then Array.set state.htab hval
               (RingBuffer.sanitize state.ringbuffer (rpos + anchor));

        (* if we have a pattern (size of pattern is 3-bytes at least) *)
        if !distance = 0 || !distance >= ((1 lsl state.window_bits) + 8191 - 1) (* TODO: max far distance, check that! *)
           || cmp_and_incr () = false
           || cmp_and_incr () = false
           || cmp_and_incr () = false
        then raise Literal;

        (* far, we needs at least 5-bytes *)
        if state.level >= 5 && !distance >= 8191
        then begin
          if cmp_and_incr () = false
             || cmp_and_incr () = false
          then raise Literal;

          len := !len + 2
        end;

        raise Match
      with
      | Literal ->
        Buffer.add_atom state.buffer (ip_chr anchor);
        ip := anchor + 1;

      | Match ->
        begin
          ip := anchor + !len;

          (* in this case, we have a distone. we save the [pattern] and compare
           * per 8 bytes [op] (in [value2]) and [value1] (the 8 first bytes of
           * [ip]). if the compare fails, we compare per one byte [op] and
           * pattern (because [value1] is wrong). after, we break to the sanity
           * compute of [ip] and [op]. *)
          if !distance = 1
          then begin
            let pattern  = ip_chr (!ip - 1) in
            let value1   = repeat pattern in

            try begin
              while !ip < (bound - 8 - 2)
              do
                let value2 = rb_u64 !op in

                if value1 <> value2
                then begin
                  (* find the byte that starts to differ *)
                  while !ip < bound
                  do if rb_chr !op <> pattern
                     then raise Break
                     else begin
                       op := RingBuffer.sanitize state.ringbuffer (!op + 1);
                       incr ip; end
                  done
                end else begin
                  op := RingBuffer.sanitize state.ringbuffer (!op + 8);
                  ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              (* sanitize [ip] and [op] to lower than [bound]. *)
              if !ip > bound
              then begin
                let l = !ip - bound in
                ip := !ip - l;
                op := RingBuffer.sanitize state.ringbuffer (!op - l);
              end;

          (* in this case, we compute a general [Insert] value (not a specific
           * distone). so we check per 8 bytes and if the compare fails, we
           * compare per one byte [op] and [ip]. after, we break to the sanity
           * compute of [ip] and [op].
           *
           * the diff between compute the distone and general dist is the
           * access with [ip], in first case, we use [pattern] to avoid the
           * access in buffer. in other case, we have an access in buffer by
           * [ip]. *)
          end else begin
            try begin
              while !ip < (bound - 8 - 2)
              do
                if rb_u64 !op <> ip_u64 !ip
                then begin
                  (* find the byte that starts to differ *)
                  while !ip < bound
                  do if cmp_and_incr () = false then raise Break done;

                  raise Break;
                end else begin op := RingBuffer.sanitize state.ringbuffer (!op + 8); ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              if !ip > bound
              then begin
                let l = !ip - bound in
                ip := !ip - l;
                op := RingBuffer.sanitize state.ringbuffer (!op - l);
              end
          end;

          ip := !ip - 3;

          (* update the match at match boundary *)
          Array.set state.htab (hash !ip) (RingBuffer.sanitize state.ringbuffer (rpos + !ip));
          incr ip;
          Array.set state.htab (hash !ip) (RingBuffer.sanitize state.ringbuffer (rpos + !ip));
          incr ip;

          len := !ip - anchor;

          flushing ();
          state.res <- Insert (!distance, !len) :: state.res;
      end
    done;

    RingBuffer.drop state.ringbuffer !ip

  let atomic_compress state buff off len =
    RingBuffer.write state.ringbuffer buff off len;

    if RingBuffer.available_to_read state.ringbuffer > 12
    then really_compress state
    else ()

  let finish state buff off len =
    RingBuffer.write state.ringbuffer buff off len;

    if RingBuffer.available_to_read state.ringbuffer > 12
    then really_compress state;

    if RingBuffer.available_to_read state.ringbuffer > 0
    then begin
      let len = RingBuffer.available_to_read state.ringbuffer in
      Buffer.resize state.buffer len;

      (* XXX: hack! we need to know the Buffer module. *)
      RingBuffer.read state.ringbuffer
        state.buffer.Buffer.buffer state.buffer.Buffer.position len;
      state.buffer.Buffer.position <- state.buffer.Buffer.position + len;

      state.res <- Buffer (Buffer.contents state.buffer) :: state.res;
    end;

    List.rev state.res

  let compress ?(window_bits = 15) ?(level = 0) ?(chunk = (1 lsl 5)) buff off len =
    let n = len / chunk in
    let s = make ~window_bits ~level () in
    let r = ref len in

    for i = 0 to n
    do atomic_compress s buff (i * chunk) (min chunk !r);
       r := !r - (1 lsl 5);
       r := if !r < 0 then 0 else !r; done;

    finish s buff len 0

  (* XXX: non atomic function

  let compress ?(window_bits = 15) ?(level = 0) buff off len =
    let bound    = off + len - 2 in
    (* we limit the input with a bound = 2 because it's more efficient to finish
     * with 2 literals (and avoid the distance). *)
    let hlog     = Array.get hlog level in
    let htab     = Array.make (1 lsl hlog) 0 in
    let buffer   = Buffer.create 16 in
    let result   = ref [] in

    let literal buffer =
      Format.printf "%a\n%!" pp_elt (Buffer buffer);
      result := Buffer buffer :: !result in
    let match' off len =
      Format.printf "%a\n%!" pp_elt (Insert (off, len));
      result := Insert (off, len) :: !result in

    let ip = ref off in

    let get_chr idx = Scalar.get buff (off + idx) in
    let get_u16 idx = get_u16 buff (off + idx) in
    let get_u64 idx = get_u64 buff (off + idx) in

    let hash idx =
      let v = get_u16 idx in
      let v = (get_u16 (idx + 1) lxor (v lsr (16 - hlog))) lxor v in
      let v = v land ((1 lsl hlog) - 1) in
      v
    in
    let cmp_and_incr ref ip =
      let result = get_chr !ref = get_chr !ip in
      incr ref;
      incr ip;
      result
    in
    let flushing () =
      if Buffer.empty buffer = false
      then begin
        literal (Buffer.contents buffer);
        Buffer.clear buffer;
      end else ()
    in

    Buffer.add_atom buffer (get_chr !ip);
    incr ip;
    Buffer.add_atom buffer (get_chr !ip);
    incr ip;

    Format.printf "%a\n%!" pp_elt (Buffer (Buffer.contents buffer));

    while !ip < len - 12 do   (* [-12] to avoid 2 bytes necessary for distance,
                                 8 bytes uses in reading ([get_u64]) and 2 bytes
                                 necessary for next distance. *)
      let anchor   = !ip in   (* comparison starting-point *)

      let op       = ref 0 in
      let len      = ref 3 in (* the len of match *)
      let distance = ref 0 in (* the distance = anchor - op *)

      try
        (* if we have ['a'; 'a'; 'a'; 'a'], so we have a dist-one match. *)
        if get_chr !ip = get_chr (!ip - 1)
           && get_u16 (!ip - 1) = get_u16 (!ip + 1)
        then begin
          distance := 1;
          ip       := !ip + 3;
          op       := anchor - 1 + 3;

          raise Match
        end;

        let hval = hash !ip in

        op       := off + (Array.get htab hval);
        distance := anchor - !op;

        if (!distance land 1) = 0 (* TODO: define acc *)
        then Array.set htab hval (anchor - off);

        (* if we don't have distance or the distance is larger than the max far
         * distance or we don't have the same 3 bytes: it's a literal. *)
        if !distance = 0 || !distance >= ((1 lsl window_bits) + 8191 - 1) (* TODO: max far distance, check that! *)
           || cmp_and_incr op ip = false
           || cmp_and_incr op ip = false
           || cmp_and_incr op ip = false
        then raise Literal;

        (* far, we need at least 5 bytes match. *)
        if level >= 5 && !distance >= 8191
        then begin
          if cmp_and_incr op ip = false
             || cmp_and_incr op ip = false
          then raise Literal;

          len := !len + 2
        end;

        raise Match
      with
      | Literal ->
        Format.printf "We have a literal [%c] data: %a\n%!"
          (get_chr anchor)
          pp_elt (Buffer (Buffer.contents buffer));

        Buffer.add_atom buffer (get_chr anchor);
        ip := anchor + 1;

      | Match ->
        begin
          (* sanitize the [ip], [cmp_and_incr] changes [ip] but we saved
           * the comparison point at [anchor] and we saved the len of
           * minimum match (3 or 5 according to optimization). *)
          ip := anchor + !len;

          (* in this case, we have a distone. we save the [pattern] and compare
           * per 8 bytes [op] (in [value2]) and [value1] (the 8 first bytes of
           * [ip]). if the compare fails, we compare per one byte [op] and
           * pattern (because [value1] is wrong). after, we break to the sanity
           * compute of [ip] and [op]. *)
          if !distance = 1
          then begin

            let pattern  = get_chr (!ip - 1) in
            let value1   = repeat pattern in

            try begin
              while !ip < (bound - 8 - 2)
              do
                let value2 = get_u64 !op in

                if value1 <> value2
                then begin
                  while !ip < bound
                  do if get_chr !op <> pattern
                     then raise Break
                     else begin incr op; incr ip; end
                  done
                end else begin op := !op + 8; ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              (* sanitize [ip] to lower than [bound]. *)

              if !ip > bound
              then begin
                 let l = !ip - bound in
                 ip := !ip - l;
                 op := !op - l;
              end;

          (* in this case, we compute a general [Insert] value (not a specific
           * distone). so we check per 8 bytes and if the compare fails, we
           * compare per one byte [op] and [ip]. after, we break to the sanity
           * compute of [ip] and [op].
           *
           * the diff between compute the distone and general dist is the
           * access with [ip], in first case, we use [pattern] to avoid the
           * access in buffer. in other case, we have an access in buffer by
           * [ip]. *)
          end else begin
            try begin
              while !ip < (bound - 8 - 2)
              do
                if get_u64 !op <> get_u64 !ip
                then begin
                  while !ip < bound
                  do if cmp_and_incr op ip = false then raise Break done;

                  raise Break;
                end else begin op := !op + 8; ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              if !ip > bound
              then begin
                let l = !ip - bound in
                ip := !ip - l;
                op := !op - l;
              end
          end;

          ip := !ip - 3;

          Array.set htab (hash !ip) (!ip - off);
          incr ip;
          Array.set htab (hash !ip) (!ip - off);
          incr ip;

          len := !ip - anchor;

          flushing ();
          match' !distance !len;
      end
    done;

    for idx = !ip to len - 1 do Buffer.add_atom buffer (get_chr idx) done;
    flushing ();

    List.rev !result
  *)
end
