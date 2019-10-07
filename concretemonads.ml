module type TYPE = sig
   type t
end

module type MONOID = sig
   type t
   val nil : t
   val add : t -> t -> t
end

module type MONAD = sig
  (* monads basics *)
  type 'a monad
  val return : 'a -> ('a monad)
  val bind' : ('a -> 'b monad) -> ('a monad -> 'b monad)
  val map' : ('a -> 'b) -> ('a monad -> 'b monad)
    
  val bind : ('a monad) -> ('a -> 'b monad) -> ('b monad)
  val map : ('a monad) -> ('a -> 'b) -> 'b monad
                                           
  val (>>=) : ('a monad) -> ('a -> 'b monad) -> ('b monad)
    
  (* apparently mandatory for omonad package : fail function *)
  val fail : string -> 'a monad

  (* safe recursive functions *)
  val m_for : int -> ('a -> 'a monad) -> ('a -> 'a monad)
  val m_while : ('a -> bool) -> ('a -> 'a monad) -> ('a -> 'a monad)
  val m_while2 : ('a -> bool monad) -> ('a -> 'a monad) -> ('a -> 'a monad)
  val m_while3 : ('a -> ('a * bool) monad) -> ('a -> 'a monad)
  val m_while4 : ('a -> 'a option monad) -> ('a -> 'a monad)
                                                           
  (* concretes *)
  type ('a,'id) mconc     (* always abstract *)
  type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc }
  type 'a extract   (* never abstract  *)
          
  val apply' : ('a -> 'b monad) -> ('a,'id) mconc -> ('b,'id) mconc
  val with_mconc' : ('a,'b) f_mconc -> ('a -> 'b monad)
  val extract : ('a,'id) mconc -> 'a extract

  val apply : ('a,'id) mconc -> ('a -> 'b monad) -> ('b,'id) mconc
  val with_mconc : 'a -> ('a,'b) f_mconc -> ('b monad)
  val cmap : ('a -> 'b) -> ('a,'id) mconc -> ('b,'id) mconc
  val cset : 'b -> ('a,'id) mconc -> ('b,'id) mconc
                   
  val (>>=:) : ('a,'id) mconc -> ('a -> 'b monad) -> ('b,'id) mconc
end

module type MONAD_TRANSFORMED = sig
  include MONAD
  type 'a monad_int
  type 'a monad_ext
  val lift_int : ('a -> 'b monad_int) -> ('a -> 'b monad)
  val lift_ext : ('a -> 'b monad_ext) -> ('a -> 'b monad)
end

module type INCOMPLETE_MONAD = sig
  type 'a monad
  val return : 'a -> ('a monad)
  val bind' : ('a -> 'b monad) -> ('a monad -> 'b monad)
  val map' : ('a -> 'b) -> ('a monad -> 'b monad)

  val m_while : ('a -> bool) -> ('a -> 'a monad) -> ('a -> 'a monad)
  val m_while3 : ('a -> ('a * bool) monad) -> ('a -> 'a monad)
                                         
  type ('a,'id) mconc
  type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc } 
  type 'a extract
  val apply' : ('a -> 'b monad) -> ('a,'id) mconc -> ('b,'id) mconc
  val with_mconc' : ('a,'b) f_mconc -> ('a -> 'b monad)
  val extract : ('a,'id) mconc -> 'a extract
end
                                        
module Complete_Monad (M : INCOMPLETE_MONAD) : sig
   include MONAD with type 'a monad = ('a M.monad)
                  and type 'a extract = ('a M.extract)
end = struct
   include M

   let bind ma f = M.bind' f ma
   let map ma f = M.map' f ma

   let (>>=) ma f = M.bind' f ma

   let fail s = failwith "monadic fail function not implemented"

   let m_for n f a =
     M.map' fst
        (M.m_while
            (fun (a, i) -> i < n)
            (fun (a, i) -> M.map' (fun a2 -> (a2, i + 1)) (f a))
            (a, 0))
        
  let m_while2 test f a =
    M.m_while3
      (fun a ->
        M.bind' (fun res ->
          M.map' (fun a -> (a, res)) (f a))
          (test a))
      a

  let m_while4 testf a =
    M.m_while3
      (fun a ->
        M.map'
          (fun opta2 ->
            match opta2 with
            | Some a2 -> (a2, true)
            | None -> (a, false))
          (testf a))
      a
      
  let apply mca f = M.apply' f mca
  let with_mconc a fmconc = M.with_mconc' fmconc a
  let cmap f mca = M.apply' (fun a -> M.return (f a)) mca
  let cset b mca = M.apply' (fun a -> M.return b) mca

  let (>>=:) mca f = M.apply' f mca
end

                                                  
module Writer (Logs : MONOID) : sig

   include MONAD with type 'a monad = ('a * Logs.t)
                  and type 'a extract = ('a * Logs.t)
   val write : Logs.t -> (unit monad)
                             
end = struct
   include Complete_Monad (struct 
                 type 'a monad = ('a * Logs.t)
                         
      let return a = (a, Logs.nil)
                        
      let bind' f (a, logsA) =
        let (b, logsB) = (f a) in
        (b, Logs.add logsA logsB)
           
      let map' f (a, logs) = (f a, logs)
                                
      let m_while test f a =
        let rec loop (a, logs) =
          if (test a)
          then let (a2, logs2) = (f a) in
               loop (a2, Logs.add logs logs2)
          else (a, logs)
        in
        loop (a, Logs.nil)
             
      let m_while3 testf a =
        let rec loop (a, logs) =
          let ((a2, res), logs2) = testf a in
          if res
          then loop (a2, Logs.add logs logs2)
          else (a2, Logs.add logs logs2)
        in
        loop (a, Logs.nil)
             
      type ('a,'id) mconc = 'a monad
      type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc } 
      type 'a extract = 'a monad
                           
      let apply' = bind'
      let with_mconc' f_mconc a = f_mconc.f (return a)
      let extract ta = ta
   end)
   let write logs = ((), logs)
end

module WriterT (Logs : MONOID) (M : MONAD) : sig
   include MONAD_TRANSFORMED
           with type 'a monad = ('a * Logs.t) M.monad
            and type 'a monad_int = ('a * Logs.t)
            and type 'a monad_ext = 'a M.monad
            and type 'a extract = ('a * Logs.t) M.extract
   val write : Logs.t -> (unit monad)
end = struct
   include Complete_Monad (struct
                 type 'a monad = ('a * Logs.t) M.monad
                 let return a = M.return (a, Logs.nil)
                 let bind' f mba =
                   M.bind'
                      (fun (a, logsA) ->
                         M.map'
                            (fun (b, logsB) -> (b, Logs.add logsA logsB))
                            (f a))
                      mba
                      
                 let map' f mba = M.map' (fun (a, logs) -> (f a, logs)) mba
                                         
        let m_while test f a =
          M.m_while
             (fun (a, logs) -> test a)
             (fun (a, logs) ->
                M.map' (fun (a2, logs2) -> (a2, Logs.add logs logs2)) (f a))
             (a, Logs.nil)
             
        let m_while3 testf a =
          M.m_while3
             (fun (a, logs) ->
                M.map'
                   (fun ((a2, res), logs2) -> ((a2, Logs.add logs logs2), res))
                   (testf a))
             (a, Logs.nil)
             
        type ('a,'id) mconc = ('a * Logs.t, 'id) M.mconc
        type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc }
        type 'a extract = ('a * Logs.t) M.extract
                                        
        let apply' f ta =
          M.apply'
             (fun (a, logsA) ->
                M.map'
              (fun (b, logsB) -> (b, Logs.add logsA logsB))
              (f a))
             ta
             
        let with_mconc' f_mconc a = M.with_mconc' { M.f = f_mconc.f } (a, Logs.nil)
                        
        let extract = M.extract
              end)
                          
   type 'a monad_int = ('a * Logs.t)
   type 'a monad_ext = ('a M.monad)
                          
   let lift_int f a = M.return (f a)
   let lift_ext f a = M.map' (fun a -> (a, Logs.nil)) (f a)
                                    
   let write logs = M.return ((), logs)
end

         
module Reader (Param : TYPE) : sig
   include MONAD with type 'a monad = (Param.t -> 'a)
                  and type 'a extract = ('a * Param.t)
  val get_param : unit -> (Param.t monad)
end = struct
  include Complete_Monad (struct
    type 'a monad = (Param.t -> 'a)
                      
    let return a _param = a
                     
    let bind' f ma param = f (ma param) param

    let map' f ma param = (f (ma param))

    let m_while test f a param =
      let rec loop a =
        if (test a)
        then loop (f a param)
        else a
      in
      loop a

    let m_while3 testf a param =
      let rec loop a =
        let (a2, res) = testf a param in
        if res
        then loop a2
        else a2
      in
      loop a

    type ('a,'id) mconc = ('a * Param.t)
    type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc } 
    type 'a extract = ('a * Param.t)

    let apply' f (a, param) = (f a param, param)
    let with_mconc' f_mconc a param = fst (f_mconc.f (a, param))
    let extract ta = ta
  end)
  let get_param () param = param
end

        
module ReaderT (Param : TYPE) (M : MONAD) : sig
   include MONAD_TRANSFORMED
           with type 'a monad = (Param.t -> 'a M.monad)
            and type 'a monad_int = (Param.t -> 'a)
            and type 'a monad_ext = ('a M.monad)
            and type 'a extract = ('a * Param.t) M.extract
   val get_param : unit -> (Param.t monad)
end = struct
   include Complete_Monad (struct
      type 'a monad = (Param.t -> 'a M.monad)
                        
      let return a param = (M.return a)
      let bind' f ma param = M.bind' (fun a -> f a param) (ma param)

      let map' f ma param = M.map' f (ma param)
              
      let m_while test f a param =
        M.m_while test (fun a -> f a param) a

      let m_while3 testf a param =
        M.m_while3 (fun a -> testf a param) a
                  
      type ('a,'id) mconc = ('a * Param.t, 'id) M.mconc
      type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc } 
      type 'a extract = ('a * Param.t) M.extract
        
      let apply' f ta =
        M.apply'
          (fun (a, param) ->
            M.map'
              (fun b -> (b, param))
              (f a param))
          ta
          
      let with_mconc' f_mconc a param =
        M.map' fst (M.with_mconc' { M.f = f_mconc.f } (a, param))
                                     
      let extract = M.extract
              end)
                          
   type 'a monad_int = (Param.t -> 'a)
   type 'a monad_ext = ('a M.monad)
                          
   let lift_int f a param = (M.return (f a param))
   let lift_ext f a _param = (f a)
                                
   let get_param () param = M.return param
end


module State (St : TYPE) : sig
  include MONAD with type 'a monad = (St.t -> 'a * St.t)
                 and type 'a extract = ('a * St.t)
  val get_state : unit -> St.t monad
  val set_state : St.t -> unit monad
end = struct
  include Complete_Monad (struct
    type 'a monad = (St.t -> 'a * St.t)
                      
    let return a st = (a, st)
                     
    let bind' f ma st0 =
      let (a, st1) = (ma st0) in
      (f a st1)
                       
    let map' f ma st0 =
      let (a, st1) = (ma st0) in
      (f a, st1)
        
    let m_while test f a =
      let rec loop (a, st) =
        if (test a)
        then loop (f a st)
        else (a, st)
      in
      (fun st -> loop (a, st))

    let m_while3 testf a =
      let rec loop (a, st1) =
        let ((a2, res), st2) = testf a st1 in
        if res
        then loop (a2, st2)
        else (a2, st2)
      in
      (fun st0 -> loop (a, st0))          
        
    type ('a,'id) mconc = ('a * St.t)
    type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc }
    type 'a extract = ('a * St.t)

    let apply' f (a, st) = (f a st)
    let with_mconc' f_mconc a st = f_mconc.f (a, st)
    let extract ta = ta
                        
    let get_state () st = (st, st)
    let set_state newst _oldst = ((), newst)
  end)

  let get_state () st = (st, st)
  let set_state newst _oldst = ((), newst)
end

module StateT (St : TYPE) (M : MONAD) : sig
   include MONAD_TRANSFORMED
           with type 'a monad = (St.t -> ('a * St.t) M.monad)
            and type 'a monad_int = (St.t -> 'a * St.t)
            and type 'a monad_ext = ('a M.monad)
            and type 'a extract = ('a * St.t) M.extract
   val get_state : unit -> St.t monad
   val set_state : St.t -> unit monad
end = struct
    include Complete_Monad (struct
       type 'a monad = (St.t -> ('a * St.t) M.monad)
                        
      let return a st = M.return (a, st)
      let bind' f ma st0 = M.bind' (fun (a, st1) -> f a st1) (ma st0)
      let map' f ma st = M.map' (fun (a, st) -> (f a, st)) (ma st)
    
      let m_while test f a st =
        M.m_while
          (fun (a, st) -> test a)
          (fun (a, st) -> f a st)
          (a, st)

      let m_while3 testf a st0 =
        M.m_while3
          (fun (a, st1) ->
            M.map' (fun ((a2, res), st2) -> ((a2, st2), res)) (testf a st1))
          (a, st0)
          
      type ('a,'id) mconc = ('a * St.t, 'id) M.mconc
      type ('a,'b) f_mconc = { f : 'id. ('a,'id) mconc -> ('b,'id) mconc } 
      type 'a extract = ('a * St.t) M.extract
                                          
      let apply' f ta = M.apply' (fun (a, st) -> (f a st)) ta
      let with_mconc' f_mconc a st =  M.with_mconc' { M.f = f_mconc.f } (a, st)
      let extract = M.extract
    end)

    type 'a monad_int = (St.t -> 'a * St.t)
    type 'a monad_ext = ('a M.monad)
                          
    let lift_int f a st = M.return (f a st)
    let lift_ext f a st = M.map' (fun b -> (b, st)) (f a)
                           
    let get_state () st = M.return (st, st)
    let set_state newst _oldst = M.return ((), newst)
end
                                

module StateAbs (St : TYPE) : sig
   include MONAD with type 'a extract = ('a * St.t)
   type 'a monad_base = ('a monad)
   val run : ('a monad) -> (St.t -> 'a * St.t)
   val get_state : unit -> (St.t monad)
   val set_state : St.t -> (unit monad)

   module T : functor (M : MONAD) -> sig
                 include MONAD_TRANSFORMED
                         with type 'a monad_int = ('a monad_base)
                          and type 'a monad_ext = ('a M.monad)
                          and type 'a extract = ('a * St.t) M.extract
                 val run : ('a monad) -> (St.t -> ('a * St.t) M.monad)
                 val get_state : unit -> (St.t monad)
                 val set_state : St.t -> (unit monad)
              end
end = struct
   include State(St)
   type 'a monad_base = ('a monad)
   let run ma st = (ma st)

   module T (M : MONAD) = struct
      include StateT(St)(M)
      let run ma st = (ma st)
   end
end

                                           
module StateAbs_Writer (Logs : MONOID) : sig
  include MONAD with type 'a extract = ('a * Logs.t)
  type 'a monad_base = 'a monad
  val run : 'a monad -> ('a * Logs.t)
  val get_logs : unit -> Logs.t monad
  val write : Logs.t -> unit monad
                          
  module T : functor (M : MONAD) -> sig
                include MONAD_TRANSFORMED
                        with type 'a monad_int = ('a monad_base)
                         and type 'a monad_ext = 'a M.monad
                         and type 'a extract = ('a * Logs.t) M.extract
                val run : 'a monad -> ('a * Logs.t) M.monad
                val get_logs : unit -> Logs.t monad
                val write : Logs.t -> unit monad
             end
end = struct
   include State(Logs)
   type 'a monad_base = ('a monad)
   let run ma = ma Logs.nil
   let get_logs = get_state
   let write logs2 logs1 = ((), Logs.add logs1 logs2)
                                     
  module T (M : MONAD) = struct
    include StateT(Logs)(M)
    let run ma = ma Logs.nil
    let get_logs = get_state
    let write logs2 logs1 = M.return ((), Logs.add logs1 logs2)
  end
end
  
