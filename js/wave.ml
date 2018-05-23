(* waveform viewer for the web using js_of_ocaml. *)

(************************************************************)

open Astring

(* extendible arrays *)
let n = 10000 (* amount to extend array by *)
type exarray =
  { mutable len  : int (* actual length *)
  ; mutable data : t array (* data array *) }

let make () =
  { len  = 0
  ; data = [||]}

(* extend the array, copy old elements *)
let extend t =
  let len = Array.length t.data in
  t.data <- Array.init (len + n) (fun i ->
    if i < len
    then t.data.(i)
    else empty)

let rec set a i v =
  if i >= Array.length a.data
  then (
    extend a;
    set a i v)
  else (
    a.data.(i) <- v;
    a.len <- max a.len (i+1))

let get a i = a.data.(i)
let length a = a.len
let data a = a.data

(* wrapping of the cycle simulator *)
open Hardcaml
open Cyclesim.Api

type wave =
  { name  : string
  (* XXX str   : t -> string*)
  ; nbits : int
  ; data  : exarray }

type waves = wave array

let wrap sim =
  let ports p= List.map
                 (fun s ->
                    { name = fst s
                    (* XXX str = Bits.to_string*)
                    ; nbits = Bits.width !(snd s)
                    ; data = make () }) p
  in
  let in_ports = ports sim.sim_in_ports in
  let out_ports = ports sim.sim_out_ports in
  let trace i x = List.iter2 (fun a b -> set a.data i !(snd b)) x in
  let cycle = ref 0 in
  let sim_reset () =
    sim.sim_reset ();
    trace !cycle in_ports sim.sim_in_ports;
    trace !cycle out_ports sim.sim_out_ports;
    incr cycle
  in
  let sim_cycle_seq () =
    sim.sim_cycle_seq ();
    trace !cycle in_ports sim.sim_in_ports;
    trace !cycle out_ports sim.sim_out_ports;
    incr cycle
  in
  { sim with sim_reset = sim_reset; sim_cycle_seq = sim_cycle_seq },
  Array.of_list (in_ports @ out_ports)

module Gui = struct


  module D = Dom_html

  let jstr = Js.string
  let jstri x = jstr (string_of_int x)

  (* Waveform viewer *)

  let line ~(ctx:D.canvasRenderingContext2D Js.t) ~x0 ~y0 ~x1 ~y1 =
    let f = float_of_int in
    ctx##(moveTo (f x0) (f y0));
    ctx##(lineTo (f x1) (f y1))

  let delta d =
    let len = Array.length d in
    let rec build (prev, prev_idx) i =
      if i = len
      then []
      else
      if d.(i) = prev
      then
        build (prev, prev_idx) (i+1)
      else
        (d.(i), i) :: build (d.(i), i) (i+1)
    in
    if len=0
    then []
    else (d.(0), 0) :: (build (d.(0), 0) 1)

  let render_1
        (ox, oy) (sx, sy) max_t
        (ctx:D.canvasRenderingContext2D Js.t)
        d =
    let d = Array.map to_int d in
    let d = delta d in
    let line x0 y0 x1 y1 =
      line ctx
        (x0*sx+ox) (y0*sy+oy)
        (x1*sx+ox) (y1*sy+oy)
    in
    let rec render first (p_d, p_t) next =
      match next with
      | [] -> (* render from p_t to max_t *)
        render_elt first p_d p_t max_t
      | (n_d, n_t) :: tl -> (* render from p_t to n_t (or max_t) *)
        if n_t < max_t
        then (
          render_elt first p_d p_t n_t;
          render false (n_d, n_t) tl)
        else
          render_elt first p_d p_t max_t
    and render_elt first p_d p_t n_t =
      line p_t (1-p_d) n_t (1-p_d);
      if not first then line p_t 0 p_t 1
    in
    match d with
    | [] -> ()
    | h :: t -> render true h t

  let render_n to_str
        (ox, oy) (sx, sy) max_t
        (ctx:D.canvasRenderingContext2D Js.t)
        d =
    let d = delta d in
    let line x0 y0 x1 y1 =
      line ctx
        (x0*sx+ox) (y0*sy+oy)
        (x1*sx+ox) (y1*sy+oy)
    in
    let rec text dotted x str w =
      let jstr = if dotted then jstr (str^"..") else jstr str in
      let max_w = float (w * sx - 4) in
      let w' = ctx##(measureText jstr)##.width in
      if w' > max_w
      then (
        (* If too wide, then shorten the string by 1 char, and append a dot - keep
           shortening until the string length is 1 (excluding the dot) then fail. *)
        let len = String.length str in
        if len > 1
        then (
          text true x (String.Sub.to_string @@ String.sub str ~start:0 ~stop:(len-1)) w))
      else
        ctx##(fillText jstr (float (x*sx+ox+2)) (float (oy+2)))
    in
    let rec render first (p_d, p_t) next =
      match next with
      | [] -> (* render from p_t to max_t *)
        render_elt first p_d p_t max_t
      | (n_d, n_t) :: tl -> (* render from p_t to n_t (or max_t) *)
        if n_t < max_t
        then (
          render_elt first p_d p_t n_t;
          render false (n_d, n_t) tl)
        else
          render_elt first p_d p_t max_t
    and render_elt first p_d p_t n_t =
      line p_t 1 n_t 1;
      line p_t 0 n_t 0;
      if not first then line p_t 0 p_t 1;
      text false p_t (to_str p_d) (n_t - p_t)
    in
    match d with
    | [] -> ()
    | h :: t -> render true h t

  let vlines (ctx:D.canvasRenderingContext2D Js.t) width height n =
    let rec f i =
      line ctx (i*width) 0 (i*width) height;
      if i=n then () else f (i+1)
    in
    f 0

  let mk_canvas width height =
    let d = D.document in
    let canvas = D.createCanvas d in
    let style = canvas##.style in
    canvas##.width := width;
    canvas##.height := height;
    style##.width := jstri width;
    style##.height := jstri height;
    canvas

  let select (d:exarray) ofs n =
    let len = length d in
    Array.init n (fun i ->
      if i + ofs < len then get d (i+ofs) else get d (len-1))

  let mk_wave_table par width height data =

    let margin = 2 in
    let w_width, w_height = ref 16, height-(2*margin) in
    let n = ref (width / !w_width) in
    let ofs = ref 0 in

    let set_w_width w =
      let w = max 1 (min width w) in
      w_width := w;
      n := width / !w_width;
    in
    let set_ofs o = ofs := max 0 o in

    (* create the table *)
    let d = D.document in
    let table = D.createTable d in
    let tbody = D.createTbody d in
    table##.className := jstr "wave-table";
    Dom.appendChild table tbody;

    (* create header row with the buttons *)
    let create_header () =
      let trow = D.createTr d in
      let td = D.createTd d in
      td##.className := jstr "wave-name";
      td##.innerHTML := jstr "cycle";
      Dom.appendChild trow td;

      let tdc = D.createTd d in
      tdc##.className := jstr "wave-data";
      tdc##.innerHTML := jstr "0";
      Dom.appendChild trow tdc;

      let td = D.createTd d in
      let buttons =
        List.map (fun (title, url, id, margin) ->
          let i = D.createInput ~_type:(jstr "image") d in
          let s = i##.style in
          i##.title := jstr title;
          i##.src := jstr url;
          i##.alt := jstr id;
          s##.marginLeft := jstr margin;
          Dom.appendChild td i;
          id, i)
          [ "scroll one page backward",
            (*"images/arrow_left_alt2_24x24.png", *)
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAASpJREFUeNq8VksOgjAUbCuJW46AJ9ClO7mBHIGlnsIj6NaVHAGX7vAk4glkS+JvXvIwUmkjQp3kpSGFmfZ9kcICbzicY4lgISzQtnNYBkuvZbk3cUgD8QxL0kBqAonFEDrqG6qBfM0n+5Zc8LsZf1vDQCPfYVmI3zFVnhfcb7f9hwCrdyGvMIGID5HDKwbs80z0i5BiUgmcWvr8q8BDYCQ5FVPhBpHiPBcuBULDZtHDzULP4PuChScwv2U9BLVnxOCh2QU2/uW4+G6l8ynhGJRFD4uLfEuMGn2uvy8tNVBw8XXJslxZKtjvIYUz5bDICKnzVlFlUezg9PGrXaO1nqnFUj/viXyD029r84D6Nw0Lrt4uSEC+bJxoNIk63mTzTv6XoS9d/7Y8BRgAoCRp6va+RI4AAAAASUVORK5CYII=",
            "page_backward", "5px"
          ; "step one cycle backward",
            (*"images/arrow_left_alt1_24x24.png", *)
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAUpJREFUeNq8Vj3OwjAMTUMk1u8GlBt0ZPu4AeUGZYONm8DK9HGEMrKVG3AD4AasSPw9IxdBaYoTymcpiiD1e67tPDdQFWaazR62GKuLFRaOt1gZVno8HBY2jMAC/IttXgJqMyJLQLQqHugS8AlHJgVX/GzGvk/WKID/YRsqf+toY8Lz6bR4IWD2T8Bzi0DyA5LlvQac80zVa12qSV6DudBp7UBww9TciqHQYexSeMLW3OdvwfG6A480xZov0TfAb3Uwb9JTBN9XNENZoGGAPF0sDiQBfWmoNhxd4bOvo1dNxVmCqNRjivC7Rf+7EFCKNtI6eFzIrRY4JKxRPpYRQSp40JckzbVoI7zNJBWRND1IbTvvImnhIofok7tcQ1p3JLGk5zUp6RTRz57mAek3DQvHKG1dNyqdaDSJPnyT6SP4vwz94NufLVcBBgCMLnX3vQebUQAAAABJRU5ErkJggg==",
            "step_backward", "5px"
          ; "step one cycle forward",
            (*"images/arrow_right_alt1_24x24.png", *)
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAUxJREFUeNq8VrsNwjAQtR1LtIwAG1DSkQ2gpAwdTMEI0FLBCKGkCxtkA2ADWiQCvIvOCEIcbCCcZDmSz+/dne8TKSpENxp9bAOsEKtVON5jJVjx+XRa2zCkBbiHbVUCahMii0C0LR6oEvAZW+YKLlg34btPEhTAl9jG4nPpKq1blyxbvxAw+zfgRjogaYJkc38DjnkifishvYl5g5XHxdRRL8cMOBV9QjM0oXij10SoUsV57iVwfeTo9UBxEYmaSEJdke+2Rz8+kiDE9BnZ6kNC4WqxULp6w/VTSqJEzaIrrJra0g/eHVyszwsNCjvPvhOapvYOnJqg+rSCHcDzRCGCuCZwktj0Ip8wpQ5VnIcHoWybLIo8HOg46kX3do3WeqAWS/38R9k5h/WLp3lA/ZuGhYeFoiKNJ6UTjSbRl57MH8H/MvRl3b8tNwEGAJsIdYLae8SOAAAAAElFTkSuQmCC",
            "step_forward", "5px"
          ; "scroll one page forward",
            (*"images/arrow_right_alt2_24x24.png", *)
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAATFJREFUeNq8VrsNwjAUtE0k2owAE0BJBxuQEVLCFIwALRUZwZR0YQM2IEwALRK/e+iBiLERITYnPVmRkzv7fSPFB0TN5hBLAhvAWsZ2Acth+nQ8Ll0c0kHcx5JZSF0gsRRCa3NDWcinfLJvyQW/m/O3JTQM8gWWkfgdPRVFrcv5vHwTYPU65A90IRJDZPWMAfs8F34xoJg8BLYVff5V4CHQVpyKvsnvgSduxXkeConiIqoDDTu44iBxjaulaIoKAlSQG06S2Fa1V8Mmv1wD33Vge5NPicCIbH6DchWOnGNgdZH0UAOaEyW21YLyUMGJg/x+O8UnCAUdvlXwQxrg9OmzXaO17qjFUj/3RD7D6eeleUD9m4YF9fOa5BnIx9aJRpOo5k1mr+R/Gfoy9G/LTYABAI8sa271ebkxAAAAAElFTkSuQmCC",
            "page_forward", "5px"
          ; "zoom out",
            (*"images/minus_alt_24x24.png", *)
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAARFJREFUeNq8VkEOgjAQbGsTrj4Bf+DRm/xAnsBRX+ET5OpJngBHb/gS5Rkkou6ahQi2BKTrJJuGFGaWnbKLFD3QnreBJYQIIPzO9g0ih0jvZZnZOKSFeA1LYiC1AcUiELp0N5SB/ECZDSUXdG9Oz7Yw65CfYNmK37FSWvuPqsq+BEh9CnmNJYjMQeTceEA1z4VbBOhJLXAdWfNBxoPAQtFRdE3+Nh65FZ1zLoSSqTxNmVDgadkca3pg/JJtAmCQHMNu41GCGbono70LAXaTFcMX3DooKJAyCqT8rYIuIobso6ZdQ2stsMViP3dEHkP2x9Y8wP6NwwL7+UTyBMh3xomGk2jim8Sf5H8Z+pL7t+UlwABnxV9UzyoJYgAAAABJRU5ErkJggg==",
            "zoom_out", "15px"
          ; "zoom in",
            (*"images/plus_alt_24x24.png", *)
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAATNJREFUeNq8lkEOgjAQRdvahK1H4Agu3dEbyBFc6inkBrJ1JUeApTu8CR6DRNQZMxioLSlC/UnTEOibdmY6A2cDkkGwgSmGoWCE2usKRgkjv9d1YWNwCziCKTNAbUJjWzB01V8IA/xIO3OFM/q2pLU9LTT4GaYd+11rIWX4aJriywBZnwJvtQIjSzBy+cSAfF6yeaUwJm0MMocFGWWTGvE9k5SKLgGt2iyBNcol8MgWlOe+FAs6si8pOeCe0nCZ9Fvcg5ncxMFPTxMd/M3HbNXGEcyz5MCODrrLOlkUucZOkj9NcUgMz9eOvxOX1BYebnDv1Ggg92ggF9QsKg9wvPmF92L3LtdQWm9YYrGezwRPAX7q9QOs39gssJ5PhGcA3xs7GnaiiSdJu/C/NH3u+7flJcAAN2ByK2/IoW0AAAAASUVORK5CYII=",
            "zoom_in", "5px" ]
      in
      Dom.appendChild trow td;
      Dom.appendChild tbody trow;
      tdc, buttons
    in

    let create_wave_row data =
      let trow = D.createTr d in

      (* name *)
      let td = D.createTd d in
      td##.className := jstr "wave-name";
      td##.innerHTML := jstr
                          (if data.nbits = 1
                           then data.name
                           else data.name ^ "[" ^ string_of_int data.nbits ^ "]");
      Dom.appendChild trow td;

      (* value *)
      let tdv = D.createTd d in
      tdv##.className := jstr "wave-data";
      tdv##.innerHTML := jstr "XXX";
      Dom.appendChild trow tdv;

      let td = D.createTd d in
      let canvas = mk_canvas width height in
      let ctx = canvas##(getContext (D._2d_)) in

      (* default settings *)
      ctx##.textBaseline := jstr "top";
      ctx##.textAlign := jstr "left";
      ctx##.font := jstr (string_of_int (w_height-(2*margin)) ^
                          "px sans-serif");
      let clear () =
        ctx##.fillStyle := jstr "white";
        ctx##(fillRect (0.) (0.) (float width) (float height));
      in
      (* draw grid *)
      let grid () =
        ctx##beginPath;
        ctx##.strokeStyle := jstr "lightgray";
        vlines ctx (!w_width) height !n;
        ctx##stroke;
        ctx##closePath;
      in
      (* render waves *)
      let render () =
        clear ();
        grid ();
        ctx##.fillStyle := jstr "black";
        ctx##beginPath;
        ctx##.strokeStyle := jstr "black";
        ctx##.lineWidth := 1.5;
        (if data.nbits = 1
         then
           render_1 (0, margin) (!w_width, w_height) !n ctx
             (select data.data !ofs !n)
         else
           let str = Bits.to_string in (* XXX data.str *)
           render_n str (0, margin) (!w_width, w_height) !n ctx
             (select data.data !ofs !n));
        ctx##stroke;
        ctx##closePath
      in

      Dom.appendChild td canvas;
      Dom.appendChild trow td;

      Dom.appendChild tbody trow;
      tdv, canvas, render, data
    in

    let cycle, buttons = create_header () in
    let waves = Array.map create_wave_row data in
    let render () =
      Array.iter (fun (_, _, r, _) -> r ()) waves
    in

    (* add click handler to update value display and cycle no. *)
    let set_values x =
      Array.iter (fun (v, _, _, d) ->
        let l = length d.data in
        (*v##innerHTML <- jstr (d.str (get d.data (min (l-1) x)))*)
        v##.innerHTML := jstr (Bits.to_string (get d.data (min (l-1) x))))
        waves
    in
    let onclick c =
      c##.onclick := D.handler (fun e ->
        let clientLeft =
          int_of_float
            ((Js.to_float
                c##getBoundingClientRect##.left) +. 0.5) in
        let x = ((e##.clientX - clientLeft) / !w_width) + !ofs in
        cycle##.innerHTML := jstri x;
        set_values x;
        Js._false)
    in
    Array.iter (fun (_, c, _, _) -> onclick c) waves;

    Dom.appendChild par table;

    (* attach button events *)
    let param_ev fn = D.handler (fun _ -> fn (); render (); Js._false) in
    let events =
      [ "zoom_in", param_ev (fun () -> set_w_width (!w_width * 2))
      ; "zoom_out", param_ev (fun () -> set_w_width (!w_width / 2))
      ; "step_forward", param_ev (fun _ -> set_ofs (!ofs + 1))
      ; "step_backward", param_ev (fun _ -> set_ofs (!ofs - 1))
      ; "page_forward", param_ev (fun _ -> set_ofs (!ofs + !n))
      ; "page_backward", param_ev (fun _ -> set_ofs (!ofs - !n)) ]
    in

    (* very odd bug occured here on one of the webapp as the assoc wouldn't work? *)
    let rec myassoc a l =
      match l with
      | [] -> failwith "myassoc"
      | (b, c) :: t -> if a=b then c else myassoc a t
    in

    List.iter (fun (id, b) ->
      (*List.iter (fun (id, _) -> log id) events;*)
      let ev =
        (* {[
             try List.assoc id events
             with _ -> failwith ("assoc: " ^ id)
           ]} *)
        myassoc id events
      in
      b##.onclick := ev)
      buttons;

    set_values 0;
    render ()

end
