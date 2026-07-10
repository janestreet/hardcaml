open! Core0

module Make (Comb : sig
    include Comb.S

    val multiport_memory_prim
      :  ?name:string
      -> ?attributes:Rtl_attribute.t list
      -> ?initialize_to:Bits.t array
      -> int
      -> remove_unused_write_ports:bool
      -> data_width:int
      -> write_ports:t Write_port.t array
      -> read_addresses:t array
      -> t array
  end) =
struct
  open Comb

  let validate_non_zero_ports ~write_ports ~read_addresses =
    if Array.is_empty write_ports
    then raise_s [%message "[Signal.multiport_memory] requires at least one write port"];
    if Array.is_empty read_addresses
    then raise_s [%message "[Signal.multiport_memory] requires at least one read port"]
  ;;

  let validate_initialize_to ~data_width ~size ~initialize_to =
    Option.iter initialize_to ~f:(fun initialize_to ->
      let initializer_array_size = Array.length initialize_to in
      if initializer_array_size <> size
      then
        raise_s
          [%message
            "[Signal.multiport_memory] size of initializer array differs from memory size"
              (initializer_array_size : int)
              (size : int)];
      Array.iter initialize_to ~f:(fun initialize_to ->
        let initializer_width = Bits.width initialize_to in
        if initializer_width <> data_width
        then
          raise_s
            [%message
              "[Signal.multiport_memory] width of initializer is different to write port \
               width"
                (initializer_width : int)
                (data_width : int)]))
  ;;

  let validate_size ~size ~address_width =
    if size <= 0
    then
      raise_s
        [%message "[Signal.multiport_memory] size must be greater than 0" (size : int)];
    if address_bits_for size <> address_width
    then
      raise_s
        [%message
          "[Signal.multiport_memory] size does not match what can be addressed"
            (size : int)
            (address_width : int)]
  ;;

  let exactly_divides a ~by = a % by = 0

  let validate_write_port
    ~single_bit_write_enable
    ~address_width
    ~data_width
    port
    (write_port : _ Write_port.t)
    =
    (* clocks must be 1 bit *)
    if width write_port.write_clock <> 1
    then
      raise_s
        [%message
          "[Signal.multiport_memory] width of clock must be 1"
            (port : int)
            ~write_enable_width:(width write_port.write_enable : int)];
    if single_bit_write_enable
    then (
      if (* all write enables must be 1 bit *)
         width write_port.write_enable <> 1
      then
        raise_s
          [%message
            "[Signal.multiport_memory] width of write enable must be 1"
              (port : int)
              ~write_enable_width:(width write_port.write_enable : int)])
    else (
      let num_enables = width write_port.write_enable in
      if not (exactly_divides data_width ~by:num_enables)
      then
        raise_s
          [%message
            "Write enables do not exactly divide the write data bus width"
              (data_width : int)
              (num_enables : int)]);
    (* all write addresses must be the same width *)
    if width write_port.write_address <> address_width
    then
      raise_s
        [%message
          "[Signal.multiport_memory] width of write address is inconsistent"
            (port : int)
            ~write_address_width:(width write_port.write_address : int)
            ~expected:(address_width : int)];
    if width write_port.write_data <> data_width
    then
      raise_s
        [%message
          "[Signal.multiport_memory] width of write data is inconsistent"
            (port : int)
            ~write_data_width:(width write_port.write_data : int)
            ~expected:(data_width : int)]
  ;;

  let validate_read_address ~address_width port read_address =
    if width read_address <> address_width
    then
      raise_s
        [%message
          "[Signal.multiport_memory] width of read address is inconsistent"
            (port : int)
            ~read_address_width:(width read_address : int)
            ~expected:(address_width : int)]
  ;;

  let validate
    ~single_bit_write_enable
    ~size
    ~data_width
    ~address_width
    ~initialize_to
    ~write_ports
    ~read_addresses
    =
    (* Size addresses full memory *)
    validate_size ~size ~address_width;
    (* Validate the number of initializer values and their widths are correct. *)
    validate_initialize_to ~data_width ~size ~initialize_to;
    (* validate write ports *)
    Array.iteri
      write_ports
      ~f:(validate_write_port ~single_bit_write_enable ~address_width ~data_width);
    (* validate read addresses *)
    Array.iteri read_addresses ~f:(validate_read_address ~address_width)
  ;;

  let compute_port_address ~size ~address ~max_num_enables ~part =
    let address =
      if max_num_enables = 1
      then address
      else if Int.ceil_pow2 max_num_enables = max_num_enables
      then (
        let log_max_num_enables = Int.ceil_log2 max_num_enables in
        address @: of_unsigned_int ~width:log_max_num_enables part)
      else (
        let max_num_enables =
          of_unsigned_int ~width:(num_bits_to_represent max_num_enables) max_num_enables
        in
        (address *: max_num_enables) +:. part)
    in
    uresize address ~width:(address_bits_for size)
  ;;

  let rec gcd a b =
    assert (a >= 0);
    assert (b >= 0);
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)
  ;;

  type write_data_and_enables =
    { data_width : int
    ; num_enables : int
    ; enable_replication : int
    }
  [@@deriving sexp_of]

  let compute_data_width_and_num_enables ~data_width (write_ports : _ Write_port.t array) =
    let ports =
      Array.map write_ports ~f:(fun port ->
        let num_enables = width port.write_enable in
        { data_width = data_width / num_enables; num_enables; enable_replication = 1 })
    in
    let min_data_width =
      Array.fold
        ports
        ~init:data_width
        ~f:(fun acc { data_width; num_enables = _; enable_replication = _ } ->
          gcd acc data_width)
    in
    let max_num_enables = data_width / min_data_width in
    let write_data_and_enables =
      Array.map ports ~f:(fun p ->
        { p with enable_replication = p.data_width / min_data_width })
    in
    Array.iter
      write_data_and_enables
      ~f:(fun { data_width = _; num_enables; enable_replication } ->
        if min_data_width * num_enables * enable_replication <> data_width
           || num_enables * enable_replication <> max_num_enables
        then
          raise_s
            [%message
              "Failed to validate port specification"
                (data_width : int)
                (min_data_width : int)
                (max_num_enables : int)
                (num_enables : int)
                (enable_replication : int)]);
    min_data_width, max_num_enables, write_data_and_enables
  ;;

  let implement_write_ports
    ~size
    ~min_data_width
    ~max_num_enables
    ~(write_ports : _ Write_port.t array)
    ~write_data_and_enables
    =
    List.init (Array.length write_ports) ~f:(fun port_idx ->
      let replicate = write_data_and_enables.(port_idx).enable_replication in
      let port = write_ports.(port_idx) in
      Array.init max_num_enables ~f:(fun part ->
        { Write_port.write_clock = port.write_clock
        ; write_address =
            compute_port_address ~size ~address:port.write_address ~max_num_enables ~part
        ; write_data = port.write_data.:+[part * min_data_width, Some min_data_width]
        ; write_enable = port.write_enable.:(part / replicate)
        }))
    |> Array.concat
  ;;

  let implement_read_ports ~size ~max_num_enables ~read_addresses =
    List.init (Array.length read_addresses) ~f:(fun idx ->
      Array.init max_num_enables ~f:(fun part ->
        compute_port_address ~size ~address:read_addresses.(idx) ~max_num_enables ~part))
    |> Array.concat
  ;;

  let split_initialize_to ~min_data_width ~initialize_to =
    Option.map initialize_to ~f:(fun d ->
      Array.map d ~f:(fun d ->
        Bits.split_lsb ~part_width:min_data_width d |> Array.of_list)
      |> Array.to_list
      |> Array.concat)
  ;;

  let construct_output_ports ~max_num_enables ~num_read_ports ~q =
    Array.init num_read_ports ~f:(fun idx ->
      Array.sub q ~pos:(idx * max_num_enables) ~len:max_num_enables
      |> Array.to_list
      |> concat_lsb)
  ;;

  let checked_multiport_memory_prim
    ?name
    ?attributes
    ?initialize_to
    size
    ~remove_unused_write_ports
    ~data_width
    ~address_width
    ~(write_ports : _ Write_port.t array)
    ~read_addresses
    =
    validate
      ~single_bit_write_enable:true
      ~size
      ~data_width
      ~address_width
      ~initialize_to
      ~write_ports
      ~read_addresses;
    multiport_memory_prim
      ?name
      ?attributes
      ?initialize_to
      size
      ~remove_unused_write_ports
      ~data_width
      ~write_ports
      ~read_addresses
  ;;

  let multiport_memory_without_enables
    ?name
    ?attributes
    ?initialize_to
    size
    ~(write_ports : _ Write_port.t array)
    ~read_addresses
    =
    let base_port = write_ports.(0) in
    let data_width = width base_port.write_data in
    let address_width = width base_port.write_address in
    checked_multiport_memory_prim
      ?name
      ?attributes
      ?initialize_to
      size
      ~remove_unused_write_ports:false
      ~data_width
      ~address_width
      ~write_ports
      ~read_addresses
  ;;

  let multiport_memory_with_enables
    ?(verbose = false)
    ?name
    ?attributes
    ?initialize_to
    size
    ~(write_ports : _ Write_port.t array)
    ~read_addresses
    =
    let base_port = write_ports.(0) in
    let data_width = width base_port.write_data in
    let address_width = width base_port.write_address in
    validate
      ~single_bit_write_enable:false
      ~size
      ~data_width
      ~address_width
      ~initialize_to
      ~write_ports
      ~read_addresses;
    let min_data_width, max_num_enables, write_data_and_enables =
      compute_data_width_and_num_enables ~data_width write_ports
    in
    if verbose
    then
      Stdio.print_s
        [%message
          (min_data_width : int)
            (max_num_enables : int)
            (write_data_and_enables : write_data_and_enables array)];
    (* New size of the ram *)
    let size = size * max_num_enables in
    let write_ports =
      implement_write_ports
        ~size
        ~min_data_width
        ~max_num_enables
        ~write_ports
        ~write_data_and_enables
    in
    let q =
      let read_addresses = implement_read_ports ~size ~max_num_enables ~read_addresses in
      let initialize_to = split_initialize_to ~min_data_width ~initialize_to in
      multiport_memory_without_enables
        ?name
        ?attributes
        ?initialize_to
        size
        ~write_ports
        ~read_addresses
    in
    construct_output_ports
      ~max_num_enables
      ~num_read_ports:(Array.length read_addresses)
      ~q
  ;;

  let scaled_write_ports (write_ports : _ Write_port.t array) =
    let widest_port =
      Array.max_elt write_ports ~compare:(fun a b ->
        Int.compare (width a.write_data) (width b.write_data))
      |> Option.value_exn ~message:"Unexpected empty write port list"
    in
    let widest_port_data_width = width widest_port.write_data in
    (* ensure that the width of each port and relative address bits are consistent *)
    Array.iter write_ports ~f:(fun write_port ->
      let widest_port_address_width = width widest_port.write_address in
      let write_port_address_width = width write_port.write_address in
      let diff = write_port_address_width - widest_port_address_width in
      if diff < 0
      then
        raise_s
          [%message
            "[Signal.multiport_memory] Write port address is too small to address RAM"
              (widest_port_address_width : int)
              (write_port_address_width : int)];
      let ratio = Int.pow 2 diff in
      let write_port_data_width = width write_port.write_data in
      if write_port_data_width * ratio <> widest_port_data_width
      then
        raise_s
          [%message
            "[Signal.multiport_memory] Width of write port data and address are \
             inconsistent with widest port"
              (write_port_data_width : int)
              (write_port_address_width : int)
              (ratio : int)
              (widest_port_data_width : int)
              (widest_port_address_width : int)]);
    let write_ports =
      Array.map write_ports ~f:(fun write_port ->
        let diff = width write_port.write_address - width widest_port.write_address in
        let ratio = Int.pow 2 diff in
        match diff with
        | 0 -> write_port
        | _ ->
          let write_address = drop_bottom write_port.write_address ~width:diff in
          let write_select = sel_bottom write_port.write_address ~width:diff in
          { write_clock = write_port.write_clock
          ; write_address
          ; write_data = repeat write_port.write_data ~count:ratio
          ; write_enable =
              List.init ratio ~f:(fun idx ->
                mux2
                  (write_select ==:. idx)
                  write_port.write_enable
                  (zero (width write_port.write_enable)))
              |> concat_lsb
          })
    in
    widest_port, write_ports
  ;;

  type scaled_read_port =
    { read_addresses : t array
    ; get_read_data : t array -> t
    }

  let scaled_read_ports ~(write_port : _ Write_port.t) ~read_addresses =
    Array.map read_addresses ~f:(fun read_address ->
      let diff = width read_address - width write_port.write_address in
      if diff = 0
      then { read_addresses = [| read_address |]; get_read_data = (fun q -> q.(0)) }
      else if diff > 0
      then (
        let ratio = Int.pow 2 diff in
        (* Read port is narrower. Split and mux the output *)
        let read_address, read_select =
          drop_bottom read_address ~width:diff, sel_bottom read_address ~width:diff
        in
        let part_width = width write_port.write_data / ratio in
        if part_width <= 0 then raise_s [%message "Cannot split read port" (ratio : int)];
        let read_mux q = mux_strict read_select (split_lsb ~part_width q.(0)) in
        { read_addresses = [| read_address |]; get_read_data = read_mux })
      else (
        (* Read port is wider. Add read ports and concat the output *)
        let diff = -diff in
        let ratio = Int.pow 2 diff in
        let read_concat q = concat_lsb (Array.to_list q) in
        { read_addresses =
            Array.init ratio ~f:(fun idx ->
              read_address @: of_unsigned_int ~width:diff idx)
        ; get_read_data = read_concat
        }))
  ;;

  let get_scaled_read_port_addresses s =
    Array.map s ~f:(fun { read_addresses; _ } -> read_addresses)
    |> Array.to_list
    |> Array.concat
  ;;

  let get_scaled_output_ports (scaled_read_ports : scaled_read_port array) q =
    Array.folding_map scaled_read_ports ~init:0 ~f:(fun pos scaled_read_port ->
      let len = Array.length scaled_read_port.read_addresses in
      pos + len, scaled_read_port.get_read_data (Array.sub q ~pos ~len))
  ;;

  let multiport_memory_with_port_scaling_and_enables
    ?(verbose = false)
    ?name
    ?attributes
    ?initialize_to
    size
    ~(write_ports : _ Write_port.t array)
    ~read_addresses
    =
    let widest_write_port, write_ports = scaled_write_ports write_ports in
    let scaled_read_ports =
      scaled_read_ports ~write_port:widest_write_port ~read_addresses
    in
    let q =
      multiport_memory_with_enables
        ~verbose
        ?name
        ?attributes
        ?initialize_to
        size
        ~write_ports
        ~read_addresses:(get_scaled_read_port_addresses scaled_read_ports)
    in
    get_scaled_output_ports scaled_read_ports q
  ;;

  let multiport_memory
    ?(enable_modelling_features = false)
    ?(verbose = false)
    ?name
    ?attributes
    ?initialize_to
    size
    ~(write_ports : _ Write_port.t array)
    ~read_addresses
    =
    validate_non_zero_ports ~write_ports ~read_addresses;
    if enable_modelling_features
    then
      multiport_memory_with_port_scaling_and_enables
        ~verbose
        ?name
        ?attributes
        ?initialize_to
        size
        ~write_ports
        ~read_addresses
    else
      multiport_memory_without_enables
        ?name
        ?attributes
        ?initialize_to
        size
        ~write_ports
        ~read_addresses
  ;;

  let rom ~read_addresses initialize_to =
    if Array.length read_addresses = 0
    then raise_s [%message "Rom must have 1 or more read addresses"];
    if Array.length initialize_to = 0
    then raise_s [%message "Rom must have 1 or more initialization values"];
    checked_multiport_memory_prim
      ~initialize_to
      (Array.length initialize_to)
      ~remove_unused_write_ports:true
      ~data_width:(Bits.width initialize_to.(0))
      ~address_width:(Int.ceil_log2 (Array.length initialize_to))
      ~write_ports:[||]
      ~read_addresses
  ;;
end
