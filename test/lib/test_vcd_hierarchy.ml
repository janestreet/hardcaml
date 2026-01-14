(* Verify that the hierarchy is properly represented in the VCD file header *)
open! Core
open! Import
open! Test_module_hierarchy

let add_indentation s =
  s
  |> String.split_lines
  |> List.fold_map ~init:0 ~f:(fun level line ->
    let downscope = String.is_prefix line ~prefix:"$scope" in
    let upscope = String.is_prefix line ~prefix:"$upscope" in
    let indent = if upscope then level - 1 else level in
    ( (if downscope then level + 1 else if upscope then level - 1 else level)
    , String.init (2 * indent) ~f:(fun _ -> ' ') ^ line ))
  |> Tuple2.get2
  |> String.concat ~sep:"\n"
;;

let%expect_test "simple vcd file" =
  let open Outer in
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (create scope) in
  let sim = Vcd.wrap Stdio.stdout sim in
  Cyclesim.cycle sim;
  [%expect.output] |> add_indentation |> print_endline;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml-cyclesim
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
      $var wire 1 ! -clock $end
      $var wire 1 " -reset $end
      $var wire 1 $ a $end
      $var wire 1 # b $end
    $upscope $end
    $scope module outputs $end
      $var wire 1 % c_0 $end
      $var wire 1 ' c_1 $end
      $var wire 1 & d_0 $end
      $var wire 1 ( d_1 $end
      $var wire 1 ) x $end
    $upscope $end
    $scope module various $end
      $scope module the_middle $end
        $var wire 1 ; x $end
        $scope module -inputs $end
          $var wire 1 . a $end
          $var wire 1 2 b $end
        $upscope $end
        $scope module -outputs $end
          $var wire 1 * c_0 $end
          $var wire 1 3 c_1 $end
          $var wire 1 / d_0 $end
          $var wire 1 7 d_1 $end
          $var wire 1 : x $end
        $upscope $end
        $scope module inner $end
          $var wire 1 , a $end
          $scope module -inputs $end
            $var wire 1 - a $end
            $var wire 1 1 b $end
          $upscope $end
          $scope module -outputs $end
            $var wire 1 + c $end
            $var wire 1 0 d $end
          $upscope $end
        $upscope $end
        $scope module inner_1 $end
          $var wire 1 5 a $end
          $scope module -inputs $end
            $var wire 1 6 a $end
            $var wire 1 9 b $end
          $upscope $end
          $scope module -outputs $end
            $var wire 1 4 c $end
            $var wire 1 8 d $end
          $upscope $end
        $upscope $end
      $upscope $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    x$
    x#
    x%
    x'
    x&
    x(
    x)
    x;
    x.
    x2
    x*
    x3
    x/
    x7
    x:
    x,
    x-
    x1
    x+
    x0
    x5
    x6
    x9
    x4
    x8
    $end
    #0
    1!
    0"
    0#
    0$
    1%
    0&
    1'
    0(
    0)
    1*
    1+
    1,
    0-
    0.
    0/
    00
    01
    02
    13
    14
    15
    06
    07
    08
    09
    0:
    0;
    #5
    0!
    |}]
;;
