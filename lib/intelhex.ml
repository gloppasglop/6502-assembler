type record_type =
  | DATA
  | EOF
  | ESA (* Extended Segment Address *)
  | SSA (* Start Segment Address *)
  | ELA (* Extended Linear Address *)
  | SLA (* Start Linear Address *)

type hex_record =
  { byte_count : int
  ; address : int
  ; record_type : record_type
  ; data : int list option
  ; checksum : int
  }

let int_of_record = function
  | DATA -> 0
  | EOF -> 1
  | ESA -> 2
  | SSA -> 3
  | ELA -> 4
  | SLA -> 5
;;

let string_of_hex_record { byte_count; address; record_type; data; checksum } =
  Printf.sprintf
    ":%02X%04X%02X%s%02X"
    byte_count
    address
    (int_of_record record_type)
    (match data with
     | None -> ""
     | Some bytes -> List.fold_left (fun acc s -> acc ^ Printf.sprintf "%02X" s) "" bytes)
    checksum
;;

let checksum record =
  let addr_sum = (record.address land 0xFF) + ((record.address land 0xFF00) lsr 8) in
  let sum_data = function
    | None -> 0
    | Some l -> List.fold_left ( + ) 0 l
  in
  (1
   + lnot
       (record.byte_count
        + int_of_record record.record_type
        + addr_sum
        + sum_data record.data))
  land 0xFF
;;

let split len list =
  let split_aux len list =
    List.fold_left
      (fun (i, acc) x ->
        if i < len
        then (
          match acc with
          | [] -> i + 1, [ [ x ] ]
          | head :: tail -> i + 1, (x :: head) :: tail)
        else 1, [ x ] :: acc)
      (0, [])
      list
  in
  List.map List.rev (snd (split_aux len list))
;;

let mem_to_hex byte_count mem =
  List.fold_left
    (fun acc (address, data) ->
      let tmp_data =
        let data' = List.rev (split byte_count data) in
        List.mapi
          (fun i data ->
            let hex_record =
              { byte_count = min byte_count (List.length data)
              ; address = address + (i * byte_count)
              ; data = Some data
              ; record_type = DATA
              ; checksum = 0
              }
            in
            { hex_record with checksum = checksum hex_record })
          data'
      in
      tmp_data @ acc)
    [ { byte_count = 0; address = 0; data = None; record_type = EOF; checksum = 0xFF } ]
    mem
;;
