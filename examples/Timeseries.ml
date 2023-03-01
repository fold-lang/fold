let device_series_to_json series =
  let series =
    Device_series.mapi
      (fun { Device_series_key.metric_name; device_id } values_by_time ->
        let values =
          Ptime_map.to_seq values_by_time
          |> Seq.map (fun (_, v) -> Json.float v)
        in
        Json.obj
          [ ("metric", Json.string metric_name)
          ; ("device_id", Vendor_device_id.to_json device_id)
          ; ("values", Json.seq values)
          ])
      series
  in
  Json.seq (Device_series.to_seq series |> Seq.map snd)

let generate_time_series_map ~start_time ~end_time ~resolution default =
  let end_time_int = int_of_float (Ptime.to_float_s end_time) in
  let rec loop time acc =
    if time >= end_time_int then acc
    else
      let ptime = Ptime.of_float_s (float time) |> Option.get in
      let acc' = Ptime_map.add ptime default acc in
      loop (time + resolution) acc'
  in
  let rounded_start_time =
    let ts = int_of_float (Ptime.to_float_s start_time) in
    ts / resolution * resolution
  in
  loop rounded_start_time Ptime_map.empty
