namespace excut.core

module Engine =
  type Options = { binSize: int; cutSize: int }
  type Cutout = { label: string; size: int }

  type private Bin =
    { cutouts: Cutout list
      remaining: int }

  type private Labeled =
    | Populated of Bin
    | Unpopulated of Bin

  type Cut =
    { cutouts: Cutout list
      used: int
      cuts: int
      rest: int }

  type private Buckets =
    { big: Cutout list
      medium: Cutout list
      small: Cutout list
      tiny: Cutout list }

  let private cutoutSize (cutout: Cutout) = cutout.size

  let private createBin (binSize: int) (cutout: Cutout) =
    { cutouts = [ cutout ]
      remaining = binSize - cutout.size }

  let private addToBin (cutout: Cutout) (bin: Bin) =
    { cutouts = cutout :: bin.cutouts
      remaining = bin.remaining - cutout.size }

  let private fitOne (binSize: int) (bins: Bin list) (cutout: Cutout) =
    let result =
      bins
      |> List.fold
           (fun (processed, found) bin ->
             match found, bin.remaining with
             | false, x when x >= cutout.size -> (bin |> addToBin cutout) :: processed, true
             | _ -> bin :: processed, found)
           ([], false)

    match result with
    | newBins, true -> newBins
    | newBins, false -> (createBin binSize cutout) :: newBins
    |> List.rev

  let private ffd (binSize: int) (cutouts: Cutout list) =
    cutouts
    |> List.sortByDescending cutoutSize
    |> List.fold (fitOne binSize) []

  let (|Big|Medium|Small|Tiny|) (binsSize: int, cutout: Cutout) =
    match cutout.size with
    | size when size > binsSize / 2 -> Big
    | size when size > binsSize / 3 -> Medium
    | size when size > binsSize / 6 -> Small
    | _ -> Tiny

  let private partiton (binSize: int) (cutouts: Cutout list) =
    let buckets =
      cutouts
      |> List.fold
           (fun state cutout ->
             match binSize, cutout with
             | Big -> { state with big = cutout :: state.big }
             | Medium ->
                 { state with
                     medium = cutout :: state.medium }
             | Small ->
                 { state with
                     small = cutout :: state.small }
             | Tiny ->
                 { state with
                     tiny = cutout :: state.tiny })
           { big = []
             medium = []
             small = []
             tiny = [] }

    { big = buckets.big |> List.sortByDescending cutoutSize
      medium = buckets.medium |> List.sortBy cutoutSize
      small = buckets.small |> List.sortBy cutoutSize
      tiny = buckets.tiny |> List.sortBy cutoutSize }

  let canFit (maxSize: int) (cutout: Cutout) = cutout.size <= maxSize

  let private takeCutout (maxSize: int) (cutouts: Cutout list) =
    let split =
      cutouts
      |> List.fold
           (fun (fit, rest) cutout ->
             match cutout.size with
             | c when c <= maxSize -> cutout :: fit, rest
             | _ -> fit, cutout :: rest)
           ([], [])

    match split with
    | head :: tail, rest -> Some(head, (rest @ tail) |> List.rev)
    | _ -> None

  let private fitMany (bin: Bin) (cutouts: Cutout list) =
    let rec loop current remaining =
      match (takeCutout current.remaining remaining) with
      | Some (cutout, rest) -> loop (current |> addToBin cutout) rest
      | _ -> (current, remaining)

    loop bin cutouts

  let private mffd (binSize: int) (cutouts: Cutout list) =
    let buckets = cutouts |> partiton binSize

    let mediumBins, medium =
      buckets.big
      |> List.map (createBin binSize)
      |> List.fold
           (fun (processed, remaining) bin ->
             match (takeCutout bin.remaining remaining) with
             | Some (cutout, rest) -> Populated(bin |> addToBin cutout) :: processed, rest
             | _ -> (Unpopulated bin) :: processed, remaining)
           ([], buckets.medium)

    let smallBins, small =
      mediumBins
      |> List.fold
           (fun (processed, remaining) labeled ->
             match labeled with
             | Unpopulated bin ->
                 match remaining with
                 | first :: second :: tail when first.size + second.size < bin.remaining ->
                     let newBin = bin |> addToBin first

                     match (takeCutout newBin.remaining (second :: tail)) with
                     | Some (cutout, rest) -> (newBin |> addToBin cutout) :: processed, rest
                     | _ -> bin :: processed, remaining
                 | [ first ] when first.size <= bin.remaining ->
                     (bin |> addToBin first) :: processed, remaining
                 | _ -> bin :: processed, remaining
             | Populated bin -> bin :: processed, remaining)
           ([], buckets.small)

    let restBins, allRest =
      smallBins
      |> List.fold
           (fun (processed, remaining) bin ->
             let newBin, rest = fitMany bin remaining
             newBin :: processed, rest)
           ([], buckets.tiny @ small @ medium)


    restBins @ (ffd binSize allRest)

  let private adjust (cutAdj: int) (cutout: Cutout) =
    { cutout with
        size = cutout.size + cutAdj }

  let private postprocess (options: Options) (bin: Bin) =
    let cutouts =
      bin.cutouts |> List.map (adjust -options.cutSize)

    let used = cutouts |> List.sumBy cutoutSize

    let raw = cutouts.Length * options.cutSize

    let (cuts, rest) =
      match used + raw with
      | total when total <= options.binSize -> raw, options.binSize - total
      | _ -> options.binSize - used, 0

    { cutouts = cutouts
      used = used
      cuts = cuts
      rest = rest }

  let public optimize (options: Options) (cutouts: Cutout list) =
    cutouts
    |> List.map (adjust options.cutSize)
    |> mffd (options.binSize + options.cutSize)
    |> List.map (postprocess options)
