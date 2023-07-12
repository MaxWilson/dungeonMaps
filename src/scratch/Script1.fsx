(* First attempt at generating a dungeon, purely in F# form (not visual or even text)
   The emphasis is on getting the domain model to have the right pieces for e.g. mana level,
   different kinds of monsters, etc. It's just a throwaway proof of concept.
*)
[<Measure>] type dollar
[<Measure>] type lb
type PluralizableName = string * string option
type RoomId = string
type Monster = PluralizableName
type Treasure = { value: int<dollar>; weight: float<lb>; description: string }
type Door = { destinationRoom: RoomId; closed: bool; locked: bool; trapped: bool }
type Drilldown =
  | Skillgate of int * string * Drilldown
  | RedHerring
  | RevealMonster of Monster
  | RevealTreasure of Treasure
  | Reveals of Drilldown list
  | Drilldown of string * Drilldown
type Affordance =
  | ObviousMonster of Monster
  | Exit of Door
  | Affordance of Drilldown
type Room = {
  id: RoomId
  description: string
  affordances: Affordance list
  }
type Dungeon =
  { rooms: Map<RoomId, Room> }

type FrequencyTest = unit -> bool
let thunk x _ = x
let thunk1 f x _ = f x
let r = System.Random()
let d n size = List.init n (fun _ -> 1 + r.Next size) |> List.sum
let freq: int -> FrequencyTest =
  fun percent () -> r.Next 100 < (percent: int)
let testfreq freqN = freq freqN ()
List.init 100 (thunk1 freq 60) |> List.sumBy (fun x -> if (x()) then 1 else 0)
let maybeMimic (ctx: Monster list) (frequency: FrequencyTest) appearance otherwise =
  if ctx.IsEmpty && frequency() then
    Drilldown(appearance, RevealMonster ("mimic", None)) // mimics don't sneak into occupied lairs!
  else
    Drilldown(appearance, otherwise())
let dagger ctx = maybeMimic ctx (freq 50) "Pile of treasure" (fun _ -> RevealTreasure { value = 20<dollar>; weight=0.25<lb>; description = "A steel dagger"})
let chair ctx = maybeMimic ctx (freq 50) "Wooden chair" (fun _ -> RevealTreasure { value = 4<dollar>; weight=10.<lb>; description = "An old wooden chair"})
let pileOfRocks ctx = maybeMimic ctx (freq 50) "Pile of rocks" (fun _ -> RedHerring)
let trollHole _ =
  let trolls = if testfreq 20 then List.init (d 2 6) (thunk (Monster ("troll", None))) else []
  { id = $"Troll Hole {System.Guid.NewGuid().ToString()}"
    description = if trolls.IsEmpty then "Empty troll hole" else $"Occupied troll hole ({trolls.Length})"
    affordances = [
      Exit { destinationRoom = "Outside"; closed = false; locked = false; trapped = false }
      for t in trolls do
        ObviousMonster t
      for rm in [dagger; chair; pileOfRocks] do
        if testfreq 60 then Affordance(rm trolls)
      ]
    }
let describeMonster (m: Monster) n =
  let (name, pluralName) = m
  if n = 1 then
    $"A {name}"
  else
    $"{n} {pluralName}"
let describe (rm: Room) =
  let monsters = rm.affordances
                  |> List.choose (function ObviousMonster m -> Some m | _ -> None) |> List.groupBy id
                  |> List.map (fun (name, lst) -> (name : PluralizableName), lst.Length) |> List.sortBy fst
  printfn $"{rm.description}"
  for m, n in monsters do
    printfn $"{describeMonster m n}"
  for item in rm.affordances do
    match item with
    | Exit e ->
      let flags = [
        if e.closed then "closed"
        if e.locked then "locked"
        if e.trapped then "trapped"
        ]
      let flags = if flags.IsEmpty then "" else $"""({flags |> String.concat ", "})"""
      printfn $"Exit to {e.destinationRoom} {flags}"
    | ObviousMonster _ -> ()
    | Affordance a ->
      let rec recur prefix a =
        match a with
        | Drilldown(txt, rest) ->
          recur (prefix + $"{txt} ▶ ") rest
        | RedHerring ->
          printfn $"{prefix}Nothing of interest"
        | RevealMonster m ->
          printfn $"{prefix}{describeMonster m 1}!"
        | RevealTreasure t ->
          printfn $"{prefix}{t.description} ({t.weight} lb., ${t.value})"
        | Reveals rests ->
          rests |> List.iter (recur prefix)
        | Skillgate(gate, skill, rest) ->
          recur (prefix + $"{skill} {gate} ▶ ") rest
      recur "" a
trollHole() |> describe
