let testFile = Node.Path.resolve("src", "test-input.txt")
let realFile = Node.Path.resolve("src", "input.txt")

let getNumbersFromInput = filename => {
  Node.Fs.readFileAsUtf8Sync(filename)
  ->Js.String2.splitByRe(%re("/\\r?\\n/"))
  ->Js.Array2.map(Js.Option.getWithDefault(""))
  ->Js.Array2.map(str =>
    Js.String2.split(str, "")
    ->Js.Array2.map(Belt.Int.fromString)
    ->Js.Array2.map(Js.Option.getWithDefault(-1))
    ->Js.Array2.filter(num => Js.Int.equal(-1, num) == false)
    ->Belt.List.fromArray
  )
  ->Belt.List.fromArray
}

let listOfNumbers = getNumbersFromInput(realFile)

let rec transpose = ls => {
  switch ls {
  | list{}
  | list{list{}, ..._} => list{}
  | ls => list{List.map(List.hd, ls), ...transpose(List.map(List.tl, ls))}
  }
}

let rec getCounts = ls => {
  ls->getCountsInner(0, 0)
}
and getCountsInner = (ls, ones, zeroes) => {
  switch ls {
  | list{first, ...rest} if first == 1 => getCountsInner(rest, ones + 1, zeroes)
  | list{first, ...rest} if first == 0 => getCountsInner(rest, ones, zeroes + 1)
  | _ => (ones, zeroes)
  }
}

let binaryOpposite = num => num == 0 ? 1 : 0

let binate = ls => ls->Belt.List.map(binaryOpposite)

let rec binaryToDecimal = ls => {
  switch ls {
  | list{first, ...rest} =>
    first * Js.Math.pow_int(~base=2, ~exp=Belt.List.length(rest)) + binaryToDecimal(rest)
  | list{} => 0
  }
}

let gammaRate = ls => ls->binaryToDecimal
let epsilonRate = ls => ls->binate->binaryToDecimal

let printAsArray = ls => ls->Belt.List.toArray->Js.Array2.map(Belt.List.toArray)->Js.log

let tap = (ls, tag) => {
  Js.log2(tag, ls)
  ls
}

let preferOnes = ls => {
  let (ones, zeroes) = getCounts(ls)
  ones >= zeroes ? 1 : 0
}

let preferZeroes = ls => {
  let (ones, zeroes) = getCounts(ls)
  zeroes <= ones ? 0 : 1
}

let rec processNumbers = ls => {
  let oxygenGeneratorRating = ls->processNumbersInner(0, preferOnes)
  let scrubberRating = ls->processNumbersInner(0, preferZeroes)
  oxygenGeneratorRating->Js.log2("oxygen")
  scrubberRating->Js.log2("scrubber")
  oxygenGeneratorRating * scrubberRating
}
and processNumbersInner = (ls, pos, reducer) => {
  switch ls->Belt.List.length {
  | 1
  | 0 => {
      let first = ls->Belt.List.head->Belt.Option.getWithDefault(list{})
      first->Belt.List.toArray->Js.log
      first->binaryToDecimal
    }
  | _ => {
      let reducedVal =
        ls
        ->Belt.List.map(ls => ls->Belt.List.get(pos))
        ->Belt.List.keep(Belt.Option.isSome)
        ->Belt.List.map(Belt.Option.getExn)
        ->reducer
      ls
      ->Belt.List.keep(ls => ls->Belt.List.get(pos)->Js.Option.getWithDefault(-1, _) == reducedVal)
      ->processNumbersInner(pos + 1, reducer)
    }
  }
}

listOfNumbers->printAsArray
listOfNumbers->processNumbers->Js.log
