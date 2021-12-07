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

let rec greatestOfList = ls => {
  ls->greatestOfListInner(0, 0)
}
and greatestOfListInner = (ls, ones, zeroes) => {
  switch ls {
  | list{first, ...rest} if first == 1 => greatestOfListInner(rest, ones + 1, zeroes)
  | list{first, ...rest} if first == 0 => greatestOfListInner(rest, ones, zeroes + 1)
  | _ => ones > zeroes ? 1 : 0
  }
}

let rec binate = ls => {
  switch ls {
  | list{1, ...rest} => list{0, ...binate(rest)}
  | list{0, ...rest} => list{1, ...binate(rest)}
  | _ => list{}
  }
}

let rec binaryToDecimal = ls => {
  switch ls {
  | list{first, ...rest} =>
    first * Js.Math.pow_int(~base=2, ~exp=Belt.List.length(rest)) + binaryToDecimal(rest)
  | list{} => 0
  }
}

let processNumbers = ls => {
  ls
  ->transpose
  ->Belt.List.reduceReverse(list{}, (acc, ls) => list{ls->greatestOfList, ...acc})
  ->(
    ls => {
      let gammaRate = ls->binaryToDecimal
      let epsilonRate = ls->binate->binaryToDecimal
      gammaRate * epsilonRate
    }
  )
}

let printAsArray = ls => ls->Belt.List.toArray->Js.Array2.map(Belt.List.toArray)->Js.log

listOfNumbers->printAsArray
listOfNumbers->processNumbers->Js.log
