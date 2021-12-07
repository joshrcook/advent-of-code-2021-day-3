let filename = Node.Path.resolve("src", "test-input.txt")
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

let listOfNumbers = getNumbersFromInput(filename)
