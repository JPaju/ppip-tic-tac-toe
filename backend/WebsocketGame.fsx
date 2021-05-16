#r "nuget: FSharp.Control.Reactive"

open FSharp.Control.Reactive

let test = Subject<int>.broadcast

test
|> Observable.subscribe (fun (value: int) -> printfn "%i" value)

test
|> Subject.onNext 34
|> Subject.onNext 37
|> Subject.onNext 37
|> Subject.onNext 37
|> Subject.onNext 37
|> Subject.onNext 69
|> Subject.onNext 420
