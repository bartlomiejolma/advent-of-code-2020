let textInput = "939
7,13,x,x,59,x,31,19"

let parseInput2 (texInput: string) =
    let _ :: bussesText :: _ = Seq.toList (texInput.Split '\n')
    bussesText.Split ','
    |> Seq.map System.Int32.TryParse
    |> Seq.indexed
    |> Seq.filter (fun (index, (isnumber, number)) -> isnumber)
    |> Seq.map (fun (index, (isnumber, number)) -> (double index, double (number)))
    |> Seq.toList

let textInput2 = "939
7,13,19"

let busses = parseInput2 textInput


let (rems, nums) = 
    busses
    |> List.map (fun (offset, bus) -> (bus - offset, bus))
    |> List.unzip

let modInverse (a:float) (m:float) =  
    let b  = a % m 
    [1.0..m]
    |> List.find (fun x ->
        ((a * x) % m = 1.0))

modInverse 3.0 11.0

let chinese (nums:double list) (rems:double list)  = 
    let prod = List.reduce (*) nums

    let pps = List.map (fun num -> prod / num) nums

    
    let invs = 
        nums
        |> List.map2 modInverse pps

    let total = 
        List.zip3 rems pps invs
        |> List.sumBy (fun (rem, pp, inv) -> rem*pp*inv)
    total % prod

chinese [3.0; 4.0; 5.0] [2.0; 3.0; 1.0]

rems
nums
chinese nums rems

chinese [7.0; 13.0; 59.0; 31.0; 19.0] [7.0; 1.0; 4.0; 6.0; 7.0]

chinese [67.0;7.0;59.0;61.0] [0.0; 1.0; 2.0; 3.0]


let solve textInput = 
    let busses = parseInput2 textInput
    let (rems, nums) = 
        busses
        |> List.map (fun (offset, bus) -> (bus - (offset % bus), bus))
        |> List.unzip
    (rems, nums)
    // chinese nums rems

solve textInput

solve "
17,x,13,19"

solve "
67,7,59,61"

solve "
67,x,7,59,61"

solve "
67,7,x,59,61"

solve "
1789,37,47,1889"
open System.IO

let scheduleText = File.ReadAllText("input.txt")

printfn "%f" (solve scheduleText)

printfn "%A" (solve scheduleText)

parseInput2 scheduleText


894954360381363.0 > 100000000000000.0 
894954360381404.0

47 % 13

23 % 37

(47 + 13) % 13