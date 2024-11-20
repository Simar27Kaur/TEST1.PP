// For more information see https://aka.ms/fsharp-console-apps


//TEST 1 Mapping, Filtering through Lists

// PART A
let x = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let isHighIncome salary = salary > 100000
let y = x |> List.filter isHighIncome

printfn "%A" y


// PART 2
let m = [75000.0; 48000.0; 120000.0; 190000.0; 300113.0; 92000.0; 36000.0]

let calculateTax salary =
    if salary <= 49020.0 then
        salary * 0.15
    elif salary <= 98040.0 then
        (49020.0 * 0.15) + ((salary - 49020.0) * 0.205)
    elif salary <= 151978.0 then
        (49020.0 * 0.15) + ((98040.0 - 49020.0) * 0.205) + ((salary - 98040.0) * 0.26)
    elif salary <= 216511.0 then
        (49020.0 * 0.15) + ((98040.0 - 49020.0) * 0.205) + ((151978.0 - 98040.0) * 0.26) + ((salary - 151978.0) * 0.29)
    else
        (49020.0 * 0.15) + ((98040.0 - 49020.0) * 0.205) + ((151978.0 - 98040.0) * 0.26) + ((216511.0 - 151978.0) * 0.29) + ((salary - 216511.0) * 0.33)

let n = m |> List.map calculateTax
printfn "%A" n


//PART 3
let p = [75000.0; 48000.0; 120000.0; 190000.0; 300113.0; 92000.0; 36000.0]

let q = p |> List.map (fun salary -> 
     if salary < 49020.0 then salary + 20000.0
     else salary)
   
printfn "%A" q


//PART 4
let a = [75000.0; 48000.0; 120000.0; 190000.0; 300113.0; 92000.0; 36000.0] 

let filteredSalaries = a |> List.filter (fun salary -> salary >= 50000.0 && salary <= 100000.0)
let b = filteredSalaries |> List.fold (fun acc salary -> acc + salary) 0.0

printfn "%A" b





// TEST 1 Tail Recursion


let rec sumMultiplesOf3 current acc target =
    if current > target then
        acc
    else
        sumMultiplesOf3 (current + 3) (acc + current) target

let res = sumMultiplesOf3 3 0 27

printfn "%A" res

