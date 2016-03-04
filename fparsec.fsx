(*type Parser<'u> = Parser of (list<char> -> list<'u * list<char>>)

let parse (Parser p) = p

let (>>=) p f = Parser (fun chars -> List.concat [for (a,cs) in parse p chars -> parse (f a) cs])

let (>>) p1 p2 = p1 >>= (fun _ -> p2)

let (++) p q = Parser (fun chars -> (parse p chars) @ (parse q chars))

type ParserBuilder() = 
   member p.Return a = Parser (fun chars -> [a,chars])
   member p.Bind x f = x >>= f
   member p.Zero() = Parser (fun chars -> [])
   member p.Combine (x,y) = x ++ y*)

#I "C:/Users/Asus/Documents/Visual Studio 2015/Projects/ConsoleApplication1/packages/FParsec.1.0.2/lib/net40-client"
#r "FParsec"
#r "FParsecCS"


open FParsec

type Date = {day : int; hour : int; minute : int}

type Speed = {direction : int; speed : int ; measur : string ; gusts : int option}

[<Measure>]
type meter

type Visibility = Visibility of float<meter> | Over10Km | LessThan50m

type Feet = Feet of float

type Sky = ClearSky | FewClouds | Scattered | Broken | Overcast | SkyObscuredVV | NoSignificantCloud

type Cover = (Sky * Feet) list

[<Measure>]
type bar //1 bar = 10^5 pascal

[<Measure>]
type celsius

type Report = {airport : string ; 
               date : Date ; 
               windspeed : Speed ; 
               visibility : Visibility
               cloudsInSky : Cover ; 
               temperature : float<celsius> ;
               dewpoint : float<celsius> ;
               pressure : float<bar>}

let test p string =
    match run p string with
    | Success(result, _, _)   -> printfn "Parsed: \n%A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: \n%s" errorMsg

let digits = fun n m -> manyMinMaxSatisfy n m isDigit

let (>>) p1 p2 = 
   p2 >>= fun _ -> p2

//let start : Parser<_,unit> = spaces >>. opt (pstring "METAR" <|> pstring "SPECI") .>> spaces

let icaoCode : Parser<_,unit> = (manySatisfy isUpper) .>> spaces

let date =
    let fdate (s1,s2,s3) = {day = int s1; hour = int s2; minute = int s3}
    (tuple3 (digits 2 2) (digits 2 2) (digits 2 2) .>> pstring "Z") .>> spaces |>> fdate

let speed =
  let fspeed (s1,s2,s3,s4) = 
   let gst = match s3 with
             | None -> None
             | Some(x) -> Some (int x)
   {direction = int s1 ; speed = int s2 ; gusts = gst ; measur = s4}
  let dir = (stringReturn "VRB" None <|> digits 3 3 |>> (int >> Some))
  let gst = (opt (pstring "G" >>. digits 2 2))
  let mst = (choice [pstring "KT"; pstring "MPS"; pstring "MPH"])
  tuple4 dir (digits 2 3) gst mst .>> spaces |>> fspeed

let wind : Parser<_,unit> =
  tuple2 (digits 2 3) ((pstring "V") >>. (digits 2 3)) .>> spaces |>> (fun (a,b) -> let a' = int a
                                                                                    let b' = int b
                                                                                    if a' <= b' then a',b' else b',a')

let visibility =
  pfloat .>> spaces |>> (fun v -> if v = 9999.0 then Over10Km
                                  elif v <= 50.0 then LessThan50m
                                  else Visibility (v*1.0<meter>))

let sky =
   let sR = stringReturn
   let f = (tuple2 (choice [sR "SKC" ClearSky ; 
                            sR "FEW" FewClouds ; 
                            sR "SCT" Scattered ; 
                            sR "BKN" Broken ;
                            sR "OVC" Overcast ;
                            sR "VV" SkyObscuredVV ;
                            (*pstring "CAVOK" ; *)]) (pfloat |>> (fun x -> Feet <| x*100.0))) .>> spaces
   many f

let tempdew =
   let ftemp ((sign1,s1),(sign2,s2)) = 
     let t,d = let s1' = (float s1)*1.0<celsius>
               let s2' = (float s2)*1.0<celsius>
               match sign1,sign2 with
               | None,None ->  s1',s2'
               | None,_ -> s1',-s2'
               | _,None -> -s1',s2'
               | _,_ -> -s1',-s2'
     t,d
   let h = tuple2 (opt (pstring "M")) (manySatisfy isDigit)
   tuple2 h (pstring "/" >>. h) .>> spaces |>> ftemp

let pressure =
    let altimeter = stringReturn "A" 33.864<bar>
    let barometer = stringReturn "Q" 1.0<bar>
    (tuple2 (altimeter <|> barometer) pfloat) .>> spaces |>> (fun (chr,num) -> chr * num)

let metarDesugared =
    icaoCode    >>= fun st ->
    date        >>= fun dt ->
    speed       >>= fun spd ->
    visibility  >>= fun vis ->
    sky         >>= fun listSky ->
    tempdew     >>= fun (tmp,dew) ->
    pressure    >>= fun prss ->
    preturn {airport = st; 
             date = dt; 
             windspeed = spd; 
             visibility = vis; 
             cloudsInSky = listSky;
             temperature = tmp;
             dewpoint = dew;
             pressure = prss}

let parser = parse {
    let! st = icaoCode
    let! dt = date
    let! spd = speed
    let! vis = visibility
    let! listSky = sky
    let! tmp,dew = tempdew
    let! prss = pressure
    return {airport = st ;
            date = dt ; 
            windspeed = spd ; 
            visibility = vis ; 
            cloudsInSky = listSky ; 
            temperature = tmp ;
            dewpoint = dew;
            pressure = prss} }

open System.Net
open Microsoft.FSharp.Control.WebExtensions

let fetchWeb (station:string) =
        let helper s = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations/" + s + ".TXT"
        let url = helper station
        try
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let html = webClient.DownloadString(uri)
            Some <| (html.Split [|'\n'|]).[1]
        with
            ex -> None
