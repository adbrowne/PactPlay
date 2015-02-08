namespace PactTests

open Xunit
open FsCheck.Xunit
open FSharp.Data
open Swensen.Unquote

type Person = {
    Id : int
    Name : string
}
    with static member Create id name = { Person.Id = id; Name = name }

type Response = {
    StatusCode : int
    Body : JsonValue
}

type PactRequest<'a> = {
    Path : string
    Method : string
    RequestBody : string option
    ResponseSpec : 'a -> string
}

type PactSerializer<'a> = {
    ToJson : 'a -> JsonValue
    FromJson : JsonValue -> 'a
}

type FieldMap<'a, 'b> = {
    Key : string
    Getter : 'a -> JsonValue
    Setter: JsonValue -> 'b
}

type Iso<'a,'b> = Iso of ('a -> 'b option) * ('b -> 'a option)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Iso =
    let nil = 
        Iso 
         ((fun () -> Some []), 
          (function | [] -> Some () | x::xs -> None))

    let cons =
        Iso 
         ((fun (x,xs) -> Some(x::xs)), 
          (function | [] -> None | x :: xs -> Some(x,xs)))

    let inverse (Iso (f, g)) = Iso (g,f)

    let apply (Iso (f,_)) = f

    let unapply iso = apply (inverse iso)

    let rec mapMaybe (f : 'a -> 'b option) (l : 'a list) : 'b list =
        match l with
        | [] -> []
        | x :: xs -> 
            match f x with
            | Some b -> b :: (mapMaybe f xs)
            | None -> mapMaybe f xs

    let listToOpt = function
    | [] -> None
    | x :: _ -> Some x

    let stringIso : Iso<string,JsonValue> =
        let ap s = Some <| JsonValue.String s
        let unap = function
            | JsonValue.String s -> Some s
            | _ -> None
        Iso (ap, unap)

    let decimalIso : Iso<decimal,JsonValue> =
        let ap i = Some <| JsonValue.Number i
        let unap = function
            | JsonValue.Number n -> Some n
            | _ -> None
        Iso (ap, unap)

    let intIso : Iso<int,JsonValue> =
        let ap i = Some <| JsonValue.Number (decimal i)
        let unap = function
            | JsonValue.Number n -> Some (int n)
            | _ -> None
        Iso (ap, unap)

    let withKey (key : string) (iso : Iso<'x,JsonValue>) : Iso<'x,(string * JsonValue)> =
        let ap = apply iso >> (Option.map (fun x -> (key,x)))
        let unap (k,v) =
            if (k = key) then
                unapply iso v
            else
                None
        Iso (ap, unap)

    let tuple2
        (xIso : Iso<'x,(string * JsonValue)>) 
        (yIso : Iso<'y,(string * JsonValue)>) 
        : Iso<('x * 'y), JsonValue> =
        let toJson a : JsonValue option =
            let (x,y) = a
            let xJson = apply xIso x
            let yJson = apply yIso y
            seq {
                match xJson with
                | Some v -> yield v
                | _ -> ()

                match yJson with
                | Some v -> yield v
                | _ -> ()
            }
            |> Array.ofSeq
            |> JsonValue.Record
            |> Some

        let fromJson (jsonValue : JsonValue) : ('x * 'y) option = 
            match jsonValue with
            | JsonValue.Record fields ->
                let fieldDict = fields |> List.ofArray
                let xValue = mapMaybe (unapply xIso) fieldDict |> listToOpt
                let yValue = mapMaybe (unapply yIso) fieldDict |> listToOpt
                match (xValue, yValue) with
                | (Some x, Some y) -> Some <| (x,y)
                | _ -> None
            | _ -> None
        Iso (toJson, fromJson)
        
    let tuple2Iso 
        (toObject : 'x -> 'y -> 'a)
        (fromObject : 'a -> ('x * 'y)) =
        Iso (fromObject >> Some, (fun (x,y) -> Some <| toObject x y))

    let compose (Iso (ax : 'a -> 'b option, ux : 'b -> 'a option))  (Iso (ay : 'b -> 'c option, uy : 'c -> 'b option)) : Iso<'a,'c> =
        let apl (a : 'a) : 'c option = 
            ax a |> Option.bind ay
        let uapl (c : 'c) : 'a option = 
            uy c |> Option.bind ux
        Iso (apl, uapl)

    let buildIso2 
        (toObject : 'x -> 'y -> 'a) 
        (fromObject : 'a -> ('x * 'y)) 
        (xIso : Iso<'x,(string * JsonValue)>) 
        (yIso : Iso<'y,(string * JsonValue)>) 
        : Iso<'a, JsonValue> =

        let t2 = tuple2 xIso yIso
        let o2 : Iso<'a,('x * 'y)> = tuple2Iso toObject fromObject
        compose o2  t2 

module PactSerializer = 
    let intGetter n =
        JsonValue.Number (decimal n)

    let intSetter = function 
        | JsonValue.Number s -> int s 
        | _ -> failwith "number expected"

    let intField key getter = 
        { Key = key; Getter = getter >> intGetter; Setter = intSetter }

    let stringGetter s = JsonValue.String s

    let stringSetter = function
        | JsonValue.String s -> s
        | _ -> failwith "string expected"

    let stringField key getter = 
        { Key = key; Getter = getter >> stringGetter; Setter = stringSetter }

    let personIso : Iso<Person, JsonValue> =
        let id = Iso.withKey "id" Iso.intIso
        let name = Iso.withKey "name" Iso.stringIso
        Iso.buildIso2 Person.Create (fun (p:Person) -> (p.Id, p.Name)) id name

    let map2 toObject (fieldMap1 : FieldMap<'a,'b>) (fieldMap2 : FieldMap<'a,'c>) =
        let toJson a = 
            let k1 = fieldMap1.Key
            let v1 = fieldMap1.Getter a
            let k2 = fieldMap2.Key
            let v2 = fieldMap2.Getter a
            JsonValue.Record [|(k1,v1); (k2,v2)|]
        let fromJson jValue = 
            match jValue with 
            | JsonValue.Record properties ->
                let properties = properties |> Map.ofArray
                let f1 = fieldMap1.Setter <| properties.Item fieldMap1.Key
                let f2 = fieldMap2.Setter <| properties.Item fieldMap2.Key
                toObject f1 f2
            | _ -> failwith "expecting a record"
            
        {
            ToJson = toJson
            FromJson = fromJson
        }

module ApiClient =
    let getPerson () = 
        let responseSpec (person : Person) =
            sprintf 
                "{\n\  
                     \"id\": %d,
                     \"name\": %s
                 }"
                 person.Id
                 person.Name

        { PactRequest.Path = "/person"
          Method = "get"
          RequestBody = None
          ResponseSpec = responseSpec }

module PactTest = 

    let pact 
        (name : string) 
        (provider_state: string)
        (request : PactRequest<'a>) 
        (code : int) 
        (result : 'a) =
           sprintf
            "{
              \"description\": \"%s\",
              \"provider_state\": \"%s\",
              \"request\": {
                \"method\": \"get\",
                \"path\": \"/alligators/Mary\",
                \"headers\": {
                  \"Accept\": \"application/json\"
                }
              },
              \"response\": {
                \"status\": 500,
                \"headers\": {
                  \"Content-Type\": \"application/json;charset=utf-8\"
                },
                \"body\": {
                  \"error\": \"Argh!!!\"
                }
              }
             }"
            name
            provider_state

    let andrew = { Id = 1; Name = "Andrew Browne" }

    open PactSerializer

    let personSerializer = 
        map2 
            Person.Create 
            (intField "id" (fun p -> p.Id))
            (stringField "name" (fun p -> p.Name))

    let reverseJson json =
        let out = Iso.unapply personIso json
        out

    [<Property>]
    let ``Can round trip person Iso`` person = 
        let result = 
            Iso.apply personIso person
            |> Option.bind reverseJson

        result =? Some person

    [<Property>]
    let ``Can round trip person`` person =
        let result = 
            personSerializer.ToJson person
            |> personSerializer.FromJson

        person =? result

    let personResponseIso : Iso<Person option, Response> = 
        let personToResponse = function
            | Some p -> 
                Iso.apply personIso p 
                |> Option.map (fun p -> { Response.StatusCode = 200; Body = p })
            | None -> Some { Response.StatusCode = 500; Body = JsonValue.Null }
        let responseToPerson = function
            | r when r.StatusCode = 500 ->
                Some None
            | r -> 
                Some <| Iso.unapply personIso r.Body

        Iso (personToResponse, responseToPerson)

    [<Fact>]
    let ``Can map person to Response`` () =
        let response = Iso.apply personResponseIso (Some andrew)

        response |> Option.map (fun r -> r.StatusCode) =? Some 200

    [<Fact>]
    let ``Can write basic pact test`` () =  
        let request = ApiClient.getPerson()

        let expected = 
            FSharp.Data.JsonValue.Parse
                "{
                  \"description\": \"a request for a person\",
                  \"provider_state\": \"Andrew is the man\",
                  \"request\": {
                    \"method\": \"get\",
                    \"path\": \"/alligators/Mary\",
                    \"headers\": {
                      \"Accept\": \"application/json\"
                    }
                  },
                  \"response\": {
                    \"status\": 500,
                    \"headers\": {
                      \"Content-Type\": \"application/json;charset=utf-8\"
                    },
                    \"body\": {
                      \"error\": \"Argh!!!\"
                    }
                  }
                 }"
        let p = 
            FSharp.Data.JsonValue.Parse
                (pact 
                    "a request for a person"
                    "Andrew is the man"
                    request 
                    200 
                    andrew)

        Assert.Equal(expected, p)