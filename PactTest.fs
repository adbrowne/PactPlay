namespace PactTests

open Xunit
open FsCheck.Xunit
open FSharp.Data
open Swensen.Unquote
open Fleece
open Fleece.Operators

type PactRequest = {
    Method : string
    Path : string
    Headers : Map<string, string>
    Body : JsonValue option }
with 
 static member ToJSON (x: PactRequest) =
    seq {
        yield "method" .= x.Method
        yield "path" .= x.Path
        yield "headers" .= x.Headers
    
        match x.Body with
        | Some b -> yield ("body", b)
        | None -> ()
    } |> jobj

type PactResponse = {
    Status : int
    Headers : Map<string, string>
    Body : JsonValue option }
with 
 static member ToJSON (x: PactResponse) =
    seq {
        yield "status" .= x.Status
        yield "headers" .= x.Headers
    
        match x.Body with
        | Some b -> yield ("body", b)
        | None -> ()
    } |> jobj

type PactInteraction = {
    Description : string
    ProviderState : string
    Request : PactRequest
    Response : PactResponse }
with 
 static member ToJSON (x: PactInteraction) =
    seq {
        yield "description" .= x.Description
        yield "provider_state" .= x.ProviderState
        yield "request" .= x.Request
        yield "response" .= x.Response
    } |> jobj

type PactSpec = {
    Provider : string
    Consumer : string
    Interactions : PactInteraction list
}
with 
 static member ToJSON (x: PactSpec) =
    seq {
        yield "provider" .= x.Provider
        yield "consumer" .= x.Consumer
        yield "Interactions" .= x.Interactions
    } |> jobj

type Person = {
    Id : int
    Name : string
}
    with static member Create id name = { Person.Id = id; Name = name }

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

module ApiClient =
    let personIso : Iso<Person, JsonValue> =
        let id = Iso.withKey "id" Iso.intIso
        let name = Iso.withKey "name" Iso.stringIso
        Iso.buildIso2 Person.Create (fun (p:Person) -> (p.Id, p.Name)) id name

    let personResponseIso : Iso<Person option, PactResponse> = 
        let personToResponse = function
            | Some p -> 
                Iso.apply personIso p 
                |> Option.map (fun p -> { PactResponse.Status = 200; Body = Some p; Headers = [("Content-Type", "application/json")] |> Map.ofSeq  })
            | None -> Some { PactResponse.Status = 500; Body = None; Headers = Map.empty }
        let responseToPerson = function
            | r when r.Status = 500 ->
                Some None
            | r -> 
                r.Body
                |> Option.bind (fun b -> Some <| Iso.unapply personIso b)
        Iso (personToResponse, responseToPerson)

    let getPerson () = 
        let request = 
            { PactRequest.Path = "/person"
              Method = "GET"
              Body = None
              Headers = [("Accept", "application/json")] |> Map.ofSeq }
        (request, personResponseIso)

module PactTest = 
    let interaction 
        (name : string) 
        (providerState: string)
        (request : PactRequest) 
        (iso : Iso<'a, PactResponse>)
        (result : 'a) =
            match result |> Iso.apply iso with
            | Some response -> 
               { PactInteraction.Description = name
                 ProviderState = providerState
                 Request = request 
                 Response = response }
            | None ->
                failwith "Could not map result to pact response"

    let andrew = { Id = 1; Name = "Andrew Browne" }

    open ApiClient

    let reverseJson json =
        let out = Iso.unapply personIso json
        out

    [<Property>]
    let ``Can round trip person Iso`` person = 
        let result = 
            Iso.apply personIso person
            |> Option.bind reverseJson

        result =? Some person

    [<Fact>]
    let ``Can map person to Response`` () =
        let response = Iso.apply personResponseIso (Some andrew)

        response |> Option.map (fun r -> r.Status) =? Some 200

    [<Fact>]
    let ``Can write basic pact test`` () =  
        let (request, iso) = ApiClient.getPerson()

        let expected = 
            { PactInteraction.Description = "a request for a person" 
              ProviderState = "Andrew is the man"
              Request = 
                { Path = "/person"
                  Method = "GET"
                  Body = None
                  Headers = [("Accept", "application/json")] |> Map.ofSeq }
              Response = 
                { Status = 200
                  Headers = Map.empty
                  Body = Some <| JsonValue.Record [|("id", JsonValue.Number (decimal andrew.Id)); ("name", JsonValue.String andrew.Name)|]
                }}

        let p = 
            (interaction 
                "a request for a person"
                "Andrew is the man"
                request 
                iso
                (Some andrew))

        expected =? p

    open PactNet
    open PactNet.Mocks.MockHttpService.Models
    open HttpClient

    let port = 1234
    let buildPact (pactSpec : PactSpec) = 
        let pact = 
            (new PactBuilder()).ServiceConsumer(pactSpec.Consumer).HasPactWith(pactSpec.Provider)

        let mockProviderService = pact.MockService(1234)

        for interaction in pactSpec.Interactions do
            let request = 
                new ProviderServiceRequest(
                    Method = HttpVerb.Get, 
                    Path = interaction.Request.Path)
            let response = 
                new ProviderServiceResponse(
                    Status = 200,
                    Body = new obj(),
                    Headers = new System.Collections.Generic.Dictionary<string,string>())
            for responseHeader in interaction.Response.Headers do
                response.Headers.Add(responseHeader.Key, responseHeader.Value)
                
            mockProviderService.Given(interaction.ProviderState).UponReceiving(interaction.Description)
             .With(request)
             .WillRespondWith(response)
            |> ignore
        mockProviderService
        
    [<Fact>]
    let ``blah`` () =
        let (request, iso) = ApiClient.getPerson()
 
        let personRequest = 
            (interaction 
                "a request for a person"
                "Andrew is the man"
                request 
                iso
                (Some andrew))
        let pactSpec = 
            { PactSpec.Consumer = "A consumer"
              Provider = "A provider"
              Interactions = [personRequest] }
        let mockProviderService = buildPact pactSpec
        let basePath = sprintf "http://localhost:%d" port
        
        let realRequest =
            createRequest Get (basePath + request.Path)
            // |> withBody (request.Body.ToString())
        
        let response = realRequest |> getResponse

        mockProviderService.VerifyInteractions()
        ()