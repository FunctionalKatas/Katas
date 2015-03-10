(*

Railway oriented programming -- with error type

If some functions give bad results, how can you connect them together?


*)


// This is a utility module
module RailwayOrientedProgramming = 

    /// A TwoTrack is a success or failure
    /// The Success case has a success value
    /// The Failure case has just a message
    type TwoTrack<'TSuccess, 'TMessage> =
        | Success of 'TSuccess 
        | Failure of 'TMessage 

    /// create a Success 
    let succeedR x =
        Success x

    /// create a Failure with a message
    let failR msg =
        Failure msg

    /// given a function that generates a new RopResult
    /// apply it only if the result is on the Success branch
    let bindR f twoTrackInput =
        match twoTrackInput with
        | Success x -> f x
        | Failure error -> Failure error

    let mapR f twoTrackInput = 
       match twoTrackInput with
         | Success s -> Success (f s)
         | Failure error -> Failure error

    let tee deadEndFunction oneTrackInput = 
        deadEndFunction oneTrackInput 
        oneTrackInput 

    let teeR deadEndFunction twoTrackInput = 
        let deadEndFunction' = tee deadEndFunction 
        twoTrackInput |> mapR deadEndFunction'

    let catch exceptionThrowingFunction exceptionMapper oneTrackInput = 
        try
            exceptionThrowingFunction oneTrackInput |> Success
        with
        | ex ->
            Failure (exceptionMapper ex)

    let catchR exceptionThrowingFunction exceptionMapper  twoTrackInput = 
        let catch' = catch exceptionThrowingFunction exceptionMapper  
        twoTrackInput |> bindR catch' 

// let's use this

open RailwayOrientedProgramming

type Request = {
    userId: int 
    name: string
    email: string }

type ErrorMessage = 
  | NameMustNotBeBlank
  | NameMustNotBeLongerThan of int
  | EmailMustNotBeBlank
  | SmtpServerError of string


let nameNotBlank input =
  if input.name = "" then 
     failR  NameMustNotBeBlank
  else succeedR input

let name50 input =
  if input.name.Length > 50 then 
     failR (NameMustNotBeLongerThan 50)
  else succeedR input

let emailNotBlank input =
  if input.email = "" then 
     failR EmailMustNotBeBlank
  else succeedR input

let nameNotBlank' = bindR nameNotBlank 
let name50' = bindR name50 
let emailNotBlank' = bindR emailNotBlank

let validateRequest = 
  nameNotBlank 
  >> name50' 
  >> emailNotBlank' 

// test validation
let goodRequest = {userId=0; name= "Alice"; email="ABC@gmail.COM"}
goodRequest |> validateRequest

let badRequest1 = {userId=0; name= ""; email="abc@example.com"}
badRequest1 |> validateRequest

let unsendableRequest = {userId=0; name= "Alice"; email="ABC@example.COM"}
unsendableRequest |> validateRequest


// ------------------------
// Add another step
// ------------------------

// trim spaces and lowercase
let canonicalizeEmail input =
   { input with email = input.email.Trim().ToLower() }

let canonicalizeEmailR twoTrackInput = 
    let canonicalizeEmail' = mapR canonicalizeEmail 
    twoTrackInput |> canonicalizeEmail' 


// test canonicalizeEmail
goodRequest |> validateRequest |> canonicalizeEmailR 


// ------------------------
// Update the database
// ------------------------

let updateDb (request:Request) =
    // do something
    // return nothing at all
    printfn "Database updated with userId=%i email=%s" request.userId request.email
    ()

let updateDbR twoTrackInput = teeR updateDb twoTrackInput 

// test updateDb
goodRequest |> validateRequest |> canonicalizeEmailR |> updateDbR 


// ------------------------
// Send an email
// ------------------------

let sendEmail (request:Request) =
    if request.email.EndsWith("example.com") then
        failwithf "Can't send email to %s" request.email
    else
        printfn "Sending email=%s" request.email
        request // return request for processing by next step

// convert SMTP exceptions to our list
let smtpExceptionMapper (ex:exn) =
    SmtpServerError ex.Message

let sendEmailR twoTrackInput = catchR sendEmail smtpExceptionMapper twoTrackInput 


// test sendEmail 
goodRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR 
unsendableRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR 

// ------------------------
// log the errors
// ------------------------

let loggerR twoTrackInput = 
    match twoTrackInput with
    | Success (req:Request) -> 
        printfn "LOG INFO Name=%s EMail=%s" req.name req.email
    | Failure err -> 
        printfn "LOG ERROR %A" err
    twoTrackInput 

// test loggerR 
goodRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR 
unsendableRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR 
badRequest1 |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR 

// ------------------------
// message converter examples
// ------------------------

// obviously the real ones would use resource files!

let translateError_EN err = 
  match err with
  | NameMustNotBeBlank -> "Name must not be blank" 
  | NameMustNotBeLongerThan i -> sprintf "Name must not be longer than %i chars" i
  | EmailMustNotBeBlank -> "Email must not be blank" 
  | SmtpServerError msg -> sprintf "SmtpServerError [%s]" msg

let translateError_FR err = 
  match err with
  | NameMustNotBeBlank -> "Nom ne doit pas être vide" 
  | NameMustNotBeLongerThan i -> sprintf "Nom ne doit pas être plus long que %i caractères" i
  | EmailMustNotBeBlank -> "Email doit pas être vide" 
  | SmtpServerError msg -> sprintf "SmtpServerError [%s]" msg

// ------------------------
// return the response
// ------------------------


let returnMessageR translator result = 
  match result with
  | Success obj -> Some obj
  | Failure msg -> 
    let errStr = translator msg 
    printfn "Error=%s" errStr 
    None

// test returnMessageR
goodRequest 
|> validateRequest 
|> canonicalizeEmailR 
|> updateDbR 
|> sendEmailR 
|> loggerR 
|> returnMessageR translateError_EN


// ------------------------
// final code
// ------------------------

let updateCustomerR request = 
  request 
  |> validateRequest 
  |> canonicalizeEmailR
  |> updateDbR
  |> sendEmailR
  |> loggerR
  |> returnMessageR translateError_EN

// test final code
goodRequest |> updateCustomerR 
badRequest1 |> updateCustomerR 
unsendableRequest |> updateCustomerR 


