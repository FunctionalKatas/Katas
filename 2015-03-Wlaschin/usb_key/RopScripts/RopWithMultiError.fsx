(*

Railway oriented programming -- with error type

If some functions give bad results, how can you connect them together?


*)


// This is a utility module
module RailwayOrientedProgramming = 

    /// A TwoTrack is a success or failure
    /// The Success case has a success value, plus a list of messages
    /// The Failure case has just a list of messages
    type TwoTrack<'TSuccess, 'TMessage> =
        | Success of 'TSuccess * 'TMessage list
        | Failure of 'TMessage list

    /// create a Success with no messages
    let succeedR x =
        Success (x,[])

    /// create a Success with a message
    let succeedWithMsg x msg =
        Success (x,[msg])

    /// create a Failure with a message
    let failR msg =
        Failure [msg]

    /// A function that applies either fSuccess or fFailure 
    /// depending on the case.
    let either fSuccess fFailure = function
        | Success (x,msgs) -> fSuccess (x,msgs) 
        | Failure errors -> fFailure errors 

    /// merge messages with a result
    let mergeMessages msgs result =
        let fSuccess (x,msgs2) = 
            Success (x, msgs @ msgs2) 
        let fFailure errs = 
            Failure (errs @ msgs) 
        either fSuccess fFailure result

    /// given a function that generates a new RopResult
    /// apply it only if the result is on the Success branch
    /// merge any existing messages with the new result
    let bindR f result =
        let fSuccess (x,msgs) = 
            f x |> mergeMessages msgs
        let fFailure errs = 
            Failure errs 
        either fSuccess fFailure result

    /// given a function wrapped in a result
    /// and a value wrapped in a result
    /// apply the function to the value only if both are Success
    let applyR f result =
        match f,result with
        | Success (f,msgs1), Success (x,msgs2) -> 
            (f x, msgs1@msgs2) |> Success 
        | Failure errs, Success (_,msgs) 
        | Success (_,msgs), Failure errs -> 
            errs @ msgs |> Failure
        | Failure errs1, Failure errs2 -> 
            errs1 @ errs2 |> Failure 

    /// infix version of apply
    let (<*>) = applyR

    /// given a function that transforms a value
    /// apply it only if the result is on the Success branch
    let liftR f result =
        let f' =  f |> succeedR
        applyR f' result 

    /// given two values wrapped in results apply a function to both
    let lift2R f result1 result2 =
        let f' = liftR f result1
        applyR f' result2 

    /// given three values wrapped in results apply a function to all
    let lift3R f result1 result2 result3 =
        let f' = lift2R f result1 result2 
        applyR f' result3

    /// given four values wrapped in results apply a function to all
    let lift4R f result1 result2 result3 result4 =
        let f' = lift3R f result1 result2 result3 
        applyR f' result4

    /// infix version of liftR
    let (<!>) = liftR

    /// synonym for liftR
    let mapR = liftR

    let tee deadEndFunction oneTrackInput = 
        deadEndFunction oneTrackInput 
        oneTrackInput 

    let teeR deadEndFunction twoTrackInput = 
        let deadEndFunction' = tee deadEndFunction 
        twoTrackInput |> mapR deadEndFunction'

    let catch exceptionThrowingFunction exceptionMapper oneTrackInput = 
        try
            exceptionThrowingFunction oneTrackInput |> succeedR
        with
        | ex ->
            failR (exceptionMapper ex)

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


let nameNotBlank name =
  if name = "" then 
     failR NameMustNotBeBlank
  else succeedR name 

let name50 (name:string) =
  if name.Length > 50 then 
     failR (NameMustNotBeLongerThan 50)
  else succeedR name

let emailNotBlank email =
  if email = "" then 
     failR EmailMustNotBeBlank
  else succeedR email


/// Validate the UserId -- this always works
let validateUserId id = 
    succeedR id

/// Validate the Name -- this might fail
let validateName name =  
    name 
    |> nameNotBlank 
    |> bindR name50

/// Validate the Email -- this might fail
let validateEmail email = 
    email 
    |> emailNotBlank

/// Create a new validated reques
let createRequest userId name email = 
    {userId=userId; name= name ; email=email }

let validateRequest req = 
  createRequest 
  <!> (validateUserId req.userId)
  <*> (validateName req.name)
  <*> (validateEmail req.email)


// test validation
let goodRequest = {userId=0; name= "Alice"; email="ABC@gmail.COM"}
goodRequest |> validateRequest

let badRequest1 = {userId=0; name= ""; email=""}
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
    | Success (req:Request,events) -> 
        printfn "LOG INFO Name=%s EMail=%s" req.name req.email
        events |> List.iter (printfn "LOG INFO %A")
    | Failure errors -> 
        errors |> List.iter (printfn "LOG ERROR %A")
    twoTrackInput 

// test loggerR 
goodRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR 
unsendableRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR 
badRequest1 |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR 

// ------------------------
// return the response
// ------------------------


let returnMessageR result = 
  match result with
  | Success (obj,events) -> Some obj
  | Failure msgs -> None

// test returnMessageR
goodRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR |> returnMessageR
unsendableRequest |> validateRequest |> canonicalizeEmailR |> updateDbR |> sendEmailR |> loggerR |> returnMessageR

// ------------------------
// final code
// ------------------------

let updateCustomerR = 
  validateRequest 
  >> canonicalizeEmailR
  >> updateDbR
  >> sendEmailR
  >> loggerR
  >> returnMessageR


// test final code
goodRequest |> updateCustomerR 
badRequest1 |> updateCustomerR 
unsendableRequest |> updateCustomerR 


