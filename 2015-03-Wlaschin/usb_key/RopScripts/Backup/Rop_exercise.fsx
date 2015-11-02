(*

This file contains a simple ROP exercise to work through.

The requirements are that you process payments in a PayPal like way.

Be sure to RESET the interactive session before starting

*)


// This is the ROP utility module -- 
// Compile all this first -- start here and highlight down to "open RailwayOrientedProgramming" below
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


open RailwayOrientedProgramming
// end utility bit

// ===========================
// Start with the domain
// ===========================

type CustomerName = CustomerName of string
type EmailAddress = EmailAddress of string
type PaymentAmount = PaymentAmount of float
type CurrencyCode = USD | GBP | EUR

/// This type has strict validation
type PaymentRequest = {
    name: CustomerName
    email: EmailAddress
    amount: PaymentAmount
    currencyCode: CurrencyCode
    }

// ===========================
// Add validation logic
// ===========================

// feel free to add to this
type ErrorMessage = 
  | NameMustNotBeBlank
  | NameMustNotBeLongerThan of int

let createCustomerName name  :TwoTrack<CustomerName,ErrorMessage> = 
    if System.String.IsNullOrEmpty(name) then 
        failR NameMustNotBeBlank
    else if name.Length > 5 then 
        failR (NameMustNotBeLongerThan 5)
    else 
        succeedR (CustomerName name) 

let createEmailAddress (s:string) :TwoTrack<EmailAddress,ErrorMessage> = = 
    if s.EndsWith("example.com")
        then failR EmailIsNotRegistered
        else succeedR (EmailAddress s)

let createPaymentAmount (amount:float) :TwoTrack<PaymentAmount,ErrorMessage> = 
    // checks that amount is positive
    failwith "Not implemented"

let createCurrencyCode (code:string) :TwoTrack<CurrencyCode,ErrorMessage> = 
    // convert input code to one of the enums
    failwith "Not implemented"


// ===========================
// create the DTO
// ===========================

type PaymentRequestDto = {
    dtoName: string
    dtoEmail: string
    dtoAmount: float
    dtoCurrencyCode: string
    }

let dtoToRequest dto :TwoTrack<PaymentRequest,ErrorMessage> = 
    let nameR = createCustomerName dto.dtoName
    let emailR = createEmailAddress dto.dtoEmail
    // what next
    failwith "Not implemented"



// let's test what we have so far

let goodRequest = {dtoName="Alice"; dtoEmail="ABC@gmail.com"; dtoAmount=1.2; dtoCurrencyCode="USD"}
goodRequest |> dtoToRequest

let badRequest1 = {dtoName=""; dtoEmail="ABC@gmail.com"; dtoAmount=1.2; dtoCurrencyCode="USD"}
badRequest1 |> dtoToRequest

let badCurrencyRequest = {dtoName="Alice"; dtoEmail="ABC@gmail.com"; dtoAmount=1.2; dtoCurrencyCode="???"}
badCurrencyRequest |> dtoToRequest


// ------------------------
// Update the database
// ------------------------

let updateDb (request:PaymentRequest) =
    // do something
    failwith "Not implemented"
    ()

let updateDbR twoTrackInput = teeR updateDb twoTrackInput 

goodRequest |> dtoToRequest |>  updateDbR 

// ------------------------
// log the errors
// ------------------------

let loggerR twoTrackInput = 
    match twoTrackInput with
    | Success (req:PaymentRequest,events) -> 
        failwith "Not implemented"
        events |> List.iter (printfn "LOG INFO %A")
    | Failure errors -> 
        errors |> List.iter (printfn "LOG ERROR %A")
    twoTrackInput 

goodRequest |> dtoToRequest |>  updateDbR |> loggerR 


// ------------------------
// return the response
// ------------------------


let returnMessageR result = 
  match result with
  | Success (obj,events) -> Some obj
  | Failure msgs -> None

goodRequest |> dtoToRequest |>  updateDbR |> loggerR |> returnMessageR


// ------------------------
// final code
// ------------------------

let processPaymentRequest = 
  dtoToRequest 
  >> updateDbR
  >> loggerR
  >> returnMessageR

goodRequest |> processPaymentRequest 
badRequest1 |> processPaymentRequest 
badCurrencyRequest |> processPaymentRequest 



