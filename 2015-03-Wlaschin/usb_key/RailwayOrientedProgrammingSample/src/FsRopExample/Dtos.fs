namespace FsRopExample.Dtos

open System
open FsRopExample.Rop
open FsRopExample.DomainPrimitiveTypes
open FsRopExample.DomainModel

// ============================== 
// DTOs
// ============================== 

/// Represents a DTO that is exposed on the wire.
/// This is a regular POCO class which can be null. 
/// To emulate the C# class, all the properties are initialized to null by default
///
/// Note that in F# you have to make quite an effort to create nullable classes with nullable fields
[<AllowNullLiteralAttribute>]
type CustomerDto() = 
    member val Id = 0 with get, set
    member val FirstName : string = null with get, set
    member val LastName : string = null with get, set
    member val Email : string  = null with get, set


// ============================== 
// DTO Converters
// ============================== 

module DtoConverter =

    /// Convert a DTO into a domain customer.
    ///
    /// We MUST handle the possibility of one or more errors
    /// because the Customer type has stricter constraints than CustomerDto
    /// and the conversion might fail.
    let dtoToCustomer(dto: CustomerDto) =
        if dto = null then 
            fail CustomerIsRequired
        else
            let customerIdOrError = 
                createCustomerId dto.Id

            let nameOrError = 
                createPersonalName
                <!> createFirstName dto.FirstName
                <*> createLastName dto.LastName

            createCustomer 
            <!> customerIdOrError 
            <*> nameOrError
            <*> createEmail dto.Email //inline this one

    /// Convert a domain Customer into a DTO.
    /// There is no possibility of an error 
    /// because the Customer type has stricter constraints than DTO.
    let customerToDto(cust:Customer) =
        // extract the raw int id from the CustomerId wrapper
        let custIdInt = cust.Id |> CustomerId.apply id

        // create the object and set the properties
        let customerDto = CustomerDto()
        customerDto.Id <- custIdInt 
        customerDto.FirstName <- cust.Name.FirstName |> String10.apply id
        customerDto.LastName <- cust.Name.LastName |> String10.apply id
        customerDto.Email <- cust.Email |> EmailAddress.apply id
        customerDto