@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order BO projection view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity ZRP_C_ORDER as projection on ZRP_I_ORDER as Orders
{
    key OrderUuid,
    @Search.defaultSearchElement: true
    OrderId,
    Name,
    Status,
     @Consumption.valueHelpDefinition: [{ entity: {name: '/dmo/customer', element: 'CustomerID' } }]
    @ObjectModel.text.element: [ 'CustomerName' ]
     @Search.defaultSearchElement: true
    Customer,
    _Customer.last_name as CustomerName,
    CreationDate,
    CancellationDate,
    CompletionDate,
    @Consumption.valueHelpDefinition: [{ entity: {name: 'I_Country', element: 'Country' } }]
    @ObjectModel.text.element: [ 'CountryName' ]
    @Search.defaultSearchElement: true
    DeliveryCountry,
    _Country.Country as CountryName,
   @Semantics.amount.currencyCode: 'Currency'
    TotalPrice,
     @Consumption.valueHelpDefinition: [{ entity: {name: 'I_Currency', element: 'Currency' } }]
    Currency,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    Complexity,
    /* Associations */
    _Currency,
    _Customer,
    _Item : redirected to composition child ZRP_C_ITEM,
    _Country
}
