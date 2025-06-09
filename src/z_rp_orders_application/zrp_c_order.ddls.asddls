@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order BO projection view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity ZRP_C_ORDER
  provider contract transactional_query as projection on ZRP_I_ORDER as Orders
{
    key OrderUuid,
    @Search.defaultSearchElement: true
    OrderId,
    Name,
    @Search.defaultSearchElement: true
    Status,
     @Consumption.valueHelpDefinition: [{ entity: {name: '/DMO/I_Customer', element: 'CustomerID' } }]
    @ObjectModel.text.element: [ 'CustomerName' ]
     @Search.defaultSearchElement: true
    Customer,
    _Customer.last_name as CustomerName,
     @Search.defaultSearchElement: true
    CreationDate,
    CancellationDate,
    CompletionDate,
    @Consumption.valueHelpDefinition: [{ entity: {name: 'I_Country', element: 'Country' } }]
    @Search.defaultSearchElement: true
    DeliveryCountry,
   @Semantics.amount.currencyCode: 'Currency'
    TotalPrice,
     @Consumption.valueHelpDefinition: [{ entity: {name: 'I_Currency', element: 'Currency' } }]
    Currency,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
   @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZRP_CL_COMPLEXITY'
    @EndUserText.label: 'Complexity'
 virtual  Complexity : abap.sstring( 10 ),
    /* Associations */
    _Currency,
    _Customer,
    _Item : redirected to composition child ZRP_C_ITEM,
    _Country
}
