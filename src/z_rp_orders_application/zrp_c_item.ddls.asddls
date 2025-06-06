@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Item BO projection view'
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZRP_C_ITEM as projection on ZRP_I_ITEM as Item
{
    key ItemUuid,
    key OrderUuid,
    Name,
    @Semantics.amount.currencyCode: 'Currency'
    Price,
    @Consumption.valueHelpDefinition: [{ entity: {name: 'I_Currency', element: 'Currency' } }]
    Currency,
    Quantity,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    /* Associations */
    _Currency,
    _Order : redirected to parent ZRP_C_ORDER
}
