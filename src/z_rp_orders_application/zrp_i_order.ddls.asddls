@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZRP_I_ORDER as select from zrp_orders as Orders

composition [1..*] of ZRP_I_ITEM as _Item 

association [0..1] to /dmo/customer as _Customer on $projection.Customer = _Customer.customer_id
association [0..1] to I_Currency as _Currency on $projection.Currency = _Currency.Currency
association [0..1] to I_Country as _Country on $projection.DeliveryCountry = _Country.Country
association [1..1] to ZRP_I_ORDER_COMPLEXITY as _Complexity on $projection.OrderUuid = _Complexity.order_uuid
{
    key order_uuid as OrderUuid,
    order_id as OrderId,
    name as Name,
    status as Status,
    customer as Customer,
    creation_date as CreationDate,
    cancellation_date as CancellationDate,
    completion_date as CompletionDate,
    delivery_country as DeliveryCountry,
    @Semantics.amount.currencyCode : 'Currency'
    total_price as TotalPrice,
    currency as Currency,
    @Semantics.user.lastChangedBy: true
    last_changed_by as LastChangedBy,
    @Semantics.systemDateTime.lastChangedAt: true
    last_changed_at as LastChangedAt,
    @Semantics.systemDateTime.localInstanceLastChangedAt: true
    local_last_changed_at as LocalLastChangedAt,
     _Complexity.ItemCount as ItemCount,
    case 
when _Complexity.ItemCount < 3 
then 'Easy'
when _Complexity.ItemCount > 2 and _Complexity.ItemCount < 5
then 'Medium'
else
'Complex'
end as Complexity,
    
    /* Associations */
   // Make association public
   _Item,
    _Currency,
    _Customer,
    _Country,
    _Complexity
     
}
