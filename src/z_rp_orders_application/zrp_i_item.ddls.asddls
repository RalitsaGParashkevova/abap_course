@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Item View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZRP_I_ITEM as select from zrp_items

association to parent ZRP_I_ORDER as _Order on $projection.OrderUuid = _Order.OrderUuid

association [0..1] to I_Currency as _Currency on $projection.Currency = _Currency.Currency
{
    key item_uuid as ItemUuid,
    order_uuid as OrderUuid,
    name as Name,
    @Semantics.amount.currencyCode : 'Currency'
    price as Price,
    currency as Currency,
    quantity as Quantity,
    last_changed_by as LastChangedBy,
    last_changed_at as LastChangedAt,
    local_last_changed_at as LocalLastChangedAt,
    
    /* Associations */
    _Order, // Make association public
    _Currency
}
