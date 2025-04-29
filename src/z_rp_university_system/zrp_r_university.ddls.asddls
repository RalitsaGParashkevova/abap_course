@AbapCatalog.sqlViewName: 'ZRP_SQL_UROOT'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'University Data Definition'
@Metadata.ignorePropagatedAnnotations: true
define view ZRP_R_UNIVERSITY 
with parameters
p_university_name : zrp_university_name
as select from zrp_university 
{
    key university_id as UniversityId,
    univeristy_name as UniveristyName,
    univeristy_location as UniveristyLocation
}
where univeristy_name = $parameters.p_university_name
